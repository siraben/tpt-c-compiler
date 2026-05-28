module Tptcc.Preprocessor
  ( preprocessFile
  ) where

import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, isSpace)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import System.FilePath ((</>), takeDirectory)
import qualified Text.Megaparsec as MP

data Macro
  = ObjectMacro String
  | FunctionMacro [String] String
  deriving (Eq, Show)

data Conditional = Conditional
  { parentActive :: Bool
  , branchActive :: Bool
  , branchTaken :: Bool
  }
  deriving (Eq, Show)

data PPState = PPState
  { macros :: Map.Map String Macro
  , conditionals :: [Conditional]
  }
  deriving (Eq, Show)

preprocessFile :: FilePath -> IO String
preprocessFile path = fst <$> preprocess path initialState

initialState :: PPState
initialState = PPState Map.empty []

preprocess :: FilePath -> PPState -> IO (String, PPState)
preprocess path state0 = do
  source <- readFile path
  (chunks, state1) <- foldLines (takeDirectory path) (lines source) ([], state0)
  pure (concat (reverse chunks), state1)

foldLines :: FilePath -> [String] -> ([String], PPState) -> IO ([String], PPState)
foldLines _ [] acc = pure acc
foldLines dir (line : rest) (chunks, state0)
  | "#" `isPrefixTrimmed` line = do
      (included, state1) <- directive dir (trimStart (drop 1 (trimStart line))) state0
      foldLines dir rest (included : chunks, state1)
  | isActive state0 =
      foldLines dir rest ((expandMacros (macros state0) line <> "\n") : chunks, state0)
  | otherwise = foldLines dir rest (chunks, state0)

directive :: FilePath -> String -> PPState -> IO (String, PPState)
directive dir raw state0 =
  case words raw of
    ("include" : rest)
      | isActive state0 -> do
          let includePath = parseIncludePath (unwords rest)
          (included, state1) <- preprocess (dir </> includePath) state0
          pure (included, state1)
      | otherwise -> pure ("", state0)
    ("define" : rest)
      | isActive state0 -> pure ("", defineMacro (unwords rest) state0)
      | otherwise -> pure ("", state0)
    ("undef" : name : _)
      | isActive state0 -> pure ("", state0 {macros = Map.delete name (macros state0)})
      | otherwise -> pure ("", state0)
    ("ifdef" : name : _) -> pure ("", pushConditional (Map.member name (macros state0)) state0)
    ("ifndef" : name : _) -> pure ("", pushConditional (not (Map.member name (macros state0))) state0)
    ("if" : rest) -> pure ("", pushConditional (evalIfExpression (macros state0) (unwords rest) /= 0) state0)
    ("elif" : rest) -> pure ("", switchElif (evalIfExpression (macros state0) (unwords rest) /= 0) state0)
    ("else" : _) -> pure ("", switchElse state0)
    ("endif" : _) -> pure ("", popConditional state0)
    _ -> pure ("", state0)

defineMacro :: String -> PPState -> PPState
defineMacro raw state0 =
  let (namePart, rest0) = span isMacroNameChar raw
   in case rest0 of
        '(' : rest
          | not (null namePart) ->
              let (paramsRaw, afterParams) = break (== ')') rest
                  params = splitCommaNames paramsRaw
                  body = trimStart (drop 1 afterParams)
               in state0 {macros = Map.insert namePart (FunctionMacro params body) (macros state0)}
        _ | not (null namePart) ->
          state0 {macros = Map.insert namePart (ObjectMacro (trimStart rest0)) (macros state0)}
        _ -> state0

expandMacros :: Map.Map String Macro -> String -> String
expandMacros table = go
  where
    go [] = []
    go input@(c : rest)
      | isIdentStart c =
          let (name, suffix) = span isMacroNameChar input
           in case Map.lookup name table of
                Just (ObjectMacro body) -> body <> go suffix
                Just (FunctionMacro params body) ->
                  case trimStart suffix of
                    '(' : callRest ->
                      case parseMacroArguments callRest of
                        Just (args, afterCall) -> expandMacros table (substitute params args body) <> go afterCall
                        Nothing -> name <> go suffix
                    _ -> name <> go suffix
                Nothing -> name <> go suffix
      | c == '"' =
          let (literal, suffix) = spanString rest
           in c : literal <> go suffix
      | c == '\'' =
          let (literal, suffix) = spanChar rest
           in c : literal <> go suffix
      | otherwise = c : go rest

type MacroParser = MP.Parsec Void String

parseMacroArguments :: String -> Maybe ([String], String)
parseMacroArguments input =
  case MP.runParser macroArgumentsParser "<macro-arguments>" input of
    Right result -> Just result
    Left _ -> Nothing

macroArgumentsParser :: MacroParser ([String], String)
macroArgumentsParser = do
  args <-
    ([] <$ MP.single ')')
      MP.<|> do
        first <- macroArgument
        rest <- MP.many (MP.single ',' *> macroArgument)
        _ <- MP.single ')'
        pure (map trim (first : rest))
  remaining <- MP.getInput
  pure (args, remaining)

macroArgument :: MacroParser String
macroArgument =
  concat <$> MP.many macroArgumentPiece

macroArgumentPiece :: MacroParser String
macroArgumentPiece =
  quoted '"'
    MP.<|> quoted '\''
    MP.<|> parenthesized
    MP.<|> normalArgumentChar

parenthesized :: MacroParser String
parenthesized = do
  _ <- MP.single '('
  body <- concat <$> MP.many parenthesizedPiece
  _ <- MP.single ')'
  pure ("(" <> body <> ")")

parenthesizedPiece :: MacroParser String
parenthesizedPiece =
  quoted '"'
    MP.<|> quoted '\''
    MP.<|> parenthesized
    MP.<|> normalParenthesizedChar

quoted :: Char -> MacroParser String
quoted quote = do
  _ <- MP.single quote
  body <- concat <$> MP.manyTill quotedPiece (MP.single quote)
  pure (quote : body <> [quote])
  where
    quotedPiece =
      escapedChar MP.<|> ((: []) <$> MP.satisfy (/= quote))

escapedChar :: MacroParser String
escapedChar = do
  _ <- MP.single '\\'
  escaped <- MP.anySingle
  pure ['\\', escaped]

normalArgumentChar :: MacroParser String
normalArgumentChar =
  (: []) <$> MP.satisfy (`notElem` [',', ')', '('])

normalParenthesizedChar :: MacroParser String
normalParenthesizedChar =
  (: []) <$> MP.satisfy (`notElem` [')', '('])

substitute :: [String] -> [String] -> String -> String
substitute params args =
  expandMacros (Map.fromList (zip params (map ObjectMacro args)))

pushConditional :: Bool -> PPState -> PPState
pushConditional condition state0 =
  let parent = isActive state0
      active = parent && condition
   in state0 {conditionals = Conditional parent active active : conditionals state0}

switchElif :: Bool -> PPState -> PPState
switchElif condition state0 =
  case conditionals state0 of
    current : rest ->
      let active = parentActive current && not (branchTaken current) && condition
       in state0 {conditionals = current {branchActive = active, branchTaken = branchTaken current || active} : rest}
    [] -> state0

switchElse :: PPState -> PPState
switchElse state0 =
  case conditionals state0 of
    current : rest ->
      let active = parentActive current && not (branchTaken current)
       in state0 {conditionals = current {branchActive = active, branchTaken = True} : rest}
    [] -> state0

popConditional :: PPState -> PPState
popConditional state0 =
  case conditionals state0 of
    _ : rest -> state0 {conditionals = rest}
    [] -> state0

isActive :: PPState -> Bool
isActive state0 = all branchActive (conditionals state0)

evalIfExpression :: Map.Map String Macro -> String -> Integer
evalIfExpression table raw =
  case reads (expandDefined table (expandMacros table raw)) of
    [(value, "")] -> value
    _ -> 0

expandDefined :: Map.Map String Macro -> String -> String
expandDefined table raw =
  case words raw of
    ["defined", name] -> if Map.member (trimDefinedName name) table then "1" else "0"
    _ -> raw

trimDefinedName :: String -> String
trimDefinedName name =
  case trim name of
    '(' : rest -> takeWhile (/= ')') rest
    other -> other

parseIncludePath :: String -> FilePath
parseIncludePath raw =
  case trim raw of
    '"' : rest -> takeWhile (/= '"') rest
    '<' : rest -> takeWhile (/= '>') rest
    other -> other

splitCommaNames :: String -> [String]
splitCommaNames "" = []
splitCommaNames raw = map trim (splitCommas raw)

splitCommas :: String -> [String]
splitCommas "" = [""]
splitCommas raw =
  let (prefix, suffix) = break (== ',') raw
   in case suffix of
        [] -> [prefix]
        _ : rest -> prefix : splitCommas rest

spanString :: String -> (String, String)
spanString [] = ([], [])
spanString ('\\' : c : rest) =
  let (literal, suffix) = spanString rest
   in ('\\' : c : literal, suffix)
spanString ('"' : rest) = ("\"", rest)
spanString (c : rest) =
  let (literal, suffix) = spanString rest
   in (c : literal, suffix)

spanChar :: String -> (String, String)
spanChar [] = ([], [])
spanChar ('\\' : c : rest) =
  let (literal, suffix) = spanChar rest
   in ('\\' : c : literal, suffix)
spanChar ('\'' : rest) = ("'", rest)
spanChar (c : rest) =
  let (literal, suffix) = spanChar rest
   in (c : literal, suffix)

trim :: String -> String
trim = reverse . trimStart . reverse . trimStart

trimStart :: String -> String
trimStart = dropWhile isSpace

isPrefixTrimmed :: String -> String -> Bool
isPrefixTrimmed prefix value = prefix == take (length prefix) (trimStart value)

isIdentStart :: Char -> Bool
isIdentStart c = c == '_' || isAsciiUpper c || isAsciiLower c

isMacroNameChar :: Char -> Bool
isMacroNameChar c = isIdentStart c || isAlphaNum c
