module Tptcc.Lexer
  ( lexC
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isHexDigit, toLower, toUpper)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Numeric (readHex)

import Tptcc.Token

lexC :: String -> [Token]
lexC = go 1 1
  where
    go line column source =
      case dropSpaceAndComments line column source of
        (line', _column', "") -> [mkToken "EOF" (ValueString "EOF") line' 1]
        (line', column', rest) ->
          let (token, line'', column'', rest') = nextToken line' column' rest
           in token : go line'' column'' rest'

nextToken :: Int -> Int -> String -> (Token, Int, Int, String)
nextToken line column source =
  firstMatch
    [ storageClass
    , operator
    , identifier
    , stringLiteral
    , characterLiteral
    , hexInteger
    , decimalInteger
    , punctuation
    , otherToken
    ]
  where
    firstMatch [] = error "lexer internal error: no token matched"
    firstMatch (scanner : scanners) =
      case scanner line column source of
        Just match -> match
        Nothing -> firstMatch scanners

type Scanner = Int -> Int -> String -> Maybe (Token, Int, Int, String)

storageClass :: Scanner
storageClass = keywordPrefixToken ["auto", "extern", "register", "static", "typedef"] (const "STORAGE_CLASS") isAlphaNum

keywordPrefixToken :: [String] -> (String -> String) -> (Char -> Bool) -> Scanner
keywordPrefixToken wordsToMatch nameOf continuation line column source =
  case firstPrefix wordsToMatch source of
    Just word
      | not (continues word) ->
          Just (mkToken (nameOf word) (ValueString (Text.pack word)) line column, line, column + length word, drop (length word) source)
    _ -> Nothing
  where
    continues word =
      case drop (length word) source of
        next : _ -> continuation next
        [] -> False

firstPrefix :: [String] -> String -> Maybe String
firstPrefix [] _ = Nothing
firstPrefix (word : wordsToMatch) source =
  case splitAt (length word) source of
    (prefix, _) | prefix == word -> Just word
    _ -> firstPrefix wordsToMatch source

operator :: Scanner
operator line column source =
  case matchOperator source of
    Just op -> Just (mkToken (map toUpper op) (ValueString (Text.pack op)) line column, line, column + length op, drop (length op) source)
    Nothing -> Nothing

matchOperator :: String -> Maybe String
matchOperator source =
  case source of
    '&' : '&' : _ -> Just "&&"
    '&' : '=' : _ -> Just "&="
    '&' : _ -> Just "&"
    '|' : '|' : _ -> Just "||"
    '|' : '=' : _ -> Just "|="
    '|' : _ -> Just "|"
    '=' : '=' : _ -> Just "=="
    '=' : _ -> Just "="
    '!' : '=' : _ -> Just "!="
    '!' : _ -> Just "!"
    '<' : '<' : '=' : _ -> Just "<<="
    '<' : '<' : _ -> Just "<<"
    '<' : '=' : _ -> Just "<="
    '<' : _ -> Just "<"
    '>' : '>' : '=' : _ -> Just ">>="
    '>' : '>' : _ -> Just ">>"
    '>' : '=' : _ -> Just ">="
    '>' : _ -> Just ">"
    '+' : '+' : _ -> Just "++"
    '+' : '=' : _ -> Just "+="
    '+' : _ -> Just "+"
    '-' : '-' : _ -> Just "--"
    '-' : '>' : _ -> Just "->"
    '-' : '=' : _ -> Just "-="
    '-' : _ -> Just "-"
    '.' : '.' : '.' : _ -> Just "..."
    '.' : _ -> Just "."
    '*' : '=' : _ -> Just "*="
    '*' : _ -> Just "*"
    '/' : '=' : _ -> Just "/="
    '/' : _ -> Just "/"
    '%' : '=' : _ -> Just "%="
    '%' : _ -> Just "%"
    '^' : '=' : _ -> Just "^="
    '^' : _ -> Just "^"
    '?' : _ -> Just "?"
    ':' : _ -> Just ":"
    '~' : _ -> Just "~"
    _ -> Nothing

identifier :: Scanner
identifier line column source =
  case source of
    c : _
      | isIdentStart c ->
          let ident = takeWhile isIdentContinue source
              name = Map.findWithDefault "ID" ident keywordTokenNames
           in Just (mkToken name (ValueString (Text.pack ident)) line column, line, column + length ident, drop (length ident) source)
    _ -> Nothing

keywordTokenNames :: Map.Map String String
keywordTokenNames =
  Map.fromList
    [ ("if", "IF")
    , ("else", "ELSE")
    , ("for", "FOR")
    , ("while", "WHILE")
    , ("do", "DO")
    , ("return", "RETURN")
    , ("break", "BREAK")
    , ("continue", "CONTINUE")
    , ("goto", "GOTO")
    , ("switch", "SWITCH")
    , ("case", "CASE")
    , ("default", "DEFAULT")
    , ("asm", "ASM")
    , ("sizeof", "SIZEOF")
    , ("auto", "STORAGE_CLASS")
    , ("extern", "STORAGE_CLASS")
    , ("register", "STORAGE_CLASS")
    , ("static", "STORAGE_CLASS")
    , ("typedef", "STORAGE_CLASS")
    , ("const", "TYPE_QUALIFIER")
    , ("volatile", "TYPE_QUALIFIER")
    , ("int", "TYPE_SPECIFIER")
    , ("char", "TYPE_SPECIFIER")
    , ("short", "TYPE_SPECIFIER")
    , ("void", "TYPE_SPECIFIER")
    , ("long", "TYPE_SPECIFIER")
    , ("unsigned", "TYPE_SPECIFIER")
    , ("signed", "TYPE_SPECIFIER")
    , ("struct", "TYPE_SPECIFIER")
    , ("union", "TYPE_SPECIFIER")
    , ("enum", "TYPE_SPECIFIER")
    ]

stringLiteral :: Scanner
stringLiteral line column source =
  case source of
    '"' : rest ->
      let body = takeWhile (/= '"') rest
          consumed = 2 + length body
       in case drop (length body) rest of
            '"' : _ ->
              let raw = '"' : body ++ "\""
               in Just (mkToken "STRING_LITERAL" (ValueString (Text.pack (decodeStringEscapes raw))) line column, line, column + consumed, drop consumed source)
            _ -> Nothing
    _ -> Nothing

characterLiteral :: Scanner
characterLiteral line column source =
  case source of
    '\'' : rest ->
      let body = takeWhile (/= '\'') rest
          consumed = 2 + length body
       in case drop (length body) rest of
            '\'' : _ ->
              let raw = '\'' : body ++ "'"
               in Just (mkToken "CHARACTER" (decodeCharacter raw) line column, line, column + consumed, drop consumed source)
            _ -> Nothing
    _ -> Nothing

hexInteger :: Scanner
hexInteger line column source =
  case source of
    '0' : x : rest
      | x == 'x' ->
          let digits = takeWhile isHexDigit rest
              suffix = case drop (length digits) rest of
                'u' : _ -> "u"
                _ -> ""
           in if null digits
                then Nothing
                else
                  let raw = "0x" ++ digits ++ suffix
                      ty = if null suffix then "INT" else "UNSIGNED_INT"
                   in Just (mkToken ty (ValueInt (parseInteger raw)) line column, line, column + length raw, drop (length raw) source)
    _ -> Nothing

decimalInteger :: Scanner
decimalInteger line column source =
  let digits = takeWhile isDigit source
   in if null digits
        then Nothing
        else
          let suffix = case drop (length digits) source of
                'u' : _ -> "u"
                _ -> ""
              raw = digits ++ suffix
              ty = if null suffix then "INT" else "UNSIGNED_INT"
           in Just (mkToken ty (ValueInt (parseInteger raw)) line column, line, column + length raw, drop (length raw) source)

punctuation :: Scanner
punctuation line column source =
  case source of
    c : rest
      | c `elem` ("(){};,[]" :: String) ->
          Just (mkToken [c] (ValueString (Text.singleton c)) line column, line, column + 1, rest)
    _ -> Nothing

otherToken :: Scanner
otherToken line column source =
  let other = takeWhile (not . isSpaceLua) source
   in if null other
        then Nothing
        else Just (mkToken "OTHER" (ValueString (Text.pack other)) line column, line, column + length other, drop (length other) source)

dropSpaceAndComments :: Int -> Int -> String -> (Int, Int, String)
dropSpaceAndComments line column source =
  case source of
    c : rest
      | isSpaceLua c ->
          let (line', column') = advance line column c
           in dropSpaceAndComments line' column' rest
    '/' : '/' : rest ->
      let comment = takeWhile (/= '\n') rest
       in dropSpaceAndComments line (column + 2 + length comment) (drop (length comment) rest)
    '/' : '*' : rest ->
      let (line', column', rest') = consumeBlockComment line (column + 2) rest
       in dropSpaceAndComments line' column' rest'
    _ -> (line, column, source)

consumeBlockComment :: Int -> Int -> String -> (Int, Int, String)
consumeBlockComment line column source =
  case source of
    '*' : '/' : rest -> (line, column + 2, rest)
    c : rest ->
      let (line', column') = advance line column c
       in consumeBlockComment line' column' rest
    [] -> (line, column, [])

advance :: Int -> Int -> Char -> (Int, Int)
advance line _ '\n' = (line + 1, 1)
advance line column _ = (line, column + 1)

isSpaceLua :: Char -> Bool
isSpaceLua c = c `elem` (" \n\t\r" :: String)

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_'

isIdentContinue :: Char -> Bool
isIdentContinue c = isAlphaNum c || c == '_'

decodeStringEscapes :: String -> String
decodeStringEscapes [] = []
decodeStringEscapes ('\\' : 'n' : rest) = '\n' : decodeStringEscapes rest
decodeStringEscapes ('\\' : '\\' : rest) = '\\' : decodeStringEscapes rest
decodeStringEscapes (c : rest) = c : decodeStringEscapes rest

decodeCharacter :: String -> TokenValue
decodeCharacter "'\\n'" = ValueInt 10
decodeCharacter "'\\\\'" = ValueInt 92
decodeCharacter [_, c, _] = ValueInt (fromIntegral (fromEnum c))
decodeCharacter raw = ValueString (Text.pack raw)

parseInteger :: String -> Integer
parseInteger raw =
  let stripped = case reverse raw of
        'u' : rest -> reverse rest
        _ -> raw
   in case stripped of
        '0' : x : digits
          | toLower x == 'x' ->
              case readHex digits of
                (n, _) : _ -> n
                [] -> 0
        _ -> read stripped

mkToken :: String -> TokenValue -> Int -> Int -> Token
mkToken name value line column =
  Token
    { tokenTypeId = tokenTypeIdFor name
    , tokenName = Text.pack name
    , tokenValue = value
    , tokenPos = SourcePos {row = line, col = column}
    }

tokenTypeIdFor :: String -> Int
tokenTypeIdFor name =
  case Map.lookup name tokenTypeIds of
    Just typeId -> typeId
    Nothing -> error ("invalid token type: " ++ name)

tokenTypeIds :: Map.Map String Int
tokenTypeIds = Map.fromList (zip tokenNames [1 ..])

tokenNames :: [String]
tokenNames =
  [ "ID"
  , "INT"
  , "FLOAT"
  , "("
  , ")"
  , "{"
  , "}"
  , "IF"
  , "ELSE"
  , "FOR"
  , "WHILE"
  , ";"
  , "="
  , "=="
  , "+"
  , "-"
  , "*"
  , "/"
  , "%"
  , "!="
  , "<"
  , ">"
  , "<="
  , ">="
  , "&&"
  , "||"
  , "!"
  , "TYPE_SPECIFIER"
  , "EOF"
  , ","
  , "RETURN"
  , "["
  , "]"
  , "STRING_LITERAL"
  , "&"
  , "CHARACTER"
  , "?"
  , ":"
  , "STORAGE_CLASS"
  , "++"
  , "--"
  , "->"
  , "<<"
  , ">>"
  , "^"
  , "|"
  , "BREAK"
  , "CONTINUE"
  , "SIZEOF"
  , "SWITCH"
  , "CASE"
  , "DEFAULT"
  , "."
  , "..."
  , "+="
  , "-="
  , "*="
  , "/="
  , "%="
  , "&="
  , "|="
  , "^="
  , "<<="
  , ">>="
  , "UNSIGNED_INT"
  , "OTHER"
  , "ASM"
  , "~"
  , "DO"
  , "TYPE_QUALIFIER"
  , "GOTO"
  ]
