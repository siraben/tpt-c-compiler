module Tptcc.Lexer
  ( lexC
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isHexDigit, toLower, toUpper)
import Data.List (isPrefixOf)
import Numeric (readHex)

import Tptcc.Token

lexC :: String -> [Token]
lexC input = go 1 1 input
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
    [ reserved
    , storageClass
    , operator
    , typeSpecifier
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

reserved :: Scanner
reserved line column source =
  keywordToken
    [ "if"
    , "else"
    , "for"
    , "while"
    , "return"
    , "break"
    , "continue"
    , "switch"
    , "case"
    , "default"
    , "asm"
    ]
    (\word -> map toUpper word)
    isIdentContinue
    line
    column
    source

storageClass :: Scanner
storageClass line column source =
  keywordToken ["auto", "register", "static", "typedef"] (const "STORAGE_CLASS") isAlphaNum line column source

typeSpecifier :: Scanner
typeSpecifier line column source =
  keywordToken
    ["int", "char", "void", "long", "unsigned", "signed", "struct", "union", "enum"]
    (const "TYPE_SPECIFIER")
    isIdentContinue
    line
    column
    source

keywordToken :: [String] -> (String -> String) -> (Char -> Bool) -> Scanner
keywordToken wordsToMatch nameOf continuation line column source =
  case firstKeyword wordsToMatch source of
    Just word
      | not (continues word) ->
          Just (mkToken (nameOf word) (ValueString word) line column, line, column + length word, drop (length word) source)
    _ -> Nothing
  where
    continues word =
      case drop (length word) source of
        next : _ -> continuation next
        [] -> False

firstKeyword :: [String] -> String -> Maybe String
firstKeyword [] _ = Nothing
firstKeyword (word : wordsToMatch) source
  | word `isPrefixOf` source = Just word
  | otherwise = firstKeyword wordsToMatch source

operator :: Scanner
operator line column source =
  case matchOperator source of
    Just op -> Just (mkToken (map toUpper op) (ValueString op) line column, line, column + length op, drop (length op) source)
    Nothing -> Nothing

matchOperator :: String -> Maybe String
matchOperator source = firstOp operatorPatterns
  where
    firstOp [] = Nothing
    firstOp (op : ops)
      | op == "<<" && "<<=" `isPrefixOf` source = firstOp ops
      | op == ">>" && ">>=" `isPrefixOf` source = firstOp ops
      | op `isPrefixOf` source = Just op
      | otherwise = firstOp ops

operatorPatterns :: [String]
operatorPatterns =
  [ "&&"
  , "||"
  , "=="
  , "!="
  , "<="
  , ">="
  , "++"
  , "--"
  , "->"
  , "..."
  , "<<"
  , ">>"
  , "sizeof"
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
  , "+"
  , "-"
  , "*"
  , "/"
  , "%"
  , "!"
  , "<"
  , "="
  , ">"
  , "&"
  , "?"
  , ":"
  , "."
  , "^"
  , "|"
  , "~"
  ]

identifier :: Scanner
identifier line column source =
  case source of
    c : _
      | isIdentStart c ->
          let ident = takeWhile isIdentContinue source
           in Just (mkToken "ID" (ValueString ident) line column, line, column + length ident, drop (length ident) source)
    _ -> Nothing

stringLiteral :: Scanner
stringLiteral line column source =
  case source of
    '"' : rest ->
      let body = takeWhile (/= '"') rest
          consumed = 2 + length body
       in case drop (length body) rest of
            '"' : _ ->
              let raw = '"' : body ++ "\""
               in Just (mkToken "STRING_LITERAL" (ValueString (decodeStringEscapes raw)) line column, line, column + consumed, drop consumed source)
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
          Just (mkToken [c] (ValueString [c]) line column, line, column + 1, rest)
    _ -> Nothing

otherToken :: Scanner
otherToken line column source =
  let other = takeWhile (not . isSpaceLua) source
   in if null other
        then Nothing
        else Just (mkToken "OTHER" (ValueString other) line column, line, column + length other, drop (length other) source)

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
decodeCharacter "'\\\\'" = ValueString "'\\'"
decodeCharacter raw = ValueString raw

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
    , tokenName = name
    , tokenValue = value
    , tokenPos = SourcePos {row = line, col = column}
    }

tokenTypeIdFor :: String -> Int
tokenTypeIdFor name =
  case lookup name tokenTypes of
    Just typeId -> typeId
    Nothing -> error ("invalid token type: " ++ name)

tokenTypes :: [(String, Int)]
tokenTypes =
  [ ("ID", 1)
  , ("INT", 2)
  , ("FLOAT", 3)
  , ("(", 4)
  , (")", 5)
  , ("{", 6)
  , ("}", 7)
  , ("IF", 8)
  , ("ELSE", 9)
  , ("FOR", 10)
  , ("WHILE", 11)
  , (";", 12)
  , ("=", 13)
  , ("==", 14)
  , ("+", 15)
  , ("-", 16)
  , ("*", 17)
  , ("/", 18)
  , ("%", 19)
  , ("!=", 20)
  , ("<", 21)
  , (">", 22)
  , ("<=", 23)
  , (">=", 24)
  , ("&&", 25)
  , ("||", 26)
  , ("!", 27)
  , ("TYPE_SPECIFIER", 28)
  , ("EOF", 29)
  , (",", 30)
  , ("RETURN", 31)
  , ("[", 32)
  , ("]", 33)
  , ("STRING_LITERAL", 34)
  , ("&", 35)
  , ("CHARACTER", 36)
  , ("?", 37)
  , (":", 38)
  , ("STORAGE_CLASS", 39)
  , ("++", 40)
  , ("--", 41)
  , ("->", 42)
  , ("<<", 43)
  , (">>", 44)
  , ("^", 45)
  , ("|", 46)
  , ("BREAK", 47)
  , ("CONTINUE", 48)
  , ("SIZEOF", 49)
  , ("SWITCH", 50)
  , ("CASE", 51)
  , ("DEFAULT", 52)
  , (".", 53)
  , ("+=", 54)
  , ("-=", 55)
  , ("*=", 56)
  , ("/=", 57)
  , ("%=", 58)
  , ("&=", 59)
  , ("|=", 60)
  , ("^=", 61)
  , ("<<=", 62)
  , (">>=", 63)
  , ("UNSIGNED_INT", 64)
  , ("OTHER", 65)
  , ("ASM", 66)
  , ("~", 67)
  ]
