module Main (main) where

import Data.List (intercalate, sort, sortOn)
import qualified Data.Text as Text
import Numeric (showHex)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath (replaceExtension)

import Tptcc.Ast (renderAst)
import Tptcc.CodeGen (CodeGenOptions (..), defaultCodeGenOptions, dumpNativeAsmOptimized, dumpNativeAsmOptimizedWithOptions, dumpNativeAsmUnoptimized)
import Tptcc.CType (renderTypePretty)
import Tptcc.IRGlobal (dumpIRGlobals)
import Tptcc.IRSimpleTac (dumpSimpleTac)
import Tptcc.Lexer (lexC)
import Tptcc.Operand (Operand (..), renderOperandValue)
import qualified Tptcc.Parser as Parser
import Tptcc.Preprocessor (preprocessFile)
import Tptcc.SSA (dumpSSA)
import Tptcc.SymbolTable (Symbol (..), defaultSymbols)
import Tptcc.Token (SourcePos (..), Token (..), TokenValue (..))
import qualified Tptcc.TypeChecker as TypeChecker

usage :: String
usage =
  "Usage: tptcc-hs input.c [--output output.asm] [--size total-memory-size] "
    <> "[--term-width width] [--term-height height] [--offset offset] "
    <> "[--symbols symbols.json] [--breakpoints \"[5, 8, 13]\"] "
    <> "[--dump-ssa input.c]"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--dump-tokens", input] -> dumpTokens input
    ["--dump-ast", input] -> dumpAst input
    ["--dump-type-events", input] -> dumpTypeEvents input
    ["--dump-ir-globals", input] -> dumpIRGlobalEvents input
    ["--dump-simple-tac", input] -> dumpSimpleTacEvents input
    ["--dump-ssa", input] -> dumpSSAEvents input
    ["--dump-native-asm-unoptimized", input] -> dumpNativeAsmUnoptimizedEvents input
    ["--dump-native-asm-optimized", input] -> dumpNativeAsmOptimizedEvents input
    ["--dump-default-symbols"] -> dumpDefaultSymbols
    _ -> runCompiler args

data CompileConfig = CompileConfig
  { compileInput :: FilePath
  , compileOutput :: FilePath
  , compileOptions :: CodeGenOptions
  , compileSymbolsOutput :: Maybe FilePath
  }
  deriving (Eq, Show)

data CompileParseResult
  = NativeConfig CompileConfig
  | ParseError String
  deriving (Eq, Show)

runCompiler :: [String] -> IO ()
runCompiler args =
  case parseCompileArgs args of
    NativeConfig config -> runNativeCompiler config
    ParseError err -> do
      putStrLn err
      putStrLn usage
      exitWith (ExitFailure 1)

runNativeCompiler :: CompileConfig -> IO ()
runNativeCompiler config = do
  source <- preprocessFile (compileInput config)
  case Parser.parse (lexC source) >>= dumpNativeAsmOptimizedWithOptions (compileOptions config) of
    Left err -> do
      putStrLn err
      exitWith (ExitFailure 1)
    Right asm -> do
      writeFile (compileOutput config) asm
      maybe (pure ()) (const (putStrLn "[ERROR] dkjson not found, symbols cannot be exported to json")) (compileSymbolsOutput config)

parseCompileArgs :: [String] -> CompileParseResult
parseCompileArgs [] = ParseError "missing input file"
parseCompileArgs (input : rest) =
  parseOptions
    rest
    CompileConfig
      { compileInput = input
      , compileOutput = replaceExtension input "asm"
      , compileOptions = defaultCodeGenOptions
      , compileSymbolsOutput = Nothing
      }

parseOptions :: [String] -> CompileConfig -> CompileParseResult
parseOptions [] config = NativeConfig config
parseOptions (flag : value : rest) config =
  case flag of
    "--output" -> parseOptions rest config {compileOutput = value}
    "--symbols" -> parseOptions rest config {compileSymbolsOutput = Just value}
    "--breakpoints" -> updateBreakpoints value
    "--size" -> updateInteger value (\n opts -> opts {codeGenMemorySize = n - 1})
    "--term-width" -> updateInteger value (\n opts -> opts {codeGenTermWidth = n})
    "--term-height" -> updateInteger value (\n opts -> opts {codeGenTermHeight = n})
    "--offset" -> updateInteger value (\n opts -> opts {codeGenGlobalAddr = codeGenGlobalAddr opts + n})
    _ -> ParseError ("unknown option: " <> flag)
  where
    updateInteger raw update =
      case reads raw of
        [(n, "")] -> parseOptions rest config {compileOptions = update n (compileOptions config)}
        _ -> ParseError ("invalid integer for " <> flag <> ": " <> raw)
    updateBreakpoints raw =
      case parseBreakpointList raw of
        Just values -> parseOptions rest config {compileOptions = (compileOptions config) {codeGenBreakpoints = sort values}}
        Nothing -> ParseError ("invalid breakpoint list: " <> raw)
parseOptions [flag] _ = ParseError ("missing value for " <> flag)

parseBreakpointList :: String -> Maybe [Integer]
parseBreakpointList raw =
  case raw of
    '[' : rest ->
      case reverse rest of
        ']' : insideReversed -> parseBreakpointValues (reverse insideReversed)
        _ -> Nothing
    _ -> Nothing

parseBreakpointValues :: String -> Maybe [Integer]
parseBreakpointValues "" = Just []
parseBreakpointValues value =
  traverse parseInteger (splitCommas value)

parseInteger :: String -> Maybe Integer
parseInteger raw =
  case reads raw of
    [(value, "")] -> Just value
    _ -> Nothing

splitCommas :: String -> [String]
splitCommas "" = []
splitCommas value =
  let (prefix, suffix) = break (== ',') value
   in case suffix of
        [] -> [prefix]
        _ : rest -> prefix : splitCommas rest

dumpNativeAsmOptimizedEvents :: FilePath -> IO ()
dumpNativeAsmOptimizedEvents input = do
  source <- preprocessFile input
  case Parser.parse (lexC source) >>= dumpNativeAsmOptimized of
    Left err -> do
      putStrLn err
      exitWith (ExitFailure 1)
    Right asm -> putStr asm

dumpNativeAsmUnoptimizedEvents :: FilePath -> IO ()
dumpNativeAsmUnoptimizedEvents input = do
  source <- preprocessFile input
  case Parser.parse (lexC source) >>= dumpNativeAsmUnoptimized of
    Left err -> do
      putStrLn err
      exitWith (ExitFailure 1)
    Right asm -> putStr asm

dumpSimpleTacEvents :: FilePath -> IO ()
dumpSimpleTacEvents input = do
  source <- preprocessFile input
  case Parser.parse (lexC source) >>= dumpSimpleTac of
    Left err -> do
      putStrLn err
      exitWith (ExitFailure 1)
    Right events -> mapM_ putStrLn events

dumpSSAEvents :: FilePath -> IO ()
dumpSSAEvents input = do
  source <- preprocessFile input
  case Parser.parse (lexC source) >>= dumpSSA of
    Left err -> do
      putStrLn err
      exitWith (ExitFailure 1)
    Right events -> mapM_ putStrLn events

dumpIRGlobalEvents :: FilePath -> IO ()
dumpIRGlobalEvents input = do
  source <- preprocessFile input
  case Parser.parse (lexC source) >>= dumpIRGlobals of
    Left err -> do
      putStrLn err
      exitWith (ExitFailure 1)
    Right events -> mapM_ putStrLn events

dumpTypeEvents :: FilePath -> IO ()
dumpTypeEvents input = do
  source <- preprocessFile input
  case Parser.parse (lexC source) >>= TypeChecker.typeEvents of
    Left err -> do
      putStrLn err
      exitWith (ExitFailure 1)
    Right events -> mapM_ putStrLn events

dumpAst :: FilePath -> IO ()
dumpAst input = do
  source <- preprocessFile input
  case Parser.parse (lexC source) of
    Left err -> do
      putStrLn err
      exitWith (ExitFailure 1)
    Right ast -> putStr (renderAst ast)

dumpDefaultSymbols :: IO ()
dumpDefaultSymbols =
  mapM_ (putStrLn . renderDefaultSymbol) (sortOn fst defaultSymbols)

renderDefaultSymbol :: (Text.Text, Symbol) -> String
renderDefaultSymbol (name, symbol) =
  intercalate
    "\t"
    [ Text.unpack name
    , Text.unpack (renderTypePretty (symbolType symbol))
    , maybe "" (Text.unpack . operandType) (symbolPlace symbol)
    , maybe "" (Text.unpack . renderOperandValue . operandValue) (symbolPlace symbol)
    , maybe "false" (renderBool . operandIsStandardFunction) (symbolPlace symbol)
    , maybe "" (maybe "" renderBool . operandIsVariadic) (symbolPlace symbol)
    ]

renderBool :: Bool -> String
renderBool True = "true"
renderBool False = "false"

dumpTokens :: FilePath -> IO ()
dumpTokens input = do
  source <- preprocessFile input
  mapM_ (putStrLn . renderToken) (lexC source)

renderToken :: Token -> String
renderToken token =
  intercalate
    "\t"
    [ show (tokenTypeId token)
    , Text.unpack (tokenName token)
    , renderValue (tokenValue token)
    , show (row (tokenPos token))
    , show (col (tokenPos token))
    ]

renderValue :: TokenValue -> String
renderValue (ValueInt value) = "N:" <> show value
renderValue (ValueString value) = "S:" <> concatMap renderHexByte (Text.unpack value)

renderHexByte :: Char -> String
renderHexByte c =
  let hex = showHex (fromEnum c) ""
   in case hex of
        [single] -> ['0', single]
        _ -> hex
