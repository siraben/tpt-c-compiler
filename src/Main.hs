module Main (main) where

import Data.List (intercalate, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Numeric (showHex)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath (replaceExtension)
import qualified Options.Applicative as OA

import Tptcc.Ast (Node, renderAst)
import Tptcc.CodeGen (CodeGenOptions (..), defaultCodeGenOptions, dumpNativeAsmWithOptions)
import Tptcc.CType (renderTypePretty)
import Tptcc.IRGlobal (dumpIRGlobals)
import Tptcc.IRSimpleTac (dumpSimpleTac)
import Tptcc.Lexer (lexC)
import Tptcc.Operand (Operand (..), renderOperandValue)
import qualified Tptcc.Parser as Parser
import Tptcc.Preprocessor (preprocessFile)
import Tptcc.SSA (dumpSSA)
import Tptcc.SymbolTable (Symbol (..), defaultSymbols)
import Tptcc.Tac (placeKindCode)
import Tptcc.Token (SourcePos (..), Token (..), TokenValue (..))
import qualified Tptcc.TypeChecker as TypeChecker

main :: IO ()
main = do
  args <- getArgs
  command <- OA.handleParseResult (OA.execParserPure OA.defaultPrefs cliInfo (normalizeArgs args))
  runCommand command

data Command
  = RunNative CompileConfig
  | DumpTokens FilePath
  | DumpAst FilePath
  | DumpTypeEvents FilePath
  | DumpIRGlobals FilePath
  | DumpSimpleTac FilePath
  | DumpSSA FilePath
  | DumpDefaultSymbols
  deriving (Eq, Show)

data CompileConfig = CompileConfig
  { compileInput :: FilePath
  , compileOutput :: FilePath
  , compileOptions :: CodeGenOptions
  }
  deriving (Eq, Show)

cliInfo :: OA.ParserInfo Command
cliInfo =
  OA.info
    (commandParser OA.<**> OA.helper)
    ( OA.fullDesc
        <> OA.forwardOptions
        <> OA.progDesc "Compile C programs for the R3 target"
        <> OA.header "tptcc-hs"
    )

commandParser :: OA.Parser Command
commandParser =
  OA.asum
    [ dumpFlag DumpTokens "dump-tokens" "Print lexer tokens"
    , dumpFlag DumpAst "dump-ast" "Print parsed AST"
    , dumpFlag DumpTypeEvents "dump-type-events" "Print type checker events"
    , dumpFlag DumpIRGlobals "dump-ir-globals" "Print global IR events"
    , dumpFlag DumpSimpleTac "dump-simple-tac" "Print simple TAC"
    , dumpFlag DumpSSA "dump-ssa" "Print SSA"
    , DumpDefaultSymbols <$ OA.flag' () (OA.long "dump-default-symbols" <> OA.help "Print built-in symbols")
    , RunNative <$> compileConfigParser
    ]

normalizeArgs :: [String] -> [String]
normalizeArgs args@(first : rest)
  | not (isOption first) = rest <> [first]
  | otherwise = args
normalizeArgs [] = []

isOption :: String -> Bool
isOption ('-' : _) = True
isOption _ = False

dumpFlag :: (FilePath -> Command) -> String -> String -> OA.Parser Command
dumpFlag command name description =
  command
    <$ OA.flag' () (OA.long name <> OA.help description)
    <*> OA.argument OA.str (OA.metavar "INPUT")

compileConfigParser :: OA.Parser CompileConfig
compileConfigParser =
  mkConfig
    <$> OA.optional (OA.strOption (OA.long "output" <> OA.metavar "OUTPUT" <> OA.help "Write assembly to OUTPUT"))
    <*> OA.many codeGenOptionParser
    <*> OA.argument OA.str (OA.metavar "INPUT")
  where
    mkConfig output optionUpdates input =
      CompileConfig
        { compileInput = input
        , compileOutput = fromMaybe (replaceExtension input "asm") output
        , compileOptions = foldl' (flip ($)) defaultCodeGenOptions optionUpdates
        }

codeGenOptionParser :: OA.Parser (CodeGenOptions -> CodeGenOptions)
codeGenOptionParser =
  OA.asum
    [ (\n opts -> opts {codeGenMemorySize = n - 1}) <$> integerOption "size" "TOTAL_MEMORY_SIZE" "Set total memory size"
    , (\n opts -> opts {codeGenTermWidth = n}) <$> integerOption "term-width" "WIDTH" "Set terminal width"
    , (\n opts -> opts {codeGenTermHeight = n}) <$> integerOption "term-height" "HEIGHT" "Set terminal height"
    , (\n opts -> opts {codeGenGlobalAddr = codeGenGlobalAddr opts + n}) <$> integerOption "offset" "OFFSET" "Offset global address"
    ]

integerOption :: String -> String -> String -> OA.Parser Integer
integerOption name metavar description =
  OA.option OA.auto (OA.long name <> OA.metavar metavar <> OA.help description)

runCommand :: Command -> IO ()
runCommand = \case
  RunNative config -> runNativeCompiler config
  DumpTokens input -> dumpTokens input
  DumpAst input -> dumpAst input
  DumpTypeEvents input -> dumpTypeEvents input
  DumpIRGlobals input -> dumpIRGlobalEvents input
  DumpSimpleTac input -> dumpSimpleTacEvents input
  DumpSSA input -> dumpSSAEvents input
  DumpDefaultSymbols -> dumpDefaultSymbols

runNativeCompiler :: CompileConfig -> IO ()
runNativeCompiler config = do
  asm <- compileInputWith (compileInput config) (dumpNativeAsmWithOptions (compileOptions config))
  writeFile (compileOutput config) asm

dumpSimpleTacEvents :: FilePath -> IO ()
dumpSimpleTacEvents input =
  printLines =<< compileInputWith input dumpSimpleTac

dumpSSAEvents :: FilePath -> IO ()
dumpSSAEvents input =
  printLines =<< compileInputWith input dumpSSA

dumpIRGlobalEvents :: FilePath -> IO ()
dumpIRGlobalEvents input =
  printLines =<< compileInputWith input dumpIRGlobals

dumpTypeEvents :: FilePath -> IO ()
dumpTypeEvents input =
  printLines =<< compileInputWith input TypeChecker.typeEvents

dumpAst :: FilePath -> IO ()
dumpAst input =
  putStr =<< compileInputWith input (Right . renderAst)

compileInputWith :: FilePath -> (Node -> Either String a) -> IO a
compileInputWith input action = do
  source <- preprocessFile input
  exitOnError (Parser.parse (lexC source) >>= action)

exitOnError :: Either String a -> IO a
exitOnError = \case
  Left err -> do
    putStrLn err
    exitWith (ExitFailure 1)
  Right value -> pure value

printLines :: [String] -> IO ()
printLines = mapM_ putStrLn

dumpDefaultSymbols :: IO ()
dumpDefaultSymbols =
  mapM_ (putStrLn . renderDefaultSymbol) (sortOn fst defaultSymbols)

renderDefaultSymbol :: (Text.Text, Symbol) -> String
renderDefaultSymbol (name, symbol) =
  intercalate
    "\t"
    [ Text.unpack name
    , Text.unpack (renderTypePretty (symbolType symbol))
    , maybe "" (Text.unpack . placeKindCode . operandKind) (symbolPlace symbol)
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
