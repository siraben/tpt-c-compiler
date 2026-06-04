module Tptcc.CodeGen
  ( CodeGenOptions (..)
  , defaultCodeGenOptions
  , dumpNativeAsmWithOptions
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.List (intercalate)

import Tptcc.Ast (Node)
import Tptcc.CodeGen.Optimize
import Tptcc.CodeGen.Options
import Tptcc.CodeGen.Render
import Tptcc.CodeGen.Stdlib
import Tptcc.IRGlobal (GlobalInfo (..), generateIRGlobalInfo)
import Tptcc.IRSimpleTac (generateSimpleTac)
import Tptcc.SSA (lowerMethodSSA)
import Tptcc.Tac
import Tptcc.TypeChecker (includedStandardFunctions)

dumpNativeAsmWithOptions :: CodeGenOptions -> Node -> Either String String
dumpNativeAsmWithOptions options ast = do
  program <- generateSimpleTac ast
  globalInfo <- generateIRGlobalInfo ast
  stdlib <- includedStandardFunctions ast
  renderCheckedProgram options program globalInfo stdlib

renderCheckedProgram :: CodeGenOptions -> TacProgram -> GlobalInfo -> [String] -> Either String String
renderCheckedProgram options program globalInfo stdlib
  | tacProgramGlobalSize program /= globalInfoSize globalInfo =
      Left $
        "global size mismatch between TAC and global-data pass: "
          <> show (tacProgramGlobalSize program)
          <> " /= "
          <> show (globalInfoSize globalInfo)
  | otherwise = renderProgram options program globalInfo stdlib

renderProgram :: CodeGenOptions -> TacProgram -> GlobalInfo -> [String] -> Either String String
renderProgram options program globalInfo stdlib = do
  globalAsm <- renderGlobalInstructions options (tacProgramGlobalInstructions program)
  methodAsm <- mapM (renderMethod options) (tacProgramMethods program)
  pure $
    concat
      [ header options globalInfo
      , globalAsm
      , entryJump
      , concat methodAsm
      , renderStandardLibrary stdlib
      ]
  where
    entryJump
      | any ((== "main") . methodOutputName) (tacProgramMethods program) = "\tjmp __tptcc_fn_main\n"
      | otherwise = "\thlt\n"

header :: CodeGenOptions -> GlobalInfo -> String
header options globalInfo =
  unlines
    [ "%include \"common\""
    , ""
    , "%define return_reg r31"
    , "%define stack_pointer r30"
    , "%define base_pointer r29"
    , "%define term_reg r28"
    , "%define return_addr_reg r27"
    , ""
    , "; Initialization and defining basic macros"
    , "%define term_base " <> show (codeGenBaseAddr options)
    , "%define term_height " <> show (codeGenTermHeight options)
    , "%define term_width " <> show (codeGenTermWidth options)
    , " "
    , "%eval term_input  term_base 0x00 +"
    , "%eval term_raw    term_base 0x04 +"
    , "%eval term_single term_base 0x05 +"
    , "%eval term_print  term_base 0x25 +"
    , "%eval term_term   term_base 0x26 +"
    , "%eval term_hrange term_base 0x42 +"
    , "%eval term_vrange term_base 0x43 +"
    , "%eval term_cursor term_base 0x44 +"
    , "%eval term_nlchar term_base 0x45 +"
    , "%eval term_colour term_base 0x46 +"
    , "%eval term_print_e term_base 0x40 +"
    , "%eval term_print_o term_base 0x41 +"
    , "%eval term_plot    term_base 0x60 +"
    , ""
    , ""
    , "%macro push thing"
    , "    subs stack_pointer, 1"
    , "    st thing, stack_pointer"
    , "%endmacro"
    , ""
    , "%macro pop thing"
    , "    ld thing, stack_pointer"
    , "    adds stack_pointer, 1"
    , "%endmacro"
    , ""
    , "%macro call thing"
    , "    push return_addr_reg"
    , "    jmp return_addr_reg, thing"
    , "%endmacro"
    , ""
    , "%macro ret"
    , "    mov r26, return_addr_reg"
    , "    pop return_addr_reg"
    , "    jmp r26"
    , "%endmacro"
    , ""
    , "%macro mull x, y"
    , "    mul x, x, y"
    , "%endmacro"
    , ""
    , "jmp init"
    , "global_data_section:"
    , "    " <> renderGlobalData options globalInfo
    , "init:"
    , "    mov term_reg, 0x25"
    , "    mov r1, { term_width 1 - 5 << }"
    , "    st r1, term_hrange"
    , "    mov r1, { term_height 1 - 5 << }"
    , "    st r1, term_vrange"
    , "    mov r1, 0x1000"
    , "    st r1, term_cursor"
    , "    mov r1, 0xF"
    , "    st r1, term_colour"
    , "    mov r1, 10"
    , "    st r1,  term_nlchar"
    , ""
    , "start:"
    , "    mov stack_pointer," <> show (codeGenMemorySize options)
    ]

renderGlobalData :: CodeGenOptions -> GlobalInfo -> String
renderGlobalData options globalInfo
  | globalInfoSize globalInfo == 0 = ""
  | otherwise =
      "dw " <> intercalate ", " (padding <> globalWords)
  where
    padding = replicate (fromInteger (codeGenGlobalAddr options - 1)) "0"
    globalWords =
      [ Text.unpack (Map.findWithDefault "0" index (globalInfoData globalInfo))
      | index <- [0 .. globalInfoSize globalInfo - 1]
      ]

renderGlobalInstructions :: CodeGenOptions -> [Instr] -> Either String String
renderGlobalInstructions options instructions = do
  (allocated, _) <- allocateRegisters (optimizeInstructions abstractLowered)
  concat <$> mapM (renderInstr options 0) (cleanupAllocatedInstructions allocated)
  where
    abstractLowered = map (lowerAbstract options 0) instructions

renderMethod :: CodeGenOptions -> MethodOutput -> Either String String
renderMethod options method = do
  (allocated, _, allocatedLocalSize) <- allocateMethodRegisters sourceLocalSize registerAllocationInput
  let loweredAbstract =
        map (lowerAbstract options allocatedLocalSize) allocated
      lowered = optimizeMethodTail method (cleanupAllocatedInstructions loweredAbstract)
      usedRegisters = usedAllocatedRegisters lowered
      savedRegisters
        | methodOutputName method == "main" = []
        | otherwise = usedRegisters
      frameLocalSize
        | not (any instrTouchesFrame lowered) = 0
        | otherwise = allocatedLocalSize
      needsFrame = frameLocalSize > 0 || any instrTouchesFrame lowered
      frameSetup
        | needsFrame = "\tpush base_pointer\n\tmov base_pointer, stack_pointer\n"
        | otherwise = ""
      frameTeardown
        | needsFrame = "\tpop base_pointer\n"
        | otherwise = ""
      localAllocation
        | frameLocalSize > 0 = "\tsub stack_pointer, " <> show frameLocalSize <> "\n"
        | otherwise = ""
      localRelease
        | frameLocalSize > 0 = "\tadd stack_pointer, " <> show frameLocalSize <> "\n"
        | otherwise = ""
  renderedInstructions <- mapM (renderInstr options allocatedLocalSize) lowered
  let methodName = Text.unpack (methodOutputName method)
  pure $
    "__tptcc_fn_"
      <> methodName
      <> ":\n"
      <> localAllocation
      <> frameSetup
      <> concatMap (\reg -> "\tpush r" <> show reg <> "\n") savedRegisters
      <> concat renderedInstructions
      <> ".exit_"
      <> methodName
      <> ":\n"
      <> concatMap (\reg -> "\tpop r" <> show reg <> "\n") (reverse savedRegisters)
      <> frameTeardown
      <> localRelease
      <> if methodOutputName method == "main"
        then "\thlt\n"
        else "\tret\n"
  where
    sourceLocalSize = methodOutputLocalSize method
    ssaInstructions =
      methodOutputInstructions $
        lowerMethodSSA method {methodOutputInstructions = promoteScalarLocals (methodOutputInstructions method)}
    registerAllocationInput = optimizeInstructions ssaInstructions

lowerAbstract :: CodeGenOptions -> Integer -> Instr -> Instr
lowerAbstract options localSize instr
  | instrType instr == IGetAddress =
      case (lookup "target" (instrFields instr), lookup "dest" (instrFields instr)) of
        (Just target, Just dest) -> emitGetAddress localSize target dest
        _ -> instr
  | otherwise = instr

emitGetAddress :: Integer -> Place -> Place -> Instr
emitGetAddress localSize target dest =
  case placeKind target of
    Global -> Instr IMov [("source", target), ("dest", dest)] []
    Parameter -> Instr IAdd3 [("source", Place Register "base_pointer"), ("offset", Place Immediate (Text.pack (show (localSize + placeInteger target + 2)))), ("dest", dest)] []
    Local -> Instr IAdd3 [("source", Place Register "base_pointer"), ("offset", Place Immediate (Text.pack (show (placeInteger target + 1)))), ("dest", dest)] []
    PointerRegister -> Instr IMov [("source", target), ("dest", dest)] []
    _ -> Instr INop [] []
