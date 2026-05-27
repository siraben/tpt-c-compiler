module Tptcc.CodeGen
  ( CodeGenOptions (..)
  , defaultCodeGenOptions
  , dumpNativeAsmUnoptimized
  , dumpNativeAsmOptimized
  , dumpNativeAsmOptimizedWithOptions
  ) where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.List (find, sortBy)
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Tptcc.Ast (Node)
import Tptcc.IRGlobal (GlobalInfo (..), generateIRGlobalInfo)
import Tptcc.IRSimpleTac
import Tptcc.TypeChecker (includedStandardFunctions)

data CodeGenOptions = CodeGenOptions
  { codeGenMemorySize :: Integer
  , codeGenTermWidth :: Integer
  , codeGenTermHeight :: Integer
  , codeGenGlobalAddr :: Integer
  , codeGenBaseAddr :: Integer
  , codeGenBreakpoints :: [Integer]
  }
  deriving (Eq, Show)

defaultCodeGenOptions :: CodeGenOptions
defaultCodeGenOptions =
  CodeGenOptions
    { codeGenMemorySize = 2047
    , codeGenTermWidth = 12
    , codeGenTermHeight = 8
    , codeGenGlobalAddr = 1
    , codeGenBaseAddr = 0x9F80
    , codeGenBreakpoints = []
    }

dumpNativeAsmUnoptimized :: Node -> Either String String
dumpNativeAsmUnoptimized ast = do
  program <- generateSimpleTacWithBreakpoints (codeGenBreakpoints defaultCodeGenOptions) ast
  globalInfo <- generateIRGlobalInfo ast
  stdlib <- includedStandardFunctions ast
  renderCheckedProgram defaultCodeGenOptions False program globalInfo stdlib

dumpNativeAsmOptimized :: Node -> Either String String
dumpNativeAsmOptimized = dumpNativeAsmOptimizedWithOptions defaultCodeGenOptions

dumpNativeAsmOptimizedWithOptions :: CodeGenOptions -> Node -> Either String String
dumpNativeAsmOptimizedWithOptions options ast = do
  program <- generateSimpleTacWithBreakpoints (codeGenBreakpoints options) ast
  globalInfo <- generateIRGlobalInfo ast
  stdlib <- includedStandardFunctions ast
  renderCheckedProgram options True program globalInfo stdlib

renderCheckedProgram :: CodeGenOptions -> Bool -> TacProgram -> GlobalInfo -> [String] -> Either String String
renderCheckedProgram options optimized program globalInfo stdlib
  | tacProgramGlobalSize program /= globalInfoSize globalInfo =
      Left $
        "global size mismatch between TAC and global-data pass: "
          <> show (tacProgramGlobalSize program)
          <> " /= "
          <> show (globalInfoSize globalInfo)
  | otherwise = renderProgram options optimized program globalInfo stdlib

renderProgram :: CodeGenOptions -> Bool -> TacProgram -> GlobalInfo -> [String] -> Either String String
renderProgram options optimized program globalInfo stdlib = do
  globalAsm <- renderGlobalInstructions options optimized (tacProgramGlobalInstructions program)
  methodAsm <- mapM (renderMethod options optimized) (tacProgramMethods program)
  pure $
    header options globalInfo
      <> globalAsm
      <> entryJump
      <> concat methodAsm
      <> renderStandardLibrary stdlib
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
    , "                              "
    , "    ld r0, term_base              "
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
  | otherwise = "dw " <> commaSep (replicate (fromInteger (codeGenGlobalAddr options - 1)) "0" <> [Map.findWithDefault "0" index (globalInfoData globalInfo) | index <- [0 .. globalInfoSize globalInfo - 1]])

commaSep :: [String] -> String
commaSep [] = ""
commaSep [value] = value
commaSep (value : values) = value <> ", " <> commaSep values

renderGlobalInstructions :: CodeGenOptions -> Bool -> [Instr] -> Either String String
renderGlobalInstructions options optimized instructions = do
  lowered <-
    if optimized
      then do
        (allocated, _) <- allocateRegisters (optimizeInstructions abstractLowered)
        pure (finalizeOptimizedInstructions options allocated)
      else pure abstractLowered
  pure (concatMap (renderInstr options 0) lowered)
  where
    abstractLowered = map (lowerAbstract options 0) instructions

renderStandardLibrary :: [String] -> String
renderStandardLibrary names =
  concatMap renderOne names
  where
    renderOne name = maybe "" (substituteStdRegisters . stripInitialNewline) (Map.lookup name standardLibraryCode)

stripInitialNewline :: String -> String
stripInitialNewline ('\n' : rest) = rest
stripInitialNewline value = value

substituteStdRegisters :: String -> String
substituteStdRegisters [] = []
substituteStdRegisters ('%' : digit : rest)
  | digit == '1' = "r22" <> substituteStdRegisters rest
  | digit == '2' = "r23" <> substituteStdRegisters rest
  | digit == '3' = "r24" <> substituteStdRegisters rest
  | digit == '4' = "r25" <> substituteStdRegisters rest
substituteStdRegisters (char : rest) = char : substituteStdRegisters rest

standardLibraryCode :: Map.Map String String
standardLibraryCode =
  Map.fromList
    [
      ( "__print_unsigned_int"
      , "\n__tptcc_fn_print_unsigned_int:\n\ttest %1, %1\n\tjnz .__print_unsigned_int_not_zero\n\tmov %1, '0'\n\tst %1, term_print\n\tjmp .__print_unsigned_int_exit\n.__print_unsigned_int_not_zero:\n\tmov %2, 4\t\t; p = 4\n.__print_unsigned_int_fixed_point:\n\tmulh %3, %1, 52429\t; q = (n * 52429) >> 16\n\tshr %3, 3\t\t; q >>= 3\n\tmul %4, %3, 10\t\t; d*q\n\tsub %1, %4\t\t; remainder = n - d*q\n\tst %1, %2, .__print_unsigned_int_buf\t\t\n\tsub %2, 1\t\t; p--;\n\tmovf %1, %3\t\t; n = q\n\tjnz .__print_unsigned_int_fixed_point\n\n\tadd %2, 1\n.__print_unsigned_int_print_int:\n\tld %1, %2, .__print_unsigned_int_buf\n\tadd %1, '0'\n\tst %1, term_reg, term_base\n\tadd %2, 1\n\tcmp %2, 5\n\tjne .__print_unsigned_int_print_int\n\t\n.__print_unsigned_int_exit:\n\tret\n.__print_unsigned_int_buf:\n\tdw 0, 0, 0, 0, 0\n"
      )
    ,
      ( "__print_signed_int"
      , "\n__tptcc_fn_print_signed_int:\n    cmp %1, 0\n    jge .__print_signed_int_not_negative\n    mov %2, '-'\n    st %2, term_reg, term_base\n\txor %1, 65535\n    add %1, 1\n.__print_signed_int_not_negative:\n    call __tptcc_fn_print_unsigned_int\n    ret\n"
      )
    ,
      ( "__print_char_array"
      , "\n__tptcc_fn_print_char_array:\n    ld %2, %1\n    test %2, %2\n    jz .__print_char_array_exit\n    st %2, term_reg, term_base\n    add %1, 1\n    jmp __tptcc_fn_print_char_array\n.__print_char_array_exit:\n    ret\n"
      )
    ,
      ( "putchar"
      , "\n__tptcc_fn_putchar:\n    st %1, term_reg, term_base\n    ret\n"
      )
    ,
      ( "getchar"
      , "\n__tptcc_fn_getchar:\n    ld return_reg, term_input\n    test return_reg, return_reg\n    jz __tptcc_fn_getchar\n    ret\n"
      )
    ,
      ( "getchar_nb"
      , "\n__tptcc_fn_getchar_nb:\n    ld return_reg, term_input\n    ret\n"
      )
    ,
      ( "set_colour"
      , "\n__tptcc_fn_set_colour:\n    ; %1 = background, %2 = foreground\n    shl %1, 4\n    add %1, %2\n    st %1, term_colour\n    ret\n"
      )
    ,
      ( "set_text_colour"
      , "\n__tptcc_fn_set_text_colour:\n    st %1, term_colour\n    ret\n"
      )
    ,
      ( "__send_raw"
      , "\n__tptcc_fn_send_raw:\n    st %1, %2\n    ret\n"
      )
    ,
      ( "__set_zero_char"
      , "\n__tptcc_fn_set_zero_char:\n    exh %2, r0, %2\n    mov %1, %2, %1\n    st %1, term_print_e\n    exh %4, r0, %4\n    mov %3, %4, %3\n    st %3, term_print_o\n    ret\n"
      )
    ,
      ( "set_cursor"
      , "\n__tptcc_fn_set_cursor:\n    ; %1 = row, %2 = column\n    shl %1, 5\n    add %1, %2\n    st %1, term_cursor\n    ret\n"
      )
    ,
      ( "__scan_unsigned_int"
      , "\n__tptcc_fn_scan_unsigned_int:\n    mov %2, 0\n__scan_unsigned_int_loop:\n    call __tptcc_fn_getchar\n    st return_reg, term_reg, term_base\n    sub return_reg, '0'\n    cmp return_reg, 9\n    jg __scan_unsigned_int_not_digit\n    cmp return_reg, 0\n    jl __scan_unsigned_int_not_digit\n    mull %2, 10\n    add %2, return_reg\n    jmp __scan_unsigned_int_loop\n__scan_unsigned_int_not_digit:\n    st %2, %1\n    ret\n\n"
      )
    ,
      ( "vscroll"
      , "\n__tptcc_fn_vscroll:\n    mov %1, ' '\n    st %1, term_raw\n    ret\n"
      )
    ,
      ( "hscroll"
      , "\n__tptcc_fn_hscroll:\n    mov %1, ' '\n    st %1, term_base\n    ret\n"
      )
    ,
      ( "set_terminal_mode"
      , "\n__tptcc_fn_set_terminal_mode:\n    mov term_reg, %1\n    ret\n"
      )
    ,
      ( "get_terminal_mode"
      , "\n__tptcc_fn_get_terminal_mode:\n    mov return_reg, term_reg\n    ret\n"
      )
    ,
      ( "plot"
      , "\n__tptcc_fn_plot:\n    ; %1 = column/x, %2 = row/y, %3 = colour\n    shl %2, 8\n    add %2, %1\n    st %2, %3, term_plot\n    ret\n"
      )
    ,
      ( "set_hrange"
      , "\n__tptcc_fn_set_hrange:\n    ; %1 = start column, %2 = end column\n    shl %2, 5\n    add %2, %1\n    st %2, term_hrange\n    ret\n"
      )
    ,
      ( "set_vrange"
      , "\n__tptcc_fn_set_vrange:\n    ; %1 = start row, %2 = end row\n    shl %2, 5\n    add %2, %1\n    st %2, term_vrange\n    ret\n"
      )
    ]

renderMethod :: CodeGenOptions -> Bool -> MethodOutput -> Either String String
renderMethod options optimized method = do
  (allocated, _, allocatedLocalSize) <-
    if optimized
      then allocateMethodRegisters sourceLocalSize optimizedInstructions
      else pure (abstractLowered, [], sourceLocalSize)
  let loweredAbstract =
        if optimized
          then map (lowerAbstract options allocatedLocalSize) allocated
          else allocated
      lowered =
        if optimized
          then optimizeMethodTail method (finalizeOptimizedInstructions options loweredAbstract)
          else loweredAbstract
      usedRegisters = usedAllocatedRegisters lowered
      savedRegisters
        | methodOutputName method == "main" = []
        | otherwise = usedRegisters
      frameLocalSize
        | optimized && not (any instrTouchesFrame lowered) = 0
        | otherwise = allocatedLocalSize
      needsFrame = not optimized || frameLocalSize > 0 || any instrTouchesFrame lowered
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
  pure $
    "__tptcc_fn_"
      <> methodOutputName method
      <> ":\n"
      <> localAllocation
      <> frameSetup
      <> concatMap (\reg -> "\tpush r" <> show reg <> "\n") savedRegisters
      <> concatMap (renderInstr options allocatedLocalSize) lowered
      <> ".exit_"
      <> methodOutputName method
      <> ":\n"
      <> concatMap (\reg -> "\tpop r" <> show reg <> "\n") (reverse savedRegisters)
      <> frameTeardown
      <> localRelease
      <> if methodOutputName method == "main"
        then "\thlt\n"
        else "\tret\n"
  where
    sourceLocalSize = methodOutputLocalSize method
    optimizedSource =
      if optimized
        then promoteScalarLocals (methodOutputInstructions method)
        else methodOutputInstructions method
    abstractLowered = map (lowerAbstract options sourceLocalSize) optimizedSource
    optimizedInstructions = optimizeInstructions optimizedSource

lowerAbstract :: CodeGenOptions -> Integer -> Instr -> Instr
lowerAbstract options localSize instr
  | instrType instr == "!get_address" =
      case (lookup "target" (instrFields instr), lookup "dest" (instrFields instr)) of
        (Just target, Just dest) -> emitGetAddress localSize target dest
        _ -> instr
  | instrType instr == "!debug_breakpoint" =
      Instr "st" [("source", Place "r" "r0"), ("dest", Place "g" (show (codeGenBaseAddr options + 0x80 - codeGenGlobalAddr options)))] []
  | instrType instr == "!debug_function_call" =
      Instr "st" [("source", fieldPlace "target" instr), ("dest", Place "g" (show (codeGenBaseAddr options + 0x8001 - codeGenGlobalAddr options)))] []
  | otherwise = instr

emitGetAddress :: Integer -> Place -> Place -> Instr
emitGetAddress localSize target dest =
  case placeType target of
    "g" -> Instr "mov" [("source", target), ("dest", dest)] []
    "p" -> Instr "add3" [("source", Place "r" "base_pointer"), ("offset", Place "i" (show (localSize + placeInteger target + 2))), ("dest", dest)] []
    "l" -> Instr "add3" [("source", Place "r" "base_pointer"), ("offset", Place "i" (show (placeInteger target + 1))), ("dest", dest)] []
    "pr" -> Instr "mov" [("source", target), ("dest", dest)] []
    _ -> Instr "nop" [] []

promoteScalarLocals :: [Instr] -> [Instr]
promoteScalarLocals instrs =
  map rewriteInstr instrs
  where
    addressTaken =
      Set.fromList
        [ placeInteger target
        | instr <- instrs
        , instrType instr == "!get_address"
        , let target = fieldPlace "target" instr
        , placeType target == "l"
        ]
    localSlots =
      Set.fromList
        [ placeInteger place
        | instr <- instrs
        , (_, place) <- instrFields instr
        , placeType place == "l"
        ]
    promotable = Set.difference localSlots addressTaken
    firstTemp = 1 + maximum (0 : [placeInteger place | instr <- instrs, (_, place) <- instrFields instr, isVirtualRegister place])
    localRegisters =
      Map.fromList
        [ (slot, Place "t" (show (firstTemp + fromIntegral index)))
        | (index, slot) <- zip [(0 :: Int) ..] (Set.toAscList promotable)
        ]

    promotedLocal place =
      Map.lookup (placeInteger place) localRegisters

    rewriteInstr instr
      | instrType instr == "st"
      , placeType (fieldPlace "dest" instr) == "l"
      , Just dest <- promotedLocal (fieldPlace "dest" instr) =
          Instr "mov" [("source", fieldPlace "source" instr), ("dest", dest)] []
      | instrType instr == "ld"
      , placeType (fieldPlace "source" instr) == "l"
      , Just source <- promotedLocal (fieldPlace "source" instr) =
          Instr "mov" [("source", source), ("dest", fieldPlace "dest" instr)] []
      | otherwise = instr

optimizeInstructions :: [Instr] -> [Instr]
optimizeInstructions = eliminateDeadVirtualWrites . propagateCopiesAndConstants

type CopyEnv = Map.Map (String, String) Place

propagateCopiesAndConstants :: [Instr] -> [Instr]
propagateCopiesAndConstants = reverse . snd . foldl' step (Map.empty, [])
  where
    step (env, out) instr
      | instructionStopsPropagation instr = (Map.empty, instr : out)
      | otherwise =
          let rewritten = rewriteInstructionUses env instr
              envAfterDefs = foldl' (flip Map.delete) env (definedPlaceKeys rewritten)
              env' = updateCopyEnv envAfterDefs rewritten
           in (env', rewritten : out)

instructionStopsPropagation :: Instr -> Bool
instructionStopsPropagation instr =
  instrType instr == "label"
    || instrType instr == "call"
    || instrType instr == "asm"
    || instrType instr == "ret"
    || isJumpInstruction (instrType instr)

rewriteInstructionUses :: CopyEnv -> Instr -> Instr
rewriteInstructionUses env instr =
  instr {instrFields = [(name, rewriteUse name place) | (name, place) <- instrFields instr]}
  where
    uses = [name | name <- useFieldNames instr, name `notElem` defFieldNames instr]
    rewriteUse name place
      | name `elem` uses =
          let resolved = resolveEnvPlace env place
           in if placeType resolved == "i" && not (fieldAcceptsImmediate instr name)
                then place
                else resolved
      | otherwise = place

fieldAcceptsImmediate :: Instr -> String -> Bool
fieldAcceptsImmediate instr name =
  case instrType instr of
    "mov" -> name == "source"
    "cmp" -> name == "second"
    "add3" -> name `elem` ["source", "offset"]
    "ldoffset" -> name == "offset"
    "mulh" -> name == "third"
    "mull3" -> name == "third"
    "sub3" -> name == "third"
    "shr3" -> name == "third"
    "add" -> name == "source"
    "sub" -> name == "source"
    "mull" -> name == "source"
    "shl" -> name == "source"
    "shr" -> name == "source"
    "xor" -> name == "source"
    "and" -> name == "source"
    "or" -> name == "source"
    _ -> False

resolveEnvPlace :: CopyEnv -> Place -> Place
resolveEnvPlace env = go Set.empty
  where
    go seen place =
      case placeKey place of
        Just key
          | key `Set.member` seen -> place
          | Just next <- Map.lookup key env -> go (Set.insert key seen) next
        _ -> place

definedPlaceKeys :: Instr -> [(String, String)]
definedPlaceKeys instr =
  [ key
  | name <- defFieldNames instr
  , let place = fieldPlace name instr
  , Just key <- [placeKey place]
  ]

updateCopyEnv :: CopyEnv -> Instr -> CopyEnv
updateCopyEnv env instr
  | instrType instr == "mov"
  , Just destKey <- placeKey (fieldPlace "dest" instr)
  , isPropagatablePlace source =
      Map.insert destKey source env
  | otherwise = env
  where
    source = fieldPlace "source" instr

isPropagatablePlace :: Place -> Bool
isPropagatablePlace place =
  placeType place `elem` ["i", "t", "vr", "pr", "r"]

placeKey :: Place -> Maybe (String, String)
placeKey place
  | isVirtualRegister place = Just (placeType place, placeValue place)
  | otherwise = Nothing

eliminateDeadVirtualWrites :: [Instr] -> [Instr]
eliminateDeadVirtualWrites instrs =
  [instr | (_, instr) <- sortBy compareIndexedInstruction kept]
  where
    blocks =
      sortBlocks $
        livenessAnalysis $
          map buildBlockUseDef (buildBasicBlocks instrs)
    kept = concatMap keepBlock blocks

    keepBlock block =
      snd $
        foldr step (blockLiveOut block, []) (blockCode block)

    step indexed@(_, instr) (live, keptInstrs)
      | pureVirtualDefinition instr
      , let (_, defs) = instructionUseDef instr
      , Set.null (Set.intersection defs live) =
          (live, keptInstrs)
      | otherwise =
          let (uses, defs) = instructionUseDef instr
              live' = Set.union uses (Set.difference live defs)
           in (live', indexed : keptInstrs)

    compareIndexedInstruction (left, _) (right, _) = compare left right

pureVirtualDefinition :: Instr -> Bool
pureVirtualDefinition instr =
  pureRegisterDefinition instr
    && case defFieldNames instr of
      [name] -> isVirtualRegister (fieldPlace name instr)
      _ -> False

data BasicBlock = BasicBlock
  { blockId :: String
  , blockCode :: [(Int, Instr)]
  , blockPred :: [String]
  , blockSucc :: [String]
  , blockUse :: Set.Set Integer
  , blockDef :: Set.Set Integer
  , blockLiveIn :: Set.Set Integer
  , blockLiveOut :: Set.Set Integer
  , blockPerOut :: [Set.Set Integer]
  }
  deriving (Eq, Show)

emptyBlock :: String -> [String] -> [String] -> BasicBlock
emptyBlock ident predIds succIds =
  BasicBlock
    { blockId = ident
    , blockCode = []
    , blockPred = predIds
    , blockSucc = succIds
    , blockUse = Set.empty
    , blockDef = Set.empty
    , blockLiveIn = Set.empty
    , blockLiveOut = Set.empty
    , blockPerOut = []
    }

allocateRegisters :: [Instr] -> Either String ([Instr], [Integer])
allocateRegisters tac =
  allocateRegistersNoSpill tac

allocateMethodRegisters :: Integer -> [Instr] -> Either String ([Instr], [Integer], Integer)
allocateMethodRegisters baseLocalSize tac = go baseLocalSize tac
  where
    go nextSpillSlot currentTac =
      case colourTac currentTac of
        Right colours ->
          let rewritten = map (rewriteInstrRegisters colours) currentTac
              used = usedAllocatedRegisters rewritten
           in pure (rewritten, used, nextSpillSlot)
        Left reg ->
          go (nextSpillSlot + 1) (spillVirtualRegister reg (Place "l" (show nextSpillSlot)) currentTac)

allocateRegistersNoSpill :: [Instr] -> Either String ([Instr], [Integer])
allocateRegistersNoSpill tac = do
  colours <- either allocationError pure (colourTac tac)
  let rewritten = map (rewriteInstrRegisters colours) tac
      used = usedAllocatedRegisters rewritten
  pure (rewritten, used)
  where
    allocationError reg =
      Left $
        "register allocation exhausted for virtual register "
          <> show reg
          <> ": all allocatable registers r1-r19 are live"

colourTac :: [Instr] -> Either Integer (Map.Map Integer Integer)
colourTac tac =
  let blocks0 = buildBasicBlocks tac
      blocks1 = map buildBlockUseDef blocks0
      blocks2 = livenessAnalysis blocks1
      ordered = sortBlocks blocks2
      blocks3 = map computePerInstructionLiveness ordered
      graph = buildInterferenceGraph blocks3
      colourOrder = luaIntegerPairsOrder (Map.keys graph)
      preferences = movePreferences tac
   in colourGraph preferences colourOrder graph

buildBasicBlocks :: [Instr] -> [BasicBlock]
buildBasicBlocks tac = finalBlocks
  where
    step (currentId, blockMap, orderRev, orderLength) (index, instr)
      | instrType instr == "label" =
          let target = placeValue (fieldPlace "target" instr)
              blockMap1 = addSucc currentId target blockMap
              (blockMap2, orderRev2, orderLength2) =
                if Map.member target blockMap1
                  then (addPred target currentId blockMap1, orderRev, orderLength)
                  else insertBlock (emptyBlock target [currentId] []) blockMap1 orderRev orderLength
              blockMap3 = Map.adjust (\b -> b {blockCode = [(index, instr)]}) target blockMap2
           in (target, blockMap3, orderRev2, orderLength2)
      | instrType instr == "jmp" =
          let target = placeValue (fieldPlace "target" instr)
              blockMap1 = addSucc currentId target blockMap
              (blockMap2, orderRev2, orderLength2) =
                if Map.member target blockMap1
                  then (addPred target currentId blockMap1, orderRev, orderLength)
                  else insertBlock (emptyBlock target [currentId] []) blockMap1 orderRev orderLength
              blockMap3 = prependCode currentId (index, instr) blockMap2
              anonId = "anon_block" <> show orderLength2
              (blockMap4, orderRev3, orderLength3) = insertBlock (emptyBlock anonId [] []) blockMap3 orderRev2 orderLength2
           in (anonId, blockMap4, orderRev3, orderLength3)
      | isJumpInstruction (instrType instr) =
          let target = placeValue (fieldPlace "target" instr)
              blockMap1 = addSucc currentId target blockMap
              (blockMap2, orderRev2, orderLength2) =
                if Map.member target blockMap1
                  then (addPred target currentId blockMap1, orderRev, orderLength)
                  else insertBlock (emptyBlock target [currentId] []) blockMap1 orderRev orderLength
              blockMap3 = prependCode currentId (index, instr) blockMap2
              anonId = "anon_block" <> show orderLength2
              blockMap4 = addSucc currentId anonId blockMap3
              (blockMap5, orderRev3, orderLength3) = insertBlock (emptyBlock anonId [currentId] []) blockMap4 orderRev2 orderLength2
           in (anonId, blockMap5, orderRev3, orderLength3)
      | otherwise =
          (currentId, prependCode currentId (index, instr) blockMap, orderRev, orderLength)

    finalBlocksFrom blockMap orderRev = [normalizeBlock (blockMap Map.! ident) | ident <- reverse orderRev]
    finalBlocks = finalBlocksFrom finalMap finalOrderRev
    (_, finalMap, finalOrderRev, _) = foldl' step ("!start", Map.fromList [("!start", emptyBlock "!start" [] [])], ["!start"], 1) (zip [1 ..] tac)

normalizeBlock :: BasicBlock -> BasicBlock
normalizeBlock block =
  block
    { blockCode = reverse (blockCode block)
    , blockPred = reverse (blockPred block)
    , blockSucc = reverse (blockSucc block)
    }

insertBlock :: BasicBlock -> Map.Map String BasicBlock -> [String] -> Int -> (Map.Map String BasicBlock, [String], Int)
insertBlock block blockMap orderRev orderLength =
  (Map.insert (blockId block) block blockMap, blockId block : orderRev, orderLength + 1)

prependCode :: String -> (Int, Instr) -> Map.Map String BasicBlock -> Map.Map String BasicBlock
prependCode ident code =
  Map.adjust (\block -> block {blockCode = code : blockCode block}) ident

addSucc :: String -> String -> Map.Map String BasicBlock -> Map.Map String BasicBlock
addSucc ident succId =
  Map.adjust (\block -> block {blockSucc = succId : blockSucc block}) ident

addPred :: String -> String -> Map.Map String BasicBlock -> Map.Map String BasicBlock
addPred ident predId =
  Map.adjust (\block -> block {blockPred = predId : blockPred block}) ident

isJumpInstruction :: String -> Bool
isJumpInstruction ty = take 1 ty == "j"

buildBlockUseDef :: BasicBlock -> BasicBlock
buildBlockUseDef = buildBlockUseDefWith instructionUseDef

buildBlockUseDefWith :: (Instr -> (Set.Set Integer, Set.Set Integer)) -> BasicBlock -> BasicBlock
buildBlockUseDefWith useDef block =
  block {blockUse = uses, blockDef = defs}
  where
    (_, uses, defs) = foldl' step (Set.empty, Set.empty, Set.empty) (map snd (blockCode block))
    step (seenDef, useAcc, defAcc) instr =
      let (useSet, defSet) = useDef instr
          newUses = Set.difference useSet seenDef
          newDefs = Set.difference defSet seenDef
       in (Set.union seenDef newDefs, Set.union useAcc newUses, Set.union defAcc newDefs)

livenessAnalysis :: [BasicBlock] -> [BasicBlock]
livenessAnalysis blocks =
  let blockMap = Map.fromList [(blockId block, block) | block <- blocks]
      fixed = livenessFixedPoint blockMap
   in [fixed Map.! blockId block | block <- blocks]

livenessFixedPoint :: Map.Map String BasicBlock -> Map.Map String BasicBlock
livenessFixedPoint blockMap =
  let (changed, nextMap) = Map.mapAccumWithKey updateBlock False blockMap
   in if changed then livenessFixedPoint nextMap else nextMap
  where
    updateBlock changed _ block =
      let succIns = [blockLiveIn (blockMap Map.! succId) | succId <- blockSucc block, Map.member succId blockMap]
          outSet = Set.unions succIns
          inSet = Set.union (blockUse block) (Set.difference outSet (blockDef block))
          block' = block {blockLiveIn = inSet, blockLiveOut = outSet}
          changed' = changed || blockLiveIn block /= inSet || blockLiveOut block /= outSet
       in (changed', block')

sortBlocks :: [BasicBlock] -> [BasicBlock]
sortBlocks =
  sortBy compareBlocks
  where
    compareBlocks a b =
      case (blockCode a, blockCode b) of
        ([], []) -> EQ
        ([], _) -> GT
        (_, []) -> LT
        ((ai, _) : _, (bi, _) : _) -> compare ai bi

computePerInstructionLiveness :: BasicBlock -> BasicBlock
computePerInstructionLiveness block =
  block {blockPerOut = outs}
  where
    (_, outs) = foldl' step (blockLiveOut block, []) (reverse (map snd (blockCode block)))
    step (live, outAcc) instr =
      let (useSet, defSet) = instructionUseDef instr
          live' = Set.union useSet (Set.difference live defSet)
       in (live', live : outAcc)

buildInterferenceGraph :: [BasicBlock] -> Map.Map Integer (Set.Set Integer)
buildInterferenceGraph blocks =
  foldl' addBlock Map.empty blocks
  where
    addBlock graph block =
      foldl' addInstr graph (zip (map snd (blockCode block)) (blockPerOut block))
    addInstr graph (instr, liveOut) =
      let (_, defSet) = instructionUseDef instr
       in foldl' (\graph' def -> addDefInterference graph' def liveOut) graph (Set.toList defSet)
    addDefInterference graph def liveOut =
      foldl' (addEdge def) (Map.insertWith Set.union def Set.empty graph) (Set.toList liveOut)
    addEdge a graph b =
      Map.insertWith Set.union b (Set.singleton a) $
        Map.insertWith Set.union a (Set.singleton b) graph

colourGraph :: Map.Map Integer [Integer] -> [Integer] -> Map.Map Integer (Set.Set Integer) -> Either Integer (Map.Map Integer Integer)
colourGraph preferences order graph =
  foldM colourOne Map.empty order
  where
    colourOne mapping reg =
      let usedColours = Set.fromList [usedColour | neighbour <- Set.toList (Map.findWithDefault Set.empty reg graph), Just usedColour <- [Map.lookup neighbour mapping]]
          preferredColours =
            [ colour
            | preferred <- Map.findWithDefault [] reg preferences
            , Just colour <- [Map.lookup preferred mapping]
            , colour `Set.notMember` usedColours
            ]
          chosen = listToMaybe preferredColours <|> firstAvailableColour usedColours
       in case chosen of
            Just chosenColour -> pure (Map.insert reg chosenColour mapping)
            Nothing -> Left reg

luaIntegerPairsOrder :: [Integer] -> [Integer]
luaIntegerPairsOrder keys =
  sortBy compare arrayKeys <> sortBy compareLuaHash hashKeys
  where
    arraySize = luaArraySize keys
    (arrayKeys, hashKeys) = spanLuaArrayKeys arraySize keys
    modulus = max 1 (nextPowerOfTwo (max 1 (length hashKeys)) - 1)
    compareLuaHash a b =
      compare (a `mod` fromIntegral modulus, a) (b `mod` fromIntegral modulus, b)

spanLuaArrayKeys :: Integer -> [Integer] -> ([Integer], [Integer])
spanLuaArrayKeys arraySize keys =
  ([key | key <- keys, key > 0 && key <= arraySize], [key | key <- keys, key <= 0 || key > arraySize])

luaArraySize :: [Integer] -> Integer
luaArraySize keys =
  fromIntegral (go 1 0)
  where
    positiveKeys = filter (> 0) keys
    maxKey = maximum (0 : positiveKeys)
    go size best
      | fromIntegral size > maxKey = best
      | countIn size > size `div` 2 = go (size * 2) size
      | otherwise = go (size * 2) best
    countIn size = length [key | key <- positiveKeys, key <= fromIntegral size]

nextPowerOfTwo :: Int -> Int
nextPowerOfTwo value = go 1
  where
    go power
      | power >= value = power
      | otherwise = go (power * 2)

uniqueInOrder :: Ord a => [a] -> [a]
uniqueInOrder = go Set.empty
  where
    go _ [] = []
    go seen (value : rest)
      | Set.member value seen = go seen rest
      | otherwise = value : go (Set.insert value seen) rest

allocatableRegisters :: [Integer]
allocatableRegisters = [1 .. 19]

callerSafeRegisters :: [Integer]
callerSafeRegisters = [1 .. 21]

spillScratchA :: Place
spillScratchA = Place "r" "20"

spillScratchB :: Place
spillScratchB = Place "r" "21"

firstAvailableColour :: Set.Set Integer -> Maybe Integer
firstAvailableColour used =
  find (`Set.notMember` used) allocatableRegisters

usedAllocatedRegisters :: [Instr] -> [Integer]
usedAllocatedRegisters instrs =
  uniqueInOrder
    [ reg
    | instr <- instrs
    , (_, place) <- instrFields instr
    , Just reg <- [allocatedPhysicalRegister place]
    ]

allocatedPhysicalRegister :: Place -> Maybe Integer
allocatedPhysicalRegister place
  | placeType place == "r" =
      case reads (placeValue place) of
        [(reg, "")] | reg `elem` callerSafeRegisters -> Just reg
        _ -> Nothing
  | otherwise = Nothing

movePreferences :: [Instr] -> Map.Map Integer [Integer]
movePreferences =
  foldl' addPreference Map.empty
  where
    addPreference preferences instr
      | instrType instr == "mov"
          && isVirtualRegister source
          && isVirtualRegister dest =
          Map.insertWith (<>) (placeInteger source) [placeInteger dest] $
            Map.insertWith (<>) (placeInteger dest) [placeInteger source] preferences
      | otherwise = preferences
      where
        source = fieldPlace "source" instr
        dest = fieldPlace "dest" instr

spillVirtualRegister :: Integer -> Place -> [Instr] -> [Instr]
spillVirtualRegister reg spillSlot =
  concatMap rewrite
  where
    rewrite instr =
      let uses = useFieldNames instr
          defs = defFieldNames instr
          usedFields = [(name, place) | (name, place) <- instrFields instr, name `elem` uses, isSpilled place]
          defFields = [(name, place) | (name, place) <- instrFields instr, name `elem` defs, isSpilled place]
          scratchPool = availableScratchRegisters instr
          scratchForUse index = scratchPool !! min index (length scratchPool - 1)
          useScratch = Map.fromList [(name, scratchForUse index) | (index, (name, _)) <- zip [(0 :: Int) ..] usedFields]
          defScratch = Map.fromList [(name, Map.findWithDefault spillScratchA name useScratch) | (name, _) <- defFields]
          scratchByField = Map.union useScratch defScratch
          loads = [Instr "ld" [("source", spillSlot), ("dest", scratch)] [] | scratch <- uniquePlaces (Map.elems useScratch)]
          rewritten =
            instr
              { instrFields =
                  [ (name, Map.findWithDefault place name scratchByField)
                  | (name, place) <- instrFields instr
                  ]
              }
          stores = [Instr "st" [("source", scratch), ("dest", spillSlot)] [] | scratch <- uniquePlaces (Map.elems defScratch)]
       in loads <> [rewritten] <> stores

    isSpilled place =
      isVirtualRegister place && placeInteger place == reg

    uniquePlaces = go []
      where
        go _ [] = []
        go seen (place : rest)
          | place `elem` seen = go seen rest
          | otherwise = place : go (place : seen) rest

availableScratchRegisters :: Instr -> [Place]
availableScratchRegisters instr =
  available <> occupiedScratch
  where
    scratch = [spillScratchA, spillScratchB]
    occupiedScratch = [place | (_, place) <- instrFields instr, place `elem` scratch]
    available = [place | place <- scratch, place `notElem` occupiedScratch]

rewriteInstrRegisters :: Map.Map Integer Integer -> Instr -> Instr
rewriteInstrRegisters colours instr =
  instr {instrFields = [(name, rewritePlace colours place) | (name, place) <- instrFields instr]}

rewritePlace :: Map.Map Integer Integer -> Place -> Place
rewritePlace colours place
  | placeType place `elem` ["t", "pr", "vr"] =
      case Map.lookup (placeInteger place) colours of
        Just colour -> Place "r" (show colour)
        Nothing -> place
  | otherwise = place

instructionUseDef :: Instr -> (Set.Set Integer, Set.Set Integer)
instructionUseDef instr =
  (regSet (useFieldNames instr), regSet (defFieldNames instr))
  where
    regSet names = Set.fromList [placeInteger place | name <- names, let place = fieldPlace name instr, isVirtualRegister place]

useFieldNames :: Instr -> [String]
useFieldNames instr =
  case instrType instr of
    "st" -> ["dest", "source"]
    "ld" -> ["source"]
    "push" -> ["target"]
    "pop" -> []
    "call" -> ["target"]
    "add3" -> ["source", "offset"]
    "ldoffset" -> ["source", "offset"]
    "cmp" -> ["first", "second"]
    "add" -> useDest
    "sub" -> useDest
    "mull" -> useDest
    "shl" -> useDest
    "shr" -> useDest
    "shr3" -> ["source", "third"]
    "xor" -> useDest
    "and" -> useDest
    "or" -> useDest
    "asm" -> []
    "mulh" -> ["source", "third"]
    "mull3" -> ["source", "third"]
    "sub3" -> ["source", "third"]
    ty
      | take 1 ty == "j" -> []
      | otherwise -> ["source"]
  where
    useDest = ["source", "dest"]

defFieldNames :: Instr -> [String]
defFieldNames instr =
  case instrType instr of
    "st" -> []
    "ld" -> ["dest"]
    "push" -> []
    "pop" -> ["target"]
    "call" -> []
    "add3" -> ["dest"]
    "ldoffset" -> ["dest"]
    "cmp" -> []
    "add" -> ["dest"]
    "sub" -> ["dest"]
    "mull" -> ["dest"]
    "shl" -> ["dest"]
    "shr" -> ["dest"]
    "shr3" -> ["dest"]
    "xor" -> ["dest"]
    "and" -> ["dest"]
    "or" -> ["dest"]
    "asm" -> []
    "mulh" -> ["dest"]
    "mull3" -> ["dest"]
    "sub3" -> ["dest"]
    ty
      | take 1 ty == "j" -> []
      | otherwise -> ["dest"]

isVirtualRegister :: Place -> Bool
isVirtualRegister place = placeType place `elem` ["t", "pr", "vr"]

peephole :: CodeGenOptions -> [Instr] -> [Instr]
peephole _ [] = []
peephole options instrs =
  foldr step [] instrs
  where
    step c acc@(nc : rest)
      | instrType c == "mov" && fieldPlace "source" c == fieldPlace "dest" c = acc
      | pureRegisterDefinition c
      , Just def <- singleDefPlace c
      , Just nextDef <- singleDefPlace nc
      , def == nextDef
      , def `notElem` usePlaces nc =
          acc
      | instrType c == "st" && instrType nc == "ld" && fieldPlace "dest" c == fieldPlace "source" nc =
          if fieldPlace "source" c == fieldPlace "dest" nc
            then c : rest
            else c : Instr "mov" [("source", fieldPlace "source" c), ("dest", fieldPlace "dest" nc)] [] : rest
      | instrType c == "add3" && instrType nc == "ld" && fieldPlace "dest" c == fieldPlace "source" nc && fieldPlace "dest" nc == fieldPlace "source" nc =
          Instr "ldoffset" [("source", fieldPlace "source" c), ("dest", fieldPlace "dest" nc), ("offset", fieldPlace "offset" c)] [] : rest
      | instrType c == "mov" && instrType nc == "ld" && fieldPlace "dest" c == fieldPlace "source" nc && fieldPlace "dest" nc == fieldPlace "source" nc =
          Instr "ld" [("source", fieldPlace "source" c), ("dest", fieldPlace "dest" nc)] [] : rest
      | instrType c == "add" && instrType nc == "ld" && fieldPlace "dest" c == fieldPlace "source" nc && fieldPlace "dest" nc == fieldPlace "source" nc =
          Instr "ldoffset" [("source", fieldPlace "source" nc), ("dest", fieldPlace "dest" nc), ("offset", fieldPlace "source" c)] [] : rest
      | instrType c == "mov" && instrType nc == "add" && fieldPlace "dest" c == fieldPlace "dest" nc && isVirtualRegister (fieldPlace "source" c) =
          Instr "add3" [("source", fieldPlace "source" c), ("dest", fieldPlace "dest" nc), ("offset", fieldPlace "source" nc)] [] : rest
      | instrType c == "mov" && instrType nc == "add" && fieldPlace "dest" c == fieldPlace "dest" nc && placeType (fieldPlace "source" c) == "i" && placeType (fieldPlace "source" nc) == "i" =
          Instr "mov" [("source", Place "i" (show (placeInteger (fieldPlace "source" c) + placeInteger (fieldPlace "source" nc)))), ("dest", fieldPlace "dest" nc)] [] : rest
      | instrType c == "add" && fieldPlace "source" c == Place "i" "0" = acc
      | instrType c == "add3" && instrType nc == "add" && fieldPlace "source" c == Place "r" "base_pointer" && placeType (fieldPlace "offset" c) == "i" && placeType (fieldPlace "source" nc) == "i" && fieldPlace "dest" c == fieldPlace "dest" nc =
          Instr "add3" [("source", Place "r" "base_pointer"), ("dest", fieldPlace "dest" c), ("offset", Place "i" (show (placeInteger (fieldPlace "offset" c) + placeInteger (fieldPlace "source" nc))))] [] : rest
      | instrType c == "jmp" && instrType nc == "label" && fieldPlace "target" c == fieldPlace "target" nc = acc
      | instrType c == "mov" && instrType nc == "cmp" && fieldPlace "dest" c == fieldPlace "second" nc && placeType (fieldPlace "source" c) `elem` ["i", "g"] =
          Instr "cmp" [("first", fieldPlace "first" nc), ("second", cmpImmediate options (fieldPlace "source" c))] [] : rest
      | otherwise = c : acc
    step c [] = [c]

    cmpImmediate opts place
      | placeType place == "g" = Place "i" (show (codeGenGlobalAddr opts + placeInteger place))
      | otherwise = place

finalizeOptimizedInstructions :: CodeGenOptions -> [Instr] -> [Instr]
finalizeOptimizedInstructions options = go (8 :: Int)
  where
    step = eliminateDeadPhysicalWrites . peephole options . collapseAdjacentLabels
    go 0 instrs = instrs
    go fuel instrs =
      let instrs' = step instrs
       in if instrs' == instrs then instrs else go (fuel - 1) instrs'

collapseAdjacentLabels :: [Instr] -> [Instr]
collapseAdjacentLabels instrs =
  map (rewriteLabelAliases aliases) kept
  where
    (aliases, keptRev, _) = foldl' step (Map.empty, [], Nothing) instrs
    kept = reverse keptRev

    step (aliasMap, out, previousLabel) instr
      | instrType instr == "label" =
          let target = fieldPlace "target" instr
           in case previousLabel of
                Just canonical ->
                  (Map.insert (placeValue target) (placeValue canonical) aliasMap, out, previousLabel)
                Nothing ->
                  (aliasMap, instr : out, Just target)
      | otherwise = (aliasMap, instr : out, Nothing)

rewriteLabelAliases :: Map.Map String String -> Instr -> Instr
rewriteLabelAliases aliases instr =
  instr {instrFields = [(name, rewritePlaceLabel place) | (name, place) <- instrFields instr]}
  where
    rewritePlaceLabel place
      | placeType place == "i" = place {placeValue = resolveLabelAlias aliases (placeValue place)}
      | otherwise = place

resolveLabelAlias :: Map.Map String String -> String -> String
resolveLabelAlias aliases = go Set.empty
  where
    go seen label
      | label `Set.member` seen = label
      | Just next <- Map.lookup label aliases = go (Set.insert label seen) next
      | otherwise = label

eliminateDeadPhysicalWrites :: [Instr] -> [Instr]
eliminateDeadPhysicalWrites instrs =
  [instr | (_, instr) <- sortBy compareIndexedInstruction kept]
  where
    blocks =
      sortBlocks $
        livenessAnalysis $
          map (buildBlockUseDefWith physicalInstructionUseDef) (buildBasicBlocks instrs)
    kept = concatMap keepBlock blocks

    keepBlock block =
      snd $
        foldr step (blockLiveOut block, []) (blockCode block)

    step indexed@(_, instr) (live, keptInstrs)
      | purePhysicalDefinition instr
      , let (_, defs) = physicalInstructionUseDef instr
      , Set.null (Set.intersection defs live) =
          (live, keptInstrs)
      | otherwise =
          let (uses, defs) = physicalInstructionUseDef instr
              live' = Set.union uses (Set.difference live defs)
           in (live', indexed : keptInstrs)

    compareIndexedInstruction (left, _) (right, _) = compare left right

physicalInstructionUseDef :: Instr -> (Set.Set Integer, Set.Set Integer)
physicalInstructionUseDef instr
  | instrType instr == "asm" = (allAllocatable, allAllocatable)
  | otherwise = (regSet (useFieldNames instr), regSet (defFieldNames instr))
  where
    allAllocatable = Set.fromList allocatableRegisters
    regSet names =
      Set.fromList
        [ reg
        | name <- names
        , Just reg <- [physicalAllocatableRegister (fieldPlace name instr)]
        ]

purePhysicalDefinition :: Instr -> Bool
purePhysicalDefinition instr =
  pureRegisterDefinition instr
    && case defFieldNames instr of
      [name] -> physicalAllocatableRegister (fieldPlace name instr) /= Nothing
      _ -> False

physicalAllocatableRegister :: Place -> Maybe Integer
physicalAllocatableRegister place
  | placeType place == "r" =
      case reads (placeValue place) of
        [(reg, "")] | reg `elem` allocatableRegisters -> Just reg
        _ -> Nothing
  | otherwise = Nothing

optimizeMethodTail :: MethodOutput -> [Instr] -> [Instr]
optimizeMethodTail method =
  removeTrailingDeadWrites . removeTrailingExitLabels . removeReturnRoundTrip . stripTrailingExitJump
  where
    exitTarget = Place "i" (".exit_" <> methodOutputName method)

    stripTrailingExitJump instrs =
      case reverse instrs of
        instr : rest
          | instrType instr == "jmp"
              && fieldPlace "target" instr == exitTarget ->
              reverse rest
        _ -> instrs

    removeTrailingExitLabels instrs =
      case span isLabelInstruction (reverse instrs) of
        ([], _) -> instrs
        (labelsRev, restRev) ->
          let aliases =
                Map.fromList
                  [ (placeValue (fieldPlace "target" label), placeValue exitTarget)
                  | label <- labelsRev
                  ]
           in map (rewriteLabelAliases aliases) (reverse restRev)

removeReturnRoundTrip :: [Instr] -> [Instr]
removeReturnRoundTrip instrs =
  case reverse instrs of
    restore : save : rest
      | instrType save == "mov"
          && instrType restore == "mov"
          && fieldPlace "source" save == Place "r" "return_reg"
          && fieldPlace "dest" save == fieldPlace "source" restore
          && fieldPlace "dest" restore == Place "r" "return_reg" ->
          reverse rest
    _ -> instrs

removeTrailingDeadWrites :: [Instr] -> [Instr]
removeTrailingDeadWrites =
  reverse . dropWhile trailingDeadWrite . reverse

trailingDeadWrite :: Instr -> Bool
trailingDeadWrite instr =
  pureRegisterDefinition instr
    && maybe False isCallerSafePhysicalRegister (singleDefPlace instr)

isLabelInstruction :: Instr -> Bool
isLabelInstruction instr = instrType instr == "label"

pureRegisterDefinition :: Instr -> Bool
pureRegisterDefinition instr =
  instrType instr `elem` ["mov", "add", "sub", "mull", "shl", "shr", "xor", "and", "or", "add3", "shr3", "mulh", "mull3", "sub3"]

singleDefPlace :: Instr -> Maybe Place
singleDefPlace instr =
  case defFieldNames instr of
    [name] -> Just (fieldPlace name instr)
    _ -> Nothing

usePlaces :: Instr -> [Place]
usePlaces instr =
  [fieldPlace name instr | name <- useFieldNames instr]

isCallerSafePhysicalRegister :: Place -> Bool
isCallerSafePhysicalRegister place =
  case allocatedPhysicalRegister place of
    Just _ -> True
    Nothing -> False

instrTouchesFrame :: Instr -> Bool
instrTouchesFrame instr =
  any placeTouchesFrame (map snd (instrFields instr))

placeTouchesFrame :: Place -> Bool
placeTouchesFrame place =
  placeType place `elem` ["l", "p"]
    || place == Place "r" "base_pointer"

renderInstr :: CodeGenOptions -> Integer -> Instr -> String
renderInstr options localSize instr =
  "\t" <> renderInstruction options localSize instr <> "\n"

renderInstruction :: CodeGenOptions -> Integer -> Instr -> String
renderInstruction options localSize instr =
  case instrType instr of
    "call" -> "call " <> renderCallTarget (fieldPlace "target" instr)
    "st" -> "st " <> asReg (fieldPlace "source" instr) <> ", " <> asMemory options localSize (fieldPlace "dest" instr)
    "ld" -> "ld " <> asReg (fieldPlace "dest" instr) <> ", " <> asMemory options localSize (fieldPlace "source" instr)
    "push" -> "push " <> asReg (fieldPlace "target" instr)
    "pop" -> "pop " <> asReg (fieldPlace "target" instr)
    "ret" -> "ret"
    "label" -> placeValue (fieldPlace "target" instr) <> ":"
    "cmp" -> "cmp " <> asReg (fieldPlace "first" instr) <> ", " <> renderImmediateOrReg (fieldPlace "second" instr)
    "nop" -> "nop"
    "add3" -> "add " <> asReg (fieldPlace "dest" instr) <> ", " <> renderImmediateOrReg (fieldPlace "source" instr) <> ", " <> renderImmediateOrReg (fieldPlace "offset" instr)
    "ldoffset" -> "ld " <> asReg (fieldPlace "dest" instr) <> ", " <> asReg (fieldPlace "source" instr) <> ", " <> renderImmediateOrReg (fieldPlace "offset" instr)
    "asm" -> fieldString "asm" instr
    "mulh" -> "mulh " <> asReg (fieldPlace "dest" instr) <> ", " <> asReg (fieldPlace "source" instr) <> ", " <> renderImmediateOrReg (fieldPlace "third" instr)
    "mull3" -> "mul " <> asReg (fieldPlace "dest" instr) <> ", " <> asReg (fieldPlace "source" instr) <> ", " <> renderImmediateOrReg (fieldPlace "third" instr)
    "sub3" -> "sub " <> asReg (fieldPlace "dest" instr) <> ", " <> asReg (fieldPlace "source" instr) <> ", " <> renderImmediateOrReg (fieldPlace "third" instr)
    ty
      | take 1 ty == "j" -> ty <> " " <> placeValue (fieldPlace "target" instr)
      | last ty == '3' -> take (length ty - 1) ty <> " " <> asReg (fieldPlace "dest" instr) <> ", " <> asReg (fieldPlace "source" instr) <> ", " <> renderImmediateOrReg (fieldPlace "third" instr)
      | otherwise -> ty <> " " <> asReg (fieldPlace "dest" instr) <> ", " <> renderImmediateOrMemory options localSize (fieldPlace "source" instr)

fieldPlace :: String -> Instr -> Place
fieldPlace name instr =
  case lookup name (instrFields instr) of
    Just place -> place
    Nothing -> Place "i" "0"

fieldString :: String -> Instr -> String
fieldString name instr =
  case lookup name (instrStringFields instr) of
    Just value -> value
    Nothing -> ""

renderCallTarget :: Place -> String
renderCallTarget place
  | placeType place == "i" = placeValue place
  | otherwise = asReg place

renderImmediateOrReg :: Place -> String
renderImmediateOrReg place
  | placeType place == "i" = placeValue place
  | otherwise = asReg place

renderImmediateOrMemory :: CodeGenOptions -> Integer -> Place -> String
renderImmediateOrMemory options localSize place
  | placeType place == "i" = placeValue place
  | otherwise = asMemory options localSize place

asMemory :: CodeGenOptions -> Integer -> Place -> String
asMemory options localSize place =
  case placeType place of
    "g" -> show (codeGenGlobalAddr options + placeInteger place)
    "l" -> "base_pointer, " <> show (placeInteger place + 1)
    "p" -> "base_pointer, " <> show (localSize + placeInteger place + 2)
    "i" -> placeValue place
    _ -> asReg place

asReg :: Place -> String
asReg place =
  case placeType place of
    "r"
      | all (`elem` ['0' .. '9']) (placeValue place) -> "r" <> placeValue place
      | otherwise -> placeValue place
    _ -> "r" <> placeValue place

placeInteger :: Place -> Integer
placeInteger = read . placeValue
