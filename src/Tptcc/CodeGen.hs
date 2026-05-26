module Tptcc.CodeGen
  ( CodeGenOptions (..)
  , defaultCodeGenOptions
  , dumpNativeAsmUnoptimized
  , dumpNativeAsmOptimized
  , dumpNativeAsmOptimizedWithOptions
  ) where

import Data.List (sortBy)
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
  | otherwise = Right (renderProgram options optimized program globalInfo stdlib)

renderProgram :: CodeGenOptions -> Bool -> TacProgram -> GlobalInfo -> [String] -> String
renderProgram options optimized program globalInfo stdlib =
  header options globalInfo
    <> renderGlobalInstructions options optimized (tacProgramGlobalInstructions program)
    <> entryJump
    <> concatMap (renderMethod options optimized) (tacProgramMethods program)
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

renderGlobalInstructions :: CodeGenOptions -> Bool -> [Instr] -> String
renderGlobalInstructions options optimized instructions =
  concatMap (renderInstr options 0) lowered
  where
    abstractLowered = map (lowerAbstract options 0) instructions
    (allocated, _) =
      if optimized
        then allocateRegisters abstractLowered
        else (abstractLowered, [])
    lowered =
      if optimized
        then peephole options allocated
        else allocated

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

renderMethod :: CodeGenOptions -> Bool -> MethodOutput -> String
renderMethod options optimized method =
  "__tptcc_fn_"
    <> methodOutputName method
    <> ":\n"
    <> localAllocation
    <> frameSetup
    <> concatMap (\reg -> "\tpush r" <> show reg <> "\n") savedRegisters
    <> concatMap (renderInstr options localSize) lowered
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
    localSize = methodOutputLocalSize method
    abstractLowered = map (lowerAbstract options localSize) (methodOutputInstructions method)
    (allocated, usedRegisters) =
      if optimized
        then allocateRegisters abstractLowered
        else (abstractLowered, [])
    lowered =
      if optimized
        then optimizeMethodTail method (peephole options allocated)
        else allocated
    savedRegisters
      | methodOutputName method == "main" = []
      | otherwise = usedRegisters
    needsFrame = not optimized || localSize > 0 || any instrTouchesFrame lowered
    frameSetup
      | needsFrame = "\tpush base_pointer\n\tmov base_pointer, stack_pointer\n"
      | otherwise = ""
    frameTeardown
      | needsFrame = "\tpop base_pointer\n"
      | otherwise = ""
    localAllocation
      | localSize > 0 = "\tsub stack_pointer, " <> show localSize <> "\n"
      | otherwise = ""
    localRelease
      | localSize > 0 = "\tadd stack_pointer, " <> show localSize <> "\n"
      | otherwise = ""

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

allocateRegisters :: [Instr] -> ([Instr], [Integer])
allocateRegisters tac =
  let blocks0 = buildBasicBlocks tac
      blocks1 = map buildBlockUseDef blocks0
      blocks2 = livenessAnalysis blocks1
      ordered = sortBlocks blocks2
      blocks3 = map computePerInstructionLiveness ordered
      graph = buildInterferenceGraph blocks3
      colourOrder = luaIntegerPairsOrder (Map.keys graph)
      colours = colourGraph colourOrder graph
      rewritten = map (rewriteInstrRegisters colours) tac
      used = uniqueInOrder [colours Map.! reg | reg <- colourOrder, Map.member reg colours]
   in (rewritten, used)

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
buildBlockUseDef block =
  block {blockUse = uses, blockDef = defs}
  where
    (_, uses, defs) = foldl' step (Set.empty, Set.empty, Set.empty) (map snd (blockCode block))
    step (seenDef, useAcc, defAcc) instr =
      let (useSet, defSet) = instructionUseDef instr
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

colourGraph :: [Integer] -> Map.Map Integer (Set.Set Integer) -> Map.Map Integer Integer
colourGraph order graph =
  foldl' colourOne Map.empty order
  where
    colourOne mapping reg =
      let usedColours = Set.fromList [usedColour | neighbour <- Set.toList (Map.findWithDefault Set.empty reg graph), Just usedColour <- [Map.lookup neighbour mapping]]
          chosenColour = firstAvailableColour usedColours 1
       in Map.insert reg chosenColour mapping

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

firstAvailableColour :: Set.Set Integer -> Integer -> Integer
firstAvailableColour used candidate
  | Set.member candidate used = firstAvailableColour used (candidate + 1)
  | otherwise = candidate

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
  case instrType instr of
    "st" -> (regSet ["dest", "source"], Set.empty)
    "ld" -> (regSet ["source"], regSet ["dest"])
    "push" -> (regSet ["target"], Set.empty)
    "pop" -> (Set.empty, regSet ["target"])
    "call" -> (regSet ["target"], Set.empty)
    "add3" -> (regSet ["source", "offset"], regSet ["dest"])
    "ldoffset" -> (regSet ["source", "offset"], regSet ["dest"])
    "cmp" -> (regSet ["first", "second"], Set.empty)
    "add" -> useDest
    "sub" -> useDest
    "mull" -> useDest
    "shl" -> useDest
    "shr" -> useDest
    "shr3" -> (regSet ["source", "third"], regSet ["dest"])
    "xor" -> useDest
    "and" -> useDest
    "or" -> useDest
    "asm" -> (Set.empty, Set.empty)
    "mulh" -> (regSet ["source", "third"], regSet ["dest"])
    "mull3" -> (regSet ["source", "third"], regSet ["dest"])
    "sub3" -> (regSet ["source", "third"], regSet ["dest"])
    _ -> (regSet ["source"], regSet ["dest"])
  where
    useDest = (regSet ["source", "dest"], regSet ["dest"])
    regSet names = Set.fromList [placeInteger place | name <- names, let place = fieldPlace name instr, isVirtualRegister place]

isVirtualRegister :: Place -> Bool
isVirtualRegister place = placeType place `elem` ["t", "pr", "vr"]

peephole :: CodeGenOptions -> [Instr] -> [Instr]
peephole _ [] = []
peephole options instrs =
  foldr step [] instrs
  where
    step c acc@(nc : rest)
      | instrType c == "mov" && fieldPlace "source" c == fieldPlace "dest" c = acc
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

optimizeMethodTail :: MethodOutput -> [Instr] -> [Instr]
optimizeMethodTail method =
  removeReturnRoundTrip . stripTrailingExitJump
  where
    exitTarget = Place "i" (".exit_" <> methodOutputName method)

    stripTrailingExitJump instrs =
      case reverse instrs of
        instr : rest
          | instrType instr == "jmp"
              && fieldPlace "target" instr == exitTarget ->
              reverse rest
        _ -> instrs

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
