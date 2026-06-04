module Tptcc.CodeGen.Optimize
  ( allocateMethodRegisters
  , allocateRegisters
  , cleanupAllocatedInstructions
  , instrTouchesFrame
  , optimizeInstructions
  , optimizeMethodTail
  , promoteScalarLocals
  , usedAllocatedRegisters
  ) where

import Control.Applicative ((<|>))
import Data.List (maximumBy, sortBy)
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Tptcc.Tac

promoteScalarLocals :: [Instr] -> [Instr]
promoteScalarLocals instrs =
  map rewriteInstr instrs
  where
    addressTaken =
      Set.fromList
        [ placeInteger target
        | instr <- instrs
        , instrType instr == IGetAddress
        , let target = fieldPlace FieldTarget instr
        , placeKind target == Local
        ]
    localSlots =
      Set.fromList
        [ placeInteger place
        | instr <- instrs
        , (_, place) <- instrFields instr
        , placeKind place == Local
        ]
    promotable = Set.difference localSlots addressTaken
    firstTemp = 1 + maximum (0 : [placeInteger place | instr <- instrs, (_, place) <- instrFields instr, isVirtualRegister place])
    localRegisters =
      Map.fromList
        [ (slot, temporaryPlace (firstTemp + fromIntegral index))
        | (index, slot) <- zip [(0 :: Int) ..] (Set.toAscList promotable)
        ]

    promotedLocal place =
      Map.lookup (placeInteger place) localRegisters

    rewriteInstr instr
      | instrType instr == ISt
      , placeKind (fieldPlace FieldDest instr) == Local
      , Just dest <- promotedLocal (fieldPlace FieldDest instr) =
          Instr IMov [(FieldSource, fieldPlace FieldSource instr), (FieldDest, dest)] []
      | instrType instr == ILd
      , placeKind (fieldPlace FieldSource instr) == Local
      , Just source <- promotedLocal (fieldPlace FieldSource instr) =
          Instr IMov [(FieldSource, source), (FieldDest, fieldPlace FieldDest instr)] []
      | otherwise = instr

optimizeInstructions :: [Instr] -> [Instr]
optimizeInstructions = eliminateDeadVirtualWrites . propagateCopiesAndConstants

cleanupAllocatedInstructions :: [Instr] -> [Instr]
cleanupAllocatedInstructions [] = []
cleanupAllocatedInstructions instrs =
  cleanupPass (cleanupPass instrs)
  where
    cleanupPass = foldr step []

    step instr acc@(next : rest)
      | instrType instr == IMov && fieldPlace FieldSource instr == fieldPlace FieldDest instr = acc
      | pureRegisterDefinition instr
      , Just def <- singleDefPlace instr
      , Just nextDef <- singleDefPlace next
      , def == nextDef
      , def `notElem` usePlaces next =
          acc
      | instrType instr == ISt
      , instrType next == ILd
      , fieldPlace FieldDest instr == fieldPlace FieldSource next =
          if fieldPlace FieldSource instr == fieldPlace FieldDest next
            then instr : rest
            else instr : Instr IMov [(FieldSource, fieldPlace FieldSource instr), (FieldDest, fieldPlace FieldDest next)] [] : rest
      | instrType instr == IAdd3
      , instrType next == ILd
      , fieldPlace FieldDest instr == fieldPlace FieldSource next
      , fieldPlace FieldDest next == fieldPlace FieldSource next =
          Instr ILdOffset [(FieldSource, fieldPlace FieldSource instr), (FieldDest, fieldPlace FieldDest next), (FieldOffset, fieldPlace FieldOffset instr)] [] : rest
      | instrType instr == IMov
      , instrType next == ILd
      , fieldPlace FieldDest instr == fieldPlace FieldSource next
      , fieldPlace FieldDest next == fieldPlace FieldSource next =
          Instr ILd [(FieldSource, fieldPlace FieldSource instr), (FieldDest, fieldPlace FieldDest next)] [] : rest
      | instrType instr == IAdd
      , instrType next == ILd
      , fieldPlace FieldDest instr == fieldPlace FieldSource next
      , fieldPlace FieldDest next == fieldPlace FieldSource next =
          Instr ILdOffset [(FieldSource, fieldPlace FieldSource next), (FieldDest, fieldPlace FieldDest next), (FieldOffset, fieldPlace FieldSource instr)] [] : rest
      | instrType instr == IMov
      , instrType next == IAdd
      , fieldPlace FieldDest instr == fieldPlace FieldDest next
      , isVirtualRegister (fieldPlace FieldSource instr) =
          Instr IAdd3 [(FieldSource, fieldPlace FieldSource instr), (FieldDest, fieldPlace FieldDest next), (FieldOffset, fieldPlace FieldSource next)] [] : rest
      | instrType instr == IMov
      , instrType next == IAdd
      , fieldPlace FieldDest instr == fieldPlace FieldDest next
      , placeKind (fieldPlace FieldSource instr) == Immediate
      , placeKind (fieldPlace FieldSource next) == Immediate
      , Just first <- placeIntegerMaybe (fieldPlace FieldSource instr)
      , Just second <- placeIntegerMaybe (fieldPlace FieldSource next) =
          Instr IMov [(FieldSource, immediateInteger (first + second)), (FieldDest, fieldPlace FieldDest next)] [] : rest
      | instrType instr == IAdd && fieldPlace FieldSource instr == immediateInteger 0 = acc
      | instrType instr == IAdd3
      , instrType next == IAdd
      , fieldPlace FieldSource instr == specialRegister BasePointer
      , placeKind (fieldPlace FieldOffset instr) == Immediate
      , placeKind (fieldPlace FieldSource next) == Immediate
      , fieldPlace FieldDest instr == fieldPlace FieldDest next
      , Just offset <- placeIntegerMaybe (fieldPlace FieldOffset instr)
      , Just source <- placeIntegerMaybe (fieldPlace FieldSource next) =
          Instr IAdd3 [(FieldSource, specialRegister BasePointer), (FieldDest, fieldPlace FieldDest instr), (FieldOffset, immediateInteger (offset + source))] [] : rest
      | instrType instr == IJmp
      , instrType next == ILabel
      , fieldPlace FieldTarget instr == fieldPlace FieldTarget next =
          acc
      | otherwise = instr : acc
    step instr [] = [instr]

type CopyEnv = Map.Map (PlaceKind, Text) Place

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
  instrType instr == ILabel
    || instrType instr == ICall
    || instrType instr == IAsm
    || instrType instr == IRet
    || isJumpInstruction (instrType instr)

rewriteInstructionUses :: CopyEnv -> Instr -> Instr
rewriteInstructionUses env instr =
  instr {instrFields = [(name, rewriteUse name place) | (name, place) <- instrFields instr]}
  where
    uses = [name | name <- useFieldNames instr, name `notElem` defFieldNames instr]
    rewriteUse name place
      | name `elem` uses =
          let resolved = resolveEnvPlace env place
           in if placeKind resolved == Immediate && not (fieldAcceptsImmediate instr name)
                then place
                else resolved
      | otherwise = place

fieldAcceptsImmediate :: Instr -> InstrFieldName -> Bool
fieldAcceptsImmediate instr name =
  case instrType instr of
    IMov -> name == FieldSource
    ICmp -> False
    IAdd3 -> name `elem` [FieldSource, FieldOffset]
    ILdOffset -> name == FieldOffset
    IMulh -> name == FieldThird
    IMull3 -> name == FieldThird
    ISub3 -> name == FieldThird
    IShr3 -> name == FieldThird
    IAdd -> name == FieldSource
    ISub -> name == FieldSource
    IMull -> name == FieldSource
    IShl -> name == FieldSource
    IShr -> name == FieldSource
    IXor -> name == FieldSource
    IAnd -> name == FieldSource
    IOr -> name == FieldSource
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

definedPlaceKeys :: Instr -> [(PlaceKind, Text)]
definedPlaceKeys instr =
  [ key
  | name <- defFieldNames instr
  , let place = fieldPlace name instr
  , Just key <- [placeKey place]
  ]

updateCopyEnv :: CopyEnv -> Instr -> CopyEnv
updateCopyEnv env instr
  | instrType instr == IMov
  , Just destKey <- placeKey (fieldPlace FieldDest instr)
  , isPropagatablePlace source =
      Map.insert destKey source env
  | otherwise = env
  where
    source = fieldPlace FieldSource instr

isPropagatablePlace :: Place -> Bool
isPropagatablePlace place =
  placeKind place `elem` [Immediate, Temporary, VirtualRegister, PointerRegister, Register]

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
  { blockId :: Text
  , blockCode :: [(Int, Instr)]
  , blockPred :: [Text]
  , blockSucc :: [Text]
  , blockUse :: Set.Set Integer
  , blockDef :: Set.Set Integer
  , blockLiveIn :: Set.Set Integer
  , blockLiveOut :: Set.Set Integer
  , blockPerOut :: [Set.Set Integer]
  }
  deriving (Eq, Show)

emptyBlock :: Text -> [Text] -> [Text] -> BasicBlock
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
allocateRegisters tac = do
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

allocateMethodRegisters :: Integer -> [Instr] -> Either String ([Instr], [Integer], Integer)
allocateMethodRegisters nextSpillSlot currentTac =
  case colourTac currentTac of
    Right colours ->
      let rewritten = map (rewriteInstrRegisters colours) currentTac
          used = usedAllocatedRegisters rewritten
       in pure (rewritten, used, nextSpillSlot)
    Left reg ->
      allocateMethodRegisters (nextSpillSlot + 1) (spillVirtualRegister reg (localPlace nextSpillSlot) currentTac)

colourTac :: [Instr] -> Either Integer (Map.Map Integer Integer)
colourTac tac =
  let blocks0 = buildBasicBlocks tac
      blocks1 = map buildBlockUseDef blocks0
      blocks2 = livenessAnalysis blocks1
      ordered = sortBlocks blocks2
      blocks3 = map computePerInstructionLiveness ordered
      graph = buildInterferenceGraph blocks3
      moves = moveSet tac
   in colourGraph moves graph

buildBasicBlocks :: [Instr] -> [BasicBlock]
buildBasicBlocks tac = finalBlocks
  where
    step (currentId, blockMap, orderRev, orderLength) (index, instr)
      | instrType instr == ILabel =
          let target = placeValue (fieldPlace FieldTarget instr)
              blockMap1 = addSucc currentId target blockMap
              (blockMap2, orderRev2, orderLength2) =
                if Map.member target blockMap1
                  then (addPred target currentId blockMap1, orderRev, orderLength)
                  else insertBlock (emptyBlock target [currentId] []) blockMap1 orderRev orderLength
              blockMap3 = Map.adjust (\b -> b {blockCode = [(index, instr)]}) target blockMap2
           in (target, blockMap3, orderRev2, orderLength2)
      | instrType instr == IJmp =
          let target = placeValue (fieldPlace FieldTarget instr)
              blockMap1 = addSucc currentId target blockMap
              (blockMap2, orderRev2, orderLength2) =
                if Map.member target blockMap1
                  then (addPred target currentId blockMap1, orderRev, orderLength)
                  else insertBlock (emptyBlock target [currentId] []) blockMap1 orderRev orderLength
              blockMap3 = prependCode currentId (index, instr) blockMap2
              anonId = "anon_block" <> Text.pack (show orderLength2)
              (blockMap4, orderRev3, orderLength3) = insertBlock (emptyBlock anonId [] []) blockMap3 orderRev2 orderLength2
           in (anonId, blockMap4, orderRev3, orderLength3)
      | isJumpInstruction (instrType instr) =
          let target = placeValue (fieldPlace FieldTarget instr)
              blockMap1 = addSucc currentId target blockMap
              (blockMap2, orderRev2, orderLength2) =
                if Map.member target blockMap1
                  then (addPred target currentId blockMap1, orderRev, orderLength)
                  else insertBlock (emptyBlock target [currentId] []) blockMap1 orderRev orderLength
              blockMap3 = prependCode currentId (index, instr) blockMap2
              anonId = "anon_block" <> Text.pack (show orderLength2)
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

insertBlock :: BasicBlock -> Map.Map Text BasicBlock -> [Text] -> Int -> (Map.Map Text BasicBlock, [Text], Int)
insertBlock block blockMap orderRev orderLength =
  (Map.insert (blockId block) block blockMap, blockId block : orderRev, orderLength + 1)

prependCode :: Text -> (Int, Instr) -> Map.Map Text BasicBlock -> Map.Map Text BasicBlock
prependCode ident code =
  Map.adjust (\block -> block {blockCode = code : blockCode block}) ident

addSucc :: Text -> Text -> Map.Map Text BasicBlock -> Map.Map Text BasicBlock
addSucc ident succId =
  Map.adjust (\block -> block {blockSucc = succId : blockSucc block}) ident

addPred :: Text -> Text -> Map.Map Text BasicBlock -> Map.Map Text BasicBlock
addPred ident predId =
  Map.adjust (\block -> block {blockPred = predId : blockPred block}) ident

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

livenessFixedPoint :: Map.Map Text BasicBlock -> Map.Map Text BasicBlock
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
buildInterferenceGraph =
  foldl' addBlock Map.empty
  where
    addBlock graph block =
      foldl' addInstr graph (zip (map snd (blockCode block)) (blockPerOut block))
    addInstr graph (instr, liveOut) =
      let (_, defSet) = instructionUseDef instr
       in foldl' (\graph' def -> addDefInterference graph' def liveOut) graph (Set.toList defSet)
    addDefInterference graph def liveOut =
      foldl' (addGraphEdge def) (Map.insertWith Set.union def Set.empty graph) (Set.toList liveOut)
    addGraphEdge a graph b
      | a == b = graph
      | otherwise =
          Map.insertWith Set.union b (Set.singleton a) $
            Map.insertWith Set.union a (Set.singleton b) graph

data AllocMove = AllocMove Integer Integer
  deriving stock (Eq, Ord, Show)

data AllocState = AllocState
  { allocAdjacency :: Map.Map Integer (Set.Set Integer)
  , allocDegree :: Map.Map Integer Int
  , allocMoveList :: Map.Map Integer (Set.Set AllocMove)
  , allocAlias :: Map.Map Integer Integer
  , allocSelectStack :: [Integer]
  , allocSimplifyWorklist :: Set.Set Integer
  , allocFreezeWorklist :: Set.Set Integer
  , allocSpillWorklist :: Set.Set Integer
  , allocSpilledNodes :: Set.Set Integer
  , allocCoalescedNodes :: Set.Set Integer
  , allocWorklistMoves :: Set.Set AllocMove
  , allocActiveMoves :: Set.Set AllocMove
  , allocCoalescedMoves :: Set.Set AllocMove
  , allocConstrainedMoves :: Set.Set AllocMove
  , allocFrozenMoves :: Set.Set AllocMove
  }
  deriving stock (Eq, Show)

colourGraph :: Set.Set AllocMove -> Map.Map Integer (Set.Set Integer) -> Either Integer (Map.Map Integer Integer)
colourGraph moves graph =
  assignColours (allocateWorklists initialState)
  where
    nodes = Set.unions (Set.fromList (Map.keys graph) : Map.elems graph <> [moveNodes moves])
    initialState =
      makeWorklists
        AllocState
          { allocAdjacency = Map.unionWith Set.union graph (Map.fromSet (const Set.empty) nodes)
          , allocDegree = Map.fromSet (\node -> Set.size (Map.findWithDefault Set.empty node graph)) nodes
          , allocMoveList = foldl' addMoveForNodes Map.empty (Set.toList moves)
          , allocAlias = Map.empty
          , allocSelectStack = []
          , allocSimplifyWorklist = Set.empty
          , allocFreezeWorklist = Set.empty
          , allocSpillWorklist = Set.empty
          , allocSpilledNodes = Set.empty
          , allocCoalescedNodes = Set.empty
          , allocWorklistMoves = moves
          , allocActiveMoves = Set.empty
          , allocCoalescedMoves = Set.empty
          , allocConstrainedMoves = Set.empty
          , allocFrozenMoves = Set.empty
          }
        nodes

allocateWorklists :: AllocState -> AllocState
allocateWorklists state
  | Just node <- popMin (allocSimplifyWorklist state) =
      allocateWorklists (simplifyNode node state)
  | Just move <- popMin (allocWorklistMoves state) =
      allocateWorklists (coalesceMove move state)
  | Just node <- popMin (allocFreezeWorklist state) =
      allocateWorklists (freezeNode node state)
  | Just node <- chooseSpillNode state =
      allocateWorklists (selectSpillNode node state)
  | otherwise = state

makeWorklists :: AllocState -> Set.Set Integer -> AllocState
makeWorklists =
  Set.foldl' addInitialNode
  where
    addInitialNode state node
      | nodeDegree state node >= registerCount =
          state {allocSpillWorklist = Set.insert node (allocSpillWorklist state)}
      | moveRelated state node =
          state {allocFreezeWorklist = Set.insert node (allocFreezeWorklist state)}
      | otherwise =
          state {allocSimplifyWorklist = Set.insert node (allocSimplifyWorklist state)}

simplifyNode :: Integer -> AllocState -> AllocState
simplifyNode node state =
  foldl' (flip decrementDegree) state1 (Set.toList (adjacentNodes state node))
  where
    state1 =
      state
        { allocSimplifyWorklist = Set.delete node (allocSimplifyWorklist state)
        , allocSelectStack = node : allocSelectStack state
        }

coalesceMove :: AllocMove -> AllocState -> AllocState
coalesceMove move@(AllocMove x y) state =
  case (getAlias state x, getAlias state y) of
    (u, v)
      | u == v ->
          addWorklist u (moveFromWorklist move (\moves -> state {allocCoalescedMoves = Set.insert move (allocCoalescedMoves state), allocWorklistMoves = moves}) state)
      | adjacentInGraph state u v ->
          addWorklist v $
            addWorklist u $
              moveFromWorklist move (\moves -> state {allocConstrainedMoves = Set.insert move (allocConstrainedMoves state), allocWorklistMoves = moves}) state
      | conservative state (Set.union (adjacentNodes state u) (adjacentNodes state v)) ->
          addWorklist u (combineNodes u v (moveFromWorklist move (\moves -> state {allocCoalescedMoves = Set.insert move (allocCoalescedMoves state), allocWorklistMoves = moves}) state))
      | otherwise ->
          moveFromWorklist move (\moves -> state {allocActiveMoves = Set.insert move (allocActiveMoves state), allocWorklistMoves = moves}) state

freezeNode :: Integer -> AllocState -> AllocState
freezeNode node state =
  freezeMoves node $
    state
      { allocFreezeWorklist = Set.delete node (allocFreezeWorklist state)
      , allocSimplifyWorklist = Set.insert node (allocSimplifyWorklist state)
      }

selectSpillNode :: Integer -> AllocState -> AllocState
selectSpillNode node state =
  freezeMoves node $
    state
      { allocSpillWorklist = Set.delete node (allocSpillWorklist state)
      , allocSimplifyWorklist = Set.insert node (allocSimplifyWorklist state)
      }

decrementDegree :: Integer -> AllocState -> AllocState
decrementDegree node state
  | oldDegree /= registerCount = state {allocDegree = Map.insert node (max 0 (oldDegree - 1)) (allocDegree state)}
  | otherwise =
      let nodes = Set.insert node (adjacentNodes state node)
          state1 = enableMoves nodes state
          state2 =
            state1
              { allocSpillWorklist = Set.delete node (allocSpillWorklist state1)
              , allocDegree = Map.insert node (oldDegree - 1) (allocDegree state1)
              }
       in if moveRelated state2 node
            then state2 {allocFreezeWorklist = Set.insert node (allocFreezeWorklist state2)}
            else state2 {allocSimplifyWorklist = Set.insert node (allocSimplifyWorklist state2)}
  where
    oldDegree = nodeDegree state node

enableMoves :: Set.Set Integer -> AllocState -> AllocState
enableMoves nodes state =
  foldl' enableOne state [move | node <- Set.toList nodes, move <- Set.toList (nodeMoves state node)]
  where
    enableOne st move
      | move `Set.member` allocActiveMoves st =
          st
            { allocActiveMoves = Set.delete move (allocActiveMoves st)
            , allocWorklistMoves = Set.insert move (allocWorklistMoves st)
            }
      | otherwise = st

combineNodes :: Integer -> Integer -> AllocState -> AllocState
combineNodes kept removed state =
  foldl' combineAdjacent state4 (Set.toList (adjacentNodes state removed))
  where
    state1 =
      state
        { allocFreezeWorklist = Set.delete removed (allocFreezeWorklist state)
        , allocSpillWorklist = Set.delete removed (allocSpillWorklist state)
        , allocCoalescedNodes = Set.insert removed (allocCoalescedNodes state)
        , allocAlias = Map.insert removed kept (allocAlias state)
        , allocMoveList =
            Map.insertWith
              Set.union
              kept
              (Map.findWithDefault Set.empty removed (allocMoveList state))
              (allocMoveList state)
        }
    state2 = enableMoves (Set.singleton removed) state1
    state3 =
      if nodeDegree state2 kept >= registerCount && kept `Set.member` allocFreezeWorklist state2
        then
          state2
            { allocFreezeWorklist = Set.delete kept (allocFreezeWorklist state2)
            , allocSpillWorklist = Set.insert kept (allocSpillWorklist state2)
            }
        else state2
    state4 = state3
    combineAdjacent st neighbor =
      decrementDegree neighbor (addEdge kept neighbor st)

freezeMoves :: Integer -> AllocState -> AllocState
freezeMoves node state =
  foldl' freezeOne state (Set.toList (nodeMoves state node))
  where
    freezeOne st move@(AllocMove x y) =
      let aliasedX = getAlias st x
          aliasedY = getAlias st y
          other = if aliasedY == getAlias st node then aliasedX else aliasedY
          st1 =
            st
              { allocActiveMoves = Set.delete move (allocActiveMoves st)
              , allocWorklistMoves = Set.delete move (allocWorklistMoves st)
              , allocFrozenMoves = Set.insert move (allocFrozenMoves st)
              }
       in if Set.null (nodeMoves st1 other) && nodeDegree st1 other < registerCount
            then
              st1
                { allocFreezeWorklist = Set.delete other (allocFreezeWorklist st1)
                , allocSimplifyWorklist = Set.insert other (allocSimplifyWorklist st1)
                }
            else st1

addWorklist :: Integer -> AllocState -> AllocState
addWorklist node state
  | not (moveRelated state node)
      && nodeDegree state node < registerCount
      && node `Set.member` allocFreezeWorklist state =
      state
        { allocFreezeWorklist = Set.delete node (allocFreezeWorklist state)
        , allocSimplifyWorklist = Set.insert node (allocSimplifyWorklist state)
        }
  | otherwise = state

addEdge :: Integer -> Integer -> AllocState -> AllocState
addEdge left right state
  | left == right || adjacentInGraph state left right = state
  | otherwise =
      state
        { allocAdjacency =
            Map.insertWith Set.union right (Set.singleton left) $
              Map.insertWith Set.union left (Set.singleton right) (allocAdjacency state)
        , allocDegree = Map.adjust (+ 1) right (Map.adjust (+ 1) left (allocDegree state))
        }

assignColours :: AllocState -> Either Integer (Map.Map Integer Integer)
assignColours state =
  let (spilled, colours) = foldl' colourOne (allocSpilledNodes state, Map.empty) (allocSelectStack state)
      coloursWithCoalesced =
        foldl'
          (\acc node -> Map.insert node (acc Map.! getAlias state node) acc)
          colours
          (Set.toList (allocCoalescedNodes state))
   in case Set.minView spilled of
        Just (node, _) -> Left node
        Nothing -> Right coloursWithCoalesced
  where
    colourOne (spilled, colours) node =
      let usedColours =
            Set.fromList
              [ colour
              | neighbor <- Set.toList (Map.findWithDefault Set.empty node (allocAdjacency state))
              , Just colour <- [Map.lookup (getAlias state neighbor) colours]
              ]
          available = [colour | colour <- allocatableRegisters, colour `Set.notMember` usedColours]
       in case preferredAvailableColour state colours usedColours node <|> listToMaybe available of
            Just colour -> (spilled, Map.insert node colour colours)
            Nothing -> (Set.insert node spilled, colours)

preferredAvailableColour :: AllocState -> Map.Map Integer Integer -> Set.Set Integer -> Integer -> Maybe Integer
preferredAvailableColour state colours usedColours node =
  listToMaybe
    [ colour
    | AllocMove x y <- Set.toList (Map.findWithDefault Set.empty node (allocMoveList state))
    , let other = if getAlias state x == node then getAlias state y else getAlias state x
    , Just colour <- [Map.lookup other colours]
    , colour `Set.notMember` usedColours
    ]

nodeMoves :: AllocState -> Integer -> Set.Set AllocMove
nodeMoves state node =
  Map.findWithDefault Set.empty node (allocMoveList state)
    `Set.intersection` Set.union (allocActiveMoves state) (allocWorklistMoves state)

moveRelated :: AllocState -> Integer -> Bool
moveRelated state = not . Set.null . nodeMoves state

adjacentNodes :: AllocState -> Integer -> Set.Set Integer
adjacentNodes state node =
  Map.findWithDefault Set.empty node (allocAdjacency state)
    `Set.difference` Set.union (Set.fromList (allocSelectStack state)) (allocCoalescedNodes state)

conservative :: AllocState -> Set.Set Integer -> Bool
conservative state nodes =
  length [node | node <- Set.toList nodes, nodeDegree state node >= registerCount] < registerCount

chooseSpillNode :: AllocState -> Maybe Integer
chooseSpillNode state =
  case Set.toList (allocSpillWorklist state) of
    [] -> Nothing
    nodes -> Just (maximumBy (comparing (nodeDegree state)) nodes)

nodeDegree :: AllocState -> Integer -> Int
nodeDegree state node =
  Map.findWithDefault 0 node (allocDegree state)

adjacentInGraph :: AllocState -> Integer -> Integer -> Bool
adjacentInGraph state left right =
  right `Set.member` Map.findWithDefault Set.empty left (allocAdjacency state)

getAlias :: AllocState -> Integer -> Integer
getAlias state node
  | node `Set.member` allocCoalescedNodes state =
      maybe node (getAlias state) (Map.lookup node (allocAlias state))
  | otherwise = node

popMin :: Set.Set a -> Maybe a
popMin = fmap fst . Set.minView

moveFromWorklist :: AllocMove -> (Set.Set AllocMove -> AllocState) -> AllocState -> AllocState
moveFromWorklist move update state =
  update (Set.delete move (allocWorklistMoves state))

addMoveForNodes :: Map.Map Integer (Set.Set AllocMove) -> AllocMove -> Map.Map Integer (Set.Set AllocMove)
addMoveForNodes moveList move@(AllocMove source dest) =
  Map.insertWith Set.union source (Set.singleton move) $
    Map.insertWith Set.union dest (Set.singleton move) moveList

moveSet :: [Instr] -> Set.Set AllocMove
moveSet =
  Set.fromList . mapMaybeMove
  where
    mapMaybeMove [] = []
    mapMaybeMove (instr : instrs)
      | instrType instr == IMov
      , let source = fieldPlace FieldSource instr
      , let dest = fieldPlace FieldDest instr
      , isVirtualRegister source
      , isVirtualRegister dest
      , placeInteger source /= placeInteger dest =
          normalizedMove (placeInteger source) (placeInteger dest) : mapMaybeMove instrs
      | otherwise = mapMaybeMove instrs

moveNodes :: Set.Set AllocMove -> Set.Set Integer
moveNodes moves =
  Set.fromList [node | AllocMove source dest <- Set.toList moves, node <- [source, dest]]

normalizedMove :: Integer -> Integer -> AllocMove
normalizedMove left right
  | left <= right = AllocMove left right
  | otherwise = AllocMove right left

registerCount :: Int
registerCount = length allocatableRegisters

allocatableRegisters :: [Integer]
allocatableRegisters = [1 .. 19]

callerSafeRegisters :: [Integer]
callerSafeRegisters = [1 .. 21]

spillScratchA :: Place
spillScratchA = registerNumber 20

spillScratchB :: Place
spillScratchB = registerNumber 21

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
  | placeKind place == Register =
      case placeIntegerMaybe place of
        Just reg | reg `elem` callerSafeRegisters -> Just reg
        _ -> Nothing
  | placeKind place `elem` [Temporary, PointerRegister, VirtualRegister] =
      case placeIntegerMaybe place of
        Just reg | reg `elem` callerSafeRegisters -> Just reg
        _ -> Nothing
  | otherwise = Nothing

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
          loads = [Instr ILd [(FieldSource, spillSlot), (FieldDest, scratch)] [] | scratch <- uniquePlaces (Map.elems useScratch)]
          rewritten =
            instr
              { instrFields =
                  [ (name, Map.findWithDefault place name scratchByField)
                  | (name, place) <- instrFields instr
                  ]
              }
          stores = [Instr ISt [(FieldSource, scratch), (FieldDest, spillSlot)] [] | scratch <- uniquePlaces (Map.elems defScratch)]
       in loads <> [rewritten] <> stores

    isSpilled place =
      isVirtualRegister place && placeInteger place == reg

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
  | placeKind place `elem` [Temporary, PointerRegister, VirtualRegister] =
      case Map.lookup (placeInteger place) colours of
        Just colour -> registerNumber colour
        Nothing -> place
  | otherwise = place

instructionUseDef :: Instr -> (Set.Set Integer, Set.Set Integer)
instructionUseDef instr =
  (regSet (useFieldNames instr), regSet (defFieldNames instr))
  where
    regSet names = Set.fromList [placeInteger place | name <- names, let place = fieldPlace name instr, isVirtualRegister place]

useFieldNames :: Instr -> [InstrFieldName]
useFieldNames instr =
  case instrType instr of
    ISt -> [FieldDest, FieldSource]
    ILd -> [FieldSource]
    IPush -> [FieldTarget]
    IPop -> []
    ICall -> [FieldTarget]
    IAdd3 -> [FieldSource, FieldOffset]
    ILdOffset -> [FieldSource, FieldOffset]
    ICmp -> [FieldFirst, FieldSecond]
    IAdd -> useDest
    ISub -> useDest
    IMull -> useDest
    IShl -> useDest
    IShr -> useDest
    IShr3 -> [FieldSource, FieldThird]
    IXor -> useDest
    IAnd -> useDest
    IOr -> useDest
    IAsm -> []
    IMulh -> [FieldSource, FieldThird]
    IMull3 -> [FieldSource, FieldThird]
    ISub3 -> [FieldSource, FieldThird]
    ty
      | isJumpInstruction ty -> []
      | otherwise -> [FieldSource]
  where
    useDest = [FieldSource, FieldDest]

defFieldNames :: Instr -> [InstrFieldName]
defFieldNames instr =
  case instrType instr of
    ISt -> []
    ILd -> [FieldDest]
    IPush -> []
    IPop -> [FieldTarget]
    ICall -> []
    IAdd3 -> [FieldDest]
    ILdOffset -> [FieldDest]
    ICmp -> []
    IAdd -> [FieldDest]
    ISub -> [FieldDest]
    IMull -> [FieldDest]
    IShl -> [FieldDest]
    IShr -> [FieldDest]
    IShr3 -> [FieldDest]
    IXor -> [FieldDest]
    IAnd -> [FieldDest]
    IOr -> [FieldDest]
    IAsm -> []
    IMulh -> [FieldDest]
    IMull3 -> [FieldDest]
    ISub3 -> [FieldDest]
    ty
      | isJumpInstruction ty -> []
      | otherwise -> [FieldDest]

optimizeMethodTail :: MethodOutput -> [Instr] -> [Instr]
optimizeMethodTail method =
  removeTrailingDeadWrites . removeTrailingExitLabels . removeReturnRoundTrip . stripTrailingExitJump
  where
    exitTarget = immediateText (".exit_" <> methodOutputName method)

    stripTrailingExitJump instrs =
      case reverse instrs of
        instr : rest
          | instrType instr == IJmp
              && fieldPlace FieldTarget instr == exitTarget ->
              reverse rest
        _ -> instrs

    removeTrailingExitLabels instrs =
      case span isLabelInstruction (reverse instrs) of
        ([], _) -> instrs
        (labelsRev, restRev) ->
          let aliases =
                Map.fromList
                  [ (placeValue (fieldPlace FieldTarget label), placeValue exitTarget)
                  | label <- labelsRev
                  ]
           in map (rewriteLabelAliases aliases) (reverse restRev)

rewriteLabelAliases :: Map.Map Text Text -> Instr -> Instr
rewriteLabelAliases aliases instr =
  instr {instrFields = [(name, rewritePlaceLabel place) | (name, place) <- instrFields instr]}
  where
    rewritePlaceLabel place
      | placeKind place == Immediate = immediateText (resolveLabelAlias aliases (placeValue place))
      | otherwise = place

resolveLabelAlias :: Map.Map Text Text -> Text -> Text
resolveLabelAlias aliases = go Set.empty
  where
    go seen label
      | label `Set.member` seen = label
      | Just next <- Map.lookup label aliases = go (Set.insert label seen) next
      | otherwise = label

removeReturnRoundTrip :: [Instr] -> [Instr]
removeReturnRoundTrip instrs =
  case reverse instrs of
    restore : save : rest
      | instrType save == IMov
          && instrType restore == IMov
          && fieldPlace FieldSource save == specialRegister ReturnReg
          && fieldPlace FieldDest save == fieldPlace FieldSource restore
          && fieldPlace FieldDest restore == specialRegister ReturnReg ->
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
isLabelInstruction instr = instrType instr == ILabel

pureRegisterDefinition :: Instr -> Bool
pureRegisterDefinition instr =
  instrType instr `elem` [IMov, IAdd, ISub, IMull, IShl, IShr, IXor, IAnd, IOr, IAdd3, IShr3, IMulh, IMull3, ISub3]

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
  any (placeTouchesFrame . snd) (instrFields instr)

placeTouchesFrame :: Place -> Bool
placeTouchesFrame place =
  placeKind place `elem` [Local, Parameter]
    || place == specialRegister BasePointer
