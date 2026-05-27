module Tptcc.SSA
  ( dumpSSA
  , lowerMethodSSA
  ) where

import Control.Applicative ((<|>))
import Data.List (isSuffixOf, sort, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PPString

import Tptcc.Ast (Node)
import Tptcc.IRSimpleTac (Instr (..), MethodOutput (..), Place (..), TacProgram (..), generateSimpleTac)

type BlockId = String

data RawBlock = RawBlock
  { rawBlockId :: BlockId
  , rawBlockCode :: [(Int, Instr)]
  , rawBlockPreds :: [BlockId]
  , rawBlockSuccs :: [BlockId]
  }
  deriving (Eq, Show)

data BlockUseDef = BlockUseDef
  { blockUse :: Set.Set Var
  , blockDef :: Set.Set Var
  }
  deriving (Eq, Show)

data SSAProgram = SSAProgram
  { ssaProgramMethods :: [SSAMethod]
  , ssaProgramGlobalInstructions :: [Instr]
  , ssaProgramGlobalSize :: Integer
  }
  deriving (Eq, Show)

data SSAMethod = SSAMethod
  { ssaMethodName :: String
  , ssaMethodLocalSize :: Integer
  , ssaMethodBlocks :: [SSABlock]
  }
  deriving (Eq, Show)

data SSABlock = SSABlock
  { ssaBlockId :: BlockId
  , ssaBlockPreds :: [BlockId]
  , ssaBlockSuccs :: [BlockId]
  , ssaBlockPhis :: [SSAPhi]
  , ssaBlockCode :: [(Int, SSAInstr)]
  }
  deriving (Eq, Show)

data SSAPhi = SSAPhi
  { ssaPhiBase :: Place
  , ssaPhiDest :: SSAPlace
  , ssaPhiInputs :: [(BlockId, SSAPlace)]
  }
  deriving (Eq, Show)

data SSAInstr = SSAInstr
  { ssaInstrType :: String
  , ssaInstrFields :: [(String, SSAPlace)]
  , ssaInstrStringFields :: [(String, String)]
  }
  deriving (Eq, Show)

data SSAPlace = SSAPlace
  { ssaPlaceBase :: Place
  , ssaPlaceVersion :: Maybe Integer
  }
  deriving (Eq, Show)

newtype Var = Var {varPlace :: Place}
  deriving (Show)

instance Eq Var where
  left == right = compare left right == EQ

instance Ord Var where
  compare (Var left) (Var right) =
    compare (ssaVariableKey left) (ssaVariableKey right)

data RenameState = RenameState
  { renameCounters :: Map.Map Var Integer
  , renameStacks :: Map.Map Var [Integer]
  , renameBlocks :: Map.Map BlockId SSABlock
  }
  deriving (Eq, Show)

type SSAPlaceKey = (String, String, Integer)
type EdgeCopies = Map.Map BlockId (Map.Map BlockId [Instr])

dumpSSA :: Node -> Either String [String]
dumpSSA ast = renderSSA . generateSSA <$> generateSimpleTac ast

generateSSA :: TacProgram -> SSAProgram
generateSSA program =
  SSAProgram
    { ssaProgramMethods = map methodToSSA (tacProgramMethods program)
    , ssaProgramGlobalInstructions = tacProgramGlobalInstructions program
    , ssaProgramGlobalSize = tacProgramGlobalSize program
    }

methodToSSA :: MethodOutput -> SSAMethod
methodToSSA = methodToSSAWith isSSAPlace

methodToSSAWith :: (Place -> Bool) -> MethodOutput -> SSAMethod
methodToSSAWith ssaPlacePredicate method =
  optimizeSSAMethod $
    SSAMethod
      { ssaMethodName = methodOutputName method
      , ssaMethodLocalSize = methodOutputLocalSize method
      , ssaMethodBlocks = renamed
      }
  where
    rawBlocks = buildRawBlocks (methodOutputInstructions method)
    doms = dominators rawBlocks
    idoms = immediateDominators doms
    frontiers = dominanceFrontiers rawBlocks idoms
    liveIns = liveInSets ssaPlacePredicate rawBlocks
    phiBases = placePhis ssaPlacePredicate liveIns rawBlocks frontiers
    seeded = seedBlocks rawBlocks phiBases
    renamed = renameSSA ssaPlacePredicate seeded idoms

buildRawBlocks :: [Instr] -> [RawBlock]
buildRawBlocks instrs
  | null indexed = [RawBlock "entry" [] [] []]
  | otherwise = attachPreds blocksWithSuccs
  where
    indexed = zip [(0 :: Int) ..] instrs
    labelAtIndex =
      Map.fromList
        [ (index, placeValue (fieldPlace "target" instr))
        | (index, instr) <- indexed
        , instrType instr == "label"
        ]
    labelTargets =
      Map.fromList
        [ (placeValue (fieldPlace "target" instr), index)
        | (index, instr) <- indexed
        , instrType instr == "label"
        ]
    leaderSet =
      Set.fromList $
        [0]
          <> Map.elems labelTargets
          <> [target | instr <- instrs, target <- jumpTargetIndices labelTargets instr]
          <> [index + 1 | (index, instr) <- indexed, terminatesBlock instr, index + 1 < length instrs]
    leaders = sort (Set.toList leaderSet)
    ranges = zip leaders (drop 1 leaders <> [length instrs])
    blockIdFor start ordinal =
      Map.findWithDefault
        (if start == 0 then "entry" else ".ssa_bb_" <> show ordinal)
        start
        labelAtIndex
    blockRanges = zipWith (\ordinal (start, end) -> (blockIdFor start ordinal, start, end)) [(0 :: Int) ..] ranges
    indexToBlock =
      Map.fromList
        [ (index, blockId)
        | (blockId, start, end) <- blockRanges
        , index <- [start .. end - 1]
        ]
    labelToBlock =
      Map.mapMaybe (`Map.lookup` indexToBlock) labelTargets
    withoutLabels start end =
      [ (index, instr)
      | (index, instr) <- take (end - start) (drop start indexed)
      , instrType instr /= "label"
      ]
    blocks =
      [ RawBlock blockId (withoutLabels start end) [] []
      | (blockId, start, end) <- blockRanges
      ]
    blockOrder = map rawBlockId blocks
    nextBlock blockId =
      case dropWhile (/= blockId) blockOrder of
        _ : next : _ -> Just next
        _ -> Nothing
    blocksWithSuccs =
      [ block {rawBlockSuccs = blockSuccessors labelToBlock (nextBlock (rawBlockId block)) block}
      | block <- blocks
      ]

jumpTargetIndices :: Map.Map String Int -> Instr -> [Int]
jumpTargetIndices labels instr
  | isJumpInstruction (instrType instr) =
      maybe [] pure (Map.lookup (placeValue (fieldPlace "target" instr)) labels)
  | otherwise = []

terminatesBlock :: Instr -> Bool
terminatesBlock instr =
  instrType instr == "ret" || isJumpInstruction (instrType instr)

blockSuccessors :: Map.Map String BlockId -> Maybe BlockId -> RawBlock -> [BlockId]
blockSuccessors labelToBlock fallthrough block =
  uniqueInOrder $
    case reverse (rawBlockCode block) of
      (_, instr) : _
        | instrType instr == "jmp" ->
            targetSuccessor instr
        | isJumpInstruction (instrType instr) ->
            targetSuccessor instr <> maybe [] pure fallthrough
        | instrType instr == "ret" ->
            []
      _ -> maybe [] pure fallthrough
  where
    targetSuccessor instr =
      maybe [] pure (Map.lookup (placeValue (fieldPlace "target" instr)) labelToBlock)

attachPreds :: [RawBlock] -> [RawBlock]
attachPreds blocks =
  [ block {rawBlockPreds = Map.findWithDefault [] (rawBlockId block) predMap}
  | block <- blocks
  ]
  where
    predMap =
      foldl'
        ( \acc block ->
            foldl'
              (\inner succId -> Map.insertWith (<>) succId [rawBlockId block] inner)
              acc
              (rawBlockSuccs block)
        )
        Map.empty
        blocks

dominators :: [RawBlock] -> Map.Map BlockId (Set.Set BlockId)
dominators [] = Map.empty
dominators blocks =
  fixedPoint initial
  where
    allIds = Set.fromList (map rawBlockId blocks)
    entry = maybe "entry" rawBlockId (listToMaybe blocks)
    predMap = Map.fromList [(rawBlockId block, rawBlockPreds block) | block <- blocks]
    initial =
      Map.fromList
        [ (rawBlockId block, if rawBlockId block == entry then Set.singleton entry else allIds)
        | block <- blocks
        ]
    fixedPoint doms =
      let doms' = foldl' updateOne doms blocks
       in if doms' == doms then doms else fixedPoint doms'
    updateOne doms block
      | rawBlockId block == entry = doms
      | null preds = Map.insert blockId (Set.singleton blockId) doms
      | otherwise =
          let predDoms = [Map.findWithDefault allIds predId doms | predId <- preds]
              newDom = Set.insert blockId (foldl1 Set.intersection predDoms)
           in Map.insert blockId newDom doms
      where
        blockId = rawBlockId block
        preds = Map.findWithDefault [] blockId predMap

immediateDominators :: Map.Map BlockId (Set.Set BlockId) -> Map.Map BlockId BlockId
immediateDominators doms =
  Map.fromList
    [ (blockId, idom)
    | (blockId, blockDoms) <- Map.toList doms
    , let strictDoms = Set.delete blockId blockDoms
    , idom <- maybe [] pure (findImmediate strictDoms)
    ]
  where
    findImmediate strictDoms =
      listToMaybe
        [ candidate
        | candidate <- Set.toList strictDoms
        , all (`Set.member` Map.findWithDefault Set.empty candidate doms) (Set.delete candidate strictDoms)
        ]

dominanceFrontiers :: [RawBlock] -> Map.Map BlockId BlockId -> Map.Map BlockId (Set.Set BlockId)
dominanceFrontiers blocks idoms =
  foldl' addJoinBlock emptyFrontiers blocks
  where
    emptyFrontiers = Map.fromList [(rawBlockId block, Set.empty) | block <- blocks]
    addJoinBlock frontiers block
      | length (rawBlockPreds block) < 2 = frontiers
      | otherwise = foldl' (addPredFrontier (rawBlockId block)) frontiers (rawBlockPreds block)
    addPredFrontier joinId frontiers predId = go frontiers predId
      where
        stop = Map.lookup joinId idoms
        go acc runner
          | Just runner == stop = acc
          | otherwise =
              let acc' = Map.insertWith Set.union runner (Set.singleton joinId) acc
               in case Map.lookup runner idoms of
                    Just next -> go acc' next
                    Nothing -> acc'

placePhis :: (Place -> Bool) -> Map.Map BlockId (Set.Set Var) -> [RawBlock] -> Map.Map BlockId (Set.Set BlockId) -> Map.Map BlockId [Place]
placePhis ssaPlacePredicate liveIns blocks frontiers =
  Map.map (map varPlace . Set.toList) $
    foldl' insertForPlace emptyPhiMap (Map.toList defSites)
  where
    emptyPhiMap = Map.fromList [(rawBlockId block, Set.empty) | block <- blocks]
    blockDefs =
      Map.fromList
        [ (rawBlockId block, Set.fromList (map Var (concatMap (ssaDefinedPlaces ssaPlacePredicate . snd) (rawBlockCode block))))
        | block <- blocks
        ]
    defSites =
      foldl'
        ( \acc (blockId, defs) ->
            foldl'
              (\inner var -> Map.insertWith Set.union var (Set.singleton blockId) inner)
              acc
              (Set.toList defs)
        )
        Map.empty
        (Map.toList blockDefs)
    insertForPlace phiMap (var, sites) = go phiMap sites Set.empty
      where
        go acc work seen =
          case Set.minView work of
            Nothing -> acc
            Just (blockId, rest)
              | blockId `Set.member` seen -> go acc rest seen
              | otherwise ->
                  let frontierBlocks = Map.findWithDefault Set.empty blockId frontiers
                      (acc', work') =
                        foldl'
                          (addPhi var)
                          (acc, rest)
                          (Set.toList frontierBlocks)
                   in go acc' work' (Set.insert blockId seen)
        addPhi phiVar (acc, work) frontierBlock =
          let existing = Map.findWithDefault Set.empty frontierBlock acc
              liveInFrontier = phiVar `Set.member` Map.findWithDefault Set.empty frontierBlock liveIns
           in if phiVar `Set.member` existing || not liveInFrontier
                then (acc, work)
                else
                  let acc' = Map.insert frontierBlock (Set.insert phiVar existing) acc
                      work' =
                        if phiVar `Set.member` Map.findWithDefault Set.empty frontierBlock blockDefs
                          then work
                          else Set.insert frontierBlock work
                   in (acc', work')

liveInSets :: (Place -> Bool) -> [RawBlock] -> Map.Map BlockId (Set.Set Var)
liveInSets ssaPlacePredicate blocks =
  fixedPoint initial
  where
    blockMap = Map.fromList [(rawBlockId block, block) | block <- blocks]
    useDefMap = Map.fromList [(rawBlockId block, blockUseDef ssaPlacePredicate block) | block <- blocks]
    initial = Map.fromList [(rawBlockId block, Set.empty) | block <- blocks]
    fixedPoint liveIns =
      let (changed, liveIns') = foldl' updateOne (False, liveIns) (reverse blocks)
       in if changed then fixedPoint liveIns' else liveIns'
    updateOne (changed, liveIns) block =
      let ident = rawBlockId block
          info = Map.findWithDefault (BlockUseDef Set.empty Set.empty) ident useDefMap
          succLiveIns =
            Set.unions
              [ Map.findWithDefault Set.empty succId liveIns
              | succId <- rawBlockSuccs (Map.findWithDefault block ident blockMap)
              ]
          liveIn = Set.union (blockUse info) (Set.difference succLiveIns (blockDef info))
          changed' = changed || liveIn /= Map.findWithDefault Set.empty ident liveIns
       in (changed', Map.insert ident liveIn liveIns)

blockUseDef :: (Place -> Bool) -> RawBlock -> BlockUseDef
blockUseDef ssaPlacePredicate block =
  BlockUseDef uses defs
  where
    (_, uses, defs) = foldl' step (Set.empty, Set.empty, Set.empty) (map snd (rawBlockCode block))
    step (seenDef, useAcc, defAcc) instr =
      let useSet = Set.fromList (map Var (ssaUsedPlaces ssaPlacePredicate instr))
          defSet = Set.fromList (map Var (ssaDefinedPlaces ssaPlacePredicate instr))
          newUses = Set.difference useSet seenDef
          newDefs = Set.difference defSet seenDef
       in (Set.union seenDef newDefs, Set.union useAcc newUses, Set.union defAcc newDefs)

seedBlocks :: [RawBlock] -> Map.Map BlockId [Place] -> [SSABlock]
seedBlocks blocks phis =
  [ SSABlock
      { ssaBlockId = rawBlockId block
      , ssaBlockPreds = rawBlockPreds block
      , ssaBlockSuccs = rawBlockSuccs block
      , ssaBlockPhis = [SSAPhi base (SSAPlace base Nothing) [] | base <- Map.findWithDefault [] (rawBlockId block) phis]
      , ssaBlockCode = [(index, SSAInstr (instrType instr) [(name, SSAPlace place Nothing) | (name, place) <- instrFields instr] (instrStringFields instr)) | (index, instr) <- rawBlockCode block]
      }
  | block <- blocks
  ]

renameSSA :: (Place -> Bool) -> [SSABlock] -> Map.Map BlockId BlockId -> [SSABlock]
renameSSA ssaPlacePredicate blocks idoms =
  [Map.findWithDefault block blockId renamedMap | block <- blocks, let blockId = ssaBlockId block]
  where
    blockMap = Map.fromList [(ssaBlockId block, block) | block <- blocks]
    children =
      foldl'
        (\acc (child, parent) -> Map.insertWith (<>) parent [child] acc)
        Map.empty
        (Map.toList idoms)
    roots =
      [ ssaBlockId block
      | block <- blocks
      , ssaBlockId block `Map.notMember` idoms
      ]
    initialState = RenameState Map.empty Map.empty Map.empty
    finalState = foldl' (\st root -> snd (renameBlock ssaPlacePredicate blockMap children root st)) initialState roots
    renamedMap = renameBlocks finalState

renameBlock :: (Place -> Bool) -> Map.Map BlockId SSABlock -> Map.Map BlockId [BlockId] -> BlockId -> RenameState -> ([Place], RenameState)
renameBlock ssaPlacePredicate blockMap children blockId state0 =
  case Map.lookup blockId (renameBlocks state0) <|> Map.lookup blockId blockMap of
    Nothing -> ([], state0)
    Just block ->
      let (renamedPhis, phiDefs, state1) = renamePhiDefs state0 (ssaBlockPhis block)
          (renamedInstrs, instrDefs, state2) = renameInstructions ssaPlacePredicate state1 (ssaBlockCode block)
          state3 = addSuccessorPhiInputs blockMap blockId (ssaBlockSuccs block) state2
          renamedBlock =
            block
              { ssaBlockPhis = renamedPhis
              , ssaBlockCode = renamedInstrs
              }
          state4 = state3 {renameBlocks = Map.insert blockId renamedBlock (renameBlocks state3)}
          state5 =
            foldl'
              ( \st child ->
                  snd (renameBlock ssaPlacePredicate blockMap children child st)
              )
              state4
              (sort (Map.findWithDefault [] blockId children))
          localDefs = phiDefs <> instrDefs
       in (localDefs, popPlaces localDefs state5)

renamePhiDefs :: RenameState -> [SSAPhi] -> ([SSAPhi], [Place], RenameState)
renamePhiDefs state =
  foldl'
    ( \(phis, defs, st) phi ->
        let (dest, st') = pushFresh (ssaPhiBase phi) st
         in (phis <> [phi {ssaPhiDest = dest}], ssaPhiBase phi : defs, st')
    )
    ([], [], state)

renameInstructions :: (Place -> Bool) -> RenameState -> [(Int, SSAInstr)] -> ([(Int, SSAInstr)], [Place], RenameState)
renameInstructions ssaPlacePredicate state =
  foldl'
    ( \(instrs, defs, st) (index, instr) ->
        let (instr', instrDefs, st') = renameInstruction ssaPlacePredicate st instr
         in (instrs <> [(index, instr')], defs <> instrDefs, st')
    )
    ([], [], state)

renameInstruction :: (Place -> Bool) -> RenameState -> SSAInstr -> (SSAInstr, [Place], RenameState)
renameInstruction ssaPlacePredicate state instr =
  (instr {ssaInstrFields = renamedFields}, defs, stateAfterDefs)
  where
    rawInstr = Instr (ssaInstrType instr) [(name, ssaPlaceBase place) | (name, place) <- ssaInstrFields instr] (ssaInstrStringFields instr)
    useNames = ssaUseFieldNames rawInstr
    defNames = ssaDefFieldNames rawInstr
    (renamedFields, defs, stateAfterDefs) = foldl' renameField ([], [], state) (ssaInstrFields instr)
    renameField (fields, defined, st) (name, place)
      | ssaPlacePredicate base && name `elem` useNames && name `elem` defNames =
          let usePlace = currentSSAPlace base st
              (defPlace, st') = pushFresh base st
           in (fields <> [(name <> "_in", usePlace), (name, defPlace)], defined <> [base], st')
      | ssaPlacePredicate base && name `elem` useNames =
          (fields <> [(name, currentSSAPlace base st)], defined, st)
      | ssaPlacePredicate base && name `elem` defNames =
          let (defPlace, st') = pushFresh base st
           in (fields <> [(name, defPlace)], defined <> [base], st')
      | otherwise = (fields <> [(name, place)], defined, st)
      where
        base = ssaPlaceBase place

addSuccessorPhiInputs :: Map.Map BlockId SSABlock -> BlockId -> [BlockId] -> RenameState -> RenameState
addSuccessorPhiInputs blockMap predId succIds state =
  foldl' addForSucc state succIds
  where
    addForSucc st succId =
      case Map.lookup succId (renameBlocks st) <|> Map.lookup succId blockMap of
        Nothing -> st
        Just succBlock ->
          let phisWithInput =
                [ phi {ssaPhiInputs = ssaPhiInputs phi <> [(predId, currentSSAPlace (ssaPhiBase phi) st)]}
                | phi <- ssaBlockPhis succBlock
                ]
              succBlock' = succBlock {ssaBlockPhis = phisWithInput}
           in st {renameBlocks = Map.insert succId succBlock' (renameBlocks st)}

pushFresh :: Place -> RenameState -> (SSAPlace, RenameState)
pushFresh place st =
  ( SSAPlace place (Just next)
  , st
      { renameCounters = Map.insert var next (renameCounters st)
      , renameStacks = Map.insert var (next : Map.findWithDefault [] var (renameStacks st)) (renameStacks st)
      }
  )
  where
    var = Var place
    next = Map.findWithDefault 0 var (renameCounters st) + 1

currentSSAPlace :: Place -> RenameState -> SSAPlace
currentSSAPlace place st =
  SSAPlace place (Just version)
  where
    var = Var place
    version =
      case Map.findWithDefault [] var (renameStacks st) of
        top : _ -> top
        [] -> 0

popPlaces :: [Place] -> RenameState -> RenameState
popPlaces places st =
  st
    { renameStacks =
        foldl'
          ( \acc place ->
              case Map.findWithDefault [] (Var place) acc of
                _ : rest -> Map.insert (Var place) rest acc
                [] -> acc
          )
          (renameStacks st)
          places
    }

optimizeSSAMethod :: SSAMethod -> SSAMethod
optimizeSSAMethod = go (8 :: Int)
  where
    go 0 method = method
    go fuel method =
      let method' = propagateSSACopies (eliminateTrivialPhis method)
       in if method' == method then method else go (fuel - 1) method'

eliminateTrivialPhis :: SSAMethod -> SSAMethod
eliminateTrivialPhis method =
  removeReplacedPhis (applySSAReplacements replacements method)
  where
    replacements = trivialPhiReplacements method
    replaced = Map.keysSet replacements
    removeReplacedPhis =
      mapSSABlocks $ \block ->
        block
          { ssaBlockPhis =
              [ phi
              | phi <- ssaBlockPhis block
              , maybe True (`Set.notMember` replaced) (ssaReplacementKey (ssaPhiDest phi))
              ]
          }

trivialPhiReplacements :: SSAMethod -> Map.Map SSAPlaceKey SSAPlace
trivialPhiReplacements method =
  Map.fromList
    [ (destKey, replacement)
    | block <- ssaMethodBlocks method
    , phi <- ssaBlockPhis block
    , Just destKey <- [ssaReplacementKey (ssaPhiDest phi)]
    , replacement <- maybe [] pure (trivialPhiReplacement destKey phi)
    ]

trivialPhiReplacement :: SSAPlaceKey -> SSAPhi -> Maybe SSAPlace
trivialPhiReplacement destKey phi =
  case Set.toList uniqueInputs of
    [replacement] -> Just (comparableSSAPlace replacement)
    _ -> Nothing
  where
    uniqueInputs =
      Set.fromList
        [ ComparableSSAPlace input
        | (_, input) <- ssaPhiInputs phi
        , ssaReplacementKey input /= Just destKey
        ]

propagateSSACopies :: SSAMethod -> SSAMethod
propagateSSACopies method =
  removeCopyInstructions (applySSAReplacements replacements method)
  where
    replacements = copyReplacements method
    replaced = Map.keysSet replacements
    removeCopyInstructions =
      mapSSABlocks $ \block ->
        block
          { ssaBlockCode =
              [ (index, instr)
              | (index, instr) <- ssaBlockCode block
              , not (isRemovedCopy replaced instr)
              ]
          }

copyReplacements :: SSAMethod -> Map.Map SSAPlaceKey SSAPlace
copyReplacements method =
  Map.fromList
    [ (destKey, source)
    | block <- ssaMethodBlocks method
    , (_, instr) <- ssaBlockCode block
    , ssaInstrType instr == "mov"
    , Just source <- [lookup "source" (ssaInstrFields instr)]
    , Just dest <- [lookup "dest" (ssaInstrFields instr)]
    , Just _ <- [ssaReplacementKey source]
    , Just destKey <- [ssaReplacementKey dest]
    , source /= dest
    ]

isRemovedCopy :: Set.Set SSAPlaceKey -> SSAInstr -> Bool
isRemovedCopy replaced instr =
  ssaInstrType instr == "mov"
    && maybe False (`Set.member` replaced) (lookup "dest" (ssaInstrFields instr) >>= ssaReplacementKey)

applySSAReplacements :: Map.Map SSAPlaceKey SSAPlace -> SSAMethod -> SSAMethod
applySSAReplacements replacements =
  mapSSABlocks rewriteBlock
  where
    rewriteBlock block =
      block
        { ssaBlockPhis = map rewritePhi (ssaBlockPhis block)
        , ssaBlockCode = [(index, rewriteInstr instr) | (index, instr) <- ssaBlockCode block]
        }
    rewritePhi phi =
      phi {ssaPhiInputs = [(predId, replaceSSAPlace replacements place) | (predId, place) <- ssaPhiInputs phi]}
    rewriteInstr instr =
      instr {ssaInstrFields = rewriteInstrFields instr}
    rewriteInstrFields instr =
      [ (name, rewriteField name place)
      | (name, place) <- ssaInstrFields instr
      ]
      where
        rawInstr = Instr (ssaInstrType instr) [(name, ssaPlaceBase place) | (name, place) <- ssaInstrFields instr] (ssaInstrStringFields instr)
        uses = Set.fromList (ssaUseFieldNames rawInstr)
        rewriteField name place
          | name `Set.member` uses || "_in" `isSuffixOf` name = replaceSSAPlace replacements place
          | otherwise = place

replaceSSAPlace :: Map.Map SSAPlaceKey SSAPlace -> SSAPlace -> SSAPlace
replaceSSAPlace replacements = go Set.empty
  where
    go seen place =
      case ssaReplacementKey place of
        Just key
          | key `Set.member` seen -> place
          | Just next <- Map.lookup key replacements -> go (Set.insert key seen) next
        _ -> place

ssaReplacementKey :: SSAPlace -> Maybe SSAPlaceKey
ssaReplacementKey place
  | isSSAPlace (ssaPlaceBase place)
  , Just version <- ssaPlaceVersion place
  , version > 0 =
      let (ty, value) = ssaVariableKey (ssaPlaceBase place)
       in Just (ty, value, version)
  | otherwise = Nothing

newtype ComparableSSAPlace = ComparableSSAPlace {comparableSSAPlace :: SSAPlace}
  deriving (Show)

instance Eq ComparableSSAPlace where
  left == right = compare left right == EQ

instance Ord ComparableSSAPlace where
  compare (ComparableSSAPlace left) (ComparableSSAPlace right) =
    compare
      (ssaVariableKey (ssaPlaceBase left), ssaPlaceVersion left)
      (ssaVariableKey (ssaPlaceBase right), ssaPlaceVersion right)

mapSSABlocks :: (SSABlock -> SSABlock) -> SSAMethod -> SSAMethod
mapSSABlocks update method =
  method {ssaMethodBlocks = map update (ssaMethodBlocks method)}

ssaDefinedPlaces :: (Place -> Bool) -> Instr -> [Place]
ssaDefinedPlaces ssaPlacePredicate instr =
  uniquePlaces [place | name <- ssaDefFieldNames instr, let place = fieldPlace name instr, ssaPlacePredicate place]

ssaUsedPlaces :: (Place -> Bool) -> Instr -> [Place]
ssaUsedPlaces ssaPlacePredicate instr =
  uniquePlaces [place | name <- ssaUseFieldNames instr, let place = fieldPlace name instr, ssaPlacePredicate place]

ssaUseFieldNames :: Instr -> [String]
ssaUseFieldNames instr =
  case instrType instr of
    "st"
      | isDirectStackPlace (fieldPlace "dest" instr) -> ["source"]
      | otherwise -> ["dest", "source"]
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
      | isJumpInstruction ty -> []
      | otherwise -> ["source"]
  where
    useDest = ["source", "dest"]

ssaDefFieldNames :: Instr -> [String]
ssaDefFieldNames instr =
  case instrType instr of
    "st"
      | isDirectStackPlace (fieldPlace "dest" instr) -> ["dest"]
      | otherwise -> []
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
      | isJumpInstruction ty -> []
      | otherwise -> ["dest"]

isDirectStackPlace :: Place -> Bool
isDirectStackPlace place = placeType place `elem` ["l", "p"]

isSSAPlace :: Place -> Bool
isSSAPlace place = placeType place `elem` ["t", "vr", "pr", "l", "p"]

isRegisterSSAPlace :: Place -> Bool
isRegisterSSAPlace place = placeType place `elem` ["t", "vr", "pr"]

lowerMethodSSA :: MethodOutput -> MethodOutput
lowerMethodSSA method =
  method {methodOutputInstructions = lowerSSAMethod (methodToSSAWith isRegisterSSAPlace method)}

lowerSSAMethod :: SSAMethod -> [Instr]
lowerSSAMethod method =
  concatMap lowerBlock (ssaMethodBlocks method)
  where
    placeMap = ssaPlaceMap method
    edgeCopies = ssaEdgeCopies placeMap method

    lowerBlock block =
      labelForBlock block
        <> lowerBlockCode block (Map.findWithDefault Map.empty (ssaBlockId block) edgeCopies)

    labelForBlock block
      | ssaBlockId block == "entry" = []
      | otherwise = [Instr "label" [("target", Place "i" (ssaBlockId block))] []]

    lowerBlockCode block copiesFromBlock =
      case reverse loweredCode of
        terminal : restRev
          | instrType terminal == "jmp" ->
              let target = placeValue (fieldPlace "target" terminal)
                  copies = copiesFor target
               in reverse restRev <> copies <> [terminal]
          | isJumpInstruction (instrType terminal) ->
              let target = placeValue (fieldPlace "target" terminal)
                  fallthroughs = [succId | succId <- ssaBlockSuccs block, succId `Set.notMember` Set.singleton target]
                  fallthrough = listToMaybe fallthroughs
                  (terminal', targetSplits) = splitConditionalTarget terminal target (copiesFor target)
                  fallthroughJump =
                    case fallthrough of
                      Just succId -> [Instr "jmp" [("target", Place "i" (splitOrOriginalTarget succId (copiesFor succId)))] []]
                      Nothing -> []
                  fallthroughSplits =
                    case fallthrough of
                      Just succId -> splitBlock succId (copiesFor succId)
                      Nothing -> []
               in reverse restRev <> [terminal'] <> fallthroughJump <> targetSplits <> fallthroughSplits
          | instrType terminal == "ret" ->
              loweredCode
        _ ->
          case ssaBlockSuccs block of
            [succId] -> loweredCode <> copiesFor succId
            _ -> loweredCode
      where
        loweredCode = concatMap (lowerSSAInstr placeMap . snd) (ssaBlockCode block)
        copiesFor succId = Map.findWithDefault [] succId copiesFromBlock
        splitLabel succId = ".ssa_phi_" <> sanitizeBlockId (ssaBlockId block) <> "_" <> sanitizeBlockId succId
        splitOrOriginalTarget succId copies
          | null copies = succId
          | otherwise = splitLabel succId
        splitBlock succId copies
          | null copies = []
          | otherwise =
              [Instr "label" [("target", Place "i" (splitLabel succId))] []]
                <> copies
                <> [Instr "jmp" [("target", Place "i" succId)] []]
        splitConditionalTarget terminal target copies
          | null copies = (terminal, [])
          | otherwise =
              ( terminal {instrFields = rewriteTarget (splitLabel target) (instrFields terminal)}
              , splitBlock target copies
              )

    rewriteTarget target =
      map
        ( \(name, place) ->
            if name == "target"
              then (name, Place "i" target)
              else (name, place)
        )

ssaPlaceMap :: SSAMethod -> Map.Map SSAPlaceKey Place
ssaPlaceMap method =
  Map.fromList (zip keys freshPlaces)
  where
    keys = sort (Set.toList (collectSSAPlaceKeys method))
    firstFresh :: Integer
    firstFresh = maximum (0 : [read value | (_, value, _) <- keys, all (`elem` ['0' .. '9']) value]) + 1
    freshPlaces =
      [ Place ty (show (firstFresh + fromIntegral index))
      | (index, (ty, _, _)) <- zip [(0 :: Int) ..] keys
      ]

collectSSAPlaceKeys :: SSAMethod -> Set.Set SSAPlaceKey
collectSSAPlaceKeys method =
  Set.fromList
    [ key
    | block <- ssaMethodBlocks method
    , place <- concatMap phiPlaces (ssaBlockPhis block) <> concatMap (instrPlaces . snd) (ssaBlockCode block)
    , Just key <- [ssaPlaceKey place]
    ]
  where
    phiPlaces phi = ssaPhiDest phi : map snd (ssaPhiInputs phi)
    instrPlaces instr = map snd (ssaInstrFields instr)

ssaPlaceKey :: SSAPlace -> Maybe SSAPlaceKey
ssaPlaceKey place
  | isRegisterSSAPlace (ssaPlaceBase place)
  , Just version <- ssaPlaceVersion place
  , version > 0 =
      let (ty, value) = ssaVariableKey (ssaPlaceBase place)
       in Just (ty, value, version)
  | otherwise = Nothing

ssaVariableKey :: Place -> (String, String)
ssaVariableKey place
  | placeType place == "pr" = ("t", placeValue place)
  | otherwise = (placeType place, placeValue place)

lowerSSAPlace :: Map.Map SSAPlaceKey Place -> SSAPlace -> Place
lowerSSAPlace placeMap place =
  case ssaPlaceKey place >>= (`Map.lookup` placeMap) of
    Just lowered -> lowered
    Nothing
      | isRegisterSSAPlace (ssaPlaceBase place)
      , ssaPlaceVersion place == Just 0 ->
          Place "i" "0"
      | otherwise -> ssaPlaceBase place

ssaEdgeCopies :: Map.Map SSAPlaceKey Place -> SSAMethod -> EdgeCopies
ssaEdgeCopies placeMap method =
  foldl' addBlock Map.empty (ssaMethodBlocks method)
  where
    addBlock acc block =
      foldl' (addPhi (ssaBlockId block)) acc (ssaBlockPhis block)
    addPhi succId acc phi =
      foldl'
        ( \inner (predId, source) ->
            let dest = lowerSSAPlace placeMap (ssaPhiDest phi)
                source' = lowerSSAPlace placeMap source
                copy =
                  if source' == dest
                    then []
                    else [Instr "mov" [("source", source'), ("dest", dest)] []]
             in Map.insertWith (Map.unionWith (<>)) predId (Map.singleton succId copy) inner
        )
        acc
        (ssaPhiInputs phi)

lowerSSAInstr :: Map.Map SSAPlaceKey Place -> SSAInstr -> [Instr]
lowerSSAInstr placeMap instr =
  prefix <> [Instr (ssaInstrType instr) loweredFields (ssaInstrStringFields instr)]
  where
    baseFields =
      [ (name, lowerSSAPlace placeMap place)
      | (name, place) <- ssaInstrFields instr
      , not ("_in" `isSuffixOf` name)
      ]
    loweredFields = baseFields
    prefix =
      case (lookup "dest_in" (ssaInstrFields instr), lookup "dest" (ssaInstrFields instr)) of
        (Just source, Just dest) ->
          let source' = lowerSSAPlace placeMap source
              dest' = lowerSSAPlace placeMap dest
           in if source' == dest'
                then []
                else [Instr "mov" [("source", source'), ("dest", dest')] []]
        _ -> []

sanitizeBlockId :: String -> String
sanitizeBlockId =
  map
    ( \char ->
        if char `elem` (['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'])
          then char
          else '_'
    )


isJumpInstruction :: String -> Bool
isJumpInstruction ty = take 1 ty == "j"

fieldPlace :: String -> Instr -> Place
fieldPlace name instr =
  fromMaybe (Place "i" "0") (lookup name (instrFields instr))

uniqueInOrder :: Ord a => [a] -> [a]
uniqueInOrder = go Set.empty
  where
    go _ [] = []
    go seen (value : rest)
      | value `Set.member` seen = go seen rest
      | otherwise = value : go (Set.insert value seen) rest

uniquePlaces :: [Place] -> [Place]
uniquePlaces = go Set.empty
  where
    key place = (placeType place, placeValue place)
    go _ [] = []
    go seen (place : rest)
      | key place `Set.member` seen = go seen rest
      | otherwise = place : go (Set.insert (key place) seen) rest

renderSSA :: SSAProgram -> [String]
renderSSA =
  lines . PPString.renderString . PP.layoutPretty PP.defaultLayoutOptions . prettySSA

prettySSA :: SSAProgram -> PP.Doc ann
prettySSA program =
  PP.vsep $
    map prettySSAMethod (ssaProgramMethods program)
      <> [PP.pretty "GLOBAL_SIZE" <> tabDoc <> PP.pretty (show (ssaProgramGlobalSize program))]
      <> zipWith prettyGlobalInstr [(1 :: Int) ..] (ssaProgramGlobalInstructions program)

prettySSAMethod :: SSAMethod -> PP.Doc ann
prettySSAMethod method =
  PP.vsep $
    [ PP.pretty "METHOD_SSA"
        <> tabDoc
        <> PP.pretty (ssaMethodName method)
        <> tabDoc
        <> PP.pretty "LOCAL_SIZE="
        <> PP.pretty (show (ssaMethodLocalSize method))
    ]
      <> map prettySSABlock (ssaMethodBlocks method)

prettySSABlock :: SSABlock -> PP.Doc ann
prettySSABlock block =
  PP.vsep $
    [ PP.pretty "BLOCK"
        <> tabDoc
        <> PP.pretty (ssaBlockId block)
        <> tabDoc
        <> PP.pretty "preds="
        <> commaSepDocs (map PP.pretty (ssaBlockPreds block))
        <> tabDoc
        <> PP.pretty "succs="
        <> commaSepDocs (map PP.pretty (ssaBlockSuccs block))
    ]
      <> map prettyPhi (sortBy comparePhi (ssaBlockPhis block))
      <> [prettySSAInstr index instr | (index, instr) <- ssaBlockCode block]

comparePhi :: SSAPhi -> SSAPhi -> Ordering
comparePhi left right = compare (renderPlace (ssaPhiBase left)) (renderPlace (ssaPhiBase right))

prettyPhi :: SSAPhi -> PP.Doc ann
prettyPhi phi =
  PP.pretty "phi"
    <> tabDoc
    <> PP.pretty "base="
    <> prettyPlace (ssaPhiBase phi)
    <> tabDoc
    <> PP.pretty "dest="
    <> prettySSAPlace (ssaPhiDest phi)
    <> tabDoc
    <> PP.pretty "inputs="
    <> commaSepDocs [PP.pretty predId <> PP.pretty ":" <> prettySSAPlace place | (predId, place) <- ssaPhiInputs phi]

prettySSAInstr :: Int -> SSAInstr -> PP.Doc ann
prettySSAInstr index instr =
  PP.pretty (show index)
    <> tabDoc
    <> PP.pretty (ssaInstrType instr)
    <> PP.hcat (map prettyField (ssaInstrFields instr))
    <> PP.hcat (map prettyStringField (ssaInstrStringFields instr))

prettyGlobalInstr :: Int -> Instr -> PP.Doc ann
prettyGlobalInstr index instr =
  PP.pretty "GLOBAL"
    <> tabDoc
    <> PP.pretty (show index)
    <> tabDoc
    <> PP.pretty (instrType instr)
    <> PP.hcat [tabDoc <> PP.pretty name <> PP.pretty "=" <> prettyPlace place | (name, place) <- instrFields instr]
    <> PP.hcat (map prettyStringField (instrStringFields instr))

prettyField :: (String, SSAPlace) -> PP.Doc ann
prettyField (name, place) = tabDoc <> PP.pretty name <> PP.pretty "=" <> prettySSAPlace place

prettyStringField :: (String, String) -> PP.Doc ann
prettyStringField (name, value) = tabDoc <> PP.pretty name <> PP.pretty "=" <> PP.pretty value

prettySSAPlace :: SSAPlace -> PP.Doc ann
prettySSAPlace place =
  prettyPlace (ssaPlaceBase place)
    <> maybe mempty (\version -> PP.pretty "#" <> PP.pretty (show version)) (ssaPlaceVersion place)

prettyPlace :: Place -> PP.Doc ann
prettyPlace place = PP.pretty (placeType place) <> PP.pretty ":" <> PP.pretty (placeValue place)

renderPlace :: Place -> String
renderPlace =
  PPString.renderString . PP.layoutPretty PP.defaultLayoutOptions . prettyPlace

commaSepDocs :: [PP.Doc ann] -> PP.Doc ann
commaSepDocs = PP.hcat . PP.punctuate (PP.pretty ",")

tabDoc :: PP.Doc ann
tabDoc = PP.pretty "\t"
