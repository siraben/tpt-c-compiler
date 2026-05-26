module Tptcc.IRSimpleTac
  ( dumpSimpleTac
  , generateSimpleTac
  , generateSimpleTacWithBreakpoints
  , Instr (..)
  , MethodOutput (..)
  , Place (..)
  , TacProgram (..)
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify', runStateT)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (><), (|>))
import qualified Data.Sequence as Seq

import Tptcc.Ast
import Tptcc.CType (CType (..), TypeKind (..))
import Tptcc.Operand (Operand (..), OperandValue (..))
import Tptcc.SymbolTable (Symbol (..), defaultSymbols)
import Tptcc.Token (SourcePos (..), Token (..))

data Place = Place
  { placeType :: String
  , placeValue :: String
  }
  deriving (Eq, Show)

data Instr = Instr
  { instrType :: String
  , instrFields :: [(String, Place)]
  , instrStringFields :: [(String, String)]
  }
  deriving (Eq, Show)

data MethodOutput = MethodOutput
  { methodOutputName :: String
  , methodOutputInstructions :: [Instr]
  , methodOutputLocalSize :: Integer
  }
  deriving (Eq, Show)

data TacProgram = TacProgram
  { tacProgramMethods :: [MethodOutput]
  , tacProgramGlobalSize :: Integer
  , tacProgramGlobalInstructions :: [Instr]
  }
  deriving (Eq, Show)

data LocalInfo = LocalInfo
  { localInfoPlace :: Place
  , localInfoPointerLevel :: Integer
  , localInfoDimensions :: [Integer]
  , localInfoPointerToArray :: Bool
  }
  deriving (Eq, Show)

data PostfixContext = PostfixContext
  { contextPointerLevel :: Integer
  , contextDimensions :: [Integer]
  , contextPointerToArray :: Bool
  }
  deriving (Eq, Show)

data CaseContext = CaseContext
  { caseEntries :: [(Place, Place)]
  , caseDefault :: Maybe Place
  }
  deriving (Eq, Show)

data TacState = TacState
  { localOffset :: Integer
  , globalOffset :: Integer
  , tempCounter :: Integer
  , labelCounter :: Integer
  , loopLabels :: [(Place, Place)]
  , caseLabels :: [CaseContext]
  , currentMethodName :: String
  , functionPlaces :: Map.Map String Place
  , functionReturns :: Map.Map String Bool
  , enumConstants :: Map.Map String Place
  , globals :: Map.Map String LocalInfo
  , locals :: Map.Map String LocalInfo
  , instructions :: Seq Instr
  , methods :: [MethodOutput]
  , breakpoints :: [Integer]
  , breakpointIndex :: Int
  }
  deriving (Eq, Show)

type TacM = StateT TacState (Either String)

dumpSimpleTac :: Node -> Either String [String]
dumpSimpleTac ast = do
  program <- generateSimpleTac ast
  pure (renderTac program)

generateSimpleTac :: Node -> Either String TacProgram
generateSimpleTac = generateSimpleTacWithBreakpoints []

generateSimpleTacWithBreakpoints :: [Integer] -> Node -> Either String TacProgram
generateSimpleTacWithBreakpoints requestedBreakpoints ast = do
  (_, st) <- runStateT (emitProgram ast) (initialState requestedBreakpoints)
  pure
    TacProgram
      { tacProgramMethods = reverse (methods st)
      , tacProgramGlobalSize = globalOffset st
      , tacProgramGlobalInstructions = toList (instructions st)
      }

initialState :: [Integer] -> TacState
initialState requestedBreakpoints =
  TacState
    { localOffset = 0
    , globalOffset = 0
    , tempCounter = 0
    , labelCounter = 0
    , loopLabels = []
    , caseLabels = []
    , currentMethodName = "main"
    , functionPlaces = Map.fromList defaultPlaces
    , functionReturns = Map.empty
    , enumConstants = Map.empty
    , globals = Map.empty
    , locals = Map.empty
    , instructions = Seq.empty
    , methods = []
    , breakpoints = requestedBreakpoints
    , breakpointIndex = 0
    }

emitProgram :: Node -> TacM ()
emitProgram program = do
  let declarations = childNodes program
      functionEntries = [(declaratorName declarator, declaration, declarator) | declaration <- declarations, Just declarator <- [fieldNodeMaybe "declarator" declaration], hasNodeField "block" declaration]
      userFunctionPlaces = Map.fromList [(name, Place "i" ("__tptcc_fn_" <> name)) | (name, _, _) <- functionEntries]
      userFunctionReturns = Map.fromList [("__tptcc_fn_" <> name, functionDeclarationReturnsValue declaration) | (name, declaration, _) <- functionEntries]
  modify' (\st -> st {functionPlaces = Map.union userFunctionPlaces (functionPlaces st), functionReturns = Map.union userFunctionReturns (functionReturns st)})
  mapM_ collectEnumConstants declarations
  mapM_ emitTopLevelDeclaration declarations

emitTopLevelDeclaration :: Node -> TacM ()
emitTopLevelDeclaration declaration =
  case fieldNodeMaybe "declarator" declaration of
    Just declarator | hasNodeField "block" declaration -> emitFunction (declaratorName declarator, declaration, declarator)
    _ -> emitGlobalDeclaration declaration

emitGlobalDeclaration :: Node -> TacM ()
emitGlobalDeclaration declaration = do
  specifier <- fieldNode "specifier" declaration
  storage <- fieldNode "storage_class" specifier
  let storageKind = fieldStringDefault "kind" "auto" storage
  unless (storageKind == "typedef") $ do
    declarators <- fieldNodeList "declarators" declaration
    forM_ declarators $ \declarator ->
      unless (hasBoolField "is_function" declarator) $ do
        let initializer = fieldNodeMaybe "initializer" declarator
        dimensions <- declaratorDimensionsWithInitializer initializer declarator
        let pointerLevel = declaratorPointerLevel declarator
            size = declaratorSlotSize dimensions
            name = declaratorName declarator
        place <-
          if pointerLevel > 0 && initializerContainsDirectString initializer
            then do
              _ <- nextGlobalSize (directInitializerStringLength initializer)
              nextGlobalSize 1
            else nextGlobalSize size
        let info = LocalInfo place pointerLevel dimensions (declaratorPointerToArray declarator)
        modify' (\st -> st {globals = Map.insert name info (globals st)})
        unless (pointerLevel > 0 && initializerContainsDirectString initializer) $
          allocateNestedInitializerStrings pointerLevel dimensions initializer
        maybe (pure ()) (simulateGlobalInitializer place) initializer

collectEnumConstants :: Node -> TacM ()
collectEnumConstants declaration =
  case enumSpecifierFromDeclaration declaration >>= fieldNodeMaybe "declaration" of
    Just enumDeclaration ->
      forM_ (zip [(0 :: Integer) ..] (childNodes enumDeclaration)) $ \(index, member) -> do
        memberId <- fieldNode "id" member
        let value = fieldIntDefault "value" index member
        modify' (\st -> st {enumConstants = Map.insert (identifierValue memberId) (Place "i" (show value)) (enumConstants st)})
    Nothing -> pure ()

enumSpecifierFromDeclaration :: Node -> Maybe Node
enumSpecifierFromDeclaration declaration = do
  specifier <- fieldNodeMaybe "specifier" declaration
  typeSpecifier <- fieldNodeMaybe "type_specifier" specifier
  case lookupField "kind" typeSpecifier of
    Just (NodeRef enumSpecifier)
      | nodeName enumSpecifier == "ENUM_SPECIFIER" -> Just enumSpecifier
    _ -> Nothing

emitFunction :: (String, Node, Node) -> TacM ()
emitFunction (name, declaration, declarator) = do
  previousMethod <- currentMethodName <$> get
  previousLocals <- locals <$> get
  previousLocalOffset <- localOffset <$> get
  previousInstructions <- instructions <$> get
  block <- fieldNode "block" declaration
  params <- parameterPlaces declarator
  modify'
    ( \st ->
        st
          { currentMethodName = name
          , localOffset = 0
          , locals = Map.fromList params
          , instructions = Seq.empty
          , loopLabels = []
          , caseLabels = []
          }
    )
  emitBlock block
  st <- get
  let output =
        MethodOutput
          { methodOutputName = name
          , methodOutputInstructions = toList (instructions st)
          , methodOutputLocalSize = localOffset st
          }
  modify'
    ( \s ->
        s
          { methods = output : methods s
          , currentMethodName = previousMethod
          , locals = previousLocals
          , localOffset = previousLocalOffset
          , instructions = previousInstructions
          , loopLabels = []
          , caseLabels = []
          }
    )

emitBlock :: Node -> TacM ()
emitBlock block = mapM_ emitStatementWithBreakpoint (childNodes block)

emitStatementWithBreakpoint :: Node -> TacM ()
emitStatementWithBreakpoint statement = do
  maybeEmitBreakpoint statement
  emitStatement statement

maybeEmitBreakpoint :: Node -> TacM ()
maybeEmitBreakpoint statement = do
  st <- get
  case drop (breakpointIndex st) (breakpoints st) of
    breakpoint : _ | fromIntegral (row (nodePos statement)) > breakpoint - 1 -> do
      emit "!debug_breakpoint" [("target", Place "i" (show breakpoint))]
      modify' (\s -> s {breakpointIndex = breakpointIndex s + 1})
    _ -> pure ()

emitStatement :: Node -> TacM ()
emitStatement statement = do
  child <- fieldNode "child" statement
  case nodeName child of
    "DECLARATION" -> emitDeclaration child
    "EXPRESSION" -> emitExpression child >> pure ()
    "IF" -> emitIf child
    "BLOCK" -> emitBlock child
    "WHILE" -> emitWhile child
    "FOR" -> emitFor child
    "BREAK" -> emitBreak
    "CONTINUE" -> emitContinue
    "SWITCH" -> emitSwitch child
    "CASE" -> emitCase child
    "DEFAULT" -> emitDefault child
    "ASM" -> emitAsm child
    "RETURN" -> emitReturn child
    "EMPTY_STATEMENT" -> pure ()
    other -> throw ("simple TAC does not support statement: " <> other)

emitDeclaration :: Node -> TacM ()
emitDeclaration declaration = do
  storageKind <- declarationStorageKind declaration
  declarators <- fieldNodeList "declarators" declaration
  forM_ declarators $ \declarator -> do
    dimensions <- declaratorDimensions declarator
    let isRegisterLocal = storageKind == "register"
        pointerLevel = declaratorPointerLevel declarator
    place <-
      if isRegisterLocal
        then do
          unless (null dimensions || pointerLevel > 0) $
            throw "simple TAC cannot allocate aggregate register"
          nextVR
        else nextLocalSize (declaratorSlotSize dimensions)
    let name = declaratorName declarator
        info = LocalInfo place pointerLevel dimensions (declaratorPointerToArray declarator)
    modify' (\st -> st {locals = Map.insert name info (locals st)})
    case fieldNodeMaybe "initializer" declarator of
      Just initializer -> do
        source <- emitInitializer initializer
        _ <- emitMove source place
        pure ()
      Nothing -> pure ()

declarationStorageKind :: Node -> TacM String
declarationStorageKind declaration = do
  specifier <- fieldNode "specifier" declaration
  storage <- fieldNode "storage_class" specifier
  pure (fieldStringDefault "kind" "auto" storage)

emitInitializer :: Node -> TacM Place
emitInitializer initializer =
  case fieldNodeMaybe "value" initializer of
    Just value -> emitAssignmentExpression value
    Nothing -> throw "simple TAC initializer missing value"

simulateGlobalInitializer :: Place -> Node -> TacM ()
simulateGlobalInitializer place initializer
  | nodeName initializer == "INITIALIZER" =
      case fieldNodeMaybe "value" initializer of
        Just value
          | nodeName value `elem` ["INT", "CHARACTER", "STRING_LITERAL"] -> pure ()
          | otherwise -> do
              source <- emitAssignmentExpression value
              _ <- emitMove source place
              pure ()
        Nothing -> pure ()
  | nodeName initializer == "INITIALIZER_LIST" =
      forM_ (zip [(0 :: Integer) ..] (childNodes initializer)) $ \(offset, child) ->
        simulateGlobalInitializer (place {placeValue = show (placeInteger place + offset)}) child
  | otherwise = pure ()

emitReturn :: Node -> TacM ()
emitReturn node = do
  value <- maybe (throw "simple TAC return missing value") pure (fieldNodeMaybe "value" node)
  place <- emitExpression value
  source <-
    if isLValue place
      then loadOperandIntoRegister place
      else pure place
  emit "mov" [("source", source), ("dest", Place "r" "return_reg")]
  method <- currentMethodName <$> get
  emit "jmp" [("target", Place "i" (".exit_" <> method))]

emitIf :: Node -> TacM ()
emitIf node = do
  falseLabel <- nextLabel
  trueLabel <- nextLabel
  endLabel <- nextLabel
  condition <- fieldNode "condition" node >>= firstExpressionChild
  emitBoolControlFlow condition trueLabel falseLabel
  emit "label" [("target", trueLabel)]
  fieldNode "true_case" node >>= emitStatement
  emit "jmp" [("target", endLabel)]
  emit "label" [("target", falseLabel)]
  maybe (pure ()) emitStatement (fieldNodeMaybe "false_case" node)
  emit "label" [("target", endLabel)]

emitWhile :: Node -> TacM ()
emitWhile node = do
  startLabel <- nextLabel
  endLabel <- nextLabel
  pushLoopLabels startLabel endLabel
  emit "label" [("target", startLabel)]
  trueLabel <- nextLabel
  condition <- fieldNode "condition" node >>= firstExpressionChild
  emitBoolControlFlow condition trueLabel endLabel
  emit "label" [("target", trueLabel)]
  fieldNode "statement" node >>= emitStatement
  emit "jmp" [("target", startLabel)]
  emit "label" [("target", endLabel)]
  popLoopLabels

emitFor :: Node -> TacM ()
emitFor node = do
  startLabel <- nextLabel
  updateLabel <- nextLabel
  endLabel <- nextLabel
  pushLoopLabels updateLabel endLabel
  maybe (pure ()) emitForInitialization (fieldNodeMaybe "initialization" node)
  emit "label" [("target", startLabel)]
  trueLabel <- nextLabel
  case fieldNodeMaybe "condition" node of
    Just condition -> firstExpressionChild condition >>= \child -> emitBoolControlFlow child trueLabel endLabel
    Nothing -> pure ()
  emit "label" [("target", trueLabel)]
  fieldNode "statement" node >>= emitStatement
  emit "label" [("target", updateLabel)]
  maybe (pure ()) (\update -> emitExpression update >> pure ()) (fieldNodeMaybe "update" node)
  emit "jmp" [("target", startLabel)]
  emit "label" [("target", endLabel)]
  popLoopLabels

emitForInitialization :: Node -> TacM ()
emitForInitialization node =
  case nodeName node of
    "DECLARATION" -> emitDeclaration node
    _ -> emitExpression node >> pure ()

emitSwitch :: Node -> TacM ()
emitSwitch node = do
  condition <- fieldNode "condition" node >>= emitExpression >>= loadOperandIntoRegister
  endLabel <- nextLabel
  mark <- Seq.length . instructions <$> get
  pushCaseLabels
  pushLoopLabels endLabel endLabel
  fieldNode "block" node >>= emitBlock
  context <- popCaseLabels
  popLoopLabels
  let comparisons = concatMap (caseComparison condition) (caseEntries context)
      dispatch =
        comparisons
          <> [ Instr "jmp" [("target", maybe endLabel id (caseDefault context))] []
             ]
  insertInstructionsAt mark dispatch
  emit "label" [("target", endLabel)]

emitCase :: Node -> TacM ()
emitCase node = do
  value <- fieldNode "value" node >>= emitPrimaryExpression
  unless (placeType value == "i") $
    throw "case values must be constants"
  trueLabel <- nextLabel
  addCaseLabel value trueLabel
  emit "label" [("target", trueLabel)]
  fieldNode "statement" node >>= emitStatement

emitDefault :: Node -> TacM ()
emitDefault node = do
  defaultLabel <- nextLabel
  setDefaultCaseLabel defaultLabel
  emit "label" [("target", defaultLabel)]
  fieldNode "statement" node >>= emitStatement

caseComparison :: Place -> (Place, Place) -> [Instr]
caseComparison condition (value, target) =
  [ Instr "cmp" [("first", condition), ("second", value)] []
  , Instr "je" [("target", target)] []
  ]

emitAsm :: Node -> TacM ()
emitAsm node = do
  maybe (pure ()) emitAsmClobbersPush (fieldNodeMaybe "clobbers" node)
  maybe (pure ()) emitAsmInputs (fieldNodeMaybe "inputs" node)
  asm <- fieldString "asm" node
  emitString "asm" [("asm", asm)]
  maybe (pure ()) emitAsmOutputs (fieldNodeMaybe "outputs" node)
  maybe (pure ()) emitAsmClobbersPop (fieldNodeMaybe "clobbers" node)

emitAsmClobbersPush :: Node -> TacM ()
emitAsmClobbersPush node =
  forM_ (childNodes node) $ \register ->
    emit "push" [("target", registerPlace register)]

emitAsmClobbersPop :: Node -> TacM ()
emitAsmClobbersPop node =
  forM_ (reverse (childNodes node)) $ \register ->
    emit "pop" [("target", registerPlace register)]

emitAsmInputs :: Node -> TacM ()
emitAsmInputs node = do
  args <- asmArguments node
  forM_ args $ \arg -> do
    reg <- asmRegisterPlace arg
    source <- asmCSymbolPlace arg
    _ <- emitMove source reg
    pure ()

emitAsmOutputs :: Node -> TacM ()
emitAsmOutputs node = do
  args <- asmArguments node
  forM_ args $ \arg -> do
    reg <- asmRegisterPlace arg
    dest <- asmCSymbolPlace arg
    _ <- emitMove reg dest
    pure ()

emitBreak :: TacM ()
emitBreak = do
  labels <- loopLabels <$> get
  case labels of
    (_, endLabel) : _ -> emit "jmp" [("target", endLabel)]
    [] -> throw "break statement must be inside a loop"

emitContinue :: TacM ()
emitContinue = do
  labels <- loopLabels <$> get
  case labels of
    (updateLabel, _) : _ -> emit "jmp" [("target", updateLabel)]
    [] -> throw "continue statement must be inside a loop"

emitExpression :: Node -> TacM Place
emitExpression node
  | nodeName node == "EXPRESSION" =
      case childNodes node of
        [] -> throw "simple TAC empty expression"
        children -> last <$> mapM emitAssignmentExpression children
  | otherwise = emitAssignmentExpression node

emitAssignmentExpression :: Node -> TacM Place
emitAssignmentExpression node
  | nodeName node == "ASSIGNMENT" = do
      op <- fieldString "op" node
      lhs <- fieldNode "lhs" node >>= emitTernaryExpression
      rhs <- fieldNode "rhs" node >>= emitAssignmentExpression
      case op of
        "=" -> emitMove rhs lhs
        "/=" -> do
          (quotient, _) <- emitDivision lhs rhs
          _ <- emitMove quotient lhs
          pure quotient
        "%=" -> do
          (_, remainder) <- emitDivision lhs rhs
          _ <- emitMove remainder lhs
          pure remainder
        _ -> emitCompoundAssignment op lhs rhs
  | otherwise = emitTernaryExpression node

emitCompoundAssignment :: String -> Place -> Place -> TacM Place
emitCompoundAssignment op lhs rhs = do
  operationType <- compoundOperationType op
  lhsPlace <-
    if isRegisterRValue lhs
      then pure lhs
      else loadOperandIntoRegister lhs
  rhsPlace <-
    if isRegisterRValue rhs
      then pure rhs
      else loadOperandIntoRegister rhs
  emit operationType [("source", rhsPlace), ("dest", lhsPlace)]
  unless (lhsPlace == lhs) $
    emitMove lhsPlace lhs >> pure ()
  pure lhsPlace

emitTernaryExpression :: Node -> TacM Place
emitTernaryExpression node
  | nodeName node == "TERNARY" = do
      falseLabel <- nextLabel
      trueLabel <- nextLabel
      endLabel <- nextLabel
      condition <- fieldNode "condition" node
      emitBoolControlFlow condition trueLabel falseLabel
      emit "label" [("target", trueLabel)]
      truePlace <- fieldNode "true_case" node >>= emitAssignmentExpression
      result <- nextTemp
      _ <- emitMove truePlace result
      emit "jmp" [("target", endLabel)]
      emit "label" [("target", falseLabel)]
      falsePlace <- fieldNode "false_case" node >>= emitBoolRValue
      _ <- emitMove falsePlace result
      emit "label" [("target", endLabel)]
      pure result
  | otherwise = emitBoolRValue node

emitInclusiveOrExpression :: Node -> TacM Place
emitInclusiveOrExpression node
  | nodeName node == "INCLUSIVE_OR_EXPRESSION" = do
      firstNode <- onlyChildNodeAt 0 (nodeChildren node)
      firstPlace <- emitInclusiveXorExpression firstNode >>= loadOperandIntoRegister
      emitFlatInfix firstPlace (drop 1 (childNodesOnly node)) emitInclusiveXorExpression "or"
  | otherwise = emitInclusiveXorExpression node

emitInclusiveXorExpression :: Node -> TacM Place
emitInclusiveXorExpression node
  | nodeName node == "INCLUSIVE_XOR_EXPRESSION" = do
      firstNode <- onlyChildNodeAt 0 (nodeChildren node)
      firstPlace <- emitInclusiveAndExpression firstNode >>= loadOperandIntoRegister
      emitFlatInfix firstPlace (drop 1 (childNodesOnly node)) emitInclusiveAndExpression "xor"
  | otherwise = emitInclusiveAndExpression node

emitInclusiveAndExpression :: Node -> TacM Place
emitInclusiveAndExpression node
  | nodeName node == "INCLUSIVE_AND_EXPRESSION" = do
      firstNode <- onlyChildNodeAt 0 (nodeChildren node)
      firstPlace <- emitEqualityValue firstNode >>= loadOperandIntoRegister
      emitFlatInfix firstPlace (drop 1 (childNodesOnly node)) emitEqualityValue "and"
  | otherwise = emitEqualityValue node

emitEqualityValue :: Node -> TacM Place
emitEqualityValue node
  | nodeName node == "EQUALITY_EXPRESSION" =
      materializeBool $ \trueLabel falseLabel ->
        emitEqualityControl node trueLabel falseLabel
  | otherwise = emitRelationalValue node

emitRelationalValue :: Node -> TacM Place
emitRelationalValue node
  | nodeName node == "RELATIONAL_EXPRESSION" =
      materializeBool $ \trueLabel falseLabel ->
        emitRelationalControl node trueLabel falseLabel
  | otherwise = emitShiftExpression node

emitShiftExpression :: Node -> TacM Place
emitShiftExpression node
  | nodeName node == "SHIFT_EXPRESSION" = do
      let children = nodeChildren node
      firstNode <- childNodeAt 0 children
      firstPlace <- emitSumExpression firstNode >>= loadOperandIntoRegister
      luaNextReg <- nextTemp
      emitShiftRest firstPlace luaNextReg (drop 1 children)
  | otherwise = emitSumExpression node

emitBoolRValue :: Node -> TacM Place
emitBoolRValue node
  | isLogicalExpression node =
      materializeBool $ \trueLabel falseLabel ->
        emitBoolControlFlow node trueLabel falseLabel
  | otherwise = emitInclusiveOrExpression node

materializeBool :: (Place -> Place -> TacM ()) -> TacM Place
materializeBool emitControl = do
  falseLabel <- nextLabel
  trueLabel <- nextLabel
  endLabel <- nextLabel
  emitControl trueLabel falseLabel
  result <- nextTemp
  emit "label" [("target", trueLabel)]
  emit "mov" [("source", Place "i" "1"), ("dest", result)]
  emit "jmp" [("target", endLabel)]
  emit "label" [("target", falseLabel)]
  emit "mov" [("source", Place "i" "0"), ("dest", result)]
  emit "label" [("target", endLabel)]
  pure result

emitBoolControlFlow :: Node -> Place -> Place -> TacM ()
emitBoolControlFlow node trueLabel falseLabel =
  case nodeName node of
    "EQUALITY_EXPRESSION" -> emitEqualityControl node trueLabel falseLabel
    "RELATIONAL_EXPRESSION" -> emitRelationalControl node trueLabel falseLabel
    "LOGICAL_AND_EXPRESSION" -> emitLogicalAndControl node trueLabel falseLabel
    "LOGICAL_OR_EXPRESSION" -> emitLogicalOrControl node trueLabel falseLabel
    _ -> do
      value <- emitBoolRValue node >>= loadOperandIntoReadOnlyRegister
      emit "cmp" [("first", value), ("second", Place "i" "0")]
      emit "je" [("target", falseLabel)]
      emit "jmp" [("target", trueLabel)]

emitLogicalAndControl :: Node -> Place -> Place -> TacM ()
emitLogicalAndControl node trueLabel falseLabel
  | nodeName node == "LOGICAL_AND_EXPRESSION" =
      case childNodesOnly node of
        [] -> throw "malformed logical and expression"
        children -> emitAndChain children
  | otherwise = emitBoolControlFlow node trueLabel falseLabel
  where
    emitAndChain [child] = emitBoolControlFlow child trueLabel falseLabel
    emitAndChain (child : rest) = do
      tempTrue <- nextLabel
      emitBoolControlFlow child tempTrue falseLabel
      emit "label" [("target", tempTrue)]
      emitAndChain rest
    emitAndChain [] = throw "malformed logical and expression"

emitLogicalOrControl :: Node -> Place -> Place -> TacM ()
emitLogicalOrControl node trueLabel falseLabel
  | nodeName node == "LOGICAL_OR_EXPRESSION" =
      case childNodesOnly node of
        [] -> throw "malformed logical or expression"
        children -> emitOrChain children
  | otherwise = emitLogicalAndControl node trueLabel falseLabel
  where
    emitOrChain [child] = emitBoolControlFlow child trueLabel falseLabel
    emitOrChain (child : rest) = do
      tempFalse <- nextLabel
      emitBoolControlFlow child trueLabel tempFalse
      emit "label" [("target", tempFalse)]
      emitOrChain rest
    emitOrChain [] = throw "malformed logical or expression"

emitEqualityControl :: Node -> Place -> Place -> TacM ()
emitEqualityControl node trueLabel falseLabel
  | nodeName node == "EQUALITY_EXPRESSION" =
      emitComparisonControl equalityJump node trueLabel falseLabel
  | otherwise = emitRelationalControl node trueLabel falseLabel

emitRelationalControl :: Node -> Place -> Place -> TacM ()
emitRelationalControl node trueLabel falseLabel
  | nodeName node == "RELATIONAL_EXPRESSION" =
      emitComparisonControl signedRelationalJump node trueLabel falseLabel
  | otherwise = do
      value <- emitShiftExpression node
      emitConditionalResultJump value trueLabel falseLabel

emitComparisonControl :: (String -> TacM String) -> Node -> Place -> Place -> TacM ()
emitComparisonControl jumpFor node trueLabel falseLabel = do
  let children = nodeChildren node
  firstNode <- childNodeAt 0 children
  tempPlace <- emitBoolRValue firstNode >>= loadOperandIntoRegister
  groups <- comparisonGroups (drop 1 children)
  case groups of
    [] -> emitConditionalResultJump tempPlace trueLabel falseLabel
    _ -> do
      let (intermediate, finalGroup) = splitLast groups
      forM_ intermediate $ \(op, rhs) -> do
        nextReg <- emitBoolRValue rhs >>= loadOperandIntoReadOnlyRegister
        jumpType <- jumpFor op
        emitConditionalEvaluation tempPlace nextReg tempPlace jumpType
      let (op, rhs) = finalGroup
      nextReg <- emitBoolRValue rhs >>= loadOperandIntoReadOnlyRegister
      jumpType <- jumpFor op
      emit "cmp" [("first", tempPlace), ("second", nextReg)]
      emit jumpType [("target", trueLabel)]
      emit "jmp" [("target", falseLabel)]

emitConditionalEvaluation :: Place -> Place -> Place -> String -> TacM ()
emitConditionalEvaluation first second result jumpType = do
  trueLabel <- nextLabel
  endLabel <- nextLabel
  emit "cmp" [("first", first), ("second", second)]
  emit jumpType [("target", trueLabel)]
  _ <- emitMove (Place "i" "0") result
  emit "jmp" [("target", endLabel)]
  emit "label" [("target", trueLabel)]
  _ <- emitMove (Place "i" "1") result
  emit "label" [("target", endLabel)]

emitConditionalResultJump :: Place -> Place -> Place -> TacM ()
emitConditionalResultJump result trueLabel falseLabel = do
  checked <- loadOperandIntoReadOnlyRegister result
  emit "cmp" [("first", checked), ("second", Place "i" "0")]
  emit "je" [("target", falseLabel)]
  emit "jmp" [("target", trueLabel)]

emitFlatInfix :: Place -> [Node] -> (Node -> TacM Place) -> String -> TacM Place
emitFlatInfix acc [] _ _ = pure acc
emitFlatInfix acc (rhs : rest) emitChild op = do
  rhsPlace <- emitChild rhs >>= loadOperandIntoRegister
  emit op [("source", rhsPlace), ("dest", acc)]
  emitFlatInfix acc rest emitChild op

emitSumExpression :: Node -> TacM Place
emitSumExpression node
  | nodeName node == "SUM_EXPRESSION" = do
      let children = nodeChildren node
      firstNode <- childNodeAt 0 children
      firstPlace <- emitTerm firstNode >>= loadOperandIntoRegister
      emitSumRest firstPlace (drop 1 children)
  | otherwise = emitTerm node

emitSumRest :: Place -> [NodeChild] -> TacM Place
emitSumRest acc [] = pure acc
emitSumRest acc (ChildToken token : ChildNode rhs : rest) = do
  rhsPlace0 <- emitTerm rhs
  rhsPlace <-
    if isRegisterRValue rhsPlace0 || placeType rhsPlace0 == "i"
      then pure rhsPlace0
      else loadOperandIntoRegister rhsPlace0
  case tokenName token of
    "+" -> emit "add" [("source", rhsPlace), ("dest", acc)]
    "-" -> emit "sub" [("source", rhsPlace), ("dest", acc)]
    op -> throw ("simple TAC unsupported sum op: " <> op)
  emitSumRest acc rest
emitSumRest _ _ = throw "malformed sum expression"

emitTerm :: Node -> TacM Place
emitTerm node
  | nodeName node == "MULTIPLICATIVE_EXPRESSION" = do
      let children = nodeChildren node
      firstNode <- childNodeAt 0 children
      firstPlace0 <- emitCastExpression firstNode
      firstPlace <-
        if length children > 1
          then loadOperandIntoRegister firstPlace0
          else pure firstPlace0
      emitTermRest firstPlace (drop 1 children)
  | otherwise = emitCastExpression node

emitTermRest :: Place -> [NodeChild] -> TacM Place
emitTermRest acc [] = pure acc
emitTermRest acc (ChildToken token : ChildNode rhs : rest) = do
  rhsPlace0 <- emitCastExpression rhs
  rhsPlace <-
    if isLValue rhsPlace0
      then loadOperandIntoRegister rhsPlace0
      else pure rhsPlace0
  case tokenName token of
    "*" -> do
      emit "mull" [("source", rhsPlace), ("dest", acc)]
      emitTermRest acc rest
    "/" -> do
      (quotient, _) <- emitDivision acc rhsPlace
      emitTermRest quotient rest
    "%" -> do
      (_, remainder) <- emitDivision acc rhsPlace
      emitTermRest remainder rest
    op -> throw ("simple TAC unsupported term op: " <> op)
emitTermRest _ _ = throw "malformed term expression"

emitDivision :: Place -> Place -> TacM (Place, Place)
emitDivision dividend0 divisor0 = do
  dividend <-
    if isRegisterRValue dividend0
      then pure dividend0
      else loadOperandIntoRegister dividend0
  if placeType divisor0 == "i"
    then emitFixedPointDivision dividend divisor0
    else do
      divisor <-
        if isRegisterRValue divisor0
          then pure divisor0
          else loadOperandIntoRegister divisor0
      emitLongDivision dividend divisor

emitFixedPointDivision :: Place -> Place -> TacM (Place, Place)
emitFixedPointDivision dividend divisor = do
  let fixedPointFactor = (2 ^ (16 :: Int)) `div` placeInteger divisor
  quotient <- nextTemp
  remainder <- nextTemp
  endLabel <- nextLabel
  emit "mulh" [("source", dividend), ("dest", quotient), ("third", Place "i" (show fixedPointFactor))]
  emit "mull3" [("source", quotient), ("dest", remainder), ("third", divisor)]
  emit "sub3" [("source", dividend), ("dest", remainder), ("third", remainder)]
  emit "cmp" [("first", remainder), ("second", divisor)]
  emit "ja" [("target", endLabel)]
  emit "add" [("source", Place "i" "1"), ("dest", quotient)]
  emit "sub" [("source", divisor), ("dest", remainder)]
  emit "label" [("target", endLabel)]
  pure (quotient, remainder)

emitLongDivision :: Place -> Place -> TacM (Place, Place)
emitLongDivision dividend divisor = do
  quotient <- nextTemp
  remainder <- nextTemp
  bitIndex <- nextTemp
  loopLabel <- nextLabel
  temp <- nextTemp
  remainderLessLabel <- nextLabel
  endLabel <- nextLabel
  emit "mov" [("source", Place "i" "0"), ("dest", quotient)]
  emit "mov" [("source", Place "i" "0"), ("dest", remainder)]
  emit "mov" [("source", Place "i" "15"), ("dest", bitIndex)]
  emit "label" [("target", loopLabel)]
  emit "cmp" [("first", bitIndex), ("second", Place "i" "0")]
  emit "jl" [("target", endLabel)]
  emit "shl" [("source", Place "i" "1"), ("dest", remainder)]
  emit "shr3" [("source", dividend), ("dest", temp), ("third", bitIndex)]
  emit "and" [("source", Place "i" "1"), ("dest", temp)]
  emit "or" [("source", temp), ("dest", remainder)]
  emit "cmp" [("first", remainder), ("second", divisor)]
  emit "jl" [("target", remainderLessLabel)]
  emit "sub" [("source", divisor), ("dest", remainder)]
  emit "mov" [("source", Place "i" "1"), ("dest", temp)]
  emit "shl" [("source", bitIndex), ("dest", temp)]
  emit "or" [("source", temp), ("dest", quotient)]
  emit "label" [("target", remainderLessLabel)]
  emit "sub" [("source", Place "i" "1"), ("dest", bitIndex)]
  emit "jmp" [("target", loopLabel)]
  emit "label" [("target", endLabel)]
  pure (quotient, remainder)

emitShiftRest :: Place -> Place -> [NodeChild] -> TacM Place
emitShiftRest acc _ [] = pure acc
emitShiftRest acc luaNextReg (ChildToken token : ChildNode rhs : rest) = do
  rhsPlace0 <- emitSumExpression rhs
  rhsPlace <-
    if isRegisterRValue rhsPlace0 || placeType rhsPlace0 == "i"
      then pure rhsPlace0
      else emitMove rhsPlace0 luaNextReg
  case tokenName token of
    "<<" -> emit "shl" [("source", rhsPlace), ("dest", acc)]
    ">>" -> emit "shr" [("source", rhsPlace), ("dest", acc)]
    op -> throw ("simple TAC unsupported shift op: " <> op)
  emitShiftRest acc rhsPlace rest
emitShiftRest _ _ _ = throw "malformed shift expression"

emitCastExpression :: Node -> TacM Place
emitCastExpression node
  | nodeName node == "CAST_EXPRESSION" = fieldNode "cast_expression" node >>= emitCastExpression
  | otherwise = emitUnaryExpression node

emitUnaryExpression :: Node -> TacM Place
emitUnaryExpression node
  | nodeName node == "UNARY_EXPRESSION" = do
      op <- fieldString "operator" node
      child <- fieldNode "child" node
      case op of
        "++" -> emitPrefixMutation child "add"
        "--" -> emitPrefixMutation child "sub"
        "SIZEOF" -> pure (Place "i" "1")
        "&" -> emitCastExpression child >>= emitAddressOf
        "*" -> emitCastExpression child >>= emitDereference
        "+" -> emitCastExpression child
        "-" -> do
          childPlace <- emitCastExpression child
          if placeType childPlace == "i"
            then pure (Place "i" (show (65536 - placeInteger childPlace)))
            else do
              result <- loadOperandIntoRegister childPlace
              emit "xor" [("source", Place "i" "65535"), ("dest", result)]
              emit "add" [("source", Place "i" "1"), ("dest", result)]
              pure result
        "~" -> do
          childPlace <- emitCastExpression child >>= loadOperandIntoRegister
          emit "xor" [("source", Place "i" "65535"), ("dest", childPlace)]
          pure childPlace
        "!" -> do
          childPlace <- emitCastExpression child >>= loadOperandIntoRegister
          emitConditionalEvaluation childPlace (Place "i" "0") childPlace "je"
          pure childPlace
        _ -> throw ("simple TAC does not support unary op: " <> op)
  | otherwise = emitPostfixExpression node

emitPrefixMutation :: Node -> String -> TacM Place
emitPrefixMutation child op = do
  childPlace <- emitUnaryExpression child
  nextReg <-
    if isRValue childPlace
      then pure childPlace
      else loadOperandIntoRegister childPlace
  emit op [("source", Place "i" "1"), ("dest", nextReg)]
  _ <- emitMove nextReg childPlace
  pure nextReg

emitAddressOf :: Place -> TacM Place
emitAddressOf place = do
  target <- nextTemp
  case placeType place of
    _ | isMemoryLValue place -> emit "!get_address" [("dest", target), ("target", place)] >> pure target
    "pr" -> emit "mov" [("source", place), ("dest", target)] >> pure target
    "t" -> pure place
    _ -> throw ("simple TAC cannot take address of place type: " <> placeType place)

emitDereference :: Place -> TacM Place
emitDereference place = do
  pointerRegister <- nextPr
  case placeType place of
    _ | not (isRValue place) -> emit "ld" [("source", place), ("dest", pointerRegister)] >> pure pointerRegister
    "vr" -> pointerFromRegister place
    "t" -> pointerFromRegister place
    "i" -> emit "mov" [("source", place), ("dest", pointerRegister)] >> pure pointerRegister
    _ -> throw ("simple TAC cannot dereference place type: " <> placeType place)
  where
    pointerFromRegister register = do
      loaded <- loadOperandIntoRegister register
      pure (loaded {placeType = "pr"})

emitPostfixExpression :: Node -> TacM Place
emitPostfixExpression node
  | nodeName node == "POSTFIX_EXPRESSION" = do
      primary <- fieldNode "primary_expression" node
      place <- emitPrimaryExpression primary
      context <- primaryPostfixContext primary
      emitPostfixOps place context (postfixOps node)
  | otherwise = emitPrimaryExpression node

emitPostfixOps :: Place -> PostfixContext -> [PostfixOp] -> TacM Place
emitPostfixOps place _ [] = pure place
emitPostfixOps place context (op : rest) = do
  (nextPlace, nextContext) <-
    case postfixType op of
      "[" -> do
        indexExpression <- postfixNode op
        indexer <- emitExpression indexExpression
        let size = indexingElementSize context
        indexed <-
          if (contextPointerLevel context > 0 && null (contextDimensions context))
            || (not (null (contextDimensions context)) && placeType place /= "pr")
            || (null (contextDimensions context) && contextPointerLevel context == 0 && placeType place == "t")
            then emitPointerIndexing place indexer size
            else emitIndexing place indexer size
        pure (indexed, afterIndexContext context)
      "(" -> do
        args <- postfixArgumentNodes op
        let isStandard = isStandardFunctionPlace place
        callReturnsValue <-
          if isStandard
            then pure (standardFunctionReturnsValue place)
            else userFunctionReturnsValue place
        emitArgumentList isStandard args
        target <-
          if isLValue place
            then loadOperandIntoRegister place
            else pure place
        whenBreakpointsEnabled $ do
          debugTarget <- nextTemp
          _ <- emitMove target debugTarget
          emit "!debug_function_call" [("target", debugTarget)]
        emit "call" [("target", target)]
        unless isStandard $
          emit "add" [("source", Place "i" (show (length args))), ("dest", Place "r" "stack_pointer")]
        if callReturnsValue
          then do
            result <- nextTemp
            _ <- emitMove (Place "r" "return_reg") result
            pure (result, emptyPostfixContext)
          else pure (target, emptyPostfixContext)
      "++" -> do
        mutated <- emitPostfixMutation place "add"
        pure (mutated, emptyPostfixContext)
      "--" -> do
        mutated <- emitPostfixMutation place "sub"
        pure (mutated, emptyPostfixContext)
      other -> throw ("simple TAC does not support postfix op: " <> other)
  emitPostfixOps nextPlace nextContext rest

emitPointerIndexing :: Place -> Place -> Integer -> TacM Place
emitPointerIndexing pointer indexer size
  | placeType pointer == "vr" && (placeType indexer == "i" || (placeType indexer == "vr" && size == 1)) = do
      pointerRegister <- nextPr
      emit "add3" [("source", pointer), ("dest", pointerRegister), ("offset", scaledImmediate size indexer)]
      pure pointerRegister
  | otherwise = do
      dereferenced <- emitDereference pointer
      emitIndexing dereferenced indexer size

emitIndexing :: Place -> Place -> Integer -> TacM Place
emitIndexing indexed indexer size
  | isRegisterRValue indexed && isRValue indexer = do
      _ <- emitAbstractAdd indexer indexed size
      pure (indexed {placeType = "pr"})
  | isLValue indexed =
      if placeType indexer == "i"
        then emitOffsetLValue indexer indexed size
        else do
          address <- emitAddressOf indexed
          emitIndexing address indexer size
  | isLValue indexer = do
      dereferenced <- emitDereference indexer
      emitIndexing indexed (dereferenced {placeType = "t"}) size
  | otherwise = throw ("simple TAC cannot index " <> placeType indexed <> " with " <> placeType indexer)

emitAbstractAdd :: Place -> Place -> Integer -> TacM Place
emitAbstractAdd source dest size
  | not (isRegisterRValue dest || placeType dest == "pr") = throw "simple TAC indexed destination must be a register"
  | not (isRegisterRValue source || placeType source == "i") = throw "simple TAC indexed source must be an rvalue"
  | placeType source == "i" = emit "add" [("source", scaledImmediate size source), ("dest", dest)] >> pure dest
  | size > 1 = do
      source' <-
        if placeType source == "vr"
          then loadOperandIntoRegister source
          else pure source
      emit "mull" [("source", Place "i" (show size)), ("dest", source')]
      emit "add" [("source", source'), ("dest", dest)]
      pure dest
  | otherwise = emit "add" [("source", source), ("dest", dest)] >> pure dest

emitOffsetLValue :: Place -> Place -> Integer -> TacM Place
emitOffsetLValue offset place size
  | placeType offset /= "i" = throw "simple TAC offset must be immediate"
  | placeType place == "pr" = do
      emit "add" [("source", scaledImmediate size offset), ("dest", place)]
      pure place
  | isLValue place || placeType place == "i" =
      pure (place {placeValue = show (placeInteger place + placeInteger offset * size)})
  | otherwise = throw ("simple TAC cannot offset place type: " <> placeType place)

scaledImmediate :: Integer -> Place -> Place
scaledImmediate size place
  | placeType place == "i" = Place "i" (show (placeInteger place * size))
  | otherwise = place

emitArgumentList :: Bool -> [Node] -> TacM ()
emitArgumentList isStandard args =
  if isStandard
    then mapM_ emitStandardArgument (reverse (zip [(1 :: Int) ..] args))
    else mapM_ emitArgument (reverse args)

emitStandardArgument :: (Int, Node) -> TacM ()
emitStandardArgument (index, arg) = do
  place <- emitAssignmentExpression arg
  _ <- emitMove place (standardArgumentRegister index)
  pure ()

emitArgument :: Node -> TacM ()
emitArgument arg = do
  place <- emitAssignmentExpression arg
  pushed <-
    if not (isRegisterRValue place) || placeType place == "i"
      then loadOperandIntoRegister place
      else pure place
  emit "push" [("target", pushed)]

whenBreakpointsEnabled :: TacM () -> TacM ()
whenBreakpointsEnabled action = do
  enabled <- not . null . breakpoints <$> get
  when enabled action

postfixArgumentNodes :: PostfixOp -> TacM [Node]
postfixArgumentNodes op =
  case postfixValue op of
    Just (NodeRef node)
      | nodeName node == "ARGUMENT_LIST" -> pure (childNodes node)
    Just (NodeList nodes) -> pure nodes
    Nothing -> pure []
    _ -> throw "malformed postfix argument list"

postfixNode :: PostfixOp -> TacM Node
postfixNode op =
  case postfixValue op of
    Just (NodeRef node) -> pure node
    _ -> throw "malformed postfix node"

asmArguments :: Node -> TacM [Node]
asmArguments node =
  case lookupField "arguments" node of
    Just (NodeList args) -> pure args
    _ -> throw "malformed asm argument list"

asmRegisterPlace :: Node -> TacM Place
asmRegisterPlace arg = registerPlace <$> fieldNode "asm_symbol" arg

asmCSymbolPlace :: Node -> TacM Place
asmCSymbolPlace arg = do
  symbol <- fieldNode "c_symbol" arg
  lookupLocal (identifierValue symbol)

registerPlace :: Node -> Place
registerPlace node = Place "r" (fieldStringDefault "id" "" node)

emitPostfixMutation :: Place -> String -> TacM Place
emitPostfixMutation place op = do
  nextReg <- loadOperandIntoRegister place
  if isRegisterRValue place
    then do
      emit op [("source", Place "i" "1"), ("dest", place)]
      pure nextReg
    else do
      tempReg <- nextTemp
      _ <- emitMove nextReg tempReg
      emit op [("source", Place "i" "1"), ("dest", nextReg)]
      _ <- emitMove nextReg place
      pure tempReg

emitPrimaryExpression :: Node -> TacM Place
emitPrimaryExpression node =
  case nodeName node of
    "INT" -> pure (Place "i" (show (fieldIntDefault "value" 0 node)))
    "CHARACTER" -> pure (Place "i" (characterValue node))
    "IDENTIFIER" -> lookupLocal (fieldStringDefault "value" (identifierValue node) node)
    "EXPRESSION" -> emitExpression node
    "STRING_LITERAL" -> do
      stringPlace <- nextGlobalSize (stringLiteralLength node)
      emitAddressOf stringPlace
    other -> throw ("simple TAC does not support primary: " <> other)

emitMove :: Place -> Place -> TacM Place
emitMove source dest
  | source == dest = pure source
  | placeType dest == "i" = throw "destination cannot be immediate"
  | placeType source == "i" && isDestMem = do
      loaded <- loadOperandIntoRegister source
      emitMove loaded dest
  | not isSourceMem && not isDestMem = emit "mov" [("source", source), ("dest", dest)] >> pure source
  | not isSourceMem && isDestMem = emit "st" [("source", source), ("dest", dest)] >> pure source
  | isSourceMem && not isDestMem = emit "ld" [("source", source), ("dest", dest)] >> pure source
  | otherwise = do
      loaded <- loadOperandIntoRegister source
      emitMove loaded dest
  where
    isSourceMem = not (isRValue source)
    isDestMem = not (isRValue dest)

loadOperandIntoRegister :: Place -> TacM Place
loadOperandIntoRegister place
  | placeType place == "t" = pure place
  | otherwise = do
      temp <- nextTemp
      _ <- emitMove place temp
      pure temp

loadOperandIntoReadOnlyRegister :: Place -> TacM Place
loadOperandIntoReadOnlyRegister place
  | isRegisterRValue place = pure place
  | otherwise = loadOperandIntoRegister place

nextLocalSize :: Integer -> TacM Place
nextLocalSize size = do
  st <- get
  let offset = localOffset st
  modify' (\s -> s {localOffset = offset + size})
  pure (Place "l" (show offset))

nextGlobalSize :: Integer -> TacM Place
nextGlobalSize size = do
  st <- get
  let offset = globalOffset st
  modify' (\s -> s {globalOffset = offset + size})
  pure (Place "g" (show offset))

nextTemp :: TacM Place
nextTemp = do
  st <- get
  let next = tempCounter st + 1
  modify' (\s -> s {tempCounter = next})
  pure (Place "t" (show next))

nextPr :: TacM Place
nextPr = do
  st <- get
  let next = tempCounter st + 1
  modify' (\s -> s {tempCounter = next})
  pure (Place "pr" (show next))

nextVR :: TacM Place
nextVR = do
  st <- get
  let next = tempCounter st + 1
  modify' (\s -> s {tempCounter = next})
  pure (Place "vr" (show next))

nextLabel :: TacM Place
nextLabel = do
  st <- get
  let next = labelCounter st
  modify' (\s -> s {labelCounter = next + 1})
  pure (Place "i" (".label_" <> show next))

standardArgumentRegister :: Int -> Place
standardArgumentRegister index =
  case index of
    1 -> Place "r" "r22"
    2 -> Place "r" "r23"
    3 -> Place "r" "r24"
    4 -> Place "r" "r25"
    _ -> Place "r" ("r" <> show (21 + index))

isStandardFunctionPlace :: Place -> Bool
isStandardFunctionPlace place =
  placeType place == "i" && Map.member (placeValue place) defaultStandardFunctionTargetMap

standardFunctionReturnsValue :: Place -> Bool
standardFunctionReturnsValue place =
  Map.findWithDefault True (placeValue place) defaultStandardFunctionTargetMap

userFunctionReturnsValue :: Place -> TacM Bool
userFunctionReturnsValue place =
  Map.findWithDefault True (placeValue place) . functionReturns <$> get

pushLoopLabels :: Place -> Place -> TacM ()
pushLoopLabels updateLabel endLabel =
  modify' (\st -> st {loopLabels = (updateLabel, endLabel) : loopLabels st})

popLoopLabels :: TacM ()
popLoopLabels =
  modify' $ \st ->
    st
      { loopLabels =
          case loopLabels st of
            [] -> []
            _ : rest -> rest
      }

pushCaseLabels :: TacM ()
pushCaseLabels =
  modify' (\st -> st {caseLabels = CaseContext [] Nothing : caseLabels st})

addCaseLabel :: Place -> Place -> TacM ()
addCaseLabel value target =
  modify' $ \st ->
    st
      { caseLabels =
          case caseLabels st of
            context : rest -> context {caseEntries = (value, target) : caseEntries context} : rest
            [] -> []
      }

setDefaultCaseLabel :: Place -> TacM ()
setDefaultCaseLabel target =
  modify' $ \st ->
    st
      { caseLabels =
          case caseLabels st of
            context : rest -> context {caseDefault = Just target} : rest
            [] -> []
      }

popCaseLabels :: TacM CaseContext
popCaseLabels = do
  labels <- caseLabels <$> get
  case labels of
    context : rest -> do
      modify' (\st -> st {caseLabels = rest})
      pure context {caseEntries = reverse (caseEntries context)}
    [] -> throw "case label stack underflow"

emit :: String -> [(String, Place)] -> TacM ()
emit ty fields =
  modify' (\st -> st {instructions = instructions st |> Instr ty fields []})

emitString :: String -> [(String, String)] -> TacM ()
emitString ty fields =
  modify' (\st -> st {instructions = instructions st |> Instr ty [] fields})

insertInstructionsAt :: Int -> [Instr] -> TacM ()
insertInstructionsAt index inserted =
  modify' $ \st ->
    let (prefix, suffix) = Seq.splitAt index (instructions st)
     in st {instructions = prefix >< Seq.fromList inserted >< suffix}

renderTac :: TacProgram -> [String]
renderTac program =
  concatMap renderMethodOutput (tacProgramMethods program)
    <> ["LOCAL_SIZE\t" <> show (mainLocalSize program)]

renderMethodOutput :: MethodOutput -> [String]
renderMethodOutput output =
  ["METHOD\t" <> methodOutputName output]
    <> zipWith renderInstr [(1 :: Int) ..] (methodOutputInstructions output)

mainLocalSize :: TacProgram -> Integer
mainLocalSize program =
  case [methodOutputLocalSize output | output <- tacProgramMethods program, methodOutputName output == "main"] of
    value : _ -> value
    [] -> 0

renderInstr :: Int -> Instr -> String
renderInstr index instr =
  show index <> "\t" <> instrType instr <> concatMap renderField (instrFields instr) <> concatMap renderStringField (instrStringFields instr)

renderField :: (String, Place) -> String
renderField (name, place) = "\t" <> name <> "=" <> renderPlace place

renderStringField :: (String, String) -> String
renderStringField (name, value) = "\t" <> name <> "=" <> value

renderPlace :: Place -> String
renderPlace place = placeType place <> ":" <> placeValue place

lookupLocal :: String -> TacM Place
lookupLocal name = do
  found <- lookupLocalInfo name
  case found of
    Just info ->
      if null (localInfoDimensions info) || localInfoPointerToArray info
        then pure (localInfoPlace info)
        else emitAddressOf (localInfoPlace info)
    Nothing -> do
      enumConstant <- Map.lookup name . enumConstants <$> get
      case enumConstant of
        Just place -> pure place
        Nothing -> do
          function <- Map.lookup name . functionPlaces <$> get
          maybe (throw ("missing local: " <> name)) pure function

lookupLocalInfo :: String -> TacM (Maybe LocalInfo)
lookupLocalInfo name = do
  local <- Map.lookup name . locals <$> get
  case local of
    Just info -> pure (Just info)
    Nothing -> Map.lookup name . globals <$> get

isRValue :: Place -> Bool
isRValue place = placeType place `elem` ["i", "t", "r", "vr"]

isRegisterRValue :: Place -> Bool
isRegisterRValue place = placeType place `elem` ["t", "r", "vr"]

isMemoryLValue :: Place -> Bool
isMemoryLValue place = placeType place `elem` ["l", "p", "g", "vr"]

isLValue :: Place -> Bool
isLValue place = placeType place `elem` ["l", "p", "g", "pr", "vr"]

childNodeAt :: Int -> [NodeChild] -> TacM Node
childNodeAt wanted children =
  case drop wanted children of
    ChildNode node : _ -> pure node
    _ -> throw "expected child node"

onlyChildNodeAt :: Int -> [NodeChild] -> TacM Node
onlyChildNodeAt = childNodeAt

childNodes :: Node -> [Node]
childNodes node = [child | ChildNode child <- nodeChildren node]

childNodesOnly :: Node -> [Node]
childNodesOnly = childNodes

postfixOps :: Node -> [PostfixOp]
postfixOps node = [op | ChildPostfix op <- nodeChildren node]

emptyPostfixContext :: PostfixContext
emptyPostfixContext = PostfixContext 0 [] False

primaryPostfixContext :: Node -> TacM PostfixContext
primaryPostfixContext node =
  case nodeName node of
    "IDENTIFIER" -> do
      info <- lookupLocalInfo (fieldStringDefault "value" (identifierValue node) node)
      pure $
        maybe
          emptyPostfixContext
          (\local -> PostfixContext (localInfoPointerLevel local) (localInfoDimensions local) (localInfoPointerToArray local))
          info
    _ -> pure emptyPostfixContext

indexingElementSize :: PostfixContext -> Integer
indexingElementSize context =
  if contextPointerToArray context
    then declaratorSlotSize (contextDimensions context)
    else
      case contextDimensions context of
        _ : rest -> declaratorSlotSize rest
        [] -> 1

afterIndexContext :: PostfixContext -> PostfixContext
afterIndexContext context =
  if contextPointerToArray context
    then context {contextPointerLevel = 0, contextPointerToArray = False}
    else
      case contextDimensions context of
        _ : rest -> context {contextDimensions = rest}
        [] -> context {contextPointerLevel = max 0 (contextPointerLevel context - 1)}

declaratorDimensions :: Node -> TacM [Integer]
declaratorDimensions declarator = do
  direct <- fieldNode "direct_declarator" declarator
  pure (fieldIntsDefault "dimensions" [] direct)

declaratorPointerLevel :: Node -> Integer
declaratorPointerLevel declarator =
  fieldIntDefault "pointer_level" 0 declarator
    + maybe 0 declaratorPointerLevel nestedDeclarator
  where
    nestedDeclarator =
      fieldNodeMaybe "direct_declarator" declarator
        >>= fieldNodeMaybe "declarator"

declaratorPointerToArray :: Node -> Bool
declaratorPointerToArray declarator =
  fieldIntDefault "pointer_level" 0 declarator == 0
    && maybe False (not . null . fieldIntsDefault "dimensions" []) directDeclarator
    && maybe False ((> 0) . declaratorPointerLevel) nestedDeclarator
  where
    directDeclarator = fieldNodeMaybe "direct_declarator" declarator
    nestedDeclarator = directDeclarator >>= fieldNodeMaybe "declarator"

declaratorDimensionsWithInitializer :: Maybe Node -> Node -> TacM [Integer]
declaratorDimensionsWithInitializer initializer declarator = do
  dimensions <- declaratorDimensions declarator
  pure (fillInferredDimensions initializer dimensions)

fillInferredDimensions :: Maybe Node -> [Integer] -> [Integer]
fillInferredDimensions initializer dimensions =
  case dimensions of
    (-1) : rest -> inferredLength initializer : rest
    _ -> dimensions

inferredLength :: Maybe Node -> Integer
inferredLength initializer =
  case initializer of
    Just node
      | nodeName node == "INITIALIZER_LIST" -> fromIntegral (length (childNodes node))
    Just node
      | nodeName node == "INITIALIZER" ->
          case fieldNodeMaybe "value" node of
            Just value | nodeName value == "STRING_LITERAL" -> stringLiteralLength value
            _ -> 1
    _ -> 1

initializerContainsDirectString :: Maybe Node -> Bool
initializerContainsDirectString initializer =
  case initializer of
    Just node
      | nodeName node == "INITIALIZER" ->
          case fieldNodeMaybe "value" node of
            Just value -> nodeName value == "STRING_LITERAL"
            _ -> False
    _ -> False

directInitializerStringLength :: Maybe Node -> Integer
directInitializerStringLength initializer =
  case initializer of
    Just node
      | nodeName node == "INITIALIZER" ->
          maybe 1 stringLiteralLength (fieldNodeMaybe "value" node)
    _ -> 1

allocateNestedInitializerStrings :: Integer -> [Integer] -> Maybe Node -> TacM ()
allocateNestedInitializerStrings pointerLevel dimensions initializer =
  case initializer of
    Just node -> allocateStringsInInitializer pointerLevel dimensions node
    Nothing -> pure ()

allocateStringsInInitializer :: Integer -> [Integer] -> Node -> TacM ()
allocateStringsInInitializer pointerLevel dimensions initializer
  | nodeName initializer == "INITIALIZER" =
      case fieldNodeMaybe "value" initializer of
        Just value
          | nodeName value == "STRING_LITERAL" && pointerLevel > 0 -> do
              _ <- nextGlobalSize (stringLiteralLength value)
              pure ()
        _ -> pure ()
  | nodeName initializer == "INITIALIZER_LIST" =
      forM_ (childNodes initializer) $ \child ->
        allocateStringsInInitializer (nestedPointerLevel pointerLevel dimensions) (nestedDimensions dimensions) child
  | otherwise = pure ()

nestedPointerLevel :: Integer -> [Integer] -> Integer
nestedPointerLevel pointerLevel dimensions =
  if null dimensions
    then max 0 (pointerLevel - 1)
    else pointerLevel

nestedDimensions :: [Integer] -> [Integer]
nestedDimensions dimensions =
  case dimensions of
    _ : rest -> rest
    [] -> []

declaratorSlotSize :: [Integer] -> Integer
declaratorSlotSize dimensions =
  product (map normalizeDimension dimensions)
  where
    normalizeDimension value
      | value > 0 = value
      | otherwise = 1

stringLiteralLength :: Node -> Integer
stringLiteralLength node =
  case lookupField "value" node of
    Just (StringValue value) -> fromIntegral (length value + 1)
    _ -> 1

firstExpressionChild :: Node -> TacM Node
firstExpressionChild node =
  case nodeName node of
    "EXPRESSION" ->
      case childNodes node of
        child : _ -> pure child
        [] -> throw "empty expression"
    _ -> pure node

fieldNode :: String -> Node -> TacM Node
fieldNode name node =
  case lookupField name node of
    Just (NodeRef child) -> pure child
    _ -> throw ("missing node field '" <> name <> "' on " <> nodeName node)

fieldNodeList :: String -> Node -> TacM [Node]
fieldNodeList name node =
  case lookupField name node of
    Just (NodeList children) -> pure children
    _ -> throw ("missing node list field '" <> name <> "' on " <> nodeName node)

fieldNodeMaybe :: String -> Node -> Maybe Node
fieldNodeMaybe name node =
  case lookupField name node of
    Just (NodeRef child) -> Just child
    _ -> Nothing

parameterPlaces :: Node -> TacM [(String, LocalInfo)]
parameterPlaces declarator = do
  direct <- fieldNode "direct_declarator" declarator
  case fieldNodeMaybe "parameter_list" direct of
    Nothing -> pure []
    Just params ->
      pure
        [ (declaratorName paramDeclarator, LocalInfo (Place "p" (show index)) (declaratorPointerLevel paramDeclarator) [] (declaratorPointerToArray paramDeclarator))
        | (index, param) <- zip [(0 :: Integer) ..] (childNodes params)
        , Just paramDeclarator <- [fieldNodeMaybe "declarator" param]
        , declaratorName paramDeclarator /= ""
        ]

fieldString :: String -> Node -> TacM String
fieldString name node =
  case lookupField name node of
    Just (StringValue value) -> pure value
    _ -> throw ("missing string field '" <> name <> "' on " <> nodeName node)

fieldStringDefault :: String -> String -> Node -> String
fieldStringDefault name fallback node =
  case lookupField name node of
    Just (StringValue value) -> value
    _ -> fallback

characterValue :: Node -> String
characterValue node =
  case lookupField "value" node of
    Just (StringValue value) -> value
    Just (IntValue value) -> show value
    _ -> "0"

fieldIntDefault :: String -> Integer -> Node -> Integer
fieldIntDefault name fallback node =
  case lookupField name node of
    Just (IntValue value) -> value
    _ -> fallback

fieldIntsDefault :: String -> [Integer] -> Node -> [Integer]
fieldIntsDefault name fallback node =
  case lookupField name node of
    Just (IntList values) -> values
    _ -> fallback

hasNodeField :: String -> Node -> Bool
hasNodeField name node =
  case lookupField name node of
    Just (NodeRef _) -> True
    _ -> False

hasBoolField :: String -> Node -> Bool
hasBoolField name node =
  case lookupField name node of
    Just (BoolValue True) -> True
    _ -> False

lookupField :: String -> Node -> Maybe NodeValue
lookupField name node =
  firstJust [Just value | NodeField fieldName' value <- nodeFields node, fieldName' == name]

identifierValue :: Node -> String
identifierValue node = fieldStringDefault "id" (fieldStringDefault "value" "" node) node

declaratorName :: Node -> String
declaratorName declarator =
  maybe "" identifierValue (fieldNodeMaybe "id" declarator)

functionDeclarationReturnsValue :: Node -> Bool
functionDeclarationReturnsValue declaration =
  case fieldNodeMaybe "specifier" declaration >>= fieldNodeMaybePure "type_specifier" >>= lookupField "kind" of
    Just (StringList ["void"]) -> False
    Just (StringList ["VOID"]) -> False
    _ -> True

fieldNodeMaybePure :: String -> Node -> Maybe Node
fieldNodeMaybePure name node =
  case lookupField name node of
    Just (NodeRef child) -> Just child
    _ -> Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just value : _) = Just value
firstJust (Nothing : values) = firstJust values

comparisonGroups :: [NodeChild] -> TacM [(String, Node)]
comparisonGroups [] = pure []
comparisonGroups (ChildToken token : ChildNode rhs : rest) =
  ((tokenName token, rhs) :) <$> comparisonGroups rest
comparisonGroups _ = throw "malformed comparison expression"

splitLast :: [a] -> ([a], a)
splitLast [] = error "splitLast: empty list"
splitLast [value] = ([], value)
splitLast (value : values) =
  let (prefix, finalValue) = splitLast values
   in (value : prefix, finalValue)

equalityJump :: String -> TacM String
equalityJump op =
  case op of
    "==" -> pure "je"
    "!=" -> pure "jne"
    _ -> throw ("simple TAC unsupported equality op: " <> op)

signedRelationalJump :: String -> TacM String
signedRelationalJump op =
  case op of
    "<" -> pure "jl"
    ">" -> pure "jg"
    "<=" -> pure "jle"
    ">=" -> pure "jge"
    _ -> throw ("simple TAC unsupported relational op: " <> op)

compoundOperationType :: String -> TacM String
compoundOperationType op =
  case op of
    "+=" -> pure "add"
    "-=" -> pure "sub"
    "*=" -> pure "mull"
    "&=" -> pure "and"
    "|=" -> pure "or"
    "^=" -> pure "xor"
    "<<=" -> pure "shl"
    ">>=" -> pure "shr"
    "/=" -> throw "simple TAC does not support division assignment"
    "%=" -> throw "simple TAC does not support remainder assignment"
    _ -> throw ("simple TAC does not support assignment op: " <> op)

isLogicalExpression :: Node -> Bool
isLogicalExpression node =
  nodeName node
    `elem` [ "LOGICAL_AND_EXPRESSION"
           , "LOGICAL_OR_EXPRESSION"
           , "RELATIONAL_EXPRESSION"
           , "EQUALITY_EXPRESSION"
           ]

placeInteger :: Place -> Integer
placeInteger = read . placeValue

defaultPlaces :: [(String, Place)]
defaultPlaces =
  [ (name, operandToPlace operand)
  | (name, symbol) <- defaultSymbols
  , Just operand <- [symbolPlace symbol]
  ]

defaultStandardFunctionTargets :: [(String, Bool)]
defaultStandardFunctionTargets =
  [ (placeValue (operandToPlace operand), returnsValue (symbolType symbol))
  | (_, symbol) <- defaultSymbols
  , Just operand <- [symbolPlace symbol]
  , operandIsStandardFunction operand
  ]

defaultStandardFunctionTargetMap :: Map.Map String Bool
defaultStandardFunctionTargetMap = Map.fromList defaultStandardFunctionTargets

operandToPlace :: Operand -> Place
operandToPlace operand =
  Place
    { placeType = operandType operand
    , placeValue = operandValueString (operandValue operand)
    }

operandValueString :: OperandValue -> String
operandValueString value =
  case value of
    OperandInt number -> show number
    OperandName name -> name

returnsValue :: CType -> Bool
returnsValue ty =
  case ty of
    FunctionType (BaseType Void _) _ -> False
    FunctionType {} -> True
    _ -> True

throw :: String -> TacM a
throw = lift . Left
