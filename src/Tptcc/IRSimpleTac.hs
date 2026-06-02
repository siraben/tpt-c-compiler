module Tptcc.IRSimpleTac
  ( dumpSimpleTac
  , generateSimpleTac
  , Instr (..)
  , MethodOutput (..)
  , Place (..)
  , TacProgram (..)
  ) where

import Control.Monad (foldM, forM, forM_, unless, void, when)
import Data.Foldable (for_, toList)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map.Strict as Map
import Data.Sequence ((><), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Effectful
import qualified Effectful.Error.Static as Error
import qualified Effectful.State.Static.Local as State

import Tptcc.Ast
import Tptcc.CType (CType (..), Member (..), TypeKind (..), applyPointers, array, base, baseFromSpecifiers, baseWithSigned, decayArrayParameter, decayExpressionType, dereferenceType, dereferenceTypeMaybe, enum, function, integerPromotion, isBaseSpecifiers, memberByName, pointer, sizeof, struct, union, usualArithmeticConversion, variadicFunction, withMemberOffsets)
import Tptcc.NodeFields
import Tptcc.NodeFields.Effectful
import Tptcc.Operand (Operand (..), OperandValue (..))
import Tptcc.IRSimpleTac.Types
import Tptcc.SymbolTable (Symbol (..), defaultSymbols)
import Tptcc.Tac hiding (fieldString)
import Tptcc.Token (Token (..))

dumpSimpleTac :: Node -> Either String [String]
dumpSimpleTac ast = do
  program <- generateSimpleTac ast
  pure (renderTac program)

generateSimpleTac :: Node -> Either String TacProgram
generateSimpleTac ast = do
  (_, st) <- runTac (emitProgram ast)
  pure
    TacProgram
      { tacProgramMethods = reverse (methods st)
      , tacProgramGlobalSize = globalOffset st
      , tacProgramGlobalInstructions = toList (instructions st)
      }

runTac :: TacM a -> Either String (a, TacState)
runTac action =
  runPureEff (Error.runErrorNoCallStack (State.runState initialState action))

initialState :: TacState
initialState =
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
    , typeTags = Map.empty
    , globals = Map.empty
    , locals = Map.empty
    , instructions = Seq.empty
    , methods = []
    }

emitProgram :: Node -> TacM ()
emitProgram program = do
  let declarations = childNodes program
      functionEntries = [(declaratorName declarator, declaration, declarator) | declaration <- declarations, hasNodeField "block" declaration, Just declarator <- [fieldNodeMaybe "declarator" declaration]]
      userFunctionPlaces = Map.fromList [(name, Place Immediate ("__tptcc_fn_" <> name)) | (name, _, _) <- functionEntries]
      userFunctionReturns = Map.fromList [("__tptcc_fn_" <> name, functionDeclarationReturnsValue declaration) | (name, declaration, _) <- functionEntries]
  State.modify (\st -> st {functionPlaces = Map.union userFunctionPlaces (functionPlaces st), functionReturns = Map.union userFunctionReturns (functionReturns st)})
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
  declaredBase <- resolveTypeSpecifier =<< fieldNode "type_specifier" specifier
  unless (storageKind == "typedef") $ do
    unless (storageKind == "extern") $ do
      declarators <- fieldNodeList "declarators" declaration
      forM_ declarators $ \declarator ->
        unless (hasBoolField "is_function" declarator) $ do
          let initializer = fieldNodeMaybe "initializer" declarator
          dimensions <- declaratorDimensionsWithInitializer initializer declarator
          declaredTy <- withArrayDimensions dimensions <$> buildDeclaratorType declarator declaredBase
          let pointerLevel = declaratorPointerLevel declarator
              size = objectSlotSize dimensions declaredTy
              name = declaratorName declarator
          place <-
            if pointerLevel > 0 && initializerContainsDirectString initializer
              then do
                _ <- nextGlobalSize (directInitializerStringLength initializer)
                nextGlobalSize 1
              else nextGlobalSize size
          let info = LocalInfo place pointerLevel dimensions (declaratorPointerToArray declarator) declaredTy
          State.modify (\st -> st {globals = Map.insert name info (globals st)})
          unless (pointerLevel > 0 && initializerContainsDirectString initializer) $
            allocateNestedInitializerStrings pointerLevel dimensions initializer
          maybe (pure ()) (simulateGlobalInitializer place) initializer

collectEnumConstants :: Node -> TacM ()
collectEnumConstants declaration =
  case enumSpecifierFromDeclaration declaration >>= fieldNodeMaybe "declaration" of
    Just enumDeclaration ->
      forM_ (enumMemberValues (childNodes enumDeclaration)) $ \(member, value) -> do
        memberId <- fieldNode "id" member
        State.modify (\st -> st {enumConstants = Map.insert (identifierValue memberId) (Place Immediate (Text.pack (show value))) (enumConstants st)})
    Nothing -> pure ()

enumSpecifierFromDeclaration :: Node -> Maybe Node
enumSpecifierFromDeclaration declaration = do
  specifier <- fieldNodeMaybe "specifier" declaration
  typeSpecifier <- fieldNodeMaybe "type_specifier" specifier
  case lookupField "kind" typeSpecifier of
    Just (NodeRef enumSpecifier)
      | nodeIs NodeEnumSpecifier enumSpecifier -> Just enumSpecifier
    _ -> Nothing

emitFunction :: (Text, Node, Node) -> TacM ()
emitFunction (name, declaration, declarator) = do
  block <- fieldNode "block" declaration
  params <- parameterPlaces declarator
  previous <- captureFunctionContext
  enterFunctionContext name params
  emitBlock block
  output <- currentMethodOutput name
  restoreFunctionContext previous output

captureFunctionContext :: TacM FunctionContext
captureFunctionContext = do
  st <- State.get
  pure
    FunctionContext
      { contextMethodName = currentMethodName st
      , contextLocalOffset = localOffset st
      , contextLocals = locals st
      , contextInstructions = instructions st
      , contextLoopLabels = loopLabels st
      , contextCaseLabels = caseLabels st
      }

enterFunctionContext :: Text -> [(Text, LocalInfo)] -> TacM ()
enterFunctionContext name params =
  State.modify $
    \st ->
      st
        { currentMethodName = name
        , localOffset = 0
        , locals = Map.fromList params
        , instructions = Seq.empty
        , loopLabels = []
        , caseLabels = []
        }

currentMethodOutput :: Text -> TacM MethodOutput
currentMethodOutput name = do
  st <- State.get
  pure
    MethodOutput
      { methodOutputName = name
      , methodOutputInstructions = toList (instructions st)
      , methodOutputLocalSize = localOffset st
      }

restoreFunctionContext :: FunctionContext -> MethodOutput -> TacM ()
restoreFunctionContext previous output =
  State.modify $
    \st ->
      st
        { methods = output : methods st
        , currentMethodName = contextMethodName previous
        , localOffset = contextLocalOffset previous
        , locals = contextLocals previous
        , instructions = contextInstructions previous
        , loopLabels = contextLoopLabels previous
        , caseLabels = contextCaseLabels previous
        }

emitBlock :: Node -> TacM ()
emitBlock block = withLocalScope (mapM_ emitStatement (childNodes block))

withLocalScope :: TacM a -> TacM a
withLocalScope action = do
  previousLocals <- locals <$> State.get
  result <- action
  State.modify (\st -> st {locals = previousLocals})
  pure result

emitStatement :: Node -> TacM ()
emitStatement statement = do
  child <- fieldNode "child" statement
  case nodeKind child of
    NodeDeclaration -> emitDeclaration child
    NodeExpression -> void (emitExpression child)
    NodeIf -> emitIf child
    NodeBlock -> emitBlock child
    NodeWhile -> emitWhile child
    NodeDoWhile -> emitDoWhile child
    NodeFor -> emitFor child
    NodeBreak -> emitBreak
    NodeContinue -> emitContinue
    NodeSwitch -> emitSwitch child
    NodeCase -> emitCase child
    NodeDefault -> emitDefault child
    NodeGoto -> emitGoto child
    NodeLabel -> emitLabel child
    NodeAsm -> emitAsm child
    NodeReturn -> emitReturn child
    NodeEmptyStatement -> pure ()
    _ -> throw ("simple TAC does not support statement: " <> nodeName child)

emitDeclaration :: Node -> TacM ()
emitDeclaration declaration = do
  storageKind <- declarationStorageKind declaration
  specifier <- fieldNode "specifier" declaration
  declaredBase <- resolveTypeSpecifier =<< fieldNode "type_specifier" specifier
  declarators <- fieldNodeList "declarators" declaration
  forM_ declarators $ \declarator -> do
    let initializer = fieldNodeMaybe "initializer" declarator
    dimensions <- declaratorDimensionsWithInitializer initializer declarator
    declaredTy <- withArrayDimensions dimensions <$> buildDeclaratorType declarator declaredBase
    let isRegisterLocal = storageKind == "register"
        pointerLevel = declaratorPointerLevel declarator
    place <-
      if isRegisterLocal
        then do
          unless (null dimensions || pointerLevel > 0) $
            throw "simple TAC cannot allocate aggregate register"
          nextVR
        else nextLocalSize (objectSlotSize dimensions declaredTy)
    let name = declaratorName declarator
        info = LocalInfo place pointerLevel dimensions (declaratorPointerToArray declarator) declaredTy
    State.modify (\st -> st {locals = Map.insert name info (locals st)})
    case initializer of
      Just initNode -> do
        source <- emitInitializer initNode
        _ <- emitMove source place
        pure ()
      Nothing -> pure ()

declarationStorageKind :: Node -> TacM Text
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
  | nodeIs NodeInitializer initializer =
      case fieldNodeMaybe "value" initializer of
        Just value
          | nodeKind value `elem` [NodeInt, NodeCharacter, NodeStringLiteral] -> pure ()
          | otherwise -> do
              source <- emitAssignmentExpression value
              _ <- emitMove source place
              pure ()
        Nothing -> pure ()
  | nodeIs NodeInitializerList initializer =
      forM_ (zip [(0 :: Integer) ..] (childNodes initializer)) $ \(offset, child) ->
        simulateGlobalInitializer (place {placeValue = Text.pack (show (placeInteger place + offset))}) child
  | otherwise = pure ()

emitReturn :: Node -> TacM ()
emitReturn node = do
  value <- maybe (throw "simple TAC return missing value") pure (fieldNodeMaybe "value" node)
  place <- emitExpression value
  source <-
    if isLValue place
      then loadOperandIntoRegister place
      else pure place
  emit IMov [("source", source), ("dest", Place Register "return_reg")]
  method <- currentMethodName <$> State.get
  emit IJmp [("target", Place Immediate (".exit_" <> method))]

emitIf :: Node -> TacM ()
emitIf node = do
  falseLabel <- nextLabel
  trueLabel <- nextLabel
  endLabel <- nextLabel
  condition <- fieldNode "condition" node >>= firstExpressionChild
  emitBoolControlFlow condition trueLabel falseLabel
  emit ILabel [("target", trueLabel)]
  fieldNode "true_case" node >>= emitStatement
  emit IJmp [("target", endLabel)]
  emit ILabel [("target", falseLabel)]
  maybe (pure ()) emitStatement (fieldNodeMaybe "false_case" node)
  emit ILabel [("target", endLabel)]

emitWhile :: Node -> TacM ()
emitWhile node = do
  startLabel <- nextLabel
  endLabel <- nextLabel
  pushLoopLabels startLabel endLabel
  emit ILabel [("target", startLabel)]
  trueLabel <- nextLabel
  condition <- fieldNode "condition" node >>= firstExpressionChild
  emitBoolControlFlow condition trueLabel endLabel
  emit ILabel [("target", trueLabel)]
  fieldNode "statement" node >>= emitStatement
  emit IJmp [("target", startLabel)]
  emit ILabel [("target", endLabel)]
  popLoopLabels

emitDoWhile :: Node -> TacM ()
emitDoWhile node = do
  startLabel <- nextLabel
  conditionLabel <- nextLabel
  endLabel <- nextLabel
  pushLoopLabels conditionLabel endLabel
  emit ILabel [("target", startLabel)]
  fieldNode "statement" node >>= emitStatement
  emit ILabel [("target", conditionLabel)]
  condition <- fieldNode "condition" node >>= firstExpressionChild
  emitBoolControlFlow condition startLabel endLabel
  emit ILabel [("target", endLabel)]
  popLoopLabels

emitFor :: Node -> TacM ()
emitFor node = withLocalScope $ do
  startLabel <- nextLabel
  updateLabel <- nextLabel
  endLabel <- nextLabel
  pushLoopLabels updateLabel endLabel
  maybe (pure ()) emitForInitialization (fieldNodeMaybe "initialization" node)
  emit ILabel [("target", startLabel)]
  trueLabel <- nextLabel
  case fieldNodeMaybe "condition" node of
    Just condition -> firstExpressionChild condition >>= \child -> emitBoolControlFlow child trueLabel endLabel
    Nothing -> pure ()
  emit ILabel [("target", trueLabel)]
  fieldNode "statement" node >>= emitStatement
  emit ILabel [("target", updateLabel)]
  maybe (pure ()) (void . emitExpression) (fieldNodeMaybe "update" node)
  emit IJmp [("target", startLabel)]
  emit ILabel [("target", endLabel)]
  popLoopLabels

emitForInitialization :: Node -> TacM ()
emitForInitialization node =
  case nodeKind node of
    NodeDeclaration -> emitDeclaration node
    _ -> void (emitExpression node)

emitSwitch :: Node -> TacM ()
emitSwitch node = do
  condition <- fieldNode "condition" node >>= emitExpression >>= loadOperandIntoRegister
  endLabel <- nextLabel
  mark <- Seq.length . instructions <$> State.get
  pushCaseLabels
  pushLoopLabels endLabel endLabel
  fieldNode "block" node >>= emitBlock
  context <- popCaseLabels
  popLoopLabels
  let comparisons = concatMap (caseComparison condition) (caseEntries context)
      dispatch =
        comparisons
          <> [ Instr IJmp [("target", fromMaybe endLabel (caseDefault context))] []
             ]
  insertInstructionsAt mark dispatch
  emit ILabel [("target", endLabel)]

emitCase :: Node -> TacM ()
emitCase node = do
  valueNode <- fieldNode "value" node
  enumEnv <- map placeToConstant . Map.toList . enumConstants <$> State.get
  let value = Place Immediate (Text.pack (show (constantValue enumEnv valueNode)))
  trueLabel <- nextLabel
  addCaseLabel value trueLabel
  emit ILabel [("target", trueLabel)]
  fieldNode "statement" node >>= emitStatement

emitDefault :: Node -> TacM ()
emitDefault node = do
  defaultLabel <- nextLabel
  setDefaultCaseLabel defaultLabel
  emit ILabel [("target", defaultLabel)]
  fieldNode "statement" node >>= emitStatement

emitGoto :: Node -> TacM ()
emitGoto node = do
  target <- fieldNode "target" node
  method <- currentMethodName <$> State.get
  emit IJmp [("target", Place Immediate (methodLabel method (identifierValue target)))]

emitLabel :: Node -> TacM ()
emitLabel node = do
  label <- fieldNode "label" node
  method <- currentMethodName <$> State.get
  emit ILabel [("target", Place Immediate (methodLabel method (identifierValue label)))]
  fieldNode "statement" node >>= emitStatement

caseComparison :: Place -> (Place, Place) -> [Instr]
caseComparison condition (value, target) =
  [ Instr ICmp [("first", condition), ("second", value)] []
  , Instr IJe [("target", target)] []
  ]

emitAsm :: Node -> TacM ()
emitAsm node = do
  maybe (pure ()) emitAsmClobbersPush (fieldNodeMaybe "clobbers" node)
  maybe (pure ()) emitAsmInputs (fieldNodeMaybe "inputs" node)
  asm <- fieldString "asm" node
  emitString IAsm [("asm", asm)]
  maybe (pure ()) emitAsmOutputs (fieldNodeMaybe "outputs" node)
  maybe (pure ()) emitAsmClobbersPop (fieldNodeMaybe "clobbers" node)

emitAsmClobbersPush :: Node -> TacM ()
emitAsmClobbersPush node =
  forM_ (childNodes node) $ \register ->
    emit IPush [("target", registerPlace register)]

emitAsmClobbersPop :: Node -> TacM ()
emitAsmClobbersPop node =
  forM_ (reverse (childNodes node)) $ \register ->
    emit IPop [("target", registerPlace register)]

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
  labels <- loopLabels <$> State.get
  case labels of
    (_, endLabel) : _ -> emit IJmp [("target", endLabel)]
    [] -> throw "break statement must be inside a loop"

emitContinue :: TacM ()
emitContinue = do
  labels <- loopLabels <$> State.get
  case labels of
    (updateLabel, _) : _ -> emit IJmp [("target", updateLabel)]
    [] -> throw "continue statement must be inside a loop"

emitExpression :: Node -> TacM Place
emitExpression node
  | nodeIs NodeExpression node =
      case childNodes node of
        [] -> throw "simple TAC empty expression"
        children -> emitExpressionChildren children
  | otherwise = emitAssignmentExpression node

emitExpressionChildren :: [Node] -> TacM Place
emitExpressionChildren [] = throw "simple TAC empty expression"
emitExpressionChildren [child] = emitAssignmentExpression child
emitExpressionChildren (child : children) =
  emitAssignmentExpression child >> emitExpressionChildren children

emitAssignmentExpression :: Node -> TacM Place
emitAssignmentExpression node
  | nodeIs NodeAssignment node = do
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

emitCompoundAssignment :: Text -> Place -> Place -> TacM Place
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
    void (emitMove lhsPlace lhs)
  pure lhsPlace

emitTernaryExpression :: Node -> TacM Place
emitTernaryExpression node
  | nodeIs NodeTernary node = do
      falseLabel <- nextLabel
      trueLabel <- nextLabel
      endLabel <- nextLabel
      condition <- fieldNode "condition" node
      emitBoolControlFlow condition trueLabel falseLabel
      emit ILabel [("target", trueLabel)]
      truePlace <- fieldNode "true_case" node >>= emitAssignmentExpression
      result <- nextTemp
      _ <- emitMove truePlace result
      emit IJmp [("target", endLabel)]
      emit ILabel [("target", falseLabel)]
      falsePlace <- fieldNode "false_case" node >>= emitBoolRValue
      _ <- emitMove falsePlace result
      emit ILabel [("target", endLabel)]
      pure result
  | otherwise = emitBoolRValue node

emitInclusiveOrExpression :: Node -> TacM Place
emitInclusiveOrExpression node
  | nodeIs NodeInclusiveOrExpression node = do
      firstNode <- childNodeAt 0 (nodeChildren node)
      firstPlace <- emitInclusiveXorExpression firstNode >>= loadOperandIntoRegister
      emitFlatInfix firstPlace (drop 1 (childNodes node)) emitInclusiveXorExpression IOr
  | otherwise = emitInclusiveXorExpression node

emitInclusiveXorExpression :: Node -> TacM Place
emitInclusiveXorExpression node
  | nodeIs NodeInclusiveXorExpression node = do
      firstNode <- childNodeAt 0 (nodeChildren node)
      firstPlace <- emitInclusiveAndExpression firstNode >>= loadOperandIntoRegister
      emitFlatInfix firstPlace (drop 1 (childNodes node)) emitInclusiveAndExpression IXor
  | otherwise = emitInclusiveAndExpression node

emitInclusiveAndExpression :: Node -> TacM Place
emitInclusiveAndExpression node
  | nodeIs NodeInclusiveAndExpression node = do
      firstNode <- childNodeAt 0 (nodeChildren node)
      firstPlace <- emitEqualityValue firstNode >>= loadOperandIntoRegister
      emitFlatInfix firstPlace (drop 1 (childNodes node)) emitEqualityValue IAnd
  | otherwise = emitEqualityValue node

emitEqualityValue :: Node -> TacM Place
emitEqualityValue node
  | nodeIs NodeEqualityExpression node =
      materializeBool $ \trueLabel falseLabel ->
        emitEqualityControl node trueLabel falseLabel
  | otherwise = emitRelationalValue node

emitRelationalValue :: Node -> TacM Place
emitRelationalValue node
  | nodeIs NodeRelationalExpression node =
      materializeBool $ \trueLabel falseLabel ->
        emitRelationalControl node trueLabel falseLabel
  | otherwise = emitShiftExpression node

emitShiftExpression :: Node -> TacM Place
emitShiftExpression node
  | nodeIs NodeShiftExpression node = do
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
  emit ILabel [("target", trueLabel)]
  emit IMov [("source", Place Immediate "1"), ("dest", result)]
  emit IJmp [("target", endLabel)]
  emit ILabel [("target", falseLabel)]
  emit IMov [("source", Place Immediate "0"), ("dest", result)]
  emit ILabel [("target", endLabel)]
  pure result

emitBoolControlFlow :: Node -> Place -> Place -> TacM ()
emitBoolControlFlow node trueLabel falseLabel =
  case nodeKind node of
    NodeEqualityExpression -> emitEqualityControl node trueLabel falseLabel
    NodeRelationalExpression -> emitRelationalControl node trueLabel falseLabel
    NodeLogicalAndExpression -> emitLogicalAndControl node trueLabel falseLabel
    NodeLogicalOrExpression -> emitLogicalOrControl node trueLabel falseLabel
    _ -> do
      value <- emitBoolRValue node >>= loadOperandIntoReadOnlyRegister
      emit ICmp [("first", value), ("second", Place Immediate "0")]
      emit IJe [("target", falseLabel)]
      emit IJmp [("target", trueLabel)]

emitLogicalAndControl :: Node -> Place -> Place -> TacM ()
emitLogicalAndControl node trueLabel falseLabel
  | nodeIs NodeLogicalAndExpression node =
      case childNodes node of
        [] -> throw "malformed logical and expression"
        children -> emitAndChain children
  | otherwise = emitBoolControlFlow node trueLabel falseLabel
  where
    emitAndChain [child] = emitBoolControlFlow child trueLabel falseLabel
    emitAndChain (child : rest) = do
      tempTrue <- nextLabel
      emitBoolControlFlow child tempTrue falseLabel
      emit ILabel [("target", tempTrue)]
      emitAndChain rest
    emitAndChain [] = throw "malformed logical and expression"

emitLogicalOrControl :: Node -> Place -> Place -> TacM ()
emitLogicalOrControl node trueLabel falseLabel
  | nodeIs NodeLogicalOrExpression node =
      case childNodes node of
        [] -> throw "malformed logical or expression"
        children -> emitOrChain children
  | otherwise = emitLogicalAndControl node trueLabel falseLabel
  where
    emitOrChain [child] = emitBoolControlFlow child trueLabel falseLabel
    emitOrChain (child : rest) = do
      tempFalse <- nextLabel
      emitBoolControlFlow child trueLabel tempFalse
      emit ILabel [("target", tempFalse)]
      emitOrChain rest
    emitOrChain [] = throw "malformed logical or expression"

emitEqualityControl :: Node -> Place -> Place -> TacM ()
emitEqualityControl node trueLabel falseLabel
  | nodeIs NodeEqualityExpression node =
      emitComparisonControl comparisonJump node trueLabel falseLabel
  | otherwise = emitRelationalControl node trueLabel falseLabel

emitRelationalControl :: Node -> Place -> Place -> TacM ()
emitRelationalControl node trueLabel falseLabel
  | nodeIs NodeRelationalExpression node =
      emitComparisonControl comparisonJump node trueLabel falseLabel
  | otherwise = do
      value <- emitShiftExpression node
      emitConditionalResultJump value trueLabel falseLabel

emitComparisonControl :: (Text -> CType -> CType -> TacM InstrType) -> Node -> Place -> Place -> TacM ()
emitComparisonControl jumpFor node trueLabel falseLabel = do
  let children = nodeChildren node
  firstNode <- childNodeAt 0 children
  firstTy <- inferExpressionType firstNode
  tempPlace <- emitBoolRValue firstNode >>= loadOperandIntoRegister
  groups <- comparisonGroups (drop 1 children)
  case groups of
    [] -> emitConditionalResultJump tempPlace trueLabel falseLabel
    _ -> do
      case splitLast groups of
        Nothing -> throw "malformed comparison expression"
        Just (intermediate, finalGroup) -> do
          lhsTy <- foldComparisonIntermediates jumpFor tempPlace firstTy intermediate
          let (op, rhs) = finalGroup
          rhsTy <- inferExpressionType rhs
          nextReg <- emitBoolRValue rhs >>= loadOperandIntoReadOnlyRegister
          jumpType <- jumpFor op lhsTy rhsTy
          emit ICmp [("first", tempPlace), ("second", nextReg)]
          emit jumpType [("target", trueLabel)]
          emit IJmp [("target", falseLabel)]

foldComparisonIntermediates :: (Text -> CType -> CType -> TacM InstrType) -> Place -> CType -> [(Text, Node)] -> TacM CType
foldComparisonIntermediates jumpFor tempPlace = foldM step
  where
    step lhsTy (op, rhs) = do
      rhsTy <- inferExpressionType rhs
      jumpType <- jumpFor op lhsTy rhsTy
      nextReg <- emitBoolRValue rhs >>= loadOperandIntoReadOnlyRegister
      emitConditionalEvaluation tempPlace nextReg tempPlace jumpType
      pure (base "INT")

emitConditionalEvaluation :: Place -> Place -> Place -> InstrType -> TacM ()
emitConditionalEvaluation first second result jumpType = do
  trueLabel <- nextLabel
  endLabel <- nextLabel
  emit ICmp [("first", first), ("second", second)]
  emit jumpType [("target", trueLabel)]
  _ <- emitMove (Place Immediate "0") result
  emit IJmp [("target", endLabel)]
  emit ILabel [("target", trueLabel)]
  _ <- emitMove (Place Immediate "1") result
  emit ILabel [("target", endLabel)]

emitConditionalResultJump :: Place -> Place -> Place -> TacM ()
emitConditionalResultJump result trueLabel falseLabel = do
  checked <- loadOperandIntoReadOnlyRegister result
  emit ICmp [("first", checked), ("second", Place Immediate "0")]
  emit IJe [("target", falseLabel)]
  emit IJmp [("target", trueLabel)]

emitFlatInfix :: Place -> [Node] -> (Node -> TacM Place) -> InstrType -> TacM Place
emitFlatInfix acc [] _ _ = pure acc
emitFlatInfix acc (rhs : rest) emitChild op = do
  rhsPlace <- emitChild rhs >>= loadOperandIntoRegister
  emit op [("source", rhsPlace), ("dest", acc)]
  emitFlatInfix acc rest emitChild op

emitSumExpression :: Node -> TacM Place
emitSumExpression node
  | nodeIs NodeSumExpression node = do
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
    if isRegisterRValue rhsPlace0 || placeKind rhsPlace0 == Immediate
      then pure rhsPlace0
      else loadOperandIntoRegister rhsPlace0
  case tokenName token of
    "+" -> emit IAdd [("source", rhsPlace), ("dest", acc)]
    "-" -> emit ISub [("source", rhsPlace), ("dest", acc)]
    op -> throw ("simple TAC unsupported sum op: " <> op)
  emitSumRest acc rest
emitSumRest _ _ = throw "malformed sum expression"

emitTerm :: Node -> TacM Place
emitTerm node
  | nodeIs NodeMultiplicativeExpression node = do
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
      emit IMull [("source", rhsPlace), ("dest", acc)]
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
  if placeKind divisor0 == Immediate
    then emitFixedPointDivision dividend divisor0
    else do
      divisor <-
        if isRegisterRValue divisor0
          then pure divisor0
          else loadOperandIntoRegister divisor0
      emitLongDivision dividend divisor

emitFixedPointDivision :: Place -> Place -> TacM (Place, Place)
emitFixedPointDivision dividend divisor = do
  let divisorValue = placeInteger divisor
  when (divisorValue == 0) $
    throw "division by constant zero"
  if divisorValue == 1
    then pure (dividend, Place Immediate "0")
    else emitReciprocalDivision dividend divisor divisorValue

emitReciprocalDivision :: Place -> Place -> Integer -> TacM (Place, Place)
emitReciprocalDivision dividend divisor divisorValue = do
  let fixedPointFactor = (2 ^ (16 :: Int)) `div` divisorValue
  quotient <- nextTemp
  remainder <- nextTemp
  endLabel <- nextLabel
  divisorForCompare <- loadOperandIntoRegister divisor
  emit IMulh [("source", dividend), ("dest", quotient), ("third", Place Immediate (Text.pack (show fixedPointFactor)))]
  emit IMull3 [("source", quotient), ("dest", remainder), ("third", divisor)]
  emit ISub3 [("source", dividend), ("dest", remainder), ("third", remainder)]
  emit ICmp [("first", remainder), ("second", divisorForCompare)]
  emit IJb [("target", endLabel)]
  emit IAdd [("source", Place Immediate "1"), ("dest", quotient)]
  emit ISub [("source", divisor), ("dest", remainder)]
  emit ILabel [("target", endLabel)]
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
  emit IMov [("source", Place Immediate "0"), ("dest", quotient)]
  emit IMov [("source", Place Immediate "0"), ("dest", remainder)]
  emit IMov [("source", Place Immediate "15"), ("dest", bitIndex)]
  emit ILabel [("target", loopLabel)]
  emit ICmp [("first", bitIndex), ("second", Place Immediate "0")]
  emit IJl [("target", endLabel)]
  emit IShl [("source", Place Immediate "1"), ("dest", remainder)]
  emit IShr3 [("source", dividend), ("dest", temp), ("third", bitIndex)]
  emit IAnd [("source", Place Immediate "1"), ("dest", temp)]
  emit IOr [("source", temp), ("dest", remainder)]
  emit ICmp [("first", remainder), ("second", divisor)]
  emit IJb [("target", remainderLessLabel)]
  emit ISub [("source", divisor), ("dest", remainder)]
  emit IMov [("source", Place Immediate "1"), ("dest", temp)]
  emit IShl [("source", bitIndex), ("dest", temp)]
  emit IOr [("source", temp), ("dest", quotient)]
  emit ILabel [("target", remainderLessLabel)]
  emit ISub [("source", Place Immediate "1"), ("dest", bitIndex)]
  emit IJmp [("target", loopLabel)]
  emit ILabel [("target", endLabel)]
  pure (quotient, remainder)

emitShiftRest :: Place -> Place -> [NodeChild] -> TacM Place
emitShiftRest acc _ [] = pure acc
emitShiftRest acc luaNextReg (ChildToken token : ChildNode rhs : rest) = do
  rhsPlace0 <- emitSumExpression rhs
  rhsPlace <-
    if isRegisterRValue rhsPlace0 || placeKind rhsPlace0 == Immediate
      then pure rhsPlace0
      else emitMove rhsPlace0 luaNextReg
  case tokenName token of
    "<<" -> emit IShl [("source", rhsPlace), ("dest", acc)]
    ">>" -> emit IShr [("source", rhsPlace), ("dest", acc)]
    op -> throw ("simple TAC unsupported shift op: " <> op)
  emitShiftRest acc rhsPlace rest
emitShiftRest _ _ _ = throw "malformed shift expression"

emitCastExpression :: Node -> TacM Place
emitCastExpression node
  | nodeIs NodeCastExpression node = fieldNode "cast_expression" node >>= emitCastExpression
  | otherwise = emitUnaryExpression node

emitUnaryExpression :: Node -> TacM Place
emitUnaryExpression node
  | nodeIs NodeUnaryExpression node = do
      op <- fieldString "operator" node
      child <- fieldNode "child" node
      case op of
        "++" -> emitPrefixMutation child IAdd
        "--" -> emitPrefixMutation child ISub
        "SIZEOF" -> pure (Place Immediate "1")
        "&" -> emitCastExpression child >>= emitAddressOf
        "*" -> emitCastExpression child >>= emitDereference
        "+" -> emitCastExpression child
        "-" -> do
          childPlace <- emitCastExpression child
          if placeKind childPlace == Immediate
            then pure (Place Immediate (Text.pack (show (65536 - placeInteger childPlace))))
            else do
              result <- loadOperandIntoRegister childPlace
              emit IXor [("source", Place Immediate "65535"), ("dest", result)]
              emit IAdd [("source", Place Immediate "1"), ("dest", result)]
              pure result
        "~" -> do
          childPlace <- emitCastExpression child >>= loadOperandIntoRegister
          emit IXor [("source", Place Immediate "65535"), ("dest", childPlace)]
          pure childPlace
        "!" -> do
          childPlace <- emitCastExpression child >>= loadOperandIntoRegister
          emitConditionalEvaluation childPlace (Place Immediate "0") childPlace IJe
          pure childPlace
        _ -> throw ("simple TAC does not support unary op: " <> op)
  | otherwise = emitPostfixExpression node

emitPrefixMutation :: Node -> InstrType -> TacM Place
emitPrefixMutation child op = do
  childPlace <- emitUnaryExpression child
  nextReg <-
    if isRValue childPlace
      then pure childPlace
      else loadOperandIntoRegister childPlace
  emit op [("source", Place Immediate "1"), ("dest", nextReg)]
  _ <- emitMove nextReg childPlace
  pure nextReg

emitAddressOf :: Place -> TacM Place
emitAddressOf place = do
  target <- nextTemp
  case placeKind place of
    _ | isMemoryLValue place -> emit IGetAddress [("dest", target), ("target", place)] >> pure target
    PointerRegister -> emit IMov [("source", place), ("dest", target)] >> pure target
    Temporary -> pure place
    _ -> throw ("simple TAC cannot take address of place type: " <> placeKindCode (placeKind place))

emitDereference :: Place -> TacM Place
emitDereference place = do
  pointerRegister <- nextPr
  case placeKind place of
    _ | not (isRValue place) -> emit ILd [("source", place), ("dest", pointerRegister)] >> pure pointerRegister
    VirtualRegister -> pointerFromRegister place
    Temporary -> pointerFromRegister place
    Immediate -> emit IMov [("source", place), ("dest", pointerRegister)] >> pure pointerRegister
    _ -> throw ("simple TAC cannot dereference place type: " <> placeKindCode (placeKind place))
  where
    pointerFromRegister register = do
      loaded <- loadOperandIntoRegister register
      pure (loaded {placeKind = PointerRegister})

emitPostfixExpression :: Node -> TacM Place
emitPostfixExpression node
  | nodeIs NodePostfixExpression node = do
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
            || (not (null (contextDimensions context)) && placeKind place /= PointerRegister)
            || (null (contextDimensions context) && contextPointerLevel context == 0 && placeKind place == Temporary)
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
        emit ICall [("target", target)]
        unless isStandard $
          emit IAdd [("source", Place Immediate (Text.pack (show (length args)))), ("dest", Place Register "stack_pointer")]
        if callReturnsValue
          then do
            result <- nextTemp
            _ <- emitMove (Place Register "return_reg") result
            pure (result, emptyPostfixContext)
          else pure (target, emptyPostfixContext)
      "++" -> do
        mutated <- emitPostfixMutation place IAdd
        pure (mutated, emptyPostfixContext)
      "--" -> do
        mutated <- emitPostfixMutation place ISub
        pure (mutated, emptyPostfixContext)
      "." -> do
        (memberPlace, memberTy) <- emitMemberAccess place context op
        pure (memberPlace, contextForType memberTy)
      "->" -> do
        dereferenced <- emitDereference place
        (memberPlace, memberTy) <- emitMemberAccess dereferenced (context {contextType = dereferenceType <$> contextType context}) op
        pure (memberPlace, contextForType memberTy)
      other -> throw ("simple TAC does not support postfix op: " <> other)
  emitPostfixOps nextPlace nextContext rest

emitMemberAccess :: Place -> PostfixContext -> PostfixOp -> TacM (Place, CType)
emitMemberAccess place context op = do
  memberNode <- postfixNode op
  memberName' <- fieldString "id" memberNode
  structTy <- maybe (throw ("simple TAC cannot infer type for member access: " <> memberName')) pure (contextType context)
  member <- maybe (throw ("missing member: " <> memberName')) pure (lookupMemberIn structTy memberName')
  offset <- maybe (throw ("missing member offset: " <> memberName')) pure (memberOffset member)
  memberPlace <- emitOffsetLValue (Place Immediate (Text.pack (show offset))) place 1
  pure (memberPlace, memberType member)

lookupMemberIn :: CType -> Text -> Maybe Member
lookupMemberIn ty memberName' =
  case ty of
    StructType _ members' -> memberByName memberName' members'
    UnionType _ members' -> memberByName memberName' members'
    _ -> Nothing

emitPointerIndexing :: Place -> Place -> Integer -> TacM Place
emitPointerIndexing pointerPlace indexer size
  | placeKind pointerPlace == VirtualRegister && (placeKind indexer == Immediate || (placeKind indexer == VirtualRegister && size == 1)) = do
      pointerRegister <- nextPr
      emit IAdd3 [("source", pointerPlace), ("dest", pointerRegister), ("offset", scaledImmediate size indexer)]
      pure pointerRegister
  | otherwise = do
      dereferenced <- emitDereference pointerPlace
      emitIndexing dereferenced indexer size

emitIndexing :: Place -> Place -> Integer -> TacM Place
emitIndexing indexed indexer size
  | isRegisterRValue indexed && isRValue indexer = do
      _ <- emitAbstractAdd indexer indexed size
      pure (indexed {placeKind = PointerRegister})
  | isLValue indexed =
      if placeKind indexer == Immediate
        then emitOffsetLValue indexer indexed size
        else do
          address <- emitAddressOf indexed
          emitIndexing address indexer size
  | isLValue indexer = do
      dereferenced <- emitDereference indexer
      emitIndexing indexed (dereferenced {placeKind = Temporary}) size
  | otherwise = throw ("simple TAC cannot index " <> placeKindCode (placeKind indexed) <> " with " <> placeKindCode (placeKind indexer))

emitAbstractAdd :: Place -> Place -> Integer -> TacM Place
emitAbstractAdd source dest size
  | not (isRegisterRValue dest || placeKind dest == PointerRegister) = throw "simple TAC indexed destination must be a register"
  | not (isRegisterRValue source || placeKind source == Immediate) = throw "simple TAC indexed source must be an rvalue"
  | placeKind source == Immediate = emit IAdd [("source", scaledImmediate size source), ("dest", dest)] >> pure dest
  | size > 1 = do
      source' <-
        if placeKind source == VirtualRegister
          then loadOperandIntoRegister source
          else pure source
      emit IMull [("source", Place Immediate (Text.pack (show size))), ("dest", source')]
      emit IAdd [("source", source'), ("dest", dest)]
      pure dest
  | otherwise = emit IAdd [("source", source), ("dest", dest)] >> pure dest

emitOffsetLValue :: Place -> Place -> Integer -> TacM Place
emitOffsetLValue offset place size
  | placeKind offset /= Immediate = throw "simple TAC offset must be immediate"
  | placeKind place == PointerRegister = do
      emit IAdd [("source", scaledImmediate size offset), ("dest", place)]
      pure place
  | isLValue place || placeKind place == Immediate =
      pure (place {placeValue = Text.pack (show (placeInteger place + placeInteger offset * size))})
  | otherwise = throw ("simple TAC cannot offset place type: " <> placeKindCode (placeKind place))

scaledImmediate :: Integer -> Place -> Place
scaledImmediate size place
  | placeKind place == Immediate = Place Immediate (Text.pack (show (placeInteger place * size)))
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
    if not (isRegisterRValue place) || placeKind place == Immediate
      then loadOperandIntoRegister place
      else pure place
  emit IPush [("target", pushed)]

postfixArgumentNodes :: PostfixOp -> TacM [Node]
postfixArgumentNodes op =
  case postfixValue op of
    Just (NodeRef node)
      | nodeIs NodeArgumentList node -> pure (childNodes node)
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
registerPlace node = Place Register (fieldStringDefault "id" "" node)

emitPostfixMutation :: Place -> InstrType -> TacM Place
emitPostfixMutation place op = do
  nextReg <- loadOperandIntoRegister place
  if isRegisterRValue place
    then do
      emit op [("source", Place Immediate "1"), ("dest", place)]
      pure nextReg
    else do
      tempReg <- nextTemp
      _ <- emitMove nextReg tempReg
      emit op [("source", Place Immediate "1"), ("dest", nextReg)]
      _ <- emitMove nextReg place
      pure tempReg

emitPrimaryExpression :: Node -> TacM Place
emitPrimaryExpression node =
  case nodeKind node of
    NodeInt -> pure (Place Immediate (Text.pack (show (fieldIntDefault "value" 0 node))))
    NodeCharacter -> pure (Place Immediate (characterValue node))
    NodeIdentifier -> lookupLocal (fieldStringDefault "value" (identifierValue node) node)
    NodeExpression -> emitExpression node
    NodeStringLiteral -> do
      stringPlace <- nextGlobalSize (stringLiteralLength node)
      emitAddressOf stringPlace
    _ -> throw ("simple TAC does not support primary: " <> nodeName node)

emitMove :: Place -> Place -> TacM Place
emitMove source dest
  | source == dest = pure source
  | placeKind dest == Immediate = throw "destination cannot be immediate"
  | placeKind source == Immediate && isDestMem = do
      loaded <- loadOperandIntoRegister source
      emitMove loaded dest
  | not isSourceMem && not isDestMem = emit IMov [("source", source), ("dest", dest)] >> pure source
  | not isSourceMem && isDestMem = emit ISt [("source", source), ("dest", dest)] >> pure source
  | isSourceMem && not isDestMem = emit ILd [("source", source), ("dest", dest)] >> pure source
  | otherwise = do
      loaded <- loadOperandIntoRegister source
      emitMove loaded dest
  where
    isSourceMem = not (isRValue source)
    isDestMem = not (isRValue dest)

loadOperandIntoRegister :: Place -> TacM Place
loadOperandIntoRegister place
  | placeKind place == Temporary = pure place
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
  st <- State.get
  let offset = localOffset st
  State.modify (\s -> s {localOffset = offset + size})
  pure (Place Local (Text.pack (show offset)))

nextGlobalSize :: Integer -> TacM Place
nextGlobalSize size = do
  st <- State.get
  let offset = globalOffset st
  State.modify (\s -> s {globalOffset = offset + size})
  pure (Place Global (Text.pack (show offset)))

nextTemp :: TacM Place
nextTemp = nextTempLike Temporary

nextPr :: TacM Place
nextPr = nextTempLike PointerRegister

nextVR :: TacM Place
nextVR = nextTempLike VirtualRegister

nextTempLike :: PlaceKind -> TacM Place
nextTempLike kind = do
  st <- State.get
  let next = tempCounter st + 1
  State.modify (\s -> s {tempCounter = next})
  pure (Place kind (Text.pack (show next)))

nextLabel :: TacM Place
nextLabel = do
  st <- State.get
  let next = labelCounter st
  State.modify (\s -> s {labelCounter = next + 1})
  pure (Place Immediate (".label_" <> Text.pack (show next)))

standardArgumentRegister :: Int -> Place
standardArgumentRegister index =
  case index of
    1 -> Place Register "r22"
    2 -> Place Register "r23"
    3 -> Place Register "r24"
    4 -> Place Register "r25"
    _ -> Place Register ("r" <> Text.pack (show (21 + index)))

isStandardFunctionPlace :: Place -> Bool
isStandardFunctionPlace place =
  placeKind place == Immediate && Map.member (placeValue place) defaultStandardFunctionTargetMap

standardFunctionReturnsValue :: Place -> Bool
standardFunctionReturnsValue place =
  Map.findWithDefault True (placeValue place) defaultStandardFunctionTargetMap

userFunctionReturnsValue :: Place -> TacM Bool
userFunctionReturnsValue place =
  Map.findWithDefault True (placeValue place) . functionReturns <$> State.get

pushLoopLabels :: Place -> Place -> TacM ()
pushLoopLabels updateLabel endLabel =
  State.modify (\st -> st {loopLabels = (updateLabel, endLabel) : loopLabels st})

popLoopLabels :: TacM ()
popLoopLabels =
  State.modify $ \st ->
    st
      { loopLabels =
          case loopLabels st of
            [] -> []
            _ : rest -> rest
      }

pushCaseLabels :: TacM ()
pushCaseLabels =
  State.modify (\st -> st {caseLabels = CaseContext [] Nothing : caseLabels st})

addCaseLabel :: Place -> Place -> TacM ()
addCaseLabel value target =
  State.modify $ \st ->
    st
      { caseLabels =
          case caseLabels st of
            context : rest -> context {caseEntries = (value, target) : caseEntries context} : rest
            [] -> []
      }

setDefaultCaseLabel :: Place -> TacM ()
setDefaultCaseLabel target =
  State.modify $ \st ->
    st
      { caseLabels =
          case caseLabels st of
            context : rest -> context {caseDefault = Just target} : rest
            [] -> []
      }

popCaseLabels :: TacM CaseContext
popCaseLabels = do
  labels <- caseLabels <$> State.get
  case labels of
    context : rest -> do
      State.modify (\st -> st {caseLabels = rest})
      pure context {caseEntries = reverse (caseEntries context)}
    [] -> throw "case label stack underflow"

emit :: InstrType -> [(InstrFieldName, Place)] -> TacM ()
emit ty fields =
  State.modify (\st -> st {instructions = instructions st |> Instr ty fields []})

emitString :: InstrType -> [(InstrFieldName, Text)] -> TacM ()
emitString ty fields =
  State.modify (\st -> st {instructions = instructions st |> Instr ty [] fields})

insertInstructionsAt :: Int -> [Instr] -> TacM ()
insertInstructionsAt index inserted =
  State.modify $ \st ->
    let (prefix, suffix) = Seq.splitAt index (instructions st)
     in st {instructions = prefix >< Seq.fromList inserted >< suffix}

renderTac :: TacProgram -> [String]
renderTac program =
  concatMap renderMethodOutput (tacProgramMethods program)
    <> ["LOCAL_SIZE\t" <> show (mainLocalSize program)]

renderMethodOutput :: MethodOutput -> [String]
renderMethodOutput output =
  ["METHOD\t" <> Text.unpack (methodOutputName output)]
    <> zipWith renderInstr [(1 :: Int) ..] (methodOutputInstructions output)

mainLocalSize :: TacProgram -> Integer
mainLocalSize program =
  case [methodOutputLocalSize output | output <- tacProgramMethods program, methodOutputName output == "main"] of
    value : _ -> value
    [] -> 0

renderInstr :: Int -> Instr -> String
renderInstr index instr =
  show index <> "\t" <> Text.unpack (instrMnemonic (instrType instr)) <> concatMap renderField (instrFields instr) <> concatMap renderStringField (instrStringFields instr)

renderField :: (InstrFieldName, Place) -> String
renderField (name, place) = "\t" <> Text.unpack (instrFieldNameText name) <> "=" <> Text.unpack (renderPlace place)

renderStringField :: (InstrFieldName, Text) -> String
renderStringField (name, value) = "\t" <> Text.unpack (instrFieldNameText name) <> "=" <> Text.unpack value

lookupLocal :: Text -> TacM Place
lookupLocal name = do
  found <- lookupLocalInfo name
  case found of
    Just info ->
      if null (localInfoDimensions info) || localInfoPointerToArray info
        then pure (localInfoPlace info)
        else emitAddressOf (localInfoPlace info)
    Nothing -> do
      enumConstant <- Map.lookup name . enumConstants <$> State.get
      case enumConstant of
        Just place -> pure place
        Nothing -> do
          functionPlace <- Map.lookup name . functionPlaces <$> State.get
          maybe (throw ("missing local: " <> name)) pure functionPlace

lookupLocalInfo :: Text -> TacM (Maybe LocalInfo)
lookupLocalInfo name = do
  local <- Map.lookup name . locals <$> State.get
  case local of
    Just info -> pure (Just info)
    Nothing -> Map.lookup name . globals <$> State.get

inferExpressionType :: Node -> TacM CType
inferExpressionType node =
  case nodeKind node of
    NodeExpression -> inferLastChild node
    NodeAssignment -> fieldNode "lhs" node >>= inferExpressionType
    NodeTernary -> do
      trueTy <- fieldNode "true_case" node >>= inferExpressionType
      falseTy <- fieldNode "false_case" node >>= inferExpressionType
      pure (fromMaybe trueTy (usualArithmeticConversion trueTy falseTy))
    NodeLogicalOrExpression -> pure (base "INT")
    NodeLogicalAndExpression -> pure (base "INT")
    NodeEqualityExpression -> pure (base "INT")
    NodeRelationalExpression -> pure (base "INT")
    NodeInclusiveOrExpression -> inferArithmeticFold node
    NodeInclusiveXorExpression -> inferArithmeticFold node
    NodeInclusiveAndExpression -> inferArithmeticFold node
    NodeShiftExpression -> inferShiftType node
    NodeSumExpression -> inferArithmeticFold node
    NodeMultiplicativeExpression -> inferArithmeticFold node
    NodeCastExpression -> inferCastType node
    NodeUnaryExpression -> inferUnaryType node
    NodePostfixExpression -> inferPostfixType node
    NodePrimaryExpression -> inferLastChild node
    NodeInt -> pure (baseWithSigned "INT" (not (hasBoolField "is_unsigned" node)))
    NodeCharacter -> pure (base "CHAR")
    NodeStringLiteral -> pure (array (stringLiteralLength node) (base "CHAR"))
    NodeIdentifier -> inferIdentifierType node
    _ -> pure (base "INT")

inferLastChild :: Node -> TacM CType
inferLastChild node =
  maybe (pure (base "VOID")) inferExpressionType (lastMaybe (childNodes node))

lastMaybe :: [a] -> Maybe a
lastMaybe = foldl' (\_ value -> Just value) Nothing

inferIdentifierType :: Node -> TacM CType
inferIdentifierType node = do
  let name = fieldStringDefault "value" (identifierValue node) node
  found <- lookupLocalInfo name
  case found of
    Just info -> pure (decayExpressionType (localInfoType info))
    Nothing -> do
      enumConstant <- Map.lookup name . enumConstants <$> State.get
      if isJust enumConstant
        then pure (base "INT")
        else do
          functionPlace <- Map.lookup name . functionPlaces <$> State.get
          case functionPlace of
            Just _ -> pure (pointer (function (base "INT") []))
            Nothing -> pure (base "INT")

inferArithmeticFold :: Node -> TacM CType
inferArithmeticFold node =
  case childNodes node of
    [] -> pure (base "INT")
    firstChild : rest -> do
      firstTy <- inferExpressionType firstChild
      foldM combine firstTy rest
  where
    combine lhs rhsNode = do
      rhs <- inferExpressionType rhsNode
      pure (fromMaybe (base "INT") (usualArithmeticConversion lhs rhs))

inferShiftType :: Node -> TacM CType
inferShiftType node =
  case childNodes node of
    firstChild : _ -> do
      lhs <- inferExpressionType firstChild
      pure (fromMaybe lhs (integerPromotion lhs))
    [] -> pure (base "INT")

inferCastType :: Node -> TacM CType
inferCastType node
  | hasNodeField "type_specifier" node = do
      resolveTypeName =<< fieldNode "type_specifier" node
  | otherwise = maybe (pure (base "INT")) inferExpressionType (fieldNodeMaybe "cast_expression" node)

inferUnaryType :: Node -> TacM CType
inferUnaryType node = do
  op <- fieldString "operator" node
  child <- fieldNode "child" node
  childTy <- inferExpressionType child
  pure $
    case op of
      "SIZEOF" -> base "INT"
      "&" -> pointer childTy
      "*" -> dereferenceType childTy
      "!" -> base "INT"
      "~" -> fromMaybe (base "INT") (integerPromotion childTy)
      "+" -> fromMaybe childTy (integerPromotion childTy)
      "-" -> fromMaybe childTy (integerPromotion childTy)
      _ -> childTy

inferPostfixType :: Node -> TacM CType
inferPostfixType node = do
  primary <- fieldNode "primary_expression" node
  primaryTy <- inferExpressionType primary
  foldM inferPostfixOp primaryTy (postfixOps node)

inferPostfixOp :: CType -> PostfixOp -> TacM CType
inferPostfixOp ty op =
  case postfixType op of
    "[" -> pure (decayExpressionType (dereferenceType ty))
    "(" -> pure (callReturnType ty)
    "++" -> pure ty
    "--" -> pure ty
    "." -> pure (memberAccessType ty op)
    "->" -> pure (memberAccessType (dereferenceType ty) op)
    _ -> pure ty

callReturnType :: CType -> CType
callReturnType ty =
  case ty of
    FunctionType ret _ _ -> ret
    PointerType (FunctionType ret _ _) -> ret
    _ -> ty

memberAccessType :: CType -> PostfixOp -> CType
memberAccessType ty op =
  let memberName' = maybe "" identifierValue (postfixNodeValue op)
      memberTy =
        case ty of
          StructType _ members' -> memberType <$> memberByName memberName' members'
          UnionType _ members' -> memberType <$> memberByName memberName' members'
          _ -> Nothing
   in maybe ty decayExpressionType memberTy

postfixNodeValue :: PostfixOp -> Maybe Node
postfixNodeValue op =
  case postfixValue op of
    Just (NodeRef node) -> Just node
    _ -> Nothing

isRValue :: Place -> Bool
isRValue place = placeKind place `elem` [Immediate, Temporary, Register, VirtualRegister]

isRegisterRValue :: Place -> Bool
isRegisterRValue place = placeKind place `elem` [Temporary, Register, VirtualRegister]

isMemoryLValue :: Place -> Bool
isMemoryLValue place = placeKind place `elem` [Local, Parameter, Global, VirtualRegister]

isLValue :: Place -> Bool
isLValue place = placeKind place `elem` [Local, Parameter, Global, PointerRegister, VirtualRegister]

childNodeAt :: Int -> [NodeChild] -> TacM Node
childNodeAt wanted children =
  case drop wanted children of
    ChildNode node : _ -> pure node
    _ -> throw "expected child node"

postfixOps :: Node -> [PostfixOp]
postfixOps node = [op | ChildPostfix op <- nodeChildren node]

emptyPostfixContext :: PostfixContext
emptyPostfixContext = PostfixContext 0 [] False Nothing

primaryPostfixContext :: Node -> TacM PostfixContext
primaryPostfixContext node =
  case nodeKind node of
    NodeIdentifier -> do
      info <- lookupLocalInfo (fieldStringDefault "value" (identifierValue node) node)
      pure $
        maybe
          emptyPostfixContext
          (\local -> PostfixContext (localInfoPointerLevel local) (localInfoDimensions local) (localInfoPointerToArray local) (Just (localInfoType local)))
          info
    _ -> pure emptyPostfixContext

contextForType :: CType -> PostfixContext
contextForType ty =
  PostfixContext
    { contextPointerLevel = pointerLevelOf ty
    , contextDimensions = dimensionsOf ty
    , contextPointerToArray = False
    , contextType = Just ty
    }

indexingElementSize :: PostfixContext -> Integer
indexingElementSize context =
  maybe fallback sizeof (contextType context >>= dereferenceTypeMaybe)
  where
    fallback =
      if contextPointerToArray context
        then declaratorSlotSize (contextDimensions context)
        else
          case contextDimensions context of
            _ : rest -> declaratorSlotSize rest
            [] -> 1

afterIndexContext :: PostfixContext -> PostfixContext
afterIndexContext context =
  if contextPointerToArray context
    then context {contextPointerLevel = 0, contextPointerToArray = False, contextType = contextType context >>= dereferenceTypeMaybe}
    else
      case contextDimensions context of
        _ : rest -> context {contextDimensions = rest, contextType = contextType context >>= dereferenceTypeMaybe}
        [] -> context {contextPointerLevel = max 0 (contextPointerLevel context - 1), contextType = contextType context >>= dereferenceTypeMaybe}

pointerLevelOf :: CType -> Integer
pointerLevelOf ty =
  case ty of
    PointerType target -> 1 + pointerLevelOf target
    _ -> 0

dimensionsOf :: CType -> [Integer]
dimensionsOf ty =
  case ty of
    ArrayType len target -> len : dimensionsOf target
    _ -> []

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

objectSlotSize :: [Integer] -> CType -> Integer
objectSlotSize dimensions ty =
  sizeof (withArrayDimensions dimensions ty)

withArrayDimensions :: [Integer] -> CType -> CType
withArrayDimensions dimensions ty =
  case (dimensions, ty) of
    (dimension : rest, ArrayType _ target) -> ArrayType (normalizeDimension dimension) (withArrayDimensions rest target)
    _ -> ty
  where
    normalizeDimension value
      | value > 0 = value
      | otherwise = 1

resolveTypeSpecifier :: Node -> TacM CType
resolveTypeSpecifier typeSpecifier = do
  case lookupField "kind" typeSpecifier of
    Just (StringList specifiers) ->
      if isBaseSpecifiers specifiers
        then pure (baseFromSpecifiers specifiers)
        else case specifiers of
          name : _ -> maybe (throw ("missing typedef: " <> name)) pure . Map.lookup name . typeTags =<< State.get
          [] -> throw "empty type specifier"
    Just (NodeRef node)
      | nodeIs NodeStructOrUnionSpecifier node -> resolveStructOrUnion node
      | nodeIs NodeEnumSpecifier node -> resolveEnum node
      | otherwise -> throw ("unexpected type specifier node: " <> nodeName node)
    _ -> throw "invalid type specifier kind"

resolveTypeName :: Node -> TacM CType
resolveTypeName node = do
  baseTy <- resolveTypeSpecifier =<< fieldNode "type_specifier" node
  declarator <- fieldNode "declarator" node
  buildAbstractDeclaratorType declarator baseTy

buildAbstractDeclaratorType :: Node -> CType -> TacM CType
buildAbstractDeclaratorType declarator baseTy = do
  let pointerTy = applyPointers (fieldIntDefault "pointer_level" 0 declarator) baseTy
  case fieldNodeMaybe "direct_abstract_declarator" declarator of
    Just direct -> buildDirectAbstractDeclaratorType direct pointerTy
    Nothing -> pure pointerTy

buildDirectAbstractDeclaratorType :: Node -> CType -> TacM CType
buildDirectAbstractDeclaratorType direct ty = do
  withFunction <-
    case fieldNodeMaybe "parameter_list" direct of
      Just params -> buildFunctionType ty params
      Nothing -> pure ty
  let dimensions = map (fieldIntDefault "value" 0) (childNodes direct)
      withArrays = foldr array withFunction dimensions
  case fieldNodeMaybe "declarator" direct of
    Just nested -> buildAbstractDeclaratorType nested withArrays
    Nothing -> pure withArrays

resolveStructOrUnion :: Node -> TacM CType
resolveStructOrUnion node = do
  let isStruct = boolFieldDefault "is_struct" False node
      typeName = maybe (if isStruct then "anon_struct" else "anon_union") identifierValue (fieldNodeMaybe "id" node)
  case fieldNodeListMaybe "declaration" node of
    Just declarations -> do
      members' <- concat <$> mapM structDeclarationMembers declarations
      let membersWithOffsets = withMemberOffsets isStruct members'
          ty = if isStruct then struct typeName membersWithOffsets else typeName `union` membersWithOffsets
      when (hasNodeField "id" node) $
        State.modify (\st -> st {typeTags = Map.insert typeName ty (typeTags st)})
      pure ty
    Nothing -> maybe (throw ("missing type tag: " <> typeName)) pure . Map.lookup typeName . typeTags =<< State.get

structDeclarationMembers :: Node -> TacM [Member]
structDeclarationMembers node = do
  typeSpecifier <- fieldNode "type_specifier" node
  memberBase <- resolveTypeSpecifier typeSpecifier
  forM (childNodes node) $ \declarator -> do
    memberTy <- buildDeclaratorType declarator memberBase
    pure Member {memberName = declaratorName declarator, memberType = memberTy, memberOffset = Nothing}

resolveEnum :: Node -> TacM CType
resolveEnum node = do
  identifier <- fieldNode "id" node
  let name = identifierValue identifier
  case fieldNodeMaybe "declaration" node of
    Just declaration -> do
      let members' = childNodes declaration
      memberNames <- mapM (fmap identifierValue . fieldNode "id") members'
      forM_ (enumMemberValues members') $ \(member, value) -> do
        memberId <- fieldNode "id" member
        State.modify (\st -> st {enumConstants = Map.insert (identifierValue memberId) (Place Immediate (Text.pack (show value))) (enumConstants st)})
      State.modify (\st -> st {typeTags = Map.insert name (enum name memberNames) (typeTags st)})
    Nothing -> pure ()
  pure (base "INT")

buildDeclaratorType :: Node -> CType -> TacM CType
buildDeclaratorType declarator baseTy = do
  direct <- fieldNode "direct_declarator" declarator
  buildDirectDeclaratorType direct (applyPointers (fieldIntDefault "pointer_level" 0 declarator) baseTy)

buildDirectDeclaratorType :: Node -> CType -> TacM CType
buildDirectDeclaratorType direct ty = do
  let withArrays = foldr array ty (fieldIntsDefault "dimensions" [] direct)
  withFunction <-
    case fieldNodeMaybe "parameter_list" direct of
      Just params -> buildFunctionType withArrays params
      Nothing -> pure withArrays
  case fieldNodeMaybe "declarator" direct of
    Just nested -> buildDeclaratorType nested withFunction
    Nothing -> pure withFunction

buildParameterListTypes :: Node -> TacM [CType]
buildParameterListTypes params =
  mapM buildParameterType (childNodes params)

buildFunctionType :: CType -> Node -> TacM CType
buildFunctionType ret params = do
  parameterTys <- buildParameterListTypes params
  pure $
    if hasBoolField "is_variadic" params
      then variadicFunction ret parameterTys
      else function ret parameterTys

buildParameterType :: Node -> TacM CType
buildParameterType parameter = do
  typeSpecifier <- fieldNode "type_specifier" parameter
  baseTy <- resolveTypeSpecifier typeSpecifier
  case fieldNodeMaybe "declarator" parameter of
    Just declarator -> decayArrayParameter <$> buildDeclaratorType declarator baseTy
    Nothing -> pure baseTy

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
      | nodeIs NodeInitializerList node -> fromIntegral (length (childNodes node))
    Just node
      | nodeIs NodeInitializer node ->
          case fieldNodeMaybe "value" node of
            Just value | nodeIs NodeStringLiteral value -> stringLiteralLength value
            _ -> 1
    _ -> 1

initializerContainsDirectString :: Maybe Node -> Bool
initializerContainsDirectString initializer =
  case initializer of
    Just node
      | nodeIs NodeInitializer node ->
          case fieldNodeMaybe "value" node of
            Just value -> nodeIs NodeStringLiteral value
            _ -> False
    _ -> False

directInitializerStringLength :: Maybe Node -> Integer
directInitializerStringLength initializer =
  case initializer of
    Just node
      | nodeIs NodeInitializer node ->
          maybe 1 stringLiteralLength (fieldNodeMaybe "value" node)
    _ -> 1

allocateNestedInitializerStrings :: Integer -> [Integer] -> Maybe Node -> TacM ()
allocateNestedInitializerStrings pointerLevel dimensions initializer =
  for_ initializer (allocateStringsInInitializer pointerLevel dimensions)

allocateStringsInInitializer :: Integer -> [Integer] -> Node -> TacM ()
allocateStringsInInitializer pointerLevel dimensions initializer
  | nodeIs NodeInitializer initializer =
      case fieldNodeMaybe "value" initializer of
        Just value
          | nodeIs NodeStringLiteral value && pointerLevel > 0 -> do
              _ <- nextGlobalSize (stringLiteralLength value)
              pure ()
        _ -> pure ()
  | nodeIs NodeInitializerList initializer =
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
    Just (StringValue value) -> fromIntegral (Text.length value + 1)
    _ -> 1

firstExpressionChild :: Node -> TacM Node
firstExpressionChild node =
  case nodeKind node of
    NodeExpression ->
      case childNodes node of
        child : _ -> pure child
        [] -> throw "empty expression"
    _ -> pure node

parameterPlaces :: Node -> TacM [(Text, LocalInfo)]
parameterPlaces declarator = do
  direct <- fieldNode "direct_declarator" declarator
  case fieldNodeMaybe "parameter_list" direct of
    Nothing -> pure []
    Just params ->
      fmap concat $
        forM (zip [(0 :: Integer) ..] (childNodes params)) $ \(index, param) ->
          case fieldNodeMaybe "declarator" param of
            Just paramDeclarator
              | declaratorName paramDeclarator /= "" -> do
                  typeSpecifier <- fieldNode "type_specifier" param
                  baseTy <- resolveTypeSpecifier typeSpecifier
                  declaredTy <- decayArrayParameter <$> buildDeclaratorType paramDeclarator baseTy
                  pure [(declaratorName paramDeclarator, LocalInfo (Place Parameter (Text.pack (show index))) (declaratorPointerLevel paramDeclarator) [] (declaratorPointerToArray paramDeclarator) declaredTy)]
            _ -> pure []

characterValue :: Node -> Text
characterValue node =
  case lookupField "value" node of
    Just (StringValue value) -> value
    Just (IntValue value) -> Text.pack (show value)
    _ -> "0"

functionDeclarationReturnsValue :: Node -> Bool
functionDeclarationReturnsValue declaration =
  case fieldNodeMaybe "specifier" declaration >>= fieldNodeMaybe "type_specifier" >>= lookupField "kind" of
    Just (StringList ["void"]) -> False
    Just (StringList ["VOID"]) -> False
    _ -> True

comparisonGroups :: [NodeChild] -> TacM [(Text, Node)]
comparisonGroups [] = pure []
comparisonGroups (ChildToken token : ChildNode rhs : rest) =
  ((tokenName token, rhs) :) <$> comparisonGroups rest
comparisonGroups _ = throw "malformed comparison expression"

splitLast :: [a] -> Maybe ([a], a)
splitLast [] = Nothing
splitLast [value] = Just ([], value)
splitLast (value : values) =
  case splitLast values of
    Just (prefix, finalValue) -> Just (value : prefix, finalValue)
    Nothing -> Nothing

comparisonJump :: Text -> CType -> CType -> TacM InstrType
comparisonJump op lhs rhs =
  case op of
    "==" -> pure IJe
    "!=" -> pure IJne
    "<" -> pure (if comparisonUsesUnsigned lhs rhs then IJb else IJl)
    ">" -> pure (if comparisonUsesUnsigned lhs rhs then IJa else IJg)
    "<=" -> pure (if comparisonUsesUnsigned lhs rhs then IJbe else IJle)
    ">=" -> pure (if comparisonUsesUnsigned lhs rhs then IJae else IJge)
    _ -> throw ("simple TAC unsupported comparison op: " <> op)

comparisonUsesUnsigned :: CType -> CType -> Bool
comparisonUsesUnsigned lhs rhs =
  case (decayExpressionType lhs, decayExpressionType rhs) of
    (PointerType {}, PointerType {}) -> True
    (lhs', rhs') ->
      case usualArithmeticConversion lhs' rhs' of
        Just (BaseType _ False) -> True
        _ -> False

compoundOperationType :: Text -> TacM InstrType
compoundOperationType op =
  case op of
    "+=" -> pure IAdd
    "-=" -> pure ISub
    "*=" -> pure IMull
    "&=" -> pure IAnd
    "|=" -> pure IOr
    "^=" -> pure IXor
    "<<=" -> pure IShl
    ">>=" -> pure IShr
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

defaultPlaces :: [(Text, Place)]
defaultPlaces =
  [ (name, operandToPlace operand)
  | (name, symbol) <- defaultSymbols
  , Just operand <- [symbolPlace symbol]
  ]

defaultStandardFunctionTargets :: [(Text, Bool)]
defaultStandardFunctionTargets =
  [ (placeValue (operandToPlace operand), returnsValue (symbolType symbol))
  | (_, symbol) <- defaultSymbols
  , Just operand <- [symbolPlace symbol]
  , operandIsStandardFunction operand
  ]

defaultStandardFunctionTargetMap :: Map.Map Text Bool
defaultStandardFunctionTargetMap = Map.fromList defaultStandardFunctionTargets

methodLabel :: Text -> Text -> Text
methodLabel method label = "." <> method <> "_" <> label

placeToConstant :: (Text, Place) -> (Text, Integer)
placeToConstant (name, place) = (name, placeInteger place)

operandToPlace :: Operand -> Place
operandToPlace operand =
  Place
    { placeKind = operandKind operand
    , placeValue = operandValueString (operandValue operand)
    }

operandValueString :: OperandValue -> Text
operandValueString value =
  case value of
    OperandInt number -> Text.pack (show number)
    OperandName name -> name

returnsValue :: CType -> Bool
returnsValue ty =
  case ty of
    FunctionType (BaseType Void _) _ _ -> False
    FunctionType {} -> True
    _ -> True

throw :: Text -> TacM a
throw = Error.throwError_ . Text.unpack
