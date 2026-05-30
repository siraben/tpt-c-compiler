module Tptcc.TypeChecker
  ( includedStandardFunctions
  , typeEvents
  ) where

import Control.Monad (foldM, forM, forM_, unless, void, when)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Monoid (Endo (..), appEndo)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Effectful
import qualified Effectful.Error.Static as Error
import qualified Effectful.State.Static.Local as State
import qualified Effectful.Writer.Static.Local as Writer

import Tptcc.Ast
import Tptcc.CType
import Tptcc.NodeFields
import Tptcc.NodeFields.Effectful
import Tptcc.Operand (Operand (..))
import Tptcc.SymbolTable (Symbol (..))
import Tptcc.Token (SourcePos (..), tokenName)
import Tptcc.TypeChecker.Types

typeEvents :: Node -> Either String [String]
typeEvents ast = do
  (st, logLines) <- runTypeCheck ast
  pure (logLines <> renderTypeAnnotations (typedNodes st) ast)

includedStandardFunctions :: Node -> Either String [String]
includedStandardFunctions ast = do
  (st, _) <- runTypeCheck ast
  pure (map Text.unpack (Set.toList (includedStandardFunctionNames st)))

runTypeCheck :: Node -> Either String (TypeState, [String])
runTypeCheck ast =
  case runPureEff (Error.runErrorNoCallStack (Writer.runWriter (State.runState initialState (checkProgram ast)))) of
    Left err -> Left err
    Right ((_, st), logLines) -> Right (st, appEndo logLines [])

checkProgram :: Node -> TypeM ()
checkProgram program = do
  requireKind NodeProgram program
  mapM_ buildDeclaration (childNodes program)

buildDeclaration :: Node -> TypeM ()
buildDeclaration declaration = do
  requireKind NodeDeclaration declaration
  specifier <- fieldNode "specifier" declaration
  typeSpecifier <- fieldNode "type_specifier" specifier
  baseType <- resolveTypeSpecifier typeSpecifier
  _ <- recordType declaration baseType
  declarators <- fieldNodeList "declarators" declaration
  forM_ declarators $ \declarator -> do
    declaredType <- adjustInitializerType declarator =<< buildDeclarator declarator baseType
    _ <- recordType declarator declaredType
    maybe (pure ()) (void . checkTopInitializerFor declaredType) (fieldNodeMaybe "initializer" declarator)
    let name = declaratorName declarator
        isFunctionDefinition = hasBoolField "is_function" declaration && hasNodeField "block" declaration
        isExternDeclaration = storageClassKind specifier == Just "extern"
        symbol =
          Symbol
            { symbolType = declaredType
            , symbolPlace = Nothing
            , symbolIsTypeName = False
            , symbolIsPrototype = isExternDeclaration || (hasBoolField "is_function" declaration && not isFunctionDefinition)
            }
    existing <- lookupCurrentSymbol Ordinary name
    case existing of
      Just existingSymbol
        | symbolIsPrototype existingSymbol -> setCurrentSymbol Ordinary name symbol
        | otherwise -> throw ("Symbol '" <> name <> "' is redefined")
      Nothing -> addSymbol Ordinary name symbol
    when isFunctionDefinition $ do
      enterScope name
      params <- functionParameters declarator
      forM_ params $ \(paramName, paramType) ->
        addSymbol
          Ordinary
          paramName
          Symbol
            { symbolType = paramType
            , symbolPlace = Nothing
            , symbolIsTypeName = False
            , symbolIsPrototype = False
            }
      case declaredType of
        FunctionType {} -> pure ()
        _ -> pure ()
      block <- fieldNode "block" declaration
      checkBlock block
      exitScope

resolveTypeSpecifier :: Node -> TypeM CType
resolveTypeSpecifier typeSpecifier = do
  requireKind NodeTypeSpecifier typeSpecifier
  kind <- field "kind" typeSpecifier
  case kind of
    StringList specifiers ->
      if isBaseSpecifiers specifiers
        then pure (baseFromSpecifiers specifiers)
        else case specifiers of
          typeName : _ -> do
            symbol <- requireSymbol Ordinary typeName
            pure (symbolType symbol)
          [] -> throw "empty type specifier"
    NodeRef node
      | nodeIs NodeStructOrUnionSpecifier node -> checkStructOrUnion node
      | nodeIs NodeEnumSpecifier node -> checkEnum node
      | otherwise -> throw ("unexpected type specifier node: " <> nodeName node)
    _ -> throw "invalid type specifier kind"

checkStructOrUnion :: Node -> TypeM CType
checkStructOrUnion node = do
  let typeName = maybe (if isStruct then "anon_struct" else "anon_union") identifierValue (fieldNodeMaybe "id" node)
      isStruct = boolFieldDefault "is_struct" False node
  case fieldNodeListMaybe "declaration" node of
    Just declarations -> do
      members' <- concat <$> mapM structDeclarationMembers declarations
      let membersWithOffsets = withMemberOffsets isStruct members'
          ty = if isStruct then struct typeName membersWithOffsets else typeName `union` membersWithOffsets
      when (hasNodeField "id" node) $
        addSymbol Tag typeName (blankSymbol ty)
      recordType node ty
    Nothing -> do
      ty <- symbolType <$> requireSymbol Tag typeName
      recordType node ty

structDeclarationMembers :: Node -> TypeM [Member]
structDeclarationMembers node = do
  typeSpecifier <- fieldNode "type_specifier" node
  memberBase <- resolveTypeSpecifier typeSpecifier
  forM (childNodes node) $ \declarator -> do
    memberTy <- buildDeclarator declarator memberBase
    pure Member {memberName = declaratorName declarator, memberType = memberTy, memberOffset = Nothing}

checkEnum :: Node -> TypeM CType
checkEnum node = do
  identifier <- fieldNode "id" node
  let name = identifierValue identifier
  case fieldNodeMaybe "declaration" node of
    Just declaration -> do
      let members' = childNodes declaration
      memberNames <- mapM (fmap identifierValue . fieldNode "id") members'
      forM_ (enumMemberValues members') $ \(memberNode, value) -> do
        memberId <- fieldNode "id" memberNode
        addSymbol Ordinary (identifierValue memberId) (blankSymbol (base "INT")) {symbolPlace = Nothing}
        -- The event stream is type-oriented; enum values are represented by their symbol type.
        value `seq` pure ()
      addSymbol Tag name (blankSymbol (enum name memberNames))
    Nothing -> pure ()
  recordType node (base "INT")

buildDeclarator :: Node -> CType -> TypeM CType
buildDeclarator declarator baseType' = do
  requireKind NodeDeclarator declarator
  pointerLevel <- fieldInt "pointer_level" declarator
  direct <- fieldNode "direct_declarator" declarator
  ty <- buildDirectDeclarator direct (applyPointers pointerLevel baseType')
  recordType declarator ty

buildDirectDeclarator :: Node -> CType -> TypeM CType
buildDirectDeclarator direct ty = do
  dimensions <- fieldInts "dimensions" direct
  let withArrays = foldr array ty dimensions
  withFunction <-
    case fieldNodeMaybe "parameter_list" direct of
      Just params -> buildFunctionType withArrays params
      Nothing -> pure withArrays
  case fieldNodeMaybe "declarator" direct of
    Just nested -> buildDeclarator nested withFunction
    Nothing -> pure withFunction

buildParameterList :: Node -> TypeM [CType]
buildParameterList params = do
  requireKind NodeParameterList params
  mapM (fmap parameterType . buildParameter) (childNodes params)

buildFunctionType :: CType -> Node -> TypeM CType
buildFunctionType ret params = do
  parameterTys <- buildParameterList params
  pure $
    if hasBoolField "is_variadic" params
      then variadicFunction ret parameterTys
      else function ret parameterTys

data Parameter = Parameter
  { parameterName :: Maybe Text
  , parameterType :: CType
  }
  deriving stock (Eq, Show)

buildParameter :: Node -> TypeM Parameter
buildParameter parameter = do
  typeSpecifier <- fieldNode "type_specifier" parameter
  baseType' <- resolveTypeSpecifier typeSpecifier
  case fieldNodeMaybe "declarator" parameter of
    Just declarator -> do
      ty <- decayArrayParameter <$> buildDeclarator declarator baseType'
      _ <- recordType parameter ty
      pure Parameter {parameterName = Just (declaratorName declarator), parameterType = ty}
    Nothing -> do
      _ <- recordType parameter baseType'
      pure Parameter {parameterName = Nothing, parameterType = baseType'}

functionParameters :: Node -> TypeM [(Text, CType)]
functionParameters declarator = do
  direct <- fieldNode "direct_declarator" declarator
  params <- maybe (pure []) (mapM buildParameter . childNodes) (fieldNodeMaybe "parameter_list" direct)
  pure [(name, ty) | Parameter (Just name) ty <- params]

adjustInitializerType :: Node -> CType -> TypeM CType
adjustInitializerType declarator ty =
  case fieldNodeMaybe "initializer" declarator of
    Nothing -> pure ty
    Just initializer -> pure (adjustFromInitializer initializer ty)

adjustFromInitializer :: Node -> CType -> CType
adjustFromInitializer initializer ty
  | nodeIs NodeInitializerList initializer =
      case ty of
        ArrayType (-1) target -> ArrayType (fromIntegral (length (childNodes initializer))) target
        _ -> ty
  | nodeIs NodeInitializer initializer =
      case (fieldNodeMaybe "value" initializer, ty) of
        (Just valueNode, ArrayType (-1) target)
          | nodeIs NodeStringLiteral valueNode ->
              ArrayType (fromIntegral (Text.length (fieldStringDefault "value" "" valueNode)) + 1) target
        _ -> ty
  | otherwise = ty

checkBlock :: Node -> TypeM ()
checkBlock block = do
  blockId <- nextBlockId
  enterScope ("block_" <> tshow blockId)
  mapM_ checkStatement (childNodes block)
  exitScope

checkStatement :: Node -> TypeM ()
checkStatement statement = do
  child <- fieldNode "child" statement
  case nodeKind child of
    NodeDeclaration -> buildDeclaration child
    NodeIf -> do
      _ <- checkExpression =<< fieldNode "condition" child
      checkStatement =<< fieldNode "true_case" child
      maybe (pure ()) checkStatement (fieldNodeMaybe "false_case" child)
    NodeBlock -> checkBlock child
    NodeFor -> checkFor child
    NodeWhile -> do
      _ <- checkExpression =<< fieldNode "condition" child
      checkStatement =<< fieldNode "statement" child
    NodeDoWhile -> do
      checkStatement =<< fieldNode "statement" child
      _ <- checkExpression =<< fieldNode "condition" child
      pure ()
    NodeSwitch -> do
      _ <- checkExpression =<< fieldNode "condition" child
      checkBlock =<< fieldNode "block" child
    NodeCase -> do
      _ <- checkPrimaryExpression =<< fieldNode "value" child
      checkStatement =<< fieldNode "statement" child
    NodeDefault -> checkStatement =<< fieldNode "statement" child
    NodeGoto -> do
      _ <- fieldNode "target" child
      pure ()
    NodeLabel -> checkStatement =<< fieldNode "statement" child
    NodeReturn -> maybe (pure ()) (void . checkExpression) (fieldNodeMaybe "value" child)
    NodeExpression -> void (checkExpression child)
    NodeAsm -> checkAsm child
    _ -> pure ()

checkFor :: Node -> TypeM ()
checkFor node = do
  blockId <- nextBlockId
  enterScope ("for_loop_" <> tshow blockId)
  case fieldNodeMaybe "initialization" node of
    Just initialization
      | nodeIs NodeDeclaration initialization -> buildDeclaration initialization
      | otherwise -> void (checkExpression initialization)
    Nothing -> pure ()
  maybe (pure ()) (void . checkExpression) (fieldNodeMaybe "condition" node)
  checkStatement =<< fieldNode "statement" node
  maybe (pure ()) (void . checkExpression) (fieldNodeMaybe "update" node)
  exitScope

checkAsm :: Node -> TypeM ()
checkAsm node = do
  maybe (pure ()) checkAsmArgumentList (fieldNodeMaybe "inputs" node)
  maybe (pure ()) checkAsmArgumentList (fieldNodeMaybe "outputs" node)

checkAsmArgumentList :: Node -> TypeM ()
checkAsmArgumentList node =
  mapM_ checkAsmArgument (fromMaybe [] (fieldNodeListMaybe "arguments" node))

checkAsmArgument :: Node -> TypeM ()
checkAsmArgument node = do
  cSymbol <- fieldNode "c_symbol" node
  _ <- requireSymbol Ordinary (identifierValue cSymbol)
  pure ()

checkTopInitializerFor :: CType -> Node -> TypeM CType
checkTopInitializerFor = checkInitializerFor True

checkInitializerFor :: Bool -> CType -> Node -> TypeM CType
checkInitializerFor recordList target initializer
  | nodeIs NodeInitializerList initializer = do
      let children = childNodes initializer
      childTypes <- mapM (checkInitializerFor False (initializerElementType target)) children
      let ty = initializerListType target childTypes
      if recordList then recordType initializer ty else pure ty
  | nodeIs NodeInitializer initializer = do
      value <- fieldNode "value" initializer
      ty <- checkAssignmentExpression value
      recordType initializer ty
  | otherwise = checkAssignmentExpression initializer

initializerElementType :: CType -> CType
initializerElementType ty =
  case ty of
    ArrayType _ target -> target
    StructType _ (member : _) -> memberType member
    UnionType _ (member : _) -> memberType member
    _ -> ty

initializerListType :: CType -> [CType] -> CType
initializerListType target childTypes =
  case target of
    ArrayType {} -> target
    StructType {} -> target
    UnionType {} -> target
    _ -> array (fromIntegral (length childTypes)) (fromMaybe (base "VOID") (firstMaybe childTypes))

checkExpression :: Node -> TypeM CType
checkExpression node
  | nodeIs NodeExpression node = do
      let children = childNodes node
      childTypes <- mapM checkAssignmentExpression children
      ty <- maybe (pure (base "VOID")) pure (lastMaybe childTypes)
      recordType node ty
  | otherwise = checkAssignmentExpression node

checkAssignmentExpression :: Node -> TypeM CType
checkAssignmentExpression node
  | nodeIs NodeAssignment node = do
      op <- fieldString "op" node
      lhsNode <- fieldNode "lhs" node
      assignable <- isAssignableExpression lhsNode
      unless assignable $
        throw "Left-hand side of assignment is not assignable"
      lhs <- checkTernaryExpression lhsNode
      unless (isAssignableType lhs) $
        throw ("Cannot assign to " <> renderTypePretty lhs)
      rhsNode <- fieldNode "rhs" node
      rhs <- checkAssignmentExpression rhsNode
      checkAssignmentCompatibility op lhs rhsNode rhs
      recordType node lhs
  | otherwise = checkTernaryExpression node

checkAssignmentCompatibility :: Text -> CType -> Node -> CType -> TypeM ()
checkAssignmentCompatibility op lhs rhsNode rhs =
  case op of
    "=" ->
      unless (canAssignFrom rhsNode rhs lhs) $
        throw ("Cannot assign " <> renderTypePretty rhs <> " to " <> renderTypePretty lhs)
    "+=" -> void (sumResultType "+" lhs rhs)
    "-=" -> void (sumResultType "-" lhs rhs)
    "*=" -> void (integerArithmeticResultType lhs rhs)
    "/=" -> void (integerArithmeticResultType lhs rhs)
    "%=" -> void (integerArithmeticResultType lhs rhs)
    "&=" -> void (integerArithmeticResultType lhs rhs)
    "|=" -> void (integerArithmeticResultType lhs rhs)
    "^=" -> void (integerArithmeticResultType lhs rhs)
    "<<=" -> void (shiftResultType "<<" lhs rhs)
    ">>=" -> void (shiftResultType ">>" lhs rhs)
    _ -> throw ("Unsupported assignment operator: " <> op)

checkTernaryExpression :: Node -> TypeM CType
checkTernaryExpression node
  | nodeIs NodeTernary node = do
      _ <- fieldNode "condition" node >>= checkLogicalOrExpression
      trueTy <- fieldNode "true_case" node >>= checkAssignmentExpression
      falseTy <- fieldNode "false_case" node >>= checkLogicalOrExpression
      unless (sameTypeChain falseTy trueTy True) $
        throw "Ternary false and true case types do not match"
      recordType node trueTy
  | otherwise = checkLogicalOrExpression node

checkLogicalOrExpression :: Node -> TypeM CType
checkLogicalOrExpression = checkLogicalNode NodeLogicalOrExpression checkLogicalAndExpression

checkLogicalAndExpression :: Node -> TypeM CType
checkLogicalAndExpression = checkLogicalNode NodeLogicalAndExpression checkInclusiveOrExpression

checkInclusiveOrExpression :: Node -> TypeM CType
checkInclusiveOrExpression = checkIntegerFoldNode NodeInclusiveOrExpression checkInclusiveXorExpression

checkInclusiveXorExpression :: Node -> TypeM CType
checkInclusiveXorExpression = checkIntegerFoldNode NodeInclusiveXorExpression checkInclusiveAndExpression

checkInclusiveAndExpression :: Node -> TypeM CType
checkInclusiveAndExpression = checkIntegerFoldNode NodeInclusiveAndExpression checkEqualityExpression

checkEqualityExpression :: Node -> TypeM CType
checkEqualityExpression = checkComparisonNode NodeEqualityExpression checkRelationalExpression

checkRelationalExpression :: Node -> TypeM CType
checkRelationalExpression node
  | nodeIs NodeRelationalExpression node = do
      _ <- foldComparisonExpression node checkShiftExpression
      recordType node (baseWithSigned "INT" True)
  | otherwise = checkShiftExpression node

checkShiftExpression :: Node -> TypeM CType
checkShiftExpression node
  | nodeIs NodeShiftExpression node = do
      ty <- foldTokenExpression node checkSumExpression shiftResultType
      recordType node ty
  | otherwise = checkSumExpression node

checkLogicalNode :: NodeKind -> (Node -> TypeM CType) -> Node -> TypeM CType
checkLogicalNode expected subChecker node
  | nodeIs expected node = do
      childTypes <- mapM subChecker (childNodes node)
      mapM_ requireScalar childTypes
      recordType node (base "INT")
  | otherwise = subChecker node

checkIntegerFoldNode :: NodeKind -> (Node -> TypeM CType) -> Node -> TypeM CType
checkIntegerFoldNode expected subChecker node
  | nodeIs expected node = do
      childTypes <- mapM subChecker (childNodes node)
      ty <- foldM integerArithmeticResultType (base "INT") childTypes
      recordType node ty
  | otherwise = subChecker node

checkComparisonNode :: NodeKind -> (Node -> TypeM CType) -> Node -> TypeM CType
checkComparisonNode expected subChecker node
  | nodeIs expected node = do
      _ <- foldComparisonExpression node subChecker
      recordType node (base "INT")
  | otherwise = subChecker node

foldComparisonExpression :: Node -> (Node -> TypeM CType) -> TypeM CType
foldComparisonExpression node subChecker =
  case nodeChildren node of
    ChildNode firstNode : rest -> do
      firstTy <- subChecker firstNode
      foldComparisonRest firstNode (decayExpressionType firstTy) rest
    _ -> throw ("malformed " <> nodeName node)
  where
    foldComparisonRest _ acc [] = pure acc
    foldComparisonRest lhsNode lhsTy (ChildToken token : ChildNode rhsNode : rest) = do
      rhsTy <- decayExpressionType <$> subChecker rhsNode
      result <- comparisonResultType (tokenName token) lhsNode lhsTy rhsNode rhsTy
      foldComparisonRest rhsNode result rest
    foldComparisonRest _ _ _ = throw ("malformed " <> nodeName node)

foldTokenExpression :: Node -> (Node -> TypeM CType) -> (Text -> CType -> CType -> TypeM CType) -> TypeM CType
foldTokenExpression node subChecker combine =
  case nodeChildren node of
    ChildNode firstNode : rest -> do
      firstTy <- subChecker firstNode
      foldTokenRest (decayExpressionType firstTy) rest
    _ -> throw ("malformed " <> nodeName node)
  where
    foldTokenRest acc [] = pure acc
    foldTokenRest acc (ChildToken token : ChildNode rhsNode : rest) = do
      rhs <- decayExpressionType <$> subChecker rhsNode
      result <- combine (tokenName token) acc rhs
      foldTokenRest result rest
    foldTokenRest _ _ = throw ("malformed " <> nodeName node)

checkSumExpression :: Node -> TypeM CType
checkSumExpression node
  | nodeIs NodeSumExpression node = do
      ty <- foldTokenExpression node checkTerm sumResultType
      recordType node ty
  | otherwise = checkTerm node

sumResultType :: Text -> CType -> CType -> TypeM CType
sumResultType op lhs rhs
  | isIntegerType lhs && isIntegerType rhs = integerArithmeticResultType lhs rhs
  | op == "+" && isPointerType lhs && isIntegerType rhs = pure lhs
  | op == "+" && isIntegerType lhs && isPointerType rhs = pure rhs
  | op == "-" && isPointerType lhs && isIntegerType rhs = pure lhs
  | op == "-" && isPointerType lhs && isPointerType rhs && compatiblePointerTypes lhs rhs = pure (base "INT")
  | op == "-" && isPointerType lhs && isPointerType rhs = throw "Cannot subtract incompatible pointer types"
  | isPointerType lhs || isPointerType rhs = throw ("Invalid pointer arithmetic using " <> op)
  | otherwise = pure (base "INT")

integerTokenResultType :: Text -> CType -> CType -> TypeM CType
integerTokenResultType _ = integerArithmeticResultType

integerArithmeticResultType :: CType -> CType -> TypeM CType
integerArithmeticResultType lhs rhs =
  case usualArithmeticConversion lhs rhs of
    Just ty -> pure ty
    Nothing -> throw ("Integer arithmetic requires integer operands, got " <> renderTypePretty lhs <> " and " <> renderTypePretty rhs)

shiftResultType :: Text -> CType -> CType -> TypeM CType
shiftResultType _ lhs rhs = do
  lhsPromoted <- requireIntegerPromotion lhs
  _ <- requireIntegerPromotion rhs
  pure lhsPromoted

comparisonResultType :: Text -> Node -> CType -> Node -> CType -> TypeM CType
comparisonResultType op lhsNode lhs rhsNode rhs
  | isIntegerType lhs && isIntegerType rhs = base "INT" <$ integerArithmeticResultType lhs rhs
  | isPointerType lhs && isPointerType rhs && compatiblePointerTypes lhs rhs = pure (base "INT")
  | op `elem` ["==", "!="] && isPointerType lhs && isNullPointerConstant rhsNode = pure (base "INT")
  | op `elem` ["==", "!="] && isNullPointerConstant lhsNode && isPointerType rhs = pure (base "INT")
  | otherwise = throw ("Cannot compare " <> renderTypePretty lhs <> " with " <> renderTypePretty rhs)

requireIntegerPromotion :: CType -> TypeM CType
requireIntegerPromotion ty =
  case integerPromotion ty of
    Just promoted -> pure promoted
    Nothing -> throw ("Expected integer type, got " <> renderTypePretty ty)

requireScalar :: CType -> TypeM ()
requireScalar ty =
  unless (isScalarType ty) $
    throw ("Expected scalar type, got " <> renderTypePretty ty)

checkTerm :: Node -> TypeM CType
checkTerm node
  | nodeIs NodeMultiplicativeExpression node = do
      ty <- foldTokenExpression node checkCastExpression integerTokenResultType
      recordType node ty
  | otherwise = checkCastExpression node

checkCastExpression :: Node -> TypeM CType
checkCastExpression node
  | nodeIs NodeCastExpression node = do
      typeName <- fieldNode "type_specifier" node
      baseTy <- checkTypeName typeName
      pointerLevel <- fieldInt "pointer_level" node
      _ <- fieldNode "cast_expression" node >>= checkCastExpression
      recordType node (applyPointers pointerLevel baseTy)
  | otherwise = checkUnaryExpression node

checkUnaryExpression :: Node -> TypeM CType
checkUnaryExpression node
  | nodeIs NodeUnaryExpression node = do
      op <- fieldString "operator" node
      child <- fieldNode "child" node
      childTy <-
        if nodeIs NodeTypeName child
          then checkTypeName child
          else checkUnaryExpression child
      let result =
            case op of
              "++" -> childTy
              "--" -> childTy
              "SIZEOF" -> base "INT"
              "&" -> pointer childTy
              "*" -> dereferenceType childTy
              "!" -> base "INT"
              "~" -> base "INT"
              "-" -> childTy
              "+" -> childTy
              _ -> base "INT"
      recordType node result
  | otherwise = checkPostfixExpression node

checkTypeName :: Node -> TypeM CType
checkTypeName node = do
  typeSpecifier <- fieldNode "type_specifier" node
  baseTy <- resolveTypeSpecifier typeSpecifier
  _ <- recordType typeSpecifier baseTy
  declarator <- fieldNode "declarator" node
  ty <- buildAbstractDeclarator declarator baseTy
  recordType node ty

buildAbstractDeclarator :: Node -> CType -> TypeM CType
buildAbstractDeclarator node baseTy = do
  pointerLevel <- fieldInt "pointer_level" node
  let pointerTy = applyPointers pointerLevel baseTy
  case fieldNodeMaybe "direct_abstract_declarator" node of
    Just direct -> buildDirectAbstractDeclarator direct pointerTy
    Nothing -> pure pointerTy

buildDirectAbstractDeclarator :: Node -> CType -> TypeM CType
buildDirectAbstractDeclarator node baseTy = do
  withFunction <-
    case fieldNodeMaybe "parameter_list" node of
      Just params -> buildFunctionType baseTy params
      Nothing -> pure baseTy
  let dimensions = map (fieldIntDefault "value" 0) (childNodes node)
      withArrays = foldr array withFunction dimensions
  case fieldNodeMaybe "declarator" node of
    Just nested -> buildAbstractDeclarator nested withArrays
    Nothing -> pure withArrays

checkPostfixExpression :: Node -> TypeM CType
checkPostfixExpression node
  | nodeIs NodePostfixExpression node = do
      primaryTy <- fieldNode "primary_expression" node >>= checkPrimaryExpression
      finalTy <- foldPostfixOps primaryTy (postfixOps node)
      recordType node finalTy
  | otherwise = checkPrimaryExpression node

foldPostfixOps :: CType -> [PostfixOp] -> TypeM CType
foldPostfixOps =
  foldM $ \ty op ->
    case postfixType op of
      "[" -> do
        _ <- maybe (pure (base "INT")) checkExpression (postfixNodeValue op)
        pure (dereferenceType ty)
      "(" -> do
        let callable = case ty of
              PointerType target -> target
              _ -> ty
        case callable of
          FunctionType ret params isVariadic -> do
            maybe (checkArgumentList emptyArgumentList params isVariadic) (\arguments -> checkArgumentList arguments params isVariadic) (postfixNodeValue op)
            pure ret
          _ -> pure ty
      "++" -> pure ty
      "--" -> pure ty
      "." -> pure (memberAccessType ty op)
      "->" -> pure (memberAccessType (dereferenceType ty) op)
      _ -> pure ty

checkArgumentList :: Node -> [CType] -> Bool -> TypeM ()
checkArgumentList arguments params isVariadic = do
  let argumentNodes = childNodes arguments
  argumentTypes <- mapM checkAssignmentExpression argumentNodes
  if isVariadic
    then
      unless (length argumentTypes >= length params) $
        throw "Variadic argument list has fewer arguments than the fixed parameter list"
    else
      unless (length argumentTypes == length params) $
        throw "Argument list length does not match the parameter list length"
  forM_ (zip argumentNodes (zip argumentTypes params)) $ \(argNode, (argTy, paramTy)) ->
    unless (canAssignFrom argNode argTy paramTy) $
      throw ("Argument type " <> renderTypePretty argTy <> " does not match parameter type " <> renderTypePretty paramTy)
  mapM_ (requireScalar . defaultArgumentPromotion) (drop (length params) argumentTypes)

checkPrimaryExpression :: Node -> TypeM CType
checkPrimaryExpression node =
  case nodeKind node of
    NodeInt -> recordType node (baseWithSigned "INT" (not (hasBoolField "is_unsigned" node)))
    NodeIdentifier -> do
      let name = fieldStringDefault "value" (identifierValue node) node
      symbol <- requireSymbol Ordinary name
      let ty = symbolType symbol
          decayed =
            case ty of
              ArrayType _ target -> pointer target
              FunctionType {} -> pointer ty
              _ -> ty
      case (ty, symbolPlace symbol) of
        (FunctionType {}, Just place) | operandIsStandardFunction place ->
          State.modify (\st -> st {includedStandardFunctionNames = Set.insert name (includedStandardFunctionNames st)})
        _ -> pure ()
      recordType node decayed
    NodeStringLiteral -> recordType node (array (fromIntegral (Text.length (fieldStringDefault "value" "" node)) + 1) (base "CHAR"))
    NodeCharacter -> recordType node (base "CHAR")
    NodeExpression -> checkExpression node
    _ -> throw ("Invalid primary expression: " <> nodeName node)

canAssignFrom :: Node -> CType -> CType -> Bool
canAssignFrom sourceNode source target =
  canCoerce source target || (isPointerType target && isNullPointerConstant sourceNode)

canCoerce :: CType -> CType -> Bool
canCoerce source target =
  case (decayExpressionType source, target) of
    (PointerType a, PointerType b) -> compatiblePointerTargets a b
    (ArrayType la a, ArrayType lb b) -> (la <= lb || lb < 0) && canCoerce a b
    (FunctionType retA paramsA variadicA, FunctionType retB paramsB variadicB) ->
      variadicA == variadicB
        && canCoerce retA retB
        && length paramsA == length paramsB
        && and (zipWith canCoerce paramsA paramsB)
    _ | isIntegerType source && isIntegerType target -> True
    (StructType ida _, StructType idb _) -> ida == idb
    (UnionType ida _, UnionType idb _) -> ida == idb
    _ -> False

emptyArgumentList :: Node
emptyArgumentList =
  Node
    { nodeKind = NodeArgumentExpressionList
    , nodeTypeId = nodeTypeIdFor "ARGUMENT_EXPRESSION_LIST"
    , nodePos = SourcePos 0 0
    , nodeChildren = []
    , nodeFields = []
    }

isNullPointerConstant :: Node -> Bool
isNullPointerConstant node =
  case nodeKind node of
    NodeInt -> fieldIntDefault "value" 1 node == 0
    NodeExpression -> case childNodes node of
      [child] -> isNullPointerConstant child
      _ -> False
    NodeCastExpression -> maybe False isNullPointerConstant (fieldNodeMaybe "cast_expression" node)
    _ -> False

isAssignableExpression :: Node -> TypeM Bool
isAssignableExpression node =
  case nodeKind node of
    NodeIdentifier -> do
      let name = fieldStringDefault "value" (identifierValue node) node
      symbol <- requireSymbol Ordinary name
      pure (isAssignableType (symbolType symbol))
    NodeUnaryExpression -> pure (fieldStringDefault "operator" "" node == "*")
    NodePostfixExpression -> pure (any postfixAssigns (postfixOps node))
    NodeExpression -> case childNodes node of
      [child] -> isAssignableExpression child
      _ -> pure False
    _ -> pure False

postfixAssigns :: PostfixOp -> Bool
postfixAssigns op = postfixType op `elem` ["[", ".", "->"]

isAssignableType :: CType -> Bool
isAssignableType ArrayType {} = False
isAssignableType FunctionType {} = False
isAssignableType _ = True

recordType :: Node -> CType -> TypeM CType
recordType node ty = do
  let key = typeKey node
  State.modify (\st -> st {typedNodes = Map.insert key ty (typedNodes st)})
  pure ty

typeKey :: Node -> TypeKey
typeKey node = TypeKey (nodeKind node) (row (nodePos node)) (col (nodePos node))

renderTypeAnnotations :: Map.Map TypeKey CType -> Node -> [String]
renderTypeAnnotations annotations =
  foldMapNode typeLine
  where
    typeLine node =
      maybe [] (\ty -> [renderTypeLine node ty]) (Map.lookup (typeKey node) annotations)

renderTypeLine :: Node -> CType -> String
renderTypeLine node ty =
  let pos = nodePos node
   in Text.unpack ("TYPE\t" <> Text.pack (show (row pos)) <> "\t" <> Text.pack (show (col pos)) <> "\t" <> nodeName node <> "\t" <> renderTypePretty ty)

postfixOps :: Node -> [PostfixOp]
postfixOps node = [op | ChildPostfix op <- nodeChildren node]

postfixNodeValue :: PostfixOp -> Maybe Node
postfixNodeValue op =
  case postfixValue op of
    Just (NodeRef node) -> Just node
    _ -> Nothing

memberAccessType :: CType -> PostfixOp -> CType
memberAccessType ty op =
  let memberName' = maybe "" identifierValue (postfixNodeValue op)
      memberTy =
        case ty of
          StructType _ members' -> memberType <$> memberByName memberName' members'
          UnionType _ members' -> memberType <$> memberByName memberName' members'
          _ -> Nothing
   in case memberTy of
        Just (ArrayType _ target) -> pointer target
        Just found -> found
        Nothing -> ty

firstMaybe :: [a] -> Maybe a
firstMaybe [] = Nothing
firstMaybe (value : _) = Just value

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [value] = Just value
lastMaybe (_ : values) = lastMaybe values

nextBlockId :: TypeM Integer
nextBlockId = do
  st <- State.get
  let current = blockCounter st
  State.modify (\s -> s {blockCounter = current + 1})
  pure current

enterScope :: Text -> TypeM ()
enterScope name = do
  current <- currentScope
  let level = frameLevel current + 1
      newFrame =
        ScopeFrame
          { frameLevel = level
          , frameName = name
          , frameOrdinary = Map.empty
          , frameTags = Map.empty
          }
  State.modify $
    \st ->
      st
        { scopeStack = newFrame :| NE.toList (scopeStack st)
        }
  appendLog ("ENTER\t" <> tshow level <> "\t" <> name)

exitScope :: TypeM ()
exitScope = do
  current <- currentScope
  when (frameLevel current == 0) (throw "cannot exit global scope")
  State.modify $ \st ->
    case scopeStack st of
      _ :| [] -> st
      _ :| next : rest ->
        st
          { scopeStack = next :| rest
          }
  appendLog ("EXIT\t" <> tshow (frameLevel current) <> "\t" <> frameName current)

addSymbol :: Namespace -> Text -> Symbol -> TypeM ()
addSymbol namespace name symbol = do
  current <- currentScope
  when (Map.member name (namespaceMap namespace current)) $
    throw ("Symbol '" <> name <> "' has already been defined")
  setCurrentSymbol namespace name symbol
  current' <- currentScope
  appendLog
    ( "ADD\t"
        <> tshow (frameLevel current')
        <> "\t"
        <> frameName current'
        <> "\t"
        <> renderNamespace namespace
        <> "\t"
        <> name
        <> "\t"
        <> renderTypePretty (symbolType symbol)
    )

setCurrentSymbol :: Namespace -> Text -> Symbol -> TypeM ()
setCurrentSymbol namespace name symbol =
  State.modify $ \st ->
    case scopeStack st of
      frame :| rest -> st {scopeStack = setFrame namespace name symbol frame :| rest}

lookupSymbol :: Namespace -> Text -> TypeM (Maybe Symbol)
lookupSymbol namespace name = do
  scopes <- scopeStack <$> State.get
  pure (firstJust (map (Map.lookup name . namespaceMap namespace) (NE.toList scopes)))

lookupCurrentSymbol :: Namespace -> Text -> TypeM (Maybe Symbol)
lookupCurrentSymbol namespace name =
  Map.lookup name . namespaceMap namespace <$> currentScope

requireSymbol :: Namespace -> Text -> TypeM Symbol
requireSymbol namespace name =
  lookupSymbol namespace name >>= maybe (throw ("missing symbol: " <> name)) pure

namespaceMap :: Namespace -> ScopeFrame -> Map.Map Text Symbol
namespaceMap Ordinary = frameOrdinary
namespaceMap Tag = frameTags

setFrame :: Namespace -> Text -> Symbol -> ScopeFrame -> ScopeFrame
setFrame namespace name symbol frame =
  case namespace of
    Ordinary -> frame {frameOrdinary = Map.insert name symbol (frameOrdinary frame)}
    Tag -> frame {frameTags = Map.insert name symbol (frameTags frame)}

blankSymbol :: CType -> Symbol
blankSymbol ty =
  Symbol
    { symbolType = ty
    , symbolPlace = Nothing
    , symbolIsTypeName = False
    , symbolIsPrototype = False
    }

currentScope :: TypeM ScopeFrame
currentScope = NE.head . scopeStack <$> State.get

appendLog :: Text -> TypeM ()
appendLog line = Writer.tell (Endo (Text.unpack line :))

renderNamespace :: Namespace -> Text
renderNamespace Ordinary = "o"
renderNamespace Tag = "t"

requireKind :: NodeKind -> Node -> TypeM ()
requireKind expected node =
  when (nodeKind node /= expected) $
    throw ("expected " <> nodeKindName expected <> ", got " <> nodeName node)

throw :: Text -> TypeM a
throw = Error.throwError_ . Text.unpack

tshow :: Show a => a -> Text
tshow = Text.pack . show
