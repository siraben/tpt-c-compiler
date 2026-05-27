module Tptcc.TypeChecker
  ( includedStandardFunctions
  , typeEvents
  ) where

import Control.Monad (foldM, forM, forM_, unless, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify', runStateT)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

import Tptcc.Ast
import Tptcc.CType
import Tptcc.NodeFields
import Tptcc.Operand (Operand (..))
import Tptcc.SymbolTable (Symbol (..), defaultSymbols)
import Tptcc.Token (SourcePos (..))

data Namespace = Ordinary | Tag
  deriving (Eq, Show)

data ScopeFrame = ScopeFrame
  { frameLevel :: Int
  , frameName :: String
  , frameOrdinary :: Map.Map String Symbol
  , frameTags :: Map.Map String Symbol
  }
  deriving (Eq, Show)

data TypeState = TypeState
  { scopeStack :: [ScopeFrame]
  , typeLog :: [String]
  , typedNodes :: Map.Map TypeKey CType
  , blockCounter :: Integer
  , includedStandardFunctionNames :: [String]
  }
  deriving (Eq, Show)

type TypeM = StateT TypeState (Either String)

type TypeKey = (String, Int, Int)

typeEvents :: Node -> Either String [String]
typeEvents ast = do
  (_, st) <- runStateT (checkProgram ast) initialState
  pure (typeLog st <> renderTypeAnnotations (typedNodes st) ast)

includedStandardFunctions :: Node -> Either String [String]
includedStandardFunctions ast = do
  (_, st) <- runStateT (checkProgram ast) initialState
  pure (includedStandardFunctionNames st)

initialState :: TypeState
initialState =
  TypeState
    { scopeStack =
        [ ScopeFrame
            { frameLevel = 0
            , frameName = "global"
            , frameOrdinary = Map.fromList defaultSymbols
            , frameTags = Map.empty
            }
        ]
    , typeLog = []
    , typedNodes = Map.empty
    , blockCounter = 0
    , includedStandardFunctionNames = []
    }

checkProgram :: Node -> TypeM ()
checkProgram program = do
  requireName "PROGRAM" program
  mapM_ buildDeclaration (childNodes program)

buildDeclaration :: Node -> TypeM ()
buildDeclaration declaration = do
  requireName "DECLARATION" declaration
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
        symbol =
          Symbol
            { symbolType = declaredType
            , symbolPlace = Nothing
            , symbolIsTypeName = False
            , symbolIsPrototype = hasBoolField "is_function" declaration && not isFunctionDefinition
            }
    existing <- lookupSymbol Ordinary name
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
        FunctionType _ _ -> pure ()
        _ -> pure ()
      block <- fieldNode "block" declaration
      checkBlock block
      exitScope

resolveTypeSpecifier :: Node -> TypeM CType
resolveTypeSpecifier typeSpecifier = do
  requireName "TYPE_SPECIFIER" typeSpecifier
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
      | nodeName node == "STRUCT_OR_UNION_SPECIFIER" -> checkStructOrUnion node
      | nodeName node == "ENUM_SPECIFIER" -> checkEnum node
      | otherwise -> throw ("unexpected type specifier node: " <> nodeName node)
    _ -> throw "invalid type specifier kind"

checkStructOrUnion :: Node -> TypeM CType
checkStructOrUnion node = do
  let typeName = maybe (if isStruct then "anon_struct" else "anon_union") identifierValue (fieldNodeMaybe "id" node)
      isStruct = boolFieldDefault "is_struct" False node
  case fieldNodeListMaybe "declaration" node of
    Just declarations -> do
      members' <- concat <$> mapM structDeclarationMembers declarations
      let ty = if isStruct then struct typeName members' else typeName `union` members'
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
      forM_ (zip [(0 :: Integer) ..] members') $ \(index, memberNode) -> do
        memberId <- fieldNode "id" memberNode
        let value = fromMaybe index (fieldIntMaybe "value" memberNode)
        addSymbol Ordinary (identifierValue memberId) (blankSymbol (base "INT")) {symbolPlace = Nothing}
        -- The event stream is type-oriented; enum values are represented by their symbol type.
        value `seq` pure ()
      addSymbol Tag name (blankSymbol (enum name memberNames))
    Nothing -> pure ()
  recordType node (base "INT")

buildDeclarator :: Node -> CType -> TypeM CType
buildDeclarator declarator baseType' = do
  requireName "DECLARATOR" declarator
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
      Just params -> function withArrays <$> buildParameterList params
      Nothing -> pure withArrays
  case fieldNodeMaybe "declarator" direct of
    Just nested -> buildDeclarator nested withFunction
    Nothing -> pure withFunction

buildParameterList :: Node -> TypeM [CType]
buildParameterList params = do
  requireName "PARAMETER_LIST" params
  mapM (fmap parameterType . buildParameter) (childNodes params)

data Parameter = Parameter
  { parameterName :: Maybe String
  , parameterType :: CType
  }
  deriving (Eq, Show)

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

functionParameters :: Node -> TypeM [(String, CType)]
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
  | nodeName initializer == "INITIALIZER_LIST" =
      case ty of
        ArrayType (-1) target -> ArrayType (fromIntegral (length (childNodes initializer))) target
        _ -> ty
  | nodeName initializer == "INITIALIZER" =
      case (fieldNodeMaybe "value" initializer, ty) of
        (Just valueNode, ArrayType (-1) target)
          | nodeName valueNode == "STRING_LITERAL" ->
              ArrayType (fromIntegral (length (fieldStringDefault "value" "" valueNode)) + 1) target
        _ -> ty
  | otherwise = ty

checkBlock :: Node -> TypeM ()
checkBlock block = do
  blockId <- nextBlockId
  enterScope ("block_" <> show blockId)
  mapM_ checkStatement (childNodes block)
  exitScope

checkStatement :: Node -> TypeM ()
checkStatement statement = do
  child <- fieldNode "child" statement
  case nodeName child of
    "DECLARATION" -> buildDeclaration child
    "IF" -> do
      _ <- checkExpression =<< fieldNode "condition" child
      checkStatement =<< fieldNode "true_case" child
      maybe (pure ()) checkStatement (fieldNodeMaybe "false_case" child)
    "BLOCK" -> checkBlock child
    "FOR" -> checkFor child
    "WHILE" -> do
      _ <- checkExpression =<< fieldNode "condition" child
      checkStatement =<< fieldNode "statement" child
    "SWITCH" -> do
      _ <- checkExpression =<< fieldNode "condition" child
      checkBlock =<< fieldNode "block" child
    "CASE" -> do
      _ <- checkPrimaryExpression =<< fieldNode "value" child
      checkStatement =<< fieldNode "statement" child
    "DEFAULT" -> checkStatement =<< fieldNode "statement" child
    "RETURN" -> maybe (pure ()) (void . checkExpression) (fieldNodeMaybe "value" child)
    "EXPRESSION" -> void (checkExpression child)
    "ASM" -> checkAsm child
    _ -> pure ()

checkFor :: Node -> TypeM ()
checkFor node = do
  blockId <- nextBlockId
  enterScope ("for_loop_" <> show blockId)
  case fieldNodeMaybe "initialization" node of
    Just initialization
      | nodeName initialization == "DECLARATION" -> buildDeclaration initialization
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
  | nodeName initializer == "INITIALIZER_LIST" = do
      let children = childNodes initializer
      childTypes <- mapM (checkInitializerFor False (initializerElementType target)) children
      let ty = initializerListType target childTypes
      if recordList then recordType initializer ty else pure ty
  | nodeName initializer == "INITIALIZER" = do
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
  | nodeName node == "EXPRESSION" = do
      let children = childNodes node
      childTypes <- mapM checkAssignmentExpression children
      ty <- maybe (pure (base "VOID")) pure (lastMaybe childTypes)
      recordType node ty
  | otherwise = checkAssignmentExpression node

checkAssignmentExpression :: Node -> TypeM CType
checkAssignmentExpression node
  | nodeName node == "ASSIGNMENT" = do
      lhs <- fieldNode "lhs" node >>= checkTernaryExpression
      _ <- fieldNode "rhs" node >>= checkAssignmentExpression
      recordType node lhs
  | otherwise = checkTernaryExpression node

checkTernaryExpression :: Node -> TypeM CType
checkTernaryExpression node
  | nodeName node == "TERNARY" = do
      _ <- fieldNode "condition" node >>= checkLogicalOrExpression
      trueTy <- fieldNode "true_case" node >>= checkAssignmentExpression
      falseTy <- fieldNode "false_case" node >>= checkLogicalOrExpression
      unless (sameTypeChain falseTy trueTy True) $
        throw "Ternary false and true case types do not match"
      recordType node trueTy
  | otherwise = checkLogicalOrExpression node

checkLogicalOrExpression :: Node -> TypeM CType
checkLogicalOrExpression = checkOperandOnlyIntNode "LOGICAL_OR_EXPRESSION" checkLogicalAndExpression

checkLogicalAndExpression :: Node -> TypeM CType
checkLogicalAndExpression = checkOperandOnlyIntNode "LOGICAL_AND_EXPRESSION" checkInclusiveOrExpression

checkInclusiveOrExpression :: Node -> TypeM CType
checkInclusiveOrExpression = checkOperandOnlyIntNode "INCLUSIVE_OR_EXPRESSION" checkInclusiveXorExpression

checkInclusiveXorExpression :: Node -> TypeM CType
checkInclusiveXorExpression = checkOperandOnlyIntNode "INCLUSIVE_XOR_EXPRESSION" checkInclusiveAndExpression

checkInclusiveAndExpression :: Node -> TypeM CType
checkInclusiveAndExpression = checkOperandOnlyIntNode "INCLUSIVE_AND_EXPRESSION" checkEqualityExpression

checkEqualityExpression :: Node -> TypeM CType
checkEqualityExpression = checkBinaryIntNode "EQUALITY_EXPRESSION" checkRelationalExpression

checkRelationalExpression :: Node -> TypeM CType
checkRelationalExpression node
  | nodeName node == "RELATIONAL_EXPRESSION" = do
      mapM_ checkShiftExpression (childNodes node)
      recordType node (baseWithSigned "INT" True)
  | otherwise = checkShiftExpression node

checkShiftExpression :: Node -> TypeM CType
checkShiftExpression = checkBinaryIntNode "SHIFT_EXPRESSION" checkSumExpression

checkOperandOnlyIntNode :: String -> (Node -> TypeM CType) -> Node -> TypeM CType
checkOperandOnlyIntNode expected subChecker node
  | nodeName node == expected = do
      childTypes <- mapM subChecker (childNodes node)
      recordType node (intFromSignedness childTypes)
  | otherwise = subChecker node

checkBinaryIntNode :: String -> (Node -> TypeM CType) -> Node -> TypeM CType
checkBinaryIntNode expected subChecker node
  | nodeName node == expected = do
      childTypes <- mapM subChecker (childNodes node)
      recordType node (intFromSignedness childTypes)
  | otherwise = subChecker node

checkSumExpression :: Node -> TypeM CType
checkSumExpression node
  | nodeName node == "SUM_EXPRESSION" = do
      childTypes <- mapM checkTerm (childNodes node)
      let ty = fromMaybe (intFromSignedness childTypes) (firstPointer childTypes)
      recordType node ty
  | otherwise = checkTerm node

checkTerm :: Node -> TypeM CType
checkTerm node
  | nodeName node == "MULTIPLICATIVE_EXPRESSION" = do
      childTypes <- mapM checkCastExpression (childNodes node)
      recordType node (fromMaybe (base "INT") (firstMaybe childTypes))
  | otherwise = checkCastExpression node

checkCastExpression :: Node -> TypeM CType
checkCastExpression node
  | nodeName node == "CAST_EXPRESSION" = do
      typeName <- fieldNode "type_specifier" node
      baseTy <- checkTypeName typeName
      pointerLevel <- fieldInt "pointer_level" node
      _ <- fieldNode "cast_expression" node >>= checkCastExpression
      recordType node (applyPointers pointerLevel baseTy)
  | otherwise = checkUnaryExpression node

checkUnaryExpression :: Node -> TypeM CType
checkUnaryExpression node
  | nodeName node == "UNARY_EXPRESSION" = do
      op <- fieldString "operator" node
      child <- fieldNode "child" node
      childTy <-
        if nodeName child == "TYPE_NAME"
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
      Just params -> function baseTy <$> buildParameterList params
      Nothing -> pure baseTy
  let dimensions = map (fieldIntDefault "value" 0) (childNodes node)
      withArrays = foldr array withFunction dimensions
  case fieldNodeMaybe "declarator" node of
    Just nested -> buildAbstractDeclarator nested withArrays
    Nothing -> pure withArrays

checkPostfixExpression :: Node -> TypeM CType
checkPostfixExpression node
  | nodeName node == "POSTFIX_EXPRESSION" = do
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
          FunctionType ret params -> do
            maybe (pure ()) (`checkArgumentList` params) (postfixNodeValue op)
            pure ret
          _ -> pure ty
      "++" -> pure ty
      "--" -> pure ty
      "." -> pure (memberAccessType ty op)
      "->" -> pure (memberAccessType (dereferenceType ty) op)
      _ -> pure ty

checkArgumentList :: Node -> [CType] -> TypeM ()
checkArgumentList arguments params = do
  argumentTypes <- mapM checkAssignmentExpression (childNodes arguments)
  unless (length argumentTypes == length params) $
    throw "Argument list length does not match the parameter list length"
  forM_ (zip argumentTypes params) $ \(argTy, paramTy) ->
    unless (canCoerce argTy paramTy) $
      throw "Argument type does not match parameter type"

checkPrimaryExpression :: Node -> TypeM CType
checkPrimaryExpression node =
  case nodeName node of
    "INT" -> recordType node (baseWithSigned "INT" (not (hasBoolField "is_unsigned" node)))
    "IDENTIFIER" -> do
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
          modify' (\st -> st {includedStandardFunctionNames = includedStandardFunctionNames st <> [name]})
        _ -> pure ()
      recordType node decayed
    "STRING_LITERAL" -> recordType node (array (fromIntegral (length (fieldStringDefault "value" "" node)) + 1) (base "CHAR"))
    "CHARACTER" -> recordType node (base "CHAR")
    "EXPRESSION" -> checkExpression node
    _ -> throw ("Invalid primary expression: " <> nodeName node)

canCoerce :: CType -> CType -> Bool
canCoerce ty target =
  case (ty, target) of
    (PointerType a, PointerType b) -> canCoerce a b
    (ArrayType _ a, PointerType b) -> canCoerce a b
    (ArrayType la a, ArrayType lb b) -> (la <= lb || lb < 0) && canCoerce a b
    (BaseType {}, BaseType {}) -> True
    (StructType ida _, StructType idb _) -> ida == idb
    (UnionType ida _, UnionType idb _) -> ida == idb
    _ -> False

recordType :: Node -> CType -> TypeM CType
recordType node ty = do
  let key = typeKey node
  modify' (\st -> st {typedNodes = Map.insert key ty (typedNodes st)})
  pure ty

typeKey :: Node -> TypeKey
typeKey node = (nodeName node, row (nodePos node), col (nodePos node))

renderTypeAnnotations :: Map.Map TypeKey CType -> Node -> [String]
renderTypeAnnotations annotations = go
  where
    go node =
      maybe [] (\ty -> [renderTypeLine node ty]) (Map.lookup (typeKey node) annotations)
        <> concatMap goChild (nodeChildren node)
        <> concatMap goField (sortOn fieldName (nodeFields node))
    goChild child =
      case child of
        ChildNode node -> go node
        ChildPostfix op -> maybe [] go (postfixNodeValue op)
        ChildToken _ -> []
    goField field' =
      case fieldValue field' of
        NodeRef node -> go node
        NodeList nodes -> concatMap go nodes
        _ -> []

renderTypeLine :: Node -> CType -> String
renderTypeLine node ty =
  let pos = nodePos node
   in "TYPE\t" <> show (row pos) <> "\t" <> show (col pos) <> "\t" <> nodeName node <> "\t" <> renderTypePretty ty

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
          StructType _ members' -> lookupMember memberName' members'
          UnionType _ members' -> lookupMember memberName' members'
          _ -> Nothing
   in case memberTy of
        Just (ArrayType _ target) -> pointer target
        Just found -> found
        Nothing -> ty

lookupMember :: String -> [Member] -> Maybe CType
lookupMember name members' =
  firstJust [Just (memberType member) | member <- members', memberName member == name]

dereferenceType :: CType -> CType
dereferenceType ty =
  case ty of
    PointerType target -> target
    ArrayType _ target -> target
    _ -> ty

intFromSignedness :: [CType] -> CType
intFromSignedness types = baseWithSigned "INT" (any isSignedType types)

isSignedType :: CType -> Bool
isSignedType (BaseType _ signed) = signed
isSignedType _ = False

firstPointer :: [CType] -> Maybe CType
firstPointer types =
  firstJust [Just ty | ty@PointerType {} <- types]

fieldString :: String -> Node -> TypeM String
fieldString name node =
  case lookupField name node of
    Just (StringValue value) -> pure value
    _ -> throw ("missing string field '" <> name <> "' on " <> nodeName node)

firstMaybe :: [a] -> Maybe a
firstMaybe [] = Nothing
firstMaybe (value : _) = Just value

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [value] = Just value
lastMaybe (_ : values) = lastMaybe values

nextBlockId :: TypeM Integer
nextBlockId = do
  st <- get
  let current = blockCounter st
  modify' (\s -> s {blockCounter = current + 1})
  pure current

decayArrayParameter :: CType -> CType
decayArrayParameter ty =
  case ty of
    ArrayType _ target -> pointer target
    _ -> ty

applyPointers :: Integer -> CType -> CType
applyPointers count ty
  | count <= 0 = ty
  | otherwise = applyPointers (count - 1) (pointer ty)

isBaseSpecifiers :: [String] -> Bool
isBaseSpecifiers specifiers =
  case specifiers of
    ["void"] -> True
    ["char"] -> True
    ["int"] -> True
    ["long"] -> True
    ["signed", _] -> True
    ["unsigned", _] -> True
    ["SIGNED", _] -> True
    ["UNSIGNED", _] -> True
    _ -> False

enterScope :: String -> TypeM ()
enterScope name = do
  current <- currentScope
  let level = frameLevel current + 1
  modify' $
    \st ->
      st
        { scopeStack =
            ScopeFrame
              { frameLevel = level
              , frameName = name
              , frameOrdinary = Map.empty
              , frameTags = Map.empty
              }
              : scopeStack st
        , typeLog = typeLog st <> ["ENTER\t" <> show level <> "\t" <> name]
        }

exitScope :: TypeM ()
exitScope = do
  current <- currentScope
  when (frameLevel current == 0) (throw "cannot exit global scope")
  modify' $ \st ->
    case scopeStack st of
      [] -> st
      _ : rest ->
        st
          { scopeStack = rest
          , typeLog = typeLog st <> ["EXIT\t" <> show (frameLevel current) <> "\t" <> frameName current]
          }

addSymbol :: Namespace -> String -> Symbol -> TypeM ()
addSymbol namespace name symbol = do
  current <- currentScope
  when (Map.member name (namespaceMap namespace current)) $
    throw ("Symbol '" <> name <> "' has already been defined")
  setCurrentSymbol namespace name symbol
  current' <- currentScope
  appendLog
    ( "ADD\t"
        <> show (frameLevel current')
        <> "\t"
        <> frameName current'
        <> "\t"
        <> renderNamespace namespace
        <> "\t"
        <> name
        <> "\t"
        <> renderTypePretty (symbolType symbol)
    )

setCurrentSymbol :: Namespace -> String -> Symbol -> TypeM ()
setCurrentSymbol namespace name symbol =
  modify' $ \st ->
    case scopeStack st of
      [] -> st
      frame : rest -> st {scopeStack = setFrame namespace name symbol frame : rest}

lookupSymbol :: Namespace -> String -> TypeM (Maybe Symbol)
lookupSymbol namespace name = do
  scopes <- scopeStack <$> get
  pure (firstJust (map (Map.lookup name . namespaceMap namespace) scopes))

requireSymbol :: Namespace -> String -> TypeM Symbol
requireSymbol namespace name =
  lookupSymbol namespace name >>= maybe (throw ("missing symbol: " <> name)) pure

namespaceMap :: Namespace -> ScopeFrame -> Map.Map String Symbol
namespaceMap Ordinary = frameOrdinary
namespaceMap Tag = frameTags

setFrame :: Namespace -> String -> Symbol -> ScopeFrame -> ScopeFrame
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
currentScope = do
  scopes <- scopeStack <$> get
  case scopes of
    scope : _ -> pure scope
    [] -> throw "missing current scope"

appendLog :: String -> TypeM ()
appendLog line = modify' (\st -> st {typeLog = typeLog st <> [line]})

renderNamespace :: Namespace -> String
renderNamespace Ordinary = "o"
renderNamespace Tag = "t"

field :: String -> Node -> TypeM NodeValue
field name node =
  maybe (throw ("missing field '" <> name <> "' on " <> nodeName node)) pure (lookupField name node)

fieldNode :: String -> Node -> TypeM Node
fieldNode name node =
  case lookupField name node of
    Just (NodeRef child) -> pure child
    _ -> throw ("missing node field '" <> name <> "' on " <> nodeName node)

fieldNodeList :: String -> Node -> TypeM [Node]
fieldNodeList name node =
  case lookupField name node of
    Just (NodeList children) -> pure children
    _ -> throw ("missing node list field '" <> name <> "' on " <> nodeName node)

fieldInt :: String -> Node -> TypeM Integer
fieldInt name node =
  case lookupField name node of
    Just (IntValue value) -> pure value
    _ -> throw ("missing int field '" <> name <> "' on " <> nodeName node)

fieldInts :: String -> Node -> TypeM [Integer]
fieldInts name node =
  case lookupField name node of
    Just (IntList values) -> pure values
    _ -> throw ("missing int list field '" <> name <> "' on " <> nodeName node)

declaratorName :: Node -> String
declaratorName declarator =
  maybe "" identifierValue (fieldNodeMaybe "id" declarator)

requireName :: String -> Node -> TypeM ()
requireName expected node =
  when (nodeName node /= expected) $
    throw ("expected " <> expected <> ", got " <> nodeName node)

throw :: String -> TypeM a
throw = lift . Left
