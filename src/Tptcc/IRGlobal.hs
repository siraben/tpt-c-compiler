module Tptcc.IRGlobal
  ( GlobalInfo (..)
  , dumpIRGlobals
  , generateIRGlobalInfo
  ) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify', runStateT)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map.Strict as Map

import Tptcc.Ast
import Tptcc.CType
import Tptcc.NodeFields
import Tptcc.SymbolTable (Symbol (..), defaultSymbols)
import Tptcc.Tac (Place (..), placeInteger)
import Tptcc.Token (SourcePos (..))

data Namespace = Ordinary | Tag
  deriving (Eq, Show)

data IRSymbol = IRSymbol
  { irSymbolType :: CType
  , irSymbolPlace :: Maybe Place
  , irSymbolPrototype :: Bool
  }
  deriving (Eq, Show)

data IRState = IRState
  { ordinarySymbols :: Map.Map String IRSymbol
  , tagSymbols :: Map.Map String IRSymbol
  , globalOffset :: Integer
  , globalData :: Map.Map Integer String
  , currentMethod :: Maybe String
  , methodLocalSizes :: Map.Map String Integer
  , placeEvents :: [String]
  }
  deriving (Eq, Show)

data GlobalInfo = GlobalInfo
  { globalInfoSize :: Integer
  , globalInfoData :: Map.Map Integer String
  }
  deriving (Eq, Show)

type IRM = StateT IRState (Either String)

dumpIRGlobals :: Node -> Either String [String]
dumpIRGlobals ast = do
  (_, st) <- runStateT (emitProgram ast) initialState
  pure (renderIRGlobals st)

generateIRGlobalInfo :: Node -> Either String GlobalInfo
generateIRGlobalInfo ast = do
  (_, st) <- runStateT (emitProgram ast) initialState
  pure
    GlobalInfo
      { globalInfoSize = globalOffset st
      , globalInfoData = globalData st
      }

initialState :: IRState
initialState =
  IRState
    { ordinarySymbols = Map.fromList [(name, fromDefault symbol) | (name, symbol) <- defaultSymbols]
    , tagSymbols = Map.empty
    , globalOffset = 0
    , globalData = Map.empty
    , currentMethod = Nothing
    , methodLocalSizes = Map.empty
    , placeEvents = []
    }

fromDefault :: Symbol -> IRSymbol
fromDefault symbol =
  IRSymbol
    { irSymbolType = symbolType symbol
    , irSymbolPlace = Nothing
    , irSymbolPrototype = symbolIsPrototype symbol
    }

emitProgram :: Node -> IRM ()
emitProgram program = mapM_ emitDeclaration (childNodes program)

emitDeclaration :: Node -> IRM ()
emitDeclaration declaration = do
  specifier <- fieldNode "specifier" declaration
  storage <- fieldNode "storage_class" specifier
  let storageKind = fieldStringDefault "kind" "auto" storage
  typeSpecifier <- fieldNode "type_specifier" specifier
  baseTy <- resolveTypeSpecifier typeSpecifier
  let isTypedef = storageKind == "typedef"
  unless isTypedef $ do
    declarators <- fieldNodeList "declarators" declaration
    forM_ declarators $ \declarator -> do
      builtTy <- buildDeclarator declarator baseTy
      let declaredTy = maybe builtTy (`adjustFromInitializer` builtTy) (fieldNodeMaybe "initializer" declarator)
          name = declaratorName declarator
      if isFunctionType declaredTy
        then do
          emitFunctionSymbol name declaredTy (hasNodeField "block" declaration)
          maybe (pure ()) (emitFunctionBody name declarator) (fieldNodeMaybe "block" declaration)
        else do
          place <- emitObject storageKind declaredTy (fieldNodeMaybe "initializer" declarator)
          upsertOrdinary name IRSymbol {irSymbolType = declaredTy, irSymbolPlace = Just place, irSymbolPrototype = False}

emitFunctionSymbol :: String -> CType -> Bool -> IRM ()
emitFunctionSymbol name ty hasBlock = do
  let symbol =
        IRSymbol
          { irSymbolType = ty
          , irSymbolPlace = Just Place {placeType = "i", placeValue = "__tptcc_fn_" <> name}
          , irSymbolPrototype = not hasBlock
          }
  existing <- lookupOrdinary name
  case existing of
    Just old | irSymbolPrototype old -> upsertOrdinary name symbol
    Just _ | not hasBlock -> upsertOrdinary name symbol
    Just _ -> upsertOrdinary name symbol
    Nothing -> upsertOrdinary name symbol

emitObject :: String -> CType -> Maybe Node -> IRM Place
emitObject storageKind declaredTy initializer =
  case initializer of
    Just initNode -> do
      initPlace <-
        if initializerHasStringValue initNode
          then allocateGlobal (sizeof (initializerValueType declaredTy initNode))
          else
            if storageKind == "register" && not (isAggregate declaredTy)
              then allocateVR
              else allocateStatic (sizeof (initializerValueType declaredTy initNode))
      emitStaticInitializer declaredTy initNode initPlace
      pure initPlace
    Nothing ->
      if storageKind == "register" && not (isAggregate declaredTy)
        then allocateVR
        else allocateStatic (sizeof declaredTy)

emitStaticInitializer :: CType -> Node -> Place -> IRM ()
emitStaticInitializer target initializer start
  | nodeName initializer == "INITIALIZER" = do
      value <- fieldNode "value" initializer
      case nodeName value of
        "INT" -> registerGlobalWord (fieldIntDefault "value" 0 value) start
        "CHARACTER" -> registerGlobalWord (fieldIntDefault "value" 0 value) start
        "STRING_LITERAL" ->
          if isCharPointer target
            then do
              stringPlace <- allocateGlobal (stringLength value)
              registerStringLiteral value stringPlace
              registerGlobalWord (placeInteger stringPlace + 1) start
            else registerStringLiteral value start
        _ -> pure ()
  | nodeName initializer == "INITIALIZER_LIST" = do
      let children = childNodes initializer
      emitInitializerChildren target children start
  | otherwise = pure ()

emitInitializerChildren :: CType -> [Node] -> Place -> IRM ()
emitInitializerChildren _ [] _ = pure ()
emitInitializerChildren target (child : rest) start = do
  let childTy = initializerElementType target
  emitStaticInitializer childTy child start
  emitInitializerChildren target rest start {placeValue = show (placeInteger start + sizeof childTy)}

registerStringLiteral :: Node -> Place -> IRM ()
registerStringLiteral node start = do
  let value = fieldStringDefault "value" "" node
      bytes = map (toInteger . fromEnum) value <> [0]
  forM_ (zip [placeInteger start ..] bytes) $ \(index, byte) ->
    setData index (show byte)

allocateStringLiteral :: Node -> IRM ()
allocateStringLiteral node = do
  place <- allocateGlobal (stringLength node)
  registerStringLiteral node place

registerGlobalWord :: Integer -> Place -> IRM ()
registerGlobalWord value place =
  when (placeType place == "g") $
    setData (placeInteger place) (show value)

allocateStatic :: Integer -> IRM Place
allocateStatic size = do
  method <- currentMethod <$> get
  case method of
    Nothing -> allocateGlobal size
    Just name -> allocateStack name size

allocateGlobal :: Integer -> IRM Place
allocateGlobal size = do
  st <- get
  let offset = globalOffset st
  modify' (\s -> s {globalOffset = offset + size})
  pure Place {placeType = "g", placeValue = show offset}

allocateStack :: String -> Integer -> IRM Place
allocateStack method size = do
  st <- get
  let offset = Map.findWithDefault 0 method (methodLocalSizes st)
  modify' (\s -> s {methodLocalSizes = Map.insert method (offset + size) (methodLocalSizes s)})
  pure Place {placeType = "l", placeValue = show offset}

allocateVR :: IRM Place
allocateVR = pure Place {placeType = "vr", placeValue = "vr"}

setData :: Integer -> String -> IRM ()
setData index value =
  modify' (\st -> st {globalData = Map.insert index value (globalData st)})

resolveTypeSpecifier :: Node -> IRM CType
resolveTypeSpecifier typeSpecifier = do
  kind <- field "kind" typeSpecifier
  case kind of
    StringList specifiers ->
      if isBaseSpecifiers specifiers
        then pure (baseFromSpecifiers specifiers)
        else case specifiers of
          name : _ -> irSymbolType <$> requireOrdinary name
          [] -> throw "empty type specifier"
    NodeRef node
      | nodeName node == "STRUCT_OR_UNION_SPECIFIER" -> resolveStructOrUnion node
      | nodeName node == "ENUM_SPECIFIER" -> resolveEnum node
      | otherwise -> throw ("unexpected type specifier node: " <> nodeName node)
    _ -> throw "invalid type specifier kind"

resolveStructOrUnion :: Node -> IRM CType
resolveStructOrUnion node = do
  let typeName = maybe (if isStruct then "anon_struct" else "anon_union") identifierValue (fieldNodeMaybe "id" node)
      isStruct = boolFieldDefault "is_struct" False node
  case fieldNodeListMaybe "declaration" node of
    Just declarations -> do
      members' <- concat <$> mapM structDeclarationMembers declarations
      let ty = if isStruct then struct typeName members' else typeName `union` members'
      when (hasNodeField "id" node) $
        upsertTag typeName IRSymbol {irSymbolType = ty, irSymbolPlace = Nothing, irSymbolPrototype = False}
      pure ty
    Nothing -> irSymbolType <$> requireTag typeName

structDeclarationMembers :: Node -> IRM [Member]
structDeclarationMembers node = do
  typeSpecifier <- fieldNode "type_specifier" node
  memberBase <- resolveTypeSpecifier typeSpecifier
  forM (childNodes node) $ \declarator -> do
    memberTy <- buildDeclarator declarator memberBase
    pure Member {memberName = declaratorName declarator, memberType = memberTy, memberOffset = Nothing}

resolveEnum :: Node -> IRM CType
resolveEnum node = do
  identifier <- fieldNode "id" node
  let name = identifierValue identifier
  case fieldNodeMaybe "declaration" node of
    Just declaration -> do
      let members' = childNodes declaration
      memberNames <- mapM (fmap identifierValue . fieldNode "id") members'
      forM_ (zip [(0 :: Integer) ..] members') $ \(index, memberNode) -> do
        memberId <- fieldNode "id" memberNode
        let value = fromMaybe index (fieldIntMaybe "value" memberNode)
        upsertOrdinary
          (identifierValue memberId)
          IRSymbol
            { irSymbolType = base "INT"
            , irSymbolPlace = Just Place {placeType = "i", placeValue = show value}
            , irSymbolPrototype = False
            }
      upsertTag name IRSymbol {irSymbolType = enum name memberNames, irSymbolPlace = Nothing, irSymbolPrototype = False}
    Nothing -> pure ()
  pure (base "INT")

buildDeclarator :: Node -> CType -> IRM CType
buildDeclarator declarator baseTy = do
  pointerLevel <- fieldInt "pointer_level" declarator
  direct <- fieldNode "direct_declarator" declarator
  buildDirectDeclarator direct (applyPointers pointerLevel baseTy)

buildDirectDeclarator :: Node -> CType -> IRM CType
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

buildParameterList :: Node -> IRM [CType]
buildParameterList params =
  mapM buildParameter (childNodes params)

buildParameter :: Node -> IRM CType
buildParameter parameter = do
  typeSpecifier <- fieldNode "type_specifier" parameter
  baseTy <- resolveTypeSpecifier typeSpecifier
  case fieldNodeMaybe "declarator" parameter of
    Just declarator -> decayArrayParameter <$> buildDeclarator declarator baseTy
    Nothing -> pure baseTy

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
              ArrayType (stringLength valueNode) target
        _ -> ty
  | otherwise = ty

initializerValueType :: CType -> Node -> CType
initializerValueType target initializer
  | nodeName initializer == "INITIALIZER" =
      case fieldNodeMaybe "value" initializer of
        Just value | nodeName value == "STRING_LITERAL" && not (isCharPointer target) -> array (stringLength value) (base "CHAR")
        _ -> target
  | otherwise = target

initializerElementType :: CType -> CType
initializerElementType ty =
  case ty of
    ArrayType _ target -> target
    StructType _ (member : _) -> memberType member
    UnionType _ (member : _) -> memberType member
    _ -> ty

sizeof :: CType -> Integer
sizeof ty =
  case ty of
    ArrayType len target -> len * sizeof target
    StructType _ members' -> sum (map (sizeof . memberType) members')
    UnionType _ members' -> maximum (0 : map (sizeof . memberType) members')
    _ -> 1

emitFunctionBody :: String -> Node -> Node -> IRM ()
emitFunctionBody method declarator block = do
  modify' (\st -> st {currentMethod = Just method, methodLocalSizes = Map.insert method 0 (methodLocalSizes st)})
  emitParameterPlaces method declarator
  emitBlockIR method block
  modify' (\st -> st {currentMethod = Nothing})

emitParameterPlaces :: String -> Node -> IRM ()
emitParameterPlaces method declarator = do
  direct <- fieldNode "direct_declarator" declarator
  case fieldNodeMaybe "parameter_list" direct of
    Nothing -> pure ()
    Just params -> do
      builtParams <- mapM buildParameterWithName (childNodes params)
      forM_ (zip [(0 :: Integer) ..] builtParams) $ \(index, (paramNode, name, ty)) -> do
        place <-
          if isAggregate ty
            then allocateStack method (sizeof ty)
            else pure Place {placeType = "p", placeValue = show index}
        appendPlaceEvent paramNode method "PARAM" name ty place

buildParameterWithName :: Node -> IRM (Node, String, CType)
buildParameterWithName parameter = do
  typeSpecifier <- fieldNode "type_specifier" parameter
  baseTy <- resolveTypeSpecifier typeSpecifier
  case fieldNodeMaybe "declarator" parameter of
    Just declarator -> do
      ty <- decayArrayParameter <$> buildDeclarator declarator baseTy
      pure (parameter, declaratorName declarator, ty)
    Nothing -> pure (parameter, "", baseTy)

emitBlockIR :: String -> Node -> IRM ()
emitBlockIR method block =
  mapM_ (emitStatementIR method) (childNodes block)

emitStatementIR :: String -> Node -> IRM ()
emitStatementIR method statement = do
  child <- fieldNode "child" statement
  case nodeName child of
    "DECLARATION" -> emitLocalDeclarationIR method child
    "IF" -> do
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "condition" child)
      maybe (pure ()) (emitStatementIR method) (fieldNodeMaybe "true_case" child)
      maybe (pure ()) (emitStatementIR method) (fieldNodeMaybe "false_case" child)
    "BLOCK" -> emitBlockIR method child
    "FOR" -> do
      maybe (pure ()) (emitForInitializationIR method) (fieldNodeMaybe "initialization" child)
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "condition" child)
      maybe (pure ()) (emitStatementIR method) (fieldNodeMaybe "statement" child)
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "update" child)
    "WHILE" -> do
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "condition" child)
      maybe (pure ()) (emitStatementIR method) (fieldNodeMaybe "statement" child)
    "DO_WHILE" -> do
      maybe (pure ()) (emitStatementIR method) (fieldNodeMaybe "statement" child)
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "condition" child)
    "SWITCH" -> do
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "condition" child)
      maybe (pure ()) (emitBlockIR method) (fieldNodeMaybe "block" child)
    "CASE" -> do
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "value" child)
      maybe (pure ()) (emitStatementIR method) (fieldNodeMaybe "statement" child)
    "DEFAULT" -> maybe (pure ()) (emitStatementIR method) (fieldNodeMaybe "statement" child)
    "RETURN" -> maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "value" child)
    "EXPRESSION" -> collectExpressionStrings child
    _ -> pure ()

emitForInitializationIR :: String -> Node -> IRM ()
emitForInitializationIR method node
  | nodeName node == "DECLARATION" = emitLocalDeclarationIR method node
  | otherwise = collectExpressionStrings node

emitLocalDeclarationIR :: String -> Node -> IRM ()
emitLocalDeclarationIR method declaration = do
  specifier <- fieldNode "specifier" declaration
  storage <- fieldNode "storage_class" specifier
  let storageKind = fieldStringDefault "kind" "auto" storage
  typeSpecifier <- fieldNode "type_specifier" specifier
  baseTy <- resolveTypeSpecifier typeSpecifier
  declarators <- fieldNodeList "declarators" declaration
  if storageKind == "static"
    then withGlobalMethod (emitDeclaration declaration)
    else
      forM_ declarators $ \declarator -> do
        builtTy <- buildDeclarator declarator baseTy
        let declaredTy = maybe builtTy (`adjustFromInitializer` builtTy) (fieldNodeMaybe "initializer" declarator)
        place <- emitObject storageKind declaredTy (fieldNodeMaybe "initializer" declarator)
        appendPlaceEvent declarator method "LOCAL" (declaratorName declarator) declaredTy place

withGlobalMethod :: IRM () -> IRM ()
withGlobalMethod action = do
  previous <- currentMethod <$> get
  modify' (\st -> st {currentMethod = Nothing})
  action
  modify' (\st -> st {currentMethod = previous})

appendPlaceEvent :: Node -> String -> String -> String -> CType -> Place -> IRM ()
appendPlaceEvent node method kind name ty place = do
  let line =
        "PLACE\t"
          <> show (rowOf node)
          <> "\t"
          <> show (colOf node)
          <> "\t"
          <> method
          <> "\t"
          <> kind
          <> "\t"
          <> name
          <> "\t"
          <> renderTypePretty ty
          <> "\t"
          <> placeType place
          <> "\t"
          <> normalizePlaceValue place
  modify' (\st -> st {placeEvents = placeEvents st <> [line]})

collectExpressionStrings :: Node -> IRM ()
collectExpressionStrings node =
  case nodeName node of
    "STRING_LITERAL" -> allocateStringLiteral node
    "EXPRESSION" -> mapM_ collectExpressionStrings (childNodes node)
    "ASSIGNMENT" -> do
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "lhs" node)
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "rhs" node)
    "TERNARY" -> do
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "condition" node)
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "true_case" node)
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "false_case" node)
    "UNARY_EXPRESSION" -> maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "child" node)
    "CAST_EXPRESSION" -> maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "cast_expression" node)
    "POSTFIX_EXPRESSION" -> do
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "primary_expression" node)
      mapM_ collectPostfixStrings (postfixOps node)
    _ -> do
      mapM_ collectNodeChildStrings (nodeChildren node)
      mapM_ collectFieldStrings (nodeFields node)

collectPostfixStrings :: PostfixOp -> IRM ()
collectPostfixStrings op =
  case postfixValue op of
    Just (NodeRef node) -> collectExpressionStrings node
    Just (NodeList nodes) -> mapM_ collectExpressionStrings nodes
    _ -> pure ()

collectNodeChildStrings :: NodeChild -> IRM ()
collectNodeChildStrings child =
  case child of
    ChildNode node -> collectExpressionStrings node
    ChildPostfix op -> collectPostfixStrings op
    ChildToken _ -> pure ()

collectFieldStrings :: NodeField -> IRM ()
collectFieldStrings field' =
  case fieldValue field' of
    NodeRef node -> collectExpressionStrings node
    NodeList nodes -> mapM_ collectExpressionStrings nodes
    _ -> pure ()

postfixOps :: Node -> [PostfixOp]
postfixOps node = [op | ChildPostfix op <- nodeChildren node]

renderIRGlobals :: IRState -> [String]
renderIRGlobals st =
  ["GLOBAL_SIZE\t" <> show (globalOffset st)]
    <> map renderData (Map.toAscList (globalData st))
    <> map renderSymbol (sortOn fst (filter isUserGlobal (Map.toList (ordinarySymbols st))))
    <> map renderMethod (Map.toAscList (methodLocalSizes st))
    <> placeEvents st

renderData :: (Integer, String) -> String
renderData (index, value) = "DATA\t" <> show index <> "\t" <> value

renderSymbol :: (String, IRSymbol) -> String
renderSymbol (name, symbol) =
  "SYMBOL\t"
    <> name
    <> "\t"
    <> renderTypePretty (irSymbolType symbol)
    <> "\t"
    <> maybe "" placeType (irSymbolPlace symbol)
    <> "\t"
    <> maybe "" placeValue (irSymbolPlace symbol)

renderMethod :: (String, Integer) -> String
renderMethod (name, localSize) = "METHOD\t" <> name <> "\t" <> show localSize

isUserGlobal :: (String, IRSymbol) -> Bool
isUserGlobal (name, symbol) =
  name `notElem` map fst defaultSymbols && isJust (irSymbolPlace symbol)

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

isFunctionType :: CType -> Bool
isFunctionType FunctionType {} = True
isFunctionType _ = False

isAggregate :: CType -> Bool
isAggregate ArrayType {} = True
isAggregate StructType {} = True
isAggregate UnionType {} = True
isAggregate _ = False

isCharPointer :: CType -> Bool
isCharPointer (PointerType (BaseType Char _)) = True
isCharPointer _ = False

initializerHasStringValue :: Node -> Bool
initializerHasStringValue node =
  case fieldNodeMaybe "value" node of
    Just value -> nodeName value == "STRING_LITERAL"
    Nothing -> False

stringLength :: Node -> Integer
stringLength node = fromIntegral (length (fieldStringDefault "value" "" node)) + 1

normalizePlaceValue :: Place -> String
normalizePlaceValue place
  | placeType place == "vr" = "vr"
  | otherwise = placeValue place

rowOf :: Node -> Int
rowOf = row . nodePos

colOf :: Node -> Int
colOf = col . nodePos

applyPointers :: Integer -> CType -> CType
applyPointers count ty
  | count <= 0 = ty
  | otherwise = applyPointers (count - 1) (pointer ty)

decayArrayParameter :: CType -> CType
decayArrayParameter (ArrayType _ target) = pointer target
decayArrayParameter ty = ty

lookupOrdinary :: String -> IRM (Maybe IRSymbol)
lookupOrdinary name = Map.lookup name . ordinarySymbols <$> get

requireOrdinary :: String -> IRM IRSymbol
requireOrdinary name =
  lookupOrdinary name >>= maybe (throw ("missing ordinary symbol: " <> name)) pure

requireTag :: String -> IRM IRSymbol
requireTag name = do
  found <- Map.lookup name . tagSymbols <$> get
  maybe (throw ("missing tag symbol: " <> name)) pure found

upsertOrdinary :: String -> IRSymbol -> IRM ()
upsertOrdinary name symbol =
  modify' (\st -> st {ordinarySymbols = Map.insert name symbol (ordinarySymbols st)})

upsertTag :: String -> IRSymbol -> IRM ()
upsertTag name symbol =
  modify' (\st -> st {tagSymbols = Map.insert name symbol (tagSymbols st)})

field :: String -> Node -> IRM NodeValue
field name node =
  maybe (throw ("missing field '" <> name <> "' on " <> nodeName node)) pure (lookupField name node)

fieldNode :: String -> Node -> IRM Node
fieldNode name node =
  case lookupField name node of
    Just (NodeRef child) -> pure child
    _ -> throw ("missing node field '" <> name <> "' on " <> nodeName node)

fieldNodeList :: String -> Node -> IRM [Node]
fieldNodeList name node =
  case lookupField name node of
    Just (NodeList children) -> pure children
    _ -> throw ("missing node list field '" <> name <> "' on " <> nodeName node)

fieldInt :: String -> Node -> IRM Integer
fieldInt name node =
  case lookupField name node of
    Just (IntValue value) -> pure value
    _ -> throw ("missing int field '" <> name <> "' on " <> nodeName node)

fieldInts :: String -> Node -> IRM [Integer]
fieldInts name node =
  case lookupField name node of
    Just (IntList values) -> pure values
    _ -> throw ("missing int list field '" <> name <> "' on " <> nodeName node)

declaratorName :: Node -> String
declaratorName declarator =
  maybe "" identifierValue (fieldNodeMaybe "id" declarator)

throw :: String -> IRM a
throw = lift . Left
