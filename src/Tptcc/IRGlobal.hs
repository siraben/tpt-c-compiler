module Tptcc.IRGlobal
  ( GlobalInfo (..)
  , dumpIRGlobals
  , generateIRGlobalInfo
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.List (sortOn)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import Data.Monoid (Endo (..), appEndo)
import Data.Text (Text)
import qualified Data.Text as Text
import Effectful
import qualified Effectful.Error.Static as Error
import qualified Effectful.Reader.Static as Reader
import qualified Effectful.State.Static.Local as State
import qualified Effectful.Writer.Static.Local as Writer

import Tptcc.Ast
import Tptcc.CType
import Tptcc.NodeFields
import Tptcc.NodeFields.Effectful
import Tptcc.IRGlobal.Types
import Tptcc.SymbolTable (defaultSymbols)
import Tptcc.Tac (Place (..), placeInteger)
import Tptcc.Token (SourcePos (..))

dumpIRGlobals :: Node -> Either String [String]
dumpIRGlobals ast = do
  (_, st, events) <- runIR (emitProgram ast)
  pure (renderIRGlobals st events)

generateIRGlobalInfo :: Node -> Either String GlobalInfo
generateIRGlobalInfo ast = do
  (_, st, _) <- runIR (emitProgram ast)
  pure
    GlobalInfo
      { globalInfoSize = globalOffset st
      , globalInfoData = globalData st
      }

runIR :: IRM a -> Either String (a, IRState, [String])
runIR action =
  case runPureEff (Error.runErrorNoCallStack (Writer.runWriter (State.runState initialState (Reader.runReader Nothing action)))) of
    Left err -> Left err
    Right ((value, st), events) -> Right (value, st, appEndo events [])

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

emitFunctionSymbol :: Text -> CType -> Bool -> IRM ()
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

emitObject :: Text -> CType -> Maybe Node -> IRM Place
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
  | nodeIs NodeInitializer initializer = do
      value <- fieldNode "value" initializer
      case nodeKind value of
        NodeInt -> registerGlobalWord (fieldIntDefault "value" 0 value) start
        NodeCharacter -> registerGlobalWord (fieldIntDefault "value" 0 value) start
        NodeStringLiteral ->
          if isCharPointer target
            then do
              stringPlace <- allocateGlobal (stringLength value)
              registerStringLiteral value stringPlace
              registerGlobalWord (placeInteger stringPlace + 1) start
            else registerStringLiteral value start
        _ -> pure ()
  | nodeIs NodeInitializerList initializer = do
      let children = childNodes initializer
      emitInitializerChildren target children start
  | otherwise = pure ()

emitInitializerChildren :: CType -> [Node] -> Place -> IRM ()
emitInitializerChildren target children =
  go children (initializerChildTypes target)
  where
    go [] _ _ = pure ()
    go _ [] _ = pure ()
    go (child : rest) (childTy : childTypes) place = do
      emitStaticInitializer childTy child place
      go rest childTypes place {placeValue = Text.pack (show (placeInteger place + sizeof childTy))}

registerStringLiteral :: Node -> Place -> IRM ()
registerStringLiteral node start = do
  let value = fieldStringDefault "value" "" node
      bytes = map (toInteger . fromEnum) (Text.unpack value) <> [0]
  forM_ (zip [placeInteger start ..] bytes) $ \(index, byte) ->
    setData index (Text.pack (show byte))

allocateStringLiteral :: Node -> IRM ()
allocateStringLiteral node = do
  place <- allocateGlobal (stringLength node)
  registerStringLiteral node place

registerGlobalWord :: Integer -> Place -> IRM ()
registerGlobalWord value place =
  when (placeType place == "g") $
    setData (placeInteger place) (Text.pack (show value))

allocateStatic :: Integer -> IRM Place
allocateStatic size = do
  method <- Reader.ask
  case method of
    Nothing -> allocateGlobal size
    Just name -> allocateStack name size

allocateGlobal :: Integer -> IRM Place
allocateGlobal size = do
  st <- State.get
  let offset = globalOffset st
  State.modify (\s -> s {globalOffset = offset + size})
  pure Place {placeType = "g", placeValue = Text.pack (show offset)}

allocateStack :: Text -> Integer -> IRM Place
allocateStack method size = do
  st <- State.get
  let offset = Map.findWithDefault 0 method (methodLocalSizes st)
  State.modify (\s -> s {methodLocalSizes = Map.insert method (offset + size) (methodLocalSizes s)})
  pure Place {placeType = "l", placeValue = Text.pack (show offset)}

allocateVR :: IRM Place
allocateVR = pure Place {placeType = "vr", placeValue = "vr"}

setData :: Integer -> Text -> IRM ()
setData index value =
  State.modify (\st -> st {globalData = Map.insert index value (globalData st)})

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
      | nodeIs NodeStructOrUnionSpecifier node -> resolveStructOrUnion node
      | nodeIs NodeEnumSpecifier node -> resolveEnum node
      | otherwise -> throw ("unexpected type specifier node: " <> nodeName node)
    _ -> throw "invalid type specifier kind"

resolveStructOrUnion :: Node -> IRM CType
resolveStructOrUnion node = do
  let typeName = maybe (if isStruct then "anon_struct" else "anon_union") identifierValue (fieldNodeMaybe "id" node)
      isStruct = boolFieldDefault "is_struct" False node
  case fieldNodeListMaybe "declaration" node of
    Just declarations -> do
      members' <- concat <$> mapM structDeclarationMembers declarations
      let membersWithOffsets = withMemberOffsets isStruct members'
          ty = if isStruct then struct typeName membersWithOffsets else typeName `union` membersWithOffsets
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
      forM_ (enumMemberValues members') $ \(memberNode, value) -> do
        memberId <- fieldNode "id" memberNode
        upsertOrdinary
          (identifierValue memberId)
          IRSymbol
            { irSymbolType = base "INT"
            , irSymbolPlace = Just Place {placeType = "i", placeValue = Text.pack (show value)}
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
      Just params -> buildFunctionType withArrays params
      Nothing -> pure withArrays
  case fieldNodeMaybe "declarator" direct of
    Just nested -> buildDeclarator nested withFunction
    Nothing -> pure withFunction

buildParameterList :: Node -> IRM [CType]
buildParameterList params =
  mapM buildParameter (childNodes params)

buildFunctionType :: CType -> Node -> IRM CType
buildFunctionType ret params = do
  parameterTys <- buildParameterList params
  pure $
    if hasBoolField "is_variadic" params
      then variadicFunction ret parameterTys
      else function ret parameterTys

buildParameter :: Node -> IRM CType
buildParameter parameter = do
  typeSpecifier <- fieldNode "type_specifier" parameter
  baseTy <- resolveTypeSpecifier typeSpecifier
  case fieldNodeMaybe "declarator" parameter of
    Just declarator -> decayArrayParameter <$> buildDeclarator declarator baseTy
    Nothing -> pure baseTy

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
              ArrayType (stringLength valueNode) target
        _ -> ty
  | otherwise = ty

initializerValueType :: CType -> Node -> CType
initializerValueType target initializer
  | nodeIs NodeInitializer initializer =
      case fieldNodeMaybe "value" initializer of
        Just value | nodeIs NodeStringLiteral value && not (isCharPointer target) -> array (stringLength value) (base "CHAR")
        _ -> target
  | otherwise = target

initializerChildTypes :: CType -> [CType]
initializerChildTypes ty =
  case ty of
    ArrayType _ target -> repeat target
    StructType _ members' -> map memberType members'
    UnionType _ (member : _) -> [memberType member]
    _ -> repeat ty

emitFunctionBody :: Text -> Node -> Node -> IRM ()
emitFunctionBody method declarator block = do
  State.modify (\st -> st {methodLocalSizes = Map.insert method 0 (methodLocalSizes st)})
  Reader.local (const (Just method)) $ do
    emitParameterPlaces method declarator
    emitBlockIR method block

emitParameterPlaces :: Text -> Node -> IRM ()
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
            else pure Place {placeType = "p", placeValue = Text.pack (show index)}
        appendPlaceEvent paramNode method "PARAM" name ty place

buildParameterWithName :: Node -> IRM (Node, Text, CType)
buildParameterWithName parameter = do
  typeSpecifier <- fieldNode "type_specifier" parameter
  baseTy <- resolveTypeSpecifier typeSpecifier
  case fieldNodeMaybe "declarator" parameter of
    Just declarator -> do
      ty <- decayArrayParameter <$> buildDeclarator declarator baseTy
      pure (parameter, declaratorName declarator, ty)
    Nothing -> pure (parameter, "", baseTy)

emitBlockIR :: Text -> Node -> IRM ()
emitBlockIR method block =
  mapM_ (emitStatementIR method) (childNodes block)

emitStatementIR :: Text -> Node -> IRM ()
emitStatementIR method statement = do
  child <- fieldNode "child" statement
  case nodeKind child of
    NodeDeclaration -> emitLocalDeclarationIR method child
    NodeIf -> do
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "condition" child)
      maybe (pure ()) (emitStatementIR method) (fieldNodeMaybe "true_case" child)
      maybe (pure ()) (emitStatementIR method) (fieldNodeMaybe "false_case" child)
    NodeBlock -> emitBlockIR method child
    NodeFor -> do
      maybe (pure ()) (emitForInitializationIR method) (fieldNodeMaybe "initialization" child)
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "condition" child)
      maybe (pure ()) (emitStatementIR method) (fieldNodeMaybe "statement" child)
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "update" child)
    NodeWhile -> do
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "condition" child)
      maybe (pure ()) (emitStatementIR method) (fieldNodeMaybe "statement" child)
    NodeDoWhile -> do
      maybe (pure ()) (emitStatementIR method) (fieldNodeMaybe "statement" child)
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "condition" child)
    NodeSwitch -> do
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "condition" child)
      maybe (pure ()) (emitBlockIR method) (fieldNodeMaybe "block" child)
    NodeCase -> do
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "value" child)
      maybe (pure ()) (emitStatementIR method) (fieldNodeMaybe "statement" child)
    NodeDefault -> maybe (pure ()) (emitStatementIR method) (fieldNodeMaybe "statement" child)
    NodeGoto -> pure ()
    NodeLabel -> maybe (pure ()) (emitStatementIR method) (fieldNodeMaybe "statement" child)
    NodeReturn -> maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "value" child)
    NodeExpression -> collectExpressionStrings child
    NodeAsm -> collectExpressionStrings child
    _ -> collectExpressionStrings child

emitForInitializationIR :: Text -> Node -> IRM ()
emitForInitializationIR method node
  | nodeIs NodeDeclaration node = emitLocalDeclarationIR method node
  | otherwise = collectExpressionStrings node

emitLocalDeclarationIR :: Text -> Node -> IRM ()
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
withGlobalMethod = Reader.local (const (Nothing :: Maybe Text))

appendPlaceEvent :: Node -> Text -> Text -> Text -> CType -> Place -> IRM ()
appendPlaceEvent node method kind name ty place = do
  let line =
        "PLACE\t"
          <> Text.pack (show (rowOf node))
          <> "\t"
          <> Text.pack (show (colOf node))
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
  Writer.tell (Endo (Text.unpack line :))

collectExpressionStrings :: Node -> IRM ()
collectExpressionStrings node =
  case nodeKind node of
    NodeStringLiteral -> allocateStringLiteral node
    NodeExpression -> mapM_ collectExpressionStrings (childNodes node)
    NodeAssignment -> do
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "lhs" node)
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "rhs" node)
    NodeTernary -> do
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "condition" node)
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "true_case" node)
      maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "false_case" node)
    NodeUnaryExpression -> maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "child" node)
    NodeCastExpression -> maybe (pure ()) collectExpressionStrings (fieldNodeMaybe "cast_expression" node)
    NodePostfixExpression -> do
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

renderIRGlobals :: IRState -> [String] -> [String]
renderIRGlobals st events =
  ["GLOBAL_SIZE\t" <> show (globalOffset st)]
    <> map renderData (Map.toAscList (globalData st))
    <> map renderSymbol (sortOn fst (filter isUserGlobal (Map.toList (ordinarySymbols st))))
    <> map renderMethod (Map.toAscList (methodLocalSizes st))
    <> events

renderData :: (Integer, Text) -> String
renderData (index, value) = "DATA\t" <> show index <> "\t" <> Text.unpack value

renderSymbol :: (Text, IRSymbol) -> String
renderSymbol (name, symbol) =
  "SYMBOL\t"
    <> Text.unpack name
    <> "\t"
    <> Text.unpack (renderTypePretty (irSymbolType symbol))
    <> "\t"
    <> maybe "" (Text.unpack . placeType) (irSymbolPlace symbol)
    <> "\t"
    <> maybe "" (Text.unpack . placeValue) (irSymbolPlace symbol)

renderMethod :: (Text, Integer) -> String
renderMethod (name, localSize) = "METHOD\t" <> Text.unpack name <> "\t" <> show localSize

isUserGlobal :: (Text, IRSymbol) -> Bool
isUserGlobal (name, symbol) =
  name `notElem` map fst defaultSymbols && isJust (irSymbolPlace symbol)

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
    Just value -> nodeIs NodeStringLiteral value
    Nothing -> False

stringLength :: Node -> Integer
stringLength node = fromIntegral (Text.length (fieldStringDefault "value" "" node)) + 1

normalizePlaceValue :: Place -> Text
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

lookupOrdinary :: Text -> IRM (Maybe IRSymbol)
lookupOrdinary name = Map.lookup name . ordinarySymbols <$> State.get

requireOrdinary :: Text -> IRM IRSymbol
requireOrdinary name =
  lookupOrdinary name >>= maybe (throw ("missing ordinary symbol: " <> name)) pure

requireTag :: Text -> IRM IRSymbol
requireTag name = do
  found <- Map.lookup name . tagSymbols <$> State.get
  maybe (throw ("missing tag symbol: " <> name)) pure found

upsertOrdinary :: Text -> IRSymbol -> IRM ()
upsertOrdinary name symbol =
  State.modify (\st -> st {ordinarySymbols = Map.insert name symbol (ordinarySymbols st)})

upsertTag :: Text -> IRSymbol -> IRM ()
upsertTag name symbol =
  State.modify (\st -> st {tagSymbols = Map.insert name symbol (tagSymbols st)})

throw :: Text -> IRM a
throw = Error.throwError_ . Text.unpack
