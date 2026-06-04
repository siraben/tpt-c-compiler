module Tptcc.Parser.Support
  ( addField
  , constantValue
  , directDeclaratorId
  , fieldByName
  , hasBlockField
  , hasBoolField
  , hasField
  , ifM
  , joinAsm
  , removeField
  , specifierTypeNode
  , storageClassKind
  , stringField
  , stringListField
  , stripQuotes
  , tokenInteger
  , tokenString
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

import Tptcc.Ast
import qualified Tptcc.NodeFields as NodeFields
import Tptcc.Token

tokenString :: Token -> Text
tokenString token =
  case tokenValue token of
    ValueString value -> value
    ValueInt value -> Text.pack (show value)

tokenInteger :: Token -> Integer
tokenInteger token =
  case tokenValue token of
    ValueInt value -> value
    ValueString value -> fromMaybe 0 (readMaybe (Text.unpack value))

constantValue :: Node -> Integer
constantValue = NodeFields.constantValue []

directDeclaratorId :: Node -> Node
directDeclaratorId node =
  case fieldByName "id" node of
    [NodeField _ (NodeRef identifier)] -> identifier
    _ -> node

storageClassKind :: Node -> Maybe Text
storageClassKind declarationSpecifier =
  case fieldByName "storage_class" declarationSpecifier of
    [NodeField _ (NodeRef storageClass)] -> stringField "kind" storageClass
    _ -> Nothing

specifierTypeNode :: Node -> Node
specifierTypeNode declarationSpecifier =
  case fieldByName "type_specifier" declarationSpecifier of
    [NodeField _ (NodeRef typeSpecifier)] -> typeSpecifier
    _ -> declarationSpecifier

hasBoolField :: FieldName -> Node -> Bool
hasBoolField name node =
  case fieldByName name node of
    [NodeField _ (BoolValue True)] -> True
    _ -> False

hasField :: FieldName -> Node -> Bool
hasField name node = not (null (fieldByName name node))

hasBlockField :: Node -> Bool
hasBlockField = hasField "block"

fieldByName :: FieldName -> Node -> [NodeField]
fieldByName name node = filter ((== name) . fieldName) (nodeFields node)

stringField :: FieldName -> Node -> Maybe Text
stringField name node =
  case fieldByName name node of
    [NodeField _ (StringValue value)] -> Just value
    _ -> Nothing

stringListField :: FieldName -> Node -> Maybe [Text]
stringListField name node =
  case fieldByName name node of
    [NodeField _ (StringList value)] -> Just value
    _ -> Nothing

stripQuotes :: Text -> Text
stripQuotes value =
  fromMaybe value $ do
    withoutLeading <- Text.stripPrefix "\"" value
    Text.stripSuffix "\"" withoutLeading

joinAsm :: [Text] -> Text
joinAsm = Text.intercalate "\n\t"

addField :: NodeField -> Node -> Node
addField field node = node {nodeFields = nodeFields node <> [field]}

removeField :: FieldName -> Node -> Node
removeField name node = node {nodeFields = filter ((/= name) . fieldName) (nodeFields node)}

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond ifTrue ifFalse = do
  result <- cond
  if result then ifTrue else ifFalse
