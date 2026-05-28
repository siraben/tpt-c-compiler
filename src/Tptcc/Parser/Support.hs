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
constantValue node =
  case nodeName node of
    "INT" -> intField "value" node
    "CHARACTER" -> intField "value" node
    "EXPRESSION" -> foldl' (\_ child -> constantValue child) 0 (childNodesLocal node)
    "UNARY_EXPRESSION" ->
      let child = nodeField "child" node
       in case stringFieldDefaultLocal "operator" "" node of
            "+" -> constantValue child
            "-" -> negate (constantValue child)
            "~" -> 65535 - constantValue child
            "!" -> if constantValue child == 0 then 1 else 0
            "SIZEOF" -> 1
            _ -> constantValue child
    "CAST_EXPRESSION" -> constantValue (nodeField "cast_expression" node)
    "TERNARY" ->
      if constantValue (nodeField "condition" node) /= 0
        then constantValue (nodeField "true_case" node)
        else constantValue (nodeField "false_case" node)
    "LOGICAL_OR_EXPRESSION" -> boolInt (any ((/= 0) . constantValue) (childNodesLocal node))
    "LOGICAL_AND_EXPRESSION" -> boolInt (all ((/= 0) . constantValue) (childNodesLocal node))
    "INCLUSIVE_OR_EXPRESSION" -> foldl' (.|.) 0 (map constantValue (childNodesLocal node))
    "INCLUSIVE_XOR_EXPRESSION" -> foldl' xorInteger 0 (map constantValue (childNodesLocal node))
    "INCLUSIVE_AND_EXPRESSION" -> foldl1Safe (.&.) (map constantValue (childNodesLocal node))
    "EQUALITY_EXPRESSION" -> compareChain node
    "RELATIONAL_EXPRESSION" -> compareChain node
    "SHIFT_EXPRESSION" -> evalInfix node
    "SUM_EXPRESSION" -> evalInfix node
    "MULTIPLICATIVE_EXPRESSION" -> evalInfix node
    _ -> 0
  where
    boolInt True = 1
    boolInt False = 0
    intField name n =
      case fieldByName name n of
        [NodeField _ (IntValue value)] -> value
        _ -> 0
    nodeField name n =
      case fieldByName name n of
        [NodeField _ (NodeRef value)] -> value
        _ -> n
    childNodesLocal n = [child | ChildNode child <- nodeChildren n]
    stringFieldDefaultLocal name fallback n =
      fromMaybe fallback (stringField name n)
    foldl1Safe _ [] = 0
    foldl1Safe f (value : values) = foldl' f value values
    xorInteger a b = (a .|. b) - (a .&. b)
    (.&.) = integerBitAnd
    (.|.) = integerBitOr

evalInfix :: Node -> Integer
evalInfix node =
  case nodeChildren node of
    ChildNode firstNode : rest -> go (constantValue firstNode) rest
    _ -> 0
  where
    go acc [] = acc
    go acc (ChildToken op : ChildNode rhsNode : rest) =
      let rhs = constantValue rhsNode
          next =
            case tokenName op of
              "+" -> acc + rhs
              "-" -> acc - rhs
              "*" -> acc * rhs
              "/" -> if rhs == 0 then 0 else acc `quot` rhs
              "%" -> if rhs == 0 then 0 else acc `rem` rhs
              "<<" -> acc * (2 ^ max 0 rhs)
              ">>" -> acc `quot` (2 ^ max 0 rhs)
              _ -> acc
       in go next rest
    go acc _ = acc

compareChain :: Node -> Integer
compareChain node =
  case nodeChildren node of
    ChildNode firstNode : rest -> go (constantValue firstNode) rest
    _ -> 0
  where
    go _ [] = 1
    go lhs (ChildToken op : ChildNode rhsNode : rest) =
      let rhs = constantValue rhsNode
          ok =
            case tokenName op of
              "==" -> lhs == rhs
              "!=" -> lhs /= rhs
              "<" -> lhs < rhs
              "<=" -> lhs <= rhs
              ">" -> lhs > rhs
              ">=" -> lhs >= rhs
              _ -> False
       in if ok then go rhs rest else 0
    go _ _ = 0

integerBitAnd :: Integer -> Integer -> Integer
integerBitAnd a b = sum [bit | bit <- bitValues, a `mod` (bit * 2) >= bit, b `mod` (bit * 2) >= bit]

integerBitOr :: Integer -> Integer -> Integer
integerBitOr a b = sum [bit | bit <- bitValues, a `mod` (bit * 2) >= bit || b `mod` (bit * 2) >= bit]

bitValues :: [Integer]
bitValues = take 16 (iterate (* 2) 1)

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
