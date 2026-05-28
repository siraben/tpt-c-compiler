module Tptcc.NodeFields
  ( boolFieldDefault
  , childNodes
  , constantValue
  , enumMemberValues
  , fieldIntDefault
  , fieldIntMaybe
  , fieldIntsDefault
  , fieldNodeListMaybe
  , fieldNodeMaybe
  , fieldStringDefault
  , firstJust
  , hasBoolField
  , hasNodeField
  , identifierValue
  , lookupField
  ) where

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Tptcc.Ast
import Tptcc.Token (tokenName)

childNodes :: Node -> [Node]
childNodes node = [child | ChildNode child <- nodeChildren node]

lookupField :: Text -> Node -> Maybe NodeValue
lookupField name node =
  listToMaybe [value | NodeField fieldName' value <- nodeFields node, fieldName' == name]

fieldNodeMaybe :: Text -> Node -> Maybe Node
fieldNodeMaybe name node =
  case lookupField name node of
    Just (NodeRef child) -> Just child
    _ -> Nothing

fieldNodeListMaybe :: Text -> Node -> Maybe [Node]
fieldNodeListMaybe name node =
  case lookupField name node of
    Just (NodeList children) -> Just children
    _ -> Nothing

fieldIntMaybe :: Text -> Node -> Maybe Integer
fieldIntMaybe name node =
  case lookupField name node of
    Just (IntValue value) -> Just value
    _ -> Nothing

fieldIntDefault :: Text -> Integer -> Node -> Integer
fieldIntDefault name fallback node =
  fromMaybe fallback (fieldIntMaybe name node)

fieldIntsDefault :: Text -> [Integer] -> Node -> [Integer]
fieldIntsDefault name fallback node =
  case lookupField name node of
    Just (IntList values) -> values
    _ -> fallback

fieldStringDefault :: Text -> Text -> Node -> Text
fieldStringDefault name fallback node =
  case lookupField name node of
    Just (StringValue value) -> value
    _ -> fallback

boolFieldDefault :: Text -> Bool -> Node -> Bool
boolFieldDefault name fallback node =
  case lookupField name node of
    Just (BoolValue value) -> value
    _ -> fallback

hasBoolField :: Text -> Node -> Bool
hasBoolField name = boolFieldDefault name False

hasNodeField :: Text -> Node -> Bool
hasNodeField name node =
  case lookupField name node of
    Just (NodeRef _) -> True
    _ -> False

identifierValue :: Node -> Text
identifierValue node = fieldStringDefault "id" (fieldStringDefault "value" "" node) node

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just value : _) = Just value
firstJust (Nothing : values) = firstJust values

enumMemberValues :: [Node] -> [(Node, Integer)]
enumMemberValues members =
  let (_, values, _) = foldl' assign (0, [], []) members
   in reverse values
  where
    assign (nextValue, values, env) member =
      let value = maybe nextValue (constantValue env) (fieldNodeMaybe "value" member)
          name = maybe "" identifierValue (fieldNodeMaybe "id" member)
       in (value + 1, (member, value) : values, (name, value) : env)

constantValue :: [(Text, Integer)] -> Node -> Integer
constantValue env node =
  case nodeKind node of
    NodeInt -> fieldIntDefault "value" 0 node
    NodeCharacter -> fieldIntDefault "value" 0 node
    NodeIdentifier -> fromMaybe 0 (lookup (identifierValue node) env)
    NodeExpression -> foldl' (\_ child -> constantValue env child) 0 (childNodes node)
    NodeUnaryExpression ->
      let value = maybe 0 (constantValue env) (fieldNodeMaybe "child" node)
       in case fieldStringDefault "operator" "" node of
            "+" -> value
            "-" -> negate value
            "~" -> 65535 - value
            "!" -> if value == 0 then 1 else 0
            "SIZEOF" -> 1
            _ -> value
    NodeCastExpression -> maybe 0 (constantValue env) (fieldNodeMaybe "cast_expression" node)
    NodeTernary ->
      if maybe 0 (constantValue env) (fieldNodeMaybe "condition" node) /= 0
        then maybe 0 (constantValue env) (fieldNodeMaybe "true_case" node)
        else maybe 0 (constantValue env) (fieldNodeMaybe "false_case" node)
    NodeLogicalOrExpression -> boolInt (any ((/= 0) . constantValue env) (childNodes node))
    NodeLogicalAndExpression -> boolInt (all ((/= 0) . constantValue env) (childNodes node))
    NodeInclusiveOrExpression -> foldl' integerBitOr 0 (map (constantValue env) (childNodes node))
    NodeInclusiveXorExpression -> foldl' xorInteger 0 (map (constantValue env) (childNodes node))
    NodeInclusiveAndExpression -> foldl1Safe integerBitAnd (map (constantValue env) (childNodes node))
    NodeEqualityExpression -> compareChain env node
    NodeRelationalExpression -> compareChain env node
    NodeShiftExpression -> evalInfix env node
    NodeSumExpression -> evalInfix env node
    NodeMultiplicativeExpression -> evalInfix env node
    _ -> 0

boolInt :: Bool -> Integer
boolInt True = 1
boolInt False = 0

evalInfix :: [(Text, Integer)] -> Node -> Integer
evalInfix env node =
  case nodeChildren node of
    ChildNode firstNode : rest -> go (constantValue env firstNode) rest
    _ -> 0
  where
    go acc [] = acc
    go acc (ChildToken op : ChildNode rhsNode : rest) =
      let rhs = constantValue env rhsNode
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

compareChain :: [(Text, Integer)] -> Node -> Integer
compareChain env node =
  case nodeChildren node of
    ChildNode firstNode : rest -> go (constantValue env firstNode) rest
    _ -> 0
  where
    go _ [] = 1
    go lhs (ChildToken op : ChildNode rhsNode : rest) =
      let rhs = constantValue env rhsNode
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

foldl1Safe :: (Integer -> Integer -> Integer) -> [Integer] -> Integer
foldl1Safe _ [] = 0
foldl1Safe f (value : values) = foldl' f value values

xorInteger :: Integer -> Integer -> Integer
xorInteger a b = integerBitOr a b - integerBitAnd a b

integerBitAnd :: Integer -> Integer -> Integer
integerBitAnd a b = sum [bit | bit <- bitValues, a `mod` (bit * 2) >= bit, b `mod` (bit * 2) >= bit]

integerBitOr :: Integer -> Integer -> Integer
integerBitOr a b = sum [bit | bit <- bitValues, a `mod` (bit * 2) >= bit || b `mod` (bit * 2) >= bit]

bitValues :: [Integer]
bitValues = take 16 (iterate (* 2) 1)
