module Tptcc.NodeFields
  ( boolFieldDefault
  , childNodes
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

import Tptcc.Ast
import Data.Maybe (fromMaybe)

childNodes :: Node -> [Node]
childNodes node = [child | ChildNode child <- nodeChildren node]

lookupField :: String -> Node -> Maybe NodeValue
lookupField name node =
  firstJust [Just value | NodeField fieldName' value <- nodeFields node, fieldName' == name]

fieldNodeMaybe :: String -> Node -> Maybe Node
fieldNodeMaybe name node =
  case lookupField name node of
    Just (NodeRef child) -> Just child
    _ -> Nothing

fieldNodeListMaybe :: String -> Node -> Maybe [Node]
fieldNodeListMaybe name node =
  case lookupField name node of
    Just (NodeList children) -> Just children
    _ -> Nothing

fieldIntMaybe :: String -> Node -> Maybe Integer
fieldIntMaybe name node =
  case lookupField name node of
    Just (IntValue value) -> Just value
    _ -> Nothing

fieldIntDefault :: String -> Integer -> Node -> Integer
fieldIntDefault name fallback node =
  fromMaybe fallback (fieldIntMaybe name node)

fieldIntsDefault :: String -> [Integer] -> Node -> [Integer]
fieldIntsDefault name fallback node =
  case lookupField name node of
    Just (IntList values) -> values
    _ -> fallback

fieldStringDefault :: String -> String -> Node -> String
fieldStringDefault name fallback node =
  case lookupField name node of
    Just (StringValue value) -> value
    _ -> fallback

boolFieldDefault :: String -> Bool -> Node -> Bool
boolFieldDefault name fallback node =
  case lookupField name node of
    Just (BoolValue value) -> value
    _ -> fallback

hasBoolField :: String -> Node -> Bool
hasBoolField name = boolFieldDefault name False

hasNodeField :: String -> Node -> Bool
hasNodeField name node =
  case lookupField name node of
    Just (NodeRef _) -> True
    _ -> False

identifierValue :: Node -> String
identifierValue node = fieldStringDefault "id" (fieldStringDefault "value" "" node) node

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just value : _) = Just value
firstJust (Nothing : values) = firstJust values

enumMemberValues :: [Node] -> [(Node, Integer)]
enumMemberValues =
  reverse . snd . foldl assign (0, [])
  where
    assign (nextValue, values) member =
      let value = fieldIntDefault "value" nextValue member
       in (value + 1, (member, value) : values)
