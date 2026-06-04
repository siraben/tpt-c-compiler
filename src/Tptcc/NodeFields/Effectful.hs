{-# LANGUAGE FlexibleContexts #-}

module Tptcc.NodeFields.Effectful
  ( field
  , fieldInt
  , fieldInts
  , fieldNode
  , fieldNodeList
  , fieldString
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Effectful
import qualified Effectful.Error.Static as Error

import Tptcc.Ast
import Tptcc.NodeFields (lookupField)

field :: Error.Error String :> es => FieldName -> Node -> Eff es NodeValue
field name node =
  maybe (throwMissing "field" name node) pure (lookupField name node)

fieldNode :: Error.Error String :> es => FieldName -> Node -> Eff es Node
fieldNode = typedField "node field" $ \case
  NodeRef child -> Just child
  _ -> Nothing

fieldNodeList :: Error.Error String :> es => FieldName -> Node -> Eff es [Node]
fieldNodeList = typedField "node list field" $ \case
  NodeList children -> Just children
  _ -> Nothing

fieldInt :: Error.Error String :> es => FieldName -> Node -> Eff es Integer
fieldInt = typedField "int field" $ \case
  IntValue value -> Just value
  _ -> Nothing

fieldInts :: Error.Error String :> es => FieldName -> Node -> Eff es [Integer]
fieldInts = typedField "int list field" $ \case
  IntList values -> Just values
  _ -> Nothing

fieldString :: Error.Error String :> es => FieldName -> Node -> Eff es Text
fieldString = typedField "string field" $ \case
  StringValue value -> Just value
  _ -> Nothing

typedField :: Error.Error String :> es => Text -> (NodeValue -> Maybe a) -> FieldName -> Node -> Eff es a
typedField label match name node =
  case lookupField name node >>= match of
    Just value -> pure value
    Nothing -> throwMissing label name node

throwMissing :: Error.Error String :> es => Text -> FieldName -> Node -> Eff es a
throwMissing label name node =
  Error.throwError_ $
    Text.unpack $
      "missing " <> label <> " '" <> fieldNameText name <> "' on " <> nodeName node
