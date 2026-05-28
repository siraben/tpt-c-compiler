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
fieldNode name node =
  case lookupField name node of
    Just (NodeRef child) -> pure child
    _ -> throwMissing "node field" name node

fieldNodeList :: Error.Error String :> es => FieldName -> Node -> Eff es [Node]
fieldNodeList name node =
  case lookupField name node of
    Just (NodeList children) -> pure children
    _ -> throwMissing "node list field" name node

fieldInt :: Error.Error String :> es => FieldName -> Node -> Eff es Integer
fieldInt name node =
  case lookupField name node of
    Just (IntValue value) -> pure value
    _ -> throwMissing "int field" name node

fieldInts :: Error.Error String :> es => FieldName -> Node -> Eff es [Integer]
fieldInts name node =
  case lookupField name node of
    Just (IntList values) -> pure values
    _ -> throwMissing "int list field" name node

fieldString :: Error.Error String :> es => FieldName -> Node -> Eff es Text
fieldString name node =
  case lookupField name node of
    Just (StringValue value) -> pure value
    _ -> throwMissing "string field" name node

throwMissing :: Error.Error String :> es => Text -> FieldName -> Node -> Eff es a
throwMissing label name node =
  Error.throwError_ $
    Text.unpack $
      "missing " <> label <> " '" <> fieldNameText name <> "' on " <> nodeName node
