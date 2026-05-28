module Tptcc.Token
  ( SourcePos (..)
  , Token (..)
  , TokenValue (..)
  ) where

import Data.Text (Text)

data SourcePos = SourcePos
  { row :: !Int
  , col :: !Int
  }
  deriving (Eq, Ord, Show)

data TokenValue
  = ValueString Text
  | ValueInt Integer
  deriving (Eq, Ord, Show)

data Token = Token
  { tokenTypeId :: !Int
  , tokenName :: Text
  , tokenValue :: TokenValue
  , tokenPos :: SourcePos
  }
  deriving (Eq, Ord, Show)
