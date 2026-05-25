module Tptcc.Token
  ( SourcePos (..)
  , Token (..)
  , TokenValue (..)
  ) where

data SourcePos = SourcePos
  { row :: !Int
  , col :: !Int
  }
  deriving (Eq, Ord, Show)

data TokenValue
  = ValueString String
  | ValueInt Integer
  deriving (Eq, Ord, Show)

data Token = Token
  { tokenTypeId :: !Int
  , tokenName :: String
  , tokenValue :: TokenValue
  , tokenPos :: SourcePos
  }
  deriving (Eq, Ord, Show)
