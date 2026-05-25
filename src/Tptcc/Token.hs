module Tptcc.Token
  ( SourcePos (..)
  , Token (..)
  , TokenValue (..)
  ) where

data SourcePos = SourcePos
  { row :: !Int
  , col :: !Int
  }
  deriving (Eq, Show)

data TokenValue
  = ValueString String
  | ValueInt Integer
  deriving (Eq, Show)

data Token = Token
  { tokenTypeId :: !Int
  , tokenName :: String
  , tokenValue :: TokenValue
  , tokenPos :: SourcePos
  }
  deriving (Eq, Show)
