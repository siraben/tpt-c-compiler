module Tptcc.TypeChecker.Types
  ( Namespace (..)
  , ScopeFrame (..)
  , TypeEffects
  , TypeKey (..)
  , TypeM
  , TypeState (..)
  , initialState
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Monoid (Endo)
import qualified Data.Set as Set
import Data.Text (Text)
import Effectful
import qualified Effectful.Error.Static as Error
import Effectful.State.Static.Local (State)
import Effectful.Writer.Static.Local (Writer)

import Tptcc.Ast (NodeKind)
import Tptcc.CType (CType)
import Tptcc.SymbolTable (Symbol, defaultSymbols)

data Namespace = Ordinary | Tag
  deriving stock (Eq, Show)

data ScopeFrame = ScopeFrame
  { frameLevel :: Int
  , frameName :: Text
  , frameOrdinary :: Map.Map Text Symbol
  , frameTags :: Map.Map Text Symbol
  }
  deriving stock (Eq, Show)

data TypeState = TypeState
  { scopeStack :: NonEmpty ScopeFrame
  , typedNodes :: Map.Map TypeKey CType
  , blockCounter :: Integer
  , includedStandardFunctionNames :: Set.Set Text
  }
  deriving stock (Eq, Show)

type TypeM = Eff TypeEffects

type TypeEffects = '[State TypeState, Writer (Endo [String]), Error.Error String]

data TypeKey = TypeKey NodeKind Int Int
  deriving stock (Eq, Ord, Show)

initialState :: TypeState
initialState =
  TypeState
    { scopeStack =
        ScopeFrame
          { frameLevel = 0
          , frameName = "global"
          , frameOrdinary = Map.fromList defaultSymbols
          , frameTags = Map.empty
          }
          :| []
    , typedNodes = Map.empty
    , blockCounter = 0
    , includedStandardFunctionNames = Set.empty
    }
