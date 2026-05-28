module Tptcc.IRGlobal.Types
  ( GlobalInfo (..)
  , IREffects
  , IRM
  , IRState (..)
  , IRSymbol (..)
  , Namespace (..)
  , initialState
  ) where

import qualified Data.Map.Strict as Map
import Data.Monoid (Endo)
import Data.Text (Text)
import Effectful
import qualified Effectful.Error.Static as Error
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Effectful.Writer.Static.Local (Writer)

import Tptcc.CType (CType)
import Tptcc.SymbolTable (Symbol (..), defaultSymbols)
import Tptcc.Tac (Place)

data Namespace = Ordinary | Tag
  deriving stock (Eq, Show)

data IRSymbol = IRSymbol
  { irSymbolType :: CType
  , irSymbolPlace :: Maybe Place
  , irSymbolPrototype :: Bool
  }
  deriving stock (Eq, Show)

data IRState = IRState
  { ordinarySymbols :: Map.Map Text IRSymbol
  , tagSymbols :: Map.Map Text IRSymbol
  , globalOffset :: Integer
  , globalData :: Map.Map Integer Text
  , methodLocalSizes :: Map.Map Text Integer
  }
  deriving stock (Eq, Show)

data GlobalInfo = GlobalInfo
  { globalInfoSize :: Integer
  , globalInfoData :: Map.Map Integer Text
  }
  deriving stock (Eq, Show)

type IRM = Eff IREffects

type IREffects = '[Reader (Maybe Text), State IRState, Writer (Endo [String]), Error.Error String]

initialState :: IRState
initialState =
  IRState
    { ordinarySymbols = Map.fromList [(name, fromDefault symbol) | (name, symbol) <- defaultSymbols]
    , tagSymbols = Map.empty
    , globalOffset = 0
    , globalData = Map.empty
    , methodLocalSizes = Map.empty
    }

fromDefault :: Symbol -> IRSymbol
fromDefault symbol =
  IRSymbol
    { irSymbolType = symbolType symbol
    , irSymbolPlace = Nothing
    , irSymbolPrototype = symbolIsPrototype symbol
    }
