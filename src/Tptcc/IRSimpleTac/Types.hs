module Tptcc.IRSimpleTac.Types
  ( CaseContext (..)
  , FunctionContext (..)
  , LocalInfo (..)
  , PostfixContext (..)
  , TacEffects
  , TacM
  , TacState (..)
  ) where

import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import Data.Text (Text)
import Effectful
import qualified Effectful.Error.Static as Error
import Effectful.State.Static.Local (State)

import Tptcc.CType (CType)
import Tptcc.Tac (Instr, MethodOutput, Place)

data LocalInfo = LocalInfo
  { localInfoPlace :: Place
  , localInfoPointerLevel :: Integer
  , localInfoDimensions :: [Integer]
  , localInfoPointerToArray :: Bool
  , localInfoType :: CType
  }
  deriving stock (Eq, Show)

data PostfixContext = PostfixContext
  { contextPointerLevel :: Integer
  , contextDimensions :: [Integer]
  , contextPointerToArray :: Bool
  , contextType :: Maybe CType
  }
  deriving stock (Eq, Show)

data CaseContext = CaseContext
  { caseEntries :: [(Place, Place)]
  , caseDefault :: Maybe Place
  }
  deriving stock (Eq, Show)

data FunctionContext = FunctionContext
  { contextMethodName :: Text
  , contextLocalOffset :: Integer
  , contextLocals :: Map.Map Text LocalInfo
  , contextInstructions :: Seq Instr
  , contextLoopLabels :: [(Place, Place)]
  , contextCaseLabels :: [CaseContext]
  }
  deriving stock (Eq, Show)

data TacState = TacState
  { localOffset :: Integer
  , globalOffset :: Integer
  , tempCounter :: Integer
  , labelCounter :: Integer
  , loopLabels :: [(Place, Place)]
  , caseLabels :: [CaseContext]
  , currentMethodName :: Text
  , functionPlaces :: Map.Map Text Place
  , functionReturns :: Map.Map Text Bool
  , enumConstants :: Map.Map Text Place
  , typeTags :: Map.Map Text CType
  , globals :: Map.Map Text LocalInfo
  , locals :: Map.Map Text LocalInfo
  , instructions :: Seq Instr
  , methods :: [MethodOutput]
  , breakpoints :: [Integer]
  , breakpointIndex :: Int
  }
  deriving stock (Eq, Show)

type TacM = Eff TacEffects

type TacEffects = '[State TacState, Error.Error String]
