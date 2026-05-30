module Tptcc.Operand
  ( Operand (..)
  , OperandValue (..)
  , renderOperandValue
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Tptcc.Tac (PlaceKind)

data OperandValue
  = OperandInt Integer
  | OperandName Text
  deriving (Eq, Ord, Show)

data Operand = Operand
  { operandKind :: PlaceKind
  , operandValue :: OperandValue
  , operandOffset :: Maybe Operand
  , operandIsStandardFunction :: Bool
  , operandIsVariadic :: Maybe Bool
  }
  deriving (Eq, Show)

renderOperandValue :: OperandValue -> Text
renderOperandValue (OperandInt value) = Text.pack (show value)
renderOperandValue (OperandName value) = value
