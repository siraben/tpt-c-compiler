module Tptcc.Operand
  ( Operand (..)
  , OperandValue (..)
  , renderOperandValue
  ) where

data OperandValue
  = OperandInt Integer
  | OperandName String
  deriving (Eq, Ord, Show)

data Operand = Operand
  { operandType :: String
  , operandValue :: OperandValue
  , operandOffset :: Maybe Operand
  , operandIsStandardFunction :: Bool
  , operandIsVariadic :: Maybe Bool
  }
  deriving (Eq, Show)

renderOperandValue :: OperandValue -> String
renderOperandValue (OperandInt value) = show value
renderOperandValue (OperandName value) = value
