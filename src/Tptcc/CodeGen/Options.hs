module Tptcc.CodeGen.Options
  ( CodeGenOptions (..)
  , defaultCodeGenOptions
  ) where

data CodeGenOptions = CodeGenOptions
  { codeGenMemorySize :: Integer
  , codeGenTermWidth :: Integer
  , codeGenTermHeight :: Integer
  , codeGenGlobalAddr :: Integer
  , codeGenBaseAddr :: Integer
  , codeGenBreakpoints :: [Integer]
  }
  deriving (Eq, Show)

defaultCodeGenOptions :: CodeGenOptions
defaultCodeGenOptions =
  CodeGenOptions
    { codeGenMemorySize = 2047
    , codeGenTermWidth = 12
    , codeGenTermHeight = 8
    , codeGenGlobalAddr = 1
    , codeGenBaseAddr = 0x9F80
    , codeGenBreakpoints = []
    }
