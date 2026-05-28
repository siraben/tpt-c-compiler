module Tptcc.Tac
  ( Instr (..)
  , InstrType (..)
  , MethodOutput (..)
  , Place (..)
  , TacProgram (..)
  , fieldPlace
  , fieldString
  , instrMnemonic
  , isJumpInstruction
  , isVirtualRegister
  , placeInteger
  , placeIntegerMaybe
  , placeKey
  , renderPlace
  , textIntegerMaybe
  , uniqueInOrder
  , uniquePlaces
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead

data Place = Place
  { placeType :: Text
  , placeValue :: Text
  }
  deriving (Eq, Show)

data Instr = Instr
  { instrType :: InstrType
  , instrFields :: [(Text, Place)]
  , instrStringFields :: [(Text, Text)]
  }
  deriving (Eq, Show)

data InstrType
  = IAdd
  | IAdd3
  | IAnd
  | IAsm
  | ICall
  | ICmp
  | IDebugBreakpoint
  | IDebugFunctionCall
  | IGetAddress
  | IJa
  | IJae
  | IJb
  | IJbe
  | IJe
  | IJg
  | IJge
  | IJl
  | IJle
  | IJmp
  | IJne
  | ILabel
  | ILd
  | ILdOffset
  | IMov
  | IMulh
  | IMull
  | IMull3
  | INop
  | IOr
  | IPop
  | IPush
  | IRet
  | IShl
  | IShr
  | IShr3
  | ISt
  | ISub
  | ISub3
  | IXor
  deriving (Eq, Ord, Show)

data MethodOutput = MethodOutput
  { methodOutputName :: Text
  , methodOutputInstructions :: [Instr]
  , methodOutputLocalSize :: Integer
  }
  deriving (Eq, Show)

data TacProgram = TacProgram
  { tacProgramMethods :: [MethodOutput]
  , tacProgramGlobalSize :: Integer
  , tacProgramGlobalInstructions :: [Instr]
  }
  deriving (Eq, Show)

fieldPlace :: Text -> Instr -> Place
fieldPlace name instr =
  case lookup name (instrFields instr) of
    Just place -> place
    Nothing -> Place "i" "0"

fieldString :: Text -> Instr -> Text
fieldString name instr =
  fromMaybe "" (lookup name (instrStringFields instr))

instrMnemonic :: InstrType -> Text
instrMnemonic instr =
  case instr of
    IAdd -> "add"
    IAdd3 -> "add3"
    IAnd -> "and"
    IAsm -> "asm"
    ICall -> "call"
    ICmp -> "cmp"
    IDebugBreakpoint -> "!debug_breakpoint"
    IDebugFunctionCall -> "!debug_function_call"
    IGetAddress -> "!get_address"
    IJa -> "ja"
    IJae -> "jae"
    IJb -> "jb"
    IJbe -> "jbe"
    IJe -> "je"
    IJg -> "jg"
    IJge -> "jge"
    IJl -> "jl"
    IJle -> "jle"
    IJmp -> "jmp"
    IJne -> "jne"
    ILabel -> "label"
    ILd -> "ld"
    ILdOffset -> "ldoffset"
    IMov -> "mov"
    IMulh -> "mulh"
    IMull -> "mull"
    IMull3 -> "mull3"
    INop -> "nop"
    IOr -> "or"
    IPop -> "pop"
    IPush -> "push"
    IRet -> "ret"
    IShl -> "shl"
    IShr -> "shr"
    IShr3 -> "shr3"
    ISt -> "st"
    ISub -> "sub"
    ISub3 -> "sub3"
    IXor -> "xor"

isJumpInstruction :: InstrType -> Bool
isJumpInstruction instr =
  instr `elem` [IJa, IJae, IJb, IJbe, IJe, IJg, IJge, IJl, IJle, IJmp, IJne]

isVirtualRegister :: Place -> Bool
isVirtualRegister place = placeType place `elem` ["t", "pr", "vr"]

placeInteger :: Place -> Integer
placeInteger place =
  fromMaybe (error ("invalid integer place: " <> Text.unpack (renderPlace place))) $
    placeIntegerMaybe place

placeIntegerMaybe :: Place -> Maybe Integer
placeIntegerMaybe = textIntegerMaybe . placeValue

textIntegerMaybe :: Text -> Maybe Integer
textIntegerMaybe value =
  case TextRead.signed TextRead.decimal value of
    Right (number, rest) | Text.null rest -> Just number
    _ -> Nothing

placeKey :: Place -> Maybe (Text, Text)
placeKey place
  | isVirtualRegister place = Just (placeType place, placeValue place)
  | otherwise = Nothing

renderPlace :: Place -> Text
renderPlace place = placeType place <> ":" <> placeValue place

uniqueInOrder :: Ord a => [a] -> [a]
uniqueInOrder = go Set.empty
  where
    go _ [] = []
    go seen (value : rest)
      | value `Set.member` seen = go seen rest
      | otherwise = value : go (Set.insert value seen) rest

uniquePlaces :: [Place] -> [Place]
uniquePlaces = go Set.empty
  where
    key place = (placeType place, placeValue place)
    go _ [] = []
    go seen (place : rest)
      | key place `Set.member` seen = go seen rest
      | otherwise = place : go (Set.insert (key place) seen) rest
