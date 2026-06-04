module Tptcc.Tac
  ( Instr (..)
  , InstrFieldName (..)
  , InstrType (..)
  , MethodOutput (..)
  , Place
  , PlaceKind (..)
  , SpecialRegister (..)
  , TacProgram (..)
  , fieldPlace
  , fieldString
  , globalPlace
  , immediateInteger
  , immediateText
  , instrFieldInputName
  , instrFieldIsInputName
  , instrFieldNameText
  , instrAcceptsImmediate
  , instrDefFieldNames
  , instrIsBinary
  , instrMnemonic
  , instrPureRegisterDefinition
  , instrUseFieldNames
  , isJumpInstruction
  , isVirtualRegister
  , localPlace
  , numberedPlace
  , parameterPlace
  , placeKind
  , placeKindCode
  , placeValue
  , placeInteger
  , placeIntegerMaybe
  , placeKey
  , pointerRegisterPlace
  , registerNumber
  , registerText
  , renderPlace
  , specialRegister
  , temporaryPlace
  , textIntegerMaybe
  , uniqueInOrder
  , uniquePlaces
  , virtualRegisterPlace
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead

data Place = Place
  { placeKind :: PlaceKind
  , placeValue :: Text
  }
  deriving stock (Eq, Ord, Show)

data PlaceKind
  = Immediate
  | Register
  | Global
  | Local
  | Parameter
  | Temporary
  | PointerRegister
  | VirtualRegister
  deriving stock (Eq, Ord, Show)

data SpecialRegister
  = ReturnReg
  | StackPointer
  | BasePointer
  | TermReg
  | ReturnAddrReg
  deriving stock (Eq, Ord, Show)

immediateText :: Text -> Place
immediateText = Place Immediate

immediateInteger :: Integer -> Place
immediateInteger = numericPlace Immediate

registerNumber :: Integer -> Place
registerNumber = numericPlace Register

registerText :: Text -> Place
registerText value =
  case Text.stripPrefix "r" value >>= textIntegerMaybe of
    Just number -> registerNumber number
    Nothing -> Place Register value

specialRegister :: SpecialRegister -> Place
specialRegister reg =
  Place Register $
    case reg of
      ReturnReg -> "return_reg"
      StackPointer -> "stack_pointer"
      BasePointer -> "base_pointer"
      TermReg -> "term_reg"
      ReturnAddrReg -> "return_addr_reg"

globalPlace :: Integer -> Place
globalPlace = numericPlace Global

localPlace :: Integer -> Place
localPlace = numericPlace Local

parameterPlace :: Integer -> Place
parameterPlace = numericPlace Parameter

temporaryPlace :: Integer -> Place
temporaryPlace = numericPlace Temporary

pointerRegisterPlace :: Integer -> Place
pointerRegisterPlace = numericPlace PointerRegister

virtualRegisterPlace :: Integer -> Place
virtualRegisterPlace = numericPlace VirtualRegister

numericPlace :: PlaceKind -> Integer -> Place
numericPlace kind = Place kind . Text.pack . show

numberedPlace :: PlaceKind -> Integer -> Place
numberedPlace kind =
  case kind of
    Immediate -> immediateInteger
    Register -> registerNumber
    Global -> globalPlace
    Local -> localPlace
    Parameter -> parameterPlace
    Temporary -> temporaryPlace
    PointerRegister -> pointerRegisterPlace
    VirtualRegister -> virtualRegisterPlace

placeKindCode :: PlaceKind -> Text
placeKindCode kind =
  case kind of
    Immediate -> "i"
    Register -> "r"
    Global -> "g"
    Local -> "l"
    Parameter -> "p"
    Temporary -> "t"
    PointerRegister -> "pr"
    VirtualRegister -> "vr"

data Instr = Instr
  { instrType :: InstrType
  , instrFields :: [(InstrFieldName, Place)]
  , instrStringFields :: [(InstrFieldName, Text)]
  }
  deriving (Eq, Show)

data InstrFieldName
  = FieldSource
  | FieldDest
  | FieldTarget
  | FieldOffset
  | FieldFirst
  | FieldSecond
  | FieldThird
  | FieldAsm
  | FieldDestIn
  deriving stock (Eq, Ord, Show)

instrFieldNameText :: InstrFieldName -> Text
instrFieldNameText name =
  case name of
    FieldSource -> "source"
    FieldDest -> "dest"
    FieldTarget -> "target"
    FieldOffset -> "offset"
    FieldFirst -> "first"
    FieldSecond -> "second"
    FieldThird -> "third"
    FieldAsm -> "asm"
    FieldDestIn -> "dest_in"

instrFieldInputName :: InstrFieldName -> Maybe InstrFieldName
instrFieldInputName name =
  case name of
    FieldDest -> Just FieldDestIn
    _ -> Nothing

instrFieldIsInputName :: InstrFieldName -> Bool
instrFieldIsInputName name =
  case name of
    FieldDestIn -> True
    _ -> False

data InstrType
  = IAdd
  | IAdd3
  | IAnd
  | IAsm
  | ICall
  | ICmp
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

fieldPlace :: InstrFieldName -> Instr -> Place
fieldPlace name instr =
  case lookup name (instrFields instr) of
    Just place -> place
    Nothing -> immediateInteger 0

fieldString :: InstrFieldName -> Instr -> Text
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

instrAcceptsImmediate :: InstrType -> InstrFieldName -> Bool
instrAcceptsImmediate instr name =
  case instr of
    IMov -> name == FieldSource
    ICmp -> False
    IAdd3 -> name `elem` [FieldSource, FieldOffset]
    ILdOffset -> name == FieldOffset
    IMulh -> name == FieldThird
    IMull3 -> name == FieldThird
    ISub3 -> name == FieldThird
    IShr3 -> name == FieldThird
    IAdd -> name == FieldSource
    ISub -> name == FieldSource
    IMull -> name == FieldSource
    IShl -> name == FieldSource
    IShr -> name == FieldSource
    IXor -> name == FieldSource
    IAnd -> name == FieldSource
    IOr -> name == FieldSource
    _ -> False

instrUseFieldNames :: InstrType -> [InstrFieldName]
instrUseFieldNames instr =
  case instr of
    ISt -> [FieldDest, FieldSource]
    ILd -> [FieldSource]
    IPush -> [FieldTarget]
    IPop -> []
    ICall -> [FieldTarget]
    IAdd3 -> [FieldSource, FieldOffset]
    ILdOffset -> [FieldSource, FieldOffset]
    ICmp -> [FieldFirst, FieldSecond]
    IAdd -> useDest
    ISub -> useDest
    IMull -> useDest
    IShl -> useDest
    IShr -> useDest
    IShr3 -> [FieldSource, FieldThird]
    IXor -> useDest
    IAnd -> useDest
    IOr -> useDest
    IAsm -> []
    IMulh -> [FieldSource, FieldThird]
    IMull3 -> [FieldSource, FieldThird]
    ISub3 -> [FieldSource, FieldThird]
    ty
      | isJumpInstruction ty -> []
      | otherwise -> [FieldSource]
  where
    useDest = [FieldSource, FieldDest]

instrDefFieldNames :: InstrType -> [InstrFieldName]
instrDefFieldNames instr =
  case instr of
    ISt -> []
    ILd -> [FieldDest]
    IPush -> []
    IPop -> [FieldTarget]
    ICall -> []
    IAdd3 -> [FieldDest]
    ILdOffset -> [FieldDest]
    ICmp -> []
    IAdd -> [FieldDest]
    ISub -> [FieldDest]
    IMull -> [FieldDest]
    IShl -> [FieldDest]
    IShr -> [FieldDest]
    IShr3 -> [FieldDest]
    IXor -> [FieldDest]
    IAnd -> [FieldDest]
    IOr -> [FieldDest]
    IAsm -> []
    IMulh -> [FieldDest]
    IMull3 -> [FieldDest]
    ISub3 -> [FieldDest]
    ty
      | isJumpInstruction ty -> []
      | otherwise -> [FieldDest]

instrIsBinary :: InstrType -> Bool
instrIsBinary instr =
  instr `elem` [IMov, IAdd, ISub, IMull, IShl, IShr, IXor, IAnd, IOr]

instrPureRegisterDefinition :: InstrType -> Bool
instrPureRegisterDefinition instr =
  instr `elem` [IMov, IAdd, ISub, IMull, IShl, IShr, IXor, IAnd, IOr, IAdd3, IShr3, IMulh, IMull3, ISub3]

isJumpInstruction :: InstrType -> Bool
isJumpInstruction instr =
  instr `elem` [IJa, IJae, IJb, IJbe, IJe, IJg, IJge, IJl, IJle, IJmp, IJne]

isVirtualRegister :: Place -> Bool
isVirtualRegister place = placeKind place `elem` [Temporary, PointerRegister, VirtualRegister]

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

placeKey :: Place -> Maybe (PlaceKind, Text)
placeKey place
  | isVirtualRegister place = Just (placeKind place, placeValue place)
  | otherwise = Nothing

renderPlace :: Place -> Text
renderPlace place = placeKindCode (placeKind place) <> ":" <> placeValue place

uniqueInOrder :: Ord a => [a] -> [a]
uniqueInOrder = go Set.empty
  where
    go _ [] = []
    go seen (value : rest)
      | value `Set.member` seen = go seen rest
      | otherwise = value : go (Set.insert value seen) rest

uniquePlaces :: [Place] -> [Place]
uniquePlaces = uniqueInOrder
