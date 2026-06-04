module Tptcc.CodeGen.Render (asReg, renderInstr) where

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as Text

import Tptcc.CodeGen.Options
import Tptcc.Tac

renderInstr :: CodeGenOptions -> Integer -> Instr -> Either String String
renderInstr options localSize instr = do
  rendered <- renderInstruction options localSize instr
  pure (Text.unpack (prefix <> rendered <> "\n"))
  where
    prefix
      | instrType instr == ILabel = ""
      | otherwise = "\t"

renderInstruction :: CodeGenOptions -> Integer -> Instr -> Either String Text
renderInstruction options localSize instr =
  case instrType instr of
    ILabel -> (<> ":") . placeValue <$> requirePlace FieldTarget instr
    ty
      | isJumpInstruction ty -> (\target -> instrMnemonic ty <> " " <> placeValue target) <$> requirePlace FieldTarget instr
    ICall -> ("call " <>) . renderImmediateOrReg <$> requirePlace FieldTarget instr
    ISt -> renderBinary FieldSource FieldDest (\source dest -> "st " <> asReg source <> ", " <> asMemory options localSize dest)
    ILd -> renderBinary FieldDest FieldSource (\dest source -> "ld " <> asReg dest <> ", " <> asMemory options localSize source)
    IPush -> ("push " <>) . asReg <$> requirePlace FieldTarget instr
    IPop -> ("pop " <>) . asReg <$> requirePlace FieldTarget instr
    IRet -> pure "ret"
    ICmp -> renderBinary FieldFirst FieldSecond (\first second -> "cmp " <> asReg first <> ", " <> renderImmediateOrReg second)
    INop -> pure "nop"
    IAdd3 -> renderTernary FieldDest FieldSource FieldOffset (\dest source offset -> "add " <> asReg dest <> ", " <> renderImmediateOrReg source <> ", " <> renderImmediateOrReg offset)
    ILdOffset -> renderTernary FieldDest FieldSource FieldOffset (\dest source offset -> "ld " <> asReg dest <> ", " <> asReg source <> ", " <> renderImmediateOrReg offset)
    IAsm -> requireString FieldAsm instr
    IMulh -> renderTernary FieldDest FieldSource FieldThird (\dest source third -> "mulh " <> asReg dest <> ", " <> asReg source <> ", " <> renderImmediateOrReg third)
    IMull3 -> renderTernary FieldDest FieldSource FieldThird (\dest source third -> "mul " <> asReg dest <> ", " <> asReg source <> ", " <> renderImmediateOrReg third)
    ISub3 -> renderTernary FieldDest FieldSource FieldThird (\dest source third -> "sub " <> asReg dest <> ", " <> asReg source <> ", " <> renderImmediateOrReg third)
    IShr3 -> renderTernary FieldDest FieldSource FieldThird (\dest source third -> "shr " <> asReg dest <> ", " <> asReg source <> ", " <> renderImmediateOrReg third)
    ty
      | instrIsBinary ty -> renderBinary FieldDest FieldSource (\dest source -> instrMnemonic ty <> " " <> asReg dest <> ", " <> renderImmediateOrMemory options localSize source)
      | otherwise -> Left ("cannot render pseudo-instruction " <> Text.unpack (instrMnemonic ty))
  where
    renderBinary firstName secondName render = do
      first <- requirePlace firstName instr
      second <- requirePlace secondName instr
      pure (render first second)

    renderTernary firstName secondName thirdName render = do
      first <- requirePlace firstName instr
      second <- requirePlace secondName instr
      third <- requirePlace thirdName instr
      pure (render first second third)

requirePlace :: InstrFieldName -> Instr -> Either String Place
requirePlace name instr =
  case lookup name (instrFields instr) of
    Just place -> pure place
    Nothing -> Left ("missing instruction field '" <> Text.unpack (instrFieldNameText name) <> "' for " <> Text.unpack (instrMnemonic (instrType instr)))

requireString :: InstrFieldName -> Instr -> Either String Text
requireString name instr =
  case lookup name (instrStringFields instr) of
    Just value -> pure value
    Nothing -> Left ("missing instruction string field '" <> Text.unpack (instrFieldNameText name) <> "' for " <> Text.unpack (instrMnemonic (instrType instr)))

renderImmediateOrReg :: Place -> Text
renderImmediateOrReg place
  | placeKind place == Immediate = placeValue place
  | otherwise = asReg place

renderImmediateOrMemory :: CodeGenOptions -> Integer -> Place -> Text
renderImmediateOrMemory options localSize place
  | placeKind place == Immediate = placeValue place
  | otherwise = asMemory options localSize place

asMemory :: CodeGenOptions -> Integer -> Place -> Text
asMemory options localSize place =
  case placeKind place of
    Global -> Text.pack (show (codeGenGlobalAddr options + placeInteger place))
    Local -> "base_pointer, " <> Text.pack (show (placeInteger place + 1))
    Parameter -> "base_pointer, " <> Text.pack (show (localSize + placeInteger place + 2))
    Immediate -> placeValue place
    _ -> asReg place

asReg :: Place -> Text
asReg place =
  case placeKind place of
    Register
      | Text.all isDigit (placeValue place) -> "r" <> placeValue place
      | otherwise -> placeValue place
    _ -> "r" <> placeValue place
