module Tptcc.CodeGen.Render (asReg, renderInstr) where

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as Text

import Tptcc.CodeGen.Options
import Tptcc.Tac

renderInstr :: CodeGenOptions -> Integer -> Instr -> Either String String
renderInstr options localSize instr = do
  rendered <- renderInstruction options localSize instr
  pure (Text.unpack (linePrefix (instrType instr) <> rendered <> "\n"))

linePrefix :: InstrType -> Text
linePrefix ILabel = ""
linePrefix _ = "\t"

renderInstruction :: CodeGenOptions -> Integer -> Instr -> Either String Text
renderInstruction options localSize instr =
  case instrType instr of
    ILabel -> (<> ":") . placeValue <$> requirePlace "target" instr
    ty
      | isJumpInstruction ty -> (\target -> instrMnemonic ty <> " " <> placeValue target) <$> requirePlace "target" instr
    ICall -> ("call " <>) . renderCallTarget <$> requirePlace "target" instr
    ISt -> renderBinary "source" "dest" (\source dest -> "st " <> asReg source <> ", " <> asMemory options localSize dest)
    ILd -> renderBinary "dest" "source" (\dest source -> "ld " <> asReg dest <> ", " <> asMemory options localSize source)
    IPush -> ("push " <>) . asReg <$> requirePlace "target" instr
    IPop -> ("pop " <>) . asReg <$> requirePlace "target" instr
    IRet -> pure "ret"
    ICmp -> renderBinary "first" "second" (\first second -> "cmp " <> asReg first <> ", " <> renderImmediateOrReg second)
    INop -> pure "nop"
    IAdd3 -> renderTernary "dest" "source" "offset" (\dest source offset -> "add " <> asReg dest <> ", " <> renderImmediateOrReg source <> ", " <> renderImmediateOrReg offset)
    ILdOffset -> renderTernary "dest" "source" "offset" (\dest source offset -> "ld " <> asReg dest <> ", " <> asReg source <> ", " <> renderImmediateOrReg offset)
    IAsm -> requireString "asm" instr
    IMulh -> renderTernary "dest" "source" "third" (\dest source third -> "mulh " <> asReg dest <> ", " <> asReg source <> ", " <> renderImmediateOrReg third)
    IMull3 -> renderTernary "dest" "source" "third" (\dest source third -> "mul " <> asReg dest <> ", " <> asReg source <> ", " <> renderImmediateOrReg third)
    ISub3 -> renderTernary "dest" "source" "third" (\dest source third -> "sub " <> asReg dest <> ", " <> asReg source <> ", " <> renderImmediateOrReg third)
    IShr3 -> renderTernary "dest" "source" "third" (\dest source third -> "shr " <> asReg dest <> ", " <> asReg source <> ", " <> renderImmediateOrReg third)
    ty
      | isBinaryInstruction ty -> renderBinary "dest" "source" (\dest source -> instrMnemonic ty <> " " <> asReg dest <> ", " <> renderImmediateOrMemory options localSize source)
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

requirePlace :: Text -> Instr -> Either String Place
requirePlace name instr =
  case lookup name (instrFields instr) of
    Just place -> pure place
    Nothing -> Left ("missing instruction field '" <> Text.unpack name <> "' for " <> Text.unpack (instrMnemonic (instrType instr)))

requireString :: Text -> Instr -> Either String Text
requireString name instr =
  case lookup name (instrStringFields instr) of
    Just value -> pure value
    Nothing -> Left ("missing instruction string field '" <> Text.unpack name <> "' for " <> Text.unpack (instrMnemonic (instrType instr)))

isBinaryInstruction :: InstrType -> Bool
isBinaryInstruction instr =
  instr `elem` [IMov, IAdd, ISub, IMull, IShl, IShr, IXor, IAnd, IOr]

renderCallTarget :: Place -> Text
renderCallTarget place
  | placeType place == "i" = placeValue place
  | otherwise = asReg place

renderImmediateOrReg :: Place -> Text
renderImmediateOrReg place
  | placeType place == "i" = placeValue place
  | otherwise = asReg place

renderImmediateOrMemory :: CodeGenOptions -> Integer -> Place -> Text
renderImmediateOrMemory options localSize place
  | placeType place == "i" = placeValue place
  | otherwise = asMemory options localSize place

asMemory :: CodeGenOptions -> Integer -> Place -> Text
asMemory options localSize place =
  case placeType place of
    "g" -> Text.pack (show (codeGenGlobalAddr options + placeInteger place))
    "l" -> "base_pointer, " <> Text.pack (show (placeInteger place + 1))
    "p" -> "base_pointer, " <> Text.pack (show (localSize + placeInteger place + 2))
    "i" -> placeValue place
    _ -> asReg place

asReg :: Place -> Text
asReg place =
  case placeType place of
    "r"
      | Text.all isDigit (placeValue place) -> "r" <> placeValue place
      | otherwise -> placeValue place
    _ -> "r" <> placeValue place
