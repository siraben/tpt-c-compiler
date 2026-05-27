module Tptcc.CodeGen.Render (asReg, renderInstr) where

import Tptcc.CodeGen.Options
import Tptcc.Tac

renderInstr :: CodeGenOptions -> Integer -> Instr -> String
renderInstr options localSize instr =
  "\t" <> renderInstruction options localSize instr <> "\n"

renderInstruction :: CodeGenOptions -> Integer -> Instr -> String
renderInstruction options localSize instr =
  case instrType instr of
    "call" -> "call " <> renderCallTarget (fieldPlace "target" instr)
    "st" -> "st " <> asReg (fieldPlace "source" instr) <> ", " <> asMemory options localSize (fieldPlace "dest" instr)
    "ld" -> "ld " <> asReg (fieldPlace "dest" instr) <> ", " <> asMemory options localSize (fieldPlace "source" instr)
    "push" -> "push " <> asReg (fieldPlace "target" instr)
    "pop" -> "pop " <> asReg (fieldPlace "target" instr)
    "ret" -> "ret"
    "label" -> placeValue (fieldPlace "target" instr) <> ":"
    "cmp" -> "cmp " <> asReg (fieldPlace "first" instr) <> ", " <> renderImmediateOrReg (fieldPlace "second" instr)
    "nop" -> "nop"
    "add3" -> "add " <> asReg (fieldPlace "dest" instr) <> ", " <> renderImmediateOrReg (fieldPlace "source" instr) <> ", " <> renderImmediateOrReg (fieldPlace "offset" instr)
    "ldoffset" -> "ld " <> asReg (fieldPlace "dest" instr) <> ", " <> asReg (fieldPlace "source" instr) <> ", " <> renderImmediateOrReg (fieldPlace "offset" instr)
    "asm" -> fieldString "asm" instr
    "mulh" -> "mulh " <> asReg (fieldPlace "dest" instr) <> ", " <> asReg (fieldPlace "source" instr) <> ", " <> renderImmediateOrReg (fieldPlace "third" instr)
    "mull3" -> "mul " <> asReg (fieldPlace "dest" instr) <> ", " <> asReg (fieldPlace "source" instr) <> ", " <> renderImmediateOrReg (fieldPlace "third" instr)
    "sub3" -> "sub " <> asReg (fieldPlace "dest" instr) <> ", " <> asReg (fieldPlace "source" instr) <> ", " <> renderImmediateOrReg (fieldPlace "third" instr)
    ty
      | take 1 ty == "j" -> ty <> " " <> placeValue (fieldPlace "target" instr)
      | last ty == '3' -> take (length ty - 1) ty <> " " <> asReg (fieldPlace "dest" instr) <> ", " <> asReg (fieldPlace "source" instr) <> ", " <> renderImmediateOrReg (fieldPlace "third" instr)
      | otherwise -> ty <> " " <> asReg (fieldPlace "dest" instr) <> ", " <> renderImmediateOrMemory options localSize (fieldPlace "source" instr)

renderCallTarget :: Place -> String
renderCallTarget place
  | placeType place == "i" = placeValue place
  | otherwise = asReg place

renderImmediateOrReg :: Place -> String
renderImmediateOrReg place
  | placeType place == "i" = placeValue place
  | otherwise = asReg place

renderImmediateOrMemory :: CodeGenOptions -> Integer -> Place -> String
renderImmediateOrMemory options localSize place
  | placeType place == "i" = placeValue place
  | otherwise = asMemory options localSize place

asMemory :: CodeGenOptions -> Integer -> Place -> String
asMemory options localSize place =
  case placeType place of
    "g" -> show (codeGenGlobalAddr options + placeInteger place)
    "l" -> "base_pointer, " <> show (placeInteger place + 1)
    "p" -> "base_pointer, " <> show (localSize + placeInteger place + 2)
    "i" -> placeValue place
    _ -> asReg place

asReg :: Place -> String
asReg place =
  case placeType place of
    "r"
      | all (`elem` ['0' .. '9']) (placeValue place) -> "r" <> placeValue place
      | otherwise -> placeValue place
    _ -> "r" <> placeValue place
