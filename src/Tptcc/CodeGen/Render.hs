module Tptcc.CodeGen.Render (asReg, renderInstr) where

import Tptcc.CodeGen.Options
import Tptcc.Tac

renderInstr :: CodeGenOptions -> Integer -> Instr -> Either String String
renderInstr options localSize instr =
  (<> "\n") . ("\t" <>) <$> renderInstruction options localSize instr

renderInstruction :: CodeGenOptions -> Integer -> Instr -> Either String String
renderInstruction options localSize instr =
  case instrType instr of
    "call" -> ("call " <>) . renderCallTarget <$> requirePlace "target" instr
    "st" -> renderBinary "source" "dest" (\source dest -> "st " <> asReg source <> ", " <> asMemory options localSize dest)
    "ld" -> renderBinary "dest" "source" (\dest source -> "ld " <> asReg dest <> ", " <> asMemory options localSize source)
    "push" -> ("push " <>) . asReg <$> requirePlace "target" instr
    "pop" -> ("pop " <>) . asReg <$> requirePlace "target" instr
    "ret" -> pure "ret"
    "label" -> (<> ":") . placeValue <$> requirePlace "target" instr
    "cmp" -> renderBinary "first" "second" (\first second -> "cmp " <> asReg first <> ", " <> renderImmediateOrReg second)
    "nop" -> pure "nop"
    "add3" -> renderTernary "dest" "source" "offset" (\dest source offset -> "add " <> asReg dest <> ", " <> renderImmediateOrReg source <> ", " <> renderImmediateOrReg offset)
    "ldoffset" -> renderTernary "dest" "source" "offset" (\dest source offset -> "ld " <> asReg dest <> ", " <> asReg source <> ", " <> renderImmediateOrReg offset)
    "asm" -> requireString "asm" instr
    "mulh" -> renderTernary "dest" "source" "third" (\dest source third -> "mulh " <> asReg dest <> ", " <> asReg source <> ", " <> renderImmediateOrReg third)
    "mull3" -> renderTernary "dest" "source" "third" (\dest source third -> "mul " <> asReg dest <> ", " <> asReg source <> ", " <> renderImmediateOrReg third)
    "sub3" -> renderTernary "dest" "source" "third" (\dest source third -> "sub " <> asReg dest <> ", " <> asReg source <> ", " <> renderImmediateOrReg third)
    ty
      | null ty -> Left "cannot render instruction with empty opcode"
      | isJumpInstruction ty -> (\target -> ty <> " " <> placeValue target) <$> requirePlace "target" instr
      | endsWith3 ty -> renderTernary "dest" "source" "third" (\dest source third -> take (length ty - 1) ty <> " " <> asReg dest <> ", " <> asReg source <> ", " <> renderImmediateOrReg third)
      | otherwise -> renderBinary "dest" "source" (\dest source -> ty <> " " <> asReg dest <> ", " <> renderImmediateOrMemory options localSize source)
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

requirePlace :: String -> Instr -> Either String Place
requirePlace name instr =
  case lookup name (instrFields instr) of
    Just place -> pure place
    Nothing -> Left ("missing instruction field '" <> name <> "' for " <> instrType instr)

requireString :: String -> Instr -> Either String String
requireString name instr =
  case lookup name (instrStringFields instr) of
    Just value -> pure value
    Nothing -> Left ("missing instruction string field '" <> name <> "' for " <> instrType instr)

endsWith3 :: String -> Bool
endsWith3 [] = False
endsWith3 [char] = char == '3'
endsWith3 (_ : rest) = endsWith3 rest

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
