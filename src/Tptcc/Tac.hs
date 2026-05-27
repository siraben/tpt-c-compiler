module Tptcc.Tac
  ( Instr (..)
  , MethodOutput (..)
  , Place (..)
  , TacProgram (..)
  , fieldPlace
  , fieldString
  , isJumpInstruction
  , isVirtualRegister
  , placeInteger
  , placeKey
  , renderPlace
  , uniqueInOrder
  , uniquePlaces
  ) where

import qualified Data.Set as Set

data Place = Place
  { placeType :: String
  , placeValue :: String
  }
  deriving (Eq, Show)

data Instr = Instr
  { instrType :: String
  , instrFields :: [(String, Place)]
  , instrStringFields :: [(String, String)]
  }
  deriving (Eq, Show)

data MethodOutput = MethodOutput
  { methodOutputName :: String
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

fieldPlace :: String -> Instr -> Place
fieldPlace name instr =
  case lookup name (instrFields instr) of
    Just place -> place
    Nothing -> Place "i" "0"

fieldString :: String -> Instr -> String
fieldString name instr =
  case lookup name (instrStringFields instr) of
    Just value -> value
    Nothing -> ""

isJumpInstruction :: String -> Bool
isJumpInstruction ty = take 1 ty == "j"

isVirtualRegister :: Place -> Bool
isVirtualRegister place = placeType place `elem` ["t", "pr", "vr"]

placeInteger :: Place -> Integer
placeInteger = read . placeValue

placeKey :: Place -> Maybe (String, String)
placeKey place
  | isVirtualRegister place = Just (placeType place, placeValue place)
  | otherwise = Nothing

renderPlace :: Place -> String
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
