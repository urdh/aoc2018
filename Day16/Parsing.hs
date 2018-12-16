module Day16.Parsing
  ( Registers(..)
  , Instruction(..)
  , Sample(..)
  , parseData
  ) where

import           Control.Arrow
import           Text.ParserCombinators.ReadP
import           Utilities.Parsing

type Registers = [Int]

type Instruction = (Int, Int, Int, Int)

type Sample = (Registers, Instruction, Registers)

registers :: ReadP Registers
registers = do
  between (char '[') (char ']') (sepBy integer (char ','))

instruction :: ReadP Instruction
instruction = do
  (a:b:c:d:_) <- count 4 integer
  return $! (a, b, c, d)

sample :: ReadP Sample
sample = do
  string "Before:"
  skipSpaces
  before <- registers
  skipSpaces
  instr <- instruction
  skipSpaces
  string "After:"
  skipSpaces
  after <- registers
  skipSpaces
  eof
  return $! (before, instr, after)

splitData :: [String] -> ([String], [String])
splitData (a:b:c:d:xs)
  | all null [a, b] = ([], c : d : xs)
  | otherwise = first (unlines [a, b, c] :) $ splitData xs

parseData :: [String] -> ([Sample], [Instruction])
parseData =
  second (map (parse instruction)) . first (map (parse sample)) . splitData
