module Day19.Parsing
  ( Registers(..)
  , Instruction(..)
  , parseData
  ) where

import           Control.Arrow
import           Text.ParserCombinators.ReadP
import           Utilities.Parsing

type Registers = [Int]

type Instruction = (String, Int, Int, Int)

instruction :: ReadP Instruction
instruction = do
  op <- manyTill get (char ' ')
  (a:b:c:_) <- count 3 integer
  return $! (op, a, b, c)

iptr :: ReadP Int
iptr = do
  string "#ip"
  ip <- integer
  eof
  return $! ip

parseData :: [String] -> (Int, [Instruction])
parseData (x:xs) = (parse iptr x, map (parse instruction) xs)
