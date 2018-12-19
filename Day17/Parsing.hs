module Day17.Parsing
  ( parseLines
  ) where

import           Data.List
import           Data.Tuple
import           Text.ParserCombinators.ReadP
import           Utilities.Parsing

range :: ReadP [Int]
range = do
  a <- integer
  string ".."
  b <- integer
  return $! [a .. b]

chunk :: ReadP [(Int, Int)]
chunk = do
  a <- (satisfy (flip elem "xy"))
  string "="
  as <- integer
  string ", "
  b <- (satisfy (flip elem "xy"))
  string "="
  bs <- range
  eof
  return $! map (swapif (a == 'y')) $ zip (repeat as) bs
  where
    swapif True  = swap
    swapif False = id

parseLines :: [String] -> [(Int, Int)]
parseLines = nub . concat . map (parse chunk)
