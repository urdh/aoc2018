module Day6.Parsing
  ( Point(..)
  , parseLine
  , parseData
  ) where

import           Data.List
import           Text.ParserCombinators.ReadP
import           Utilities.Parsing

type Point = (Int, Int)

point :: ReadP Point
point = do
  x <- integer
  satisfy (== ',')
  y <- integer
  eof
  return (x, y)

parseLine :: String -> Point
parseLine = fst . head . (readP_to_S point)

parseData :: [String] -> [Point]
parseData = (map parseLine)
