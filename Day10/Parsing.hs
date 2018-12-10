module Day10.Parsing
  ( Point(..)
  , parseData
  ) where

import           Data.Array
import           Text.ParserCombinators.ReadP
import           Utilities.Parsing

data Point = Point
  { position :: (Int, Int)
  , velocity :: (Int, Int)
  } deriving (Eq, Show)

point :: ReadP Point
point = do
  string "position="
  pos <- pair '<' integer '>'
  skipSpaces
  string "velocity="
  vel <- pair '<' integer '>'
  eof
  return (Point pos vel)

parseLine :: String -> Point
parseLine = parse point

parseData :: [String] -> [Point]
parseData = (map parseLine)
