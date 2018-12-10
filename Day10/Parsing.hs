module Day10.Parsing
  ( Point(..)
  , parseData
  ) where

import           Control.Monad
import           Data.Array
import           Data.Function
import           Data.List
import           Data.Tuple
import           Text.ParserCombinators.ReadP

data Point = Point
  { position :: (Int, Int)
  , velocity :: (Int, Int)
  } deriving (Eq, Show)

integer :: ReadP Int
integer = fmap read (skipSpaces >> choice [prefixed, unprefixed])
  where
    digit = satisfy (\char -> char >= '0' && char <= '9')
    sign = choice [char '+', char '-']
    prefixed = (:) <$> sign <*> unprefixed
    unprefixed = (many1 digit)

pair :: ReadP (Int, Int)
pair = do
  satisfy (== '<')
  a <- integer
  satisfy (== ',')
  b <- integer
  satisfy (== '>')
  return (a, b)

point :: ReadP Point
point = do
  string "position="
  pos <- pair
  skipSpaces
  string "velocity="
  vel <- pair
  eof
  return (Point pos vel)

parseLine :: String -> Point
parseLine s = head [x | (x, "") <- (readP_to_S point s)]

parseData :: [String] -> [Point]
parseData = (map parseLine)
