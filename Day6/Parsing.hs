module Day6.Parsing
  ( Point(..)
  , parseLine
  , parseData
  ) where

import qualified Control.Monad                as Monad
import           Data.List
import           Text.ParserCombinators.ReadP

type Point = (Int, Int)

integer :: ReadP Int
integer = fmap read (many1 digit)
  where
    digit = satisfy (Monad.liftM2 (&&) (>= '0') (<= '9'))

point :: ReadP Point
point = do
  x <- integer
  satisfy (== ',')
  skipSpaces
  y <- integer
  eof
  return (x, y)

parseLine :: String -> Point
parseLine = fst . head . (readP_to_S point)

parseData :: [String] -> [Point]
parseData = (map parseLine)
