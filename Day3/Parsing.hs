module Day3.Parsing
  ( Rectangle(..)
  , parseLine
  ) where

import           Data.List
import           Text.ParserCombinators.ReadP
import           Utilities.Parsing

data Rectangle = Rectangle
  { position :: (Int, Int)
  , width    :: Int
  , height   :: Int
  } deriving (Eq, Show)

event :: ReadP (Int, Rectangle)
event = do
  satisfy (== '#')
  i <- integer
  string " @ "
  x <- integer
  satisfy (== ',')
  y <- integer
  satisfy (== ':')
  w <- integer
  satisfy (== 'x')
  h <- integer
  eof
  return (i, (Rectangle (x, y) w h))

parseLine :: String -> (Int, Rectangle)
parseLine = parse event
