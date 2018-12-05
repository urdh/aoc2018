module Day3.Parsing
  ( Rectangle(..)
  , parseLine
  ) where

import           Data.List
import           Text.ParserCombinators.ReadP

data Rectangle = Rectangle
  { position :: (Int, Int)
  , width    :: Int
  , height   :: Int
  } deriving (Eq, Show)

integer :: ReadP Int
integer = fmap read (many1 digit)
  where
    digit = satisfy (\char -> char >= '0' && char <= '9')

event :: ReadP (Int, Rectangle)
event = do
  satisfy (== '#')
  i <- integer
  string " @ "
  x <- integer
  satisfy (== ',')
  y <- integer
  satisfy (== ':')
  skipSpaces
  w <- integer
  satisfy (== 'x')
  h <- integer
  eof
  return (i, (Rectangle (x, y) w h))

parseLine :: String -> (Int, Rectangle)
parseLine = fst . head . (readP_to_S event)
