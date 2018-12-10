module Utilities.Parsing
  ( integer
  , pair
  , parse
  ) where

import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Tuple
import           Text.ParserCombinators.ReadP

integer :: ReadP Int
integer = fmap read (skipSpaces >> choice [signed, unsigned])
  where
    digit = satisfy (liftM2 (&&) (>= '0') (<= '9'))
    signed = (:) <$> (char '-') <*> unsigned
    unsigned = (skipMany $ char '+') >> (many1 digit)

pair :: Char -> ReadP a -> Char -> ReadP (a, a)
pair left parser right = do
  satisfy (== left)
  a <- parser
  satisfy (== ',')
  b <- parser
  satisfy (== right)
  return (a, b)

parse :: ReadP a -> String -> a
parse p s = head [x | (x, "") <- (readP_to_S p s)]
