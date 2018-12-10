{-|
Module      : Utilities.Parsing
Description : Parsing utilities for the 2018 Advent of Code problems.
-}
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

-- | Parses an returns an integer, possibly prefixed by spaces and\/or +/-.
integer :: ReadP Int
integer = fmap read (skipSpaces >> choice [signed, unsigned])
  where
    digit = satisfy (liftM2 (&&) (>= '0') (<= '9'))
    signed = (:) <$> (char '-') <*> unsigned
    unsigned = (skipMany $ char '+') >> (many1 digit)

-- | Parses and returns a tuple, using the given left/right delimiters and parser.
--
-- @
-- parse (pair '(' get ')') "(A, B)" == ('A', 'B')
-- parse (pair '{' integer '}') "{5, 6}" == (5, 6)
-- @
pair :: Char -> ReadP a -> Char -> ReadP (a, a)
pair left parser right = do
  satisfy (== left)
  a <- parser
  satisfy (== ',')
  b <- parser
  satisfy (== right)
  return (a, b)

-- | Run a parser on a string, returning the first complete match.
parse :: ReadP a -> String -> a
parse p s = head [x | (x, "") <- (readP_to_S p s)]
