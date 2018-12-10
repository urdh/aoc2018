{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day5.Solutions
Description : Solutions for day 5 of the 2018 Advent of Code.
-}
module Day5.Solutions
  ( part1
  , part2
  , input
  ) where

import           Control.Monad
import           Control.Monad.Fix
import           Data.Char
import           Data.FileEmbed
import           Data.List

-- | Turn a lower-case character to its upper-case equivalent, and vice versa.
invertCase :: Char -> Char
invertCase x
  | isUpper x = toLower x
  | isLower x = toUpper x

-- | Reduce a polymer by elimiating adjacent pairs of differing case.
reduce :: String -> String
reduce [] = []
reduce [x] = [x]
reduce (x:y:xs)
  | y == invertCase x = reduce xs
  | otherwise = x : reduce (y : xs)

-- | Given a starting input, repeatedly apply a function until the output no longer changes.
converge :: (Eq a) => (a -> a) -> a -> a
converge =
  fix .
  (\g f x ->
     if g x == x
       then x
       else (f . g) x)

-- | The first problem is fairly straight-forward: we define the 'reduce' operation as given by
--   the problem description, and simply apply it to the input until the operation is idempotent.
part1 :: String -> Int
part1 = length . converge reduce

-- | The second problem is also straight forward. For each lower-case character present in the
--   input, remove the upper- and lower-case occurrences in the polymer and run 'part1'. The
--   shortest of the resulting strings yields the answer.
part2 :: String -> Int
part2 = minimum . map part1 . ap (map . flip id) (map dropType . types)
  where
    dropType = filter . (. toLower) . (/=)
    types = nub . map toLower

-- | The input to this problem is a single character string representing a polymer.
input :: String
input = rstrip $(embedStringFile "Day5/input.txt")
  where
    rstrip = reverse . dropWhile isSpace . reverse
