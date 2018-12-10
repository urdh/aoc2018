{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day1.Solutions
Description : Solutions for day 1 of the 2018 Advent of Code.
-}
module Day1.Solutions
  ( part1
  , part2
  , input
  ) where

import           Data.FileEmbed
import           Text.ParserCombinators.ReadP
import           Utilities.Parsing

-- | Finds all duplicates in a list. Quadratic time complexity.
duplicates :: (Num a, Eq a) => [a] -> [a]
duplicates = (dups [])
  where
    dups acc [] = []
    dups acc (x:xs)
      | x `elem` acc = x : dups acc xs
      | otherwise = dups (x : acc) xs

-- | The solution to part 1 is straight-forward: simply sum the frequency changes.
part1 :: [Int] -> Int
part1 xs = sum xs

-- | The solution to part 2 is also simple, although very inefficient. Simply cycle the input,
--   calculating the partial sum along the way and stopping at the first duplicate.
part2 :: [Int] -> Int
part2 = head . duplicates . scanl1 (+) . (0 :) . cycle

-- | The input for these problems is simply a list of integers, representing frequency changes.
input :: [Int]
input = map (parse integer) . lines $ $(embedStringFile "Day1/input.txt")
