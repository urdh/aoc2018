{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day2.Solutions
Description : Solutions for day 2 of the 2018 Advent of Code.
-}
module Day2.Solutions
  ( part1
  , part2
  , input
  ) where

import           Data.FileEmbed
import           Data.List
import qualified Data.Map.Strict as Map

-- | Get all keys from 'Map k a' where the corresponding element has a specific value.
keysWithValue :: (Eq a) => a -> Map.Map k a -> [k]
keysWithValue = (Map.keys .) . Map.filter . (==)

-- | Count the number of occurrences of each charcater in a String.
characterCount :: String -> Map.Map Char Int
characterCount [] = Map.empty
characterCount (x:xs) =
  Map.fromListWith (+) ((x, 1) : Map.toList (characterCount xs))

-- | Determine if a 'String' has exactly 'n' occurrences of any particular character.
hasCharacterCount :: Int -> String -> Bool
hasCharacterCount = ((not . null) .) . (. characterCount) . keysWithValue

-- | Generate a list of all unique pairs of elements from the input list.
allPairs :: (Eq a) => [a] -> [(a, a)]
allPairs = filter (uncurry (/=)) . (\xs -> [(x, y) | x <- xs, y <- xs])

-- | Reduces two lists to a single list containing the common elements (position matters).
matchingElements :: (Eq a) => [a] -> [a] -> [a]
matchingElements [] [] = []
matchingElements (x:xs) (y:ys)
  | (x == y) = x : matchingElements xs ys
  | otherwise = matchingElements xs ys

-- | The solution is straight-forward. Count the number of strings with the given number of
--   repetitions, then multiply the two counts in order to get the checksum.
part1 :: [String] -> Int
part1 xs = foldr1 (*) (map stringsWithCount [2, 3])
  where
    stringsWithCount = length . flip filter xs . hasCharacterCount

-- | Part two is, if anything, easier. Map all pairs of strings to a string only containing
--   their common characters (where position matters). Return the longest of these strings.
part2 :: [String] -> String
part2 = last . sortOn length . map (uncurry matchingElements) . allPairs

-- | The input to this problem is a list of character strings (one box ID per line).
input :: [String]
input = lines $(embedStringFile "Day2/input.txt")
