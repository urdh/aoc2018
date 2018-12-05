{-# LANGUAGE TemplateHaskell #-}

module Day2.Solutions
  ( part1
  , part2
  , input
  ) where

import           Data.FileEmbed
import           Data.List
import qualified Data.Map.Strict as Map

keysWithValue :: (Eq a) => a -> Map.Map k a -> [k]
keysWithValue = (Map.keys .) . Map.filter . (==)

characterCount :: String -> Map.Map Char Int
characterCount [] = Map.empty
characterCount (x:xs) =
  Map.fromListWith (+) ((x, 1) : Map.toList (characterCount xs))

hasCharacterCount :: Int -> String -> Bool
hasCharacterCount = ((not . null) .) . (. characterCount) . keysWithValue

allPairs :: (Eq a) => [a] -> [(a, a)]
allPairs = filter (uncurry (/=)) . (\xs -> [(x, y) | x <- xs, y <- xs])

matchingElements :: (Eq a) => [a] -> [a] -> [a]
matchingElements [] [] = []
matchingElements (x:xs) (y:ys)
  | (x == y) = x : matchingElements xs ys
  | otherwise = matchingElements xs ys

part1 :: [String] -> Int
part1 xs = foldr1 (*) (map stringsWithCount [2, 3])
  where
    stringsWithCount = length . flip filter xs . hasCharacterCount

part2 :: [String] -> String
part2 = last . sortOn length . map (uncurry matchingElements) . allPairs

input :: [String]
input = lines $(embedStringFile "Day2/input.txt")
