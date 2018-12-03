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
keysWithValue v = (Map.keys . Map.filter (== v))

characterCount :: String -> Map.Map Char Int
characterCount [] = Map.empty
characterCount (x:xs) =
  Map.fromListWith (+) ([(x, 1)] ++ Map.toList (characterCount xs))

hasCharacterCount :: Int -> String -> Bool
hasCharacterCount x = not . null . (keysWithValue x) . characterCount

allPairs :: (Eq a) => [a] -> [(a, a)]
allPairs xs = filter (\x -> (/=) (fst x) (snd x)) [(x, y) | x <- xs, y <- xs]

matchingElements :: (Eq a) => [a] -> [a] -> [a]
matchingElements [] [] = []
matchingElements (x:xs) (y:ys)
  | (x == y) = [x] ++ (matchingElements xs ys)
  | otherwise = (matchingElements xs ys)

part1 :: [String] -> Int
part1 xs = foldr1 (*) (map stringsWithCount [2, 3])
  where
    stringsWithCount c = (length . filter (hasCharacterCount c)) xs

part2 :: [String] -> String
part2 =
  last .
  (sortOn length) . (map (\x -> matchingElements (fst x) (snd x)) . allPairs)

input :: [String]
input = lines $(embedStringFile "Day2/input.txt")
