{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE TemplateHaskell #-}

module Day1.Solutions
  ( part1
  , part2
  , input
  ) where

import           Data.FileEmbed

parseIntegers :: String -> [Int]
parseIntegers = (map $ readInt . stripPlus) . words
  where
    readWords = fmap words . readFile
    stripPlus = dropWhile (== '+')
    readInt = read :: String -> Int

duplicates ::
     (Num a)
  => (Eq a) =>
       [a] -> [a]
duplicates = (dups [])
  where
    dups acc [] = []
    dups acc (x:xs)
      | x `elem` acc = [x] ++ (dups acc xs)
      | otherwise = dups ([x] ++ acc) xs

part1 :: [Int] -> Int
part1 xs = sum xs

part2 :: [Int] -> Int
part2 = head . duplicates . scanl1 (+) . ([0] ++) . cycle

input :: [Int]
input = parseIntegers $(embedStringFile "Day1/input.txt")
