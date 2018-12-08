{-# LANGUAGE TemplateHaskell #-}

module Day8.Solutions
  ( input
  , part1
  , part2
  ) where

import           Data.FileEmbed
import           Data.List
import           Day8.Parsing   (Node (..), parseData)

part1 :: Node -> Int
part1 ns = ((sum . metadata) ns) + (sum (map part1 (children ns)))

part2 :: Node -> Int
part2 n
  | (null . children) n = (sum . metadata) n
  | otherwise =
    sum
      (map
         part2
         (foldr1
            (++)
            (map (flip (take 1 .) n . (. children) . drop) (dropcnt n))))
  where
    dropcnt = map (subtract 1) . metadata

input :: Node
input = parseData (head . lines $ $(embedStringFile "Day8/input.txt"))
