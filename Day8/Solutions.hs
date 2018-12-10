{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day8.Solutions
Description : Solutions for day 8 of the 2018 Advent of Code.
-}
module Day8.Solutions
  ( input
  , part1
  , part2
  ) where

import           Data.FileEmbed
import           Data.List
import           Day8.Parsing   (Node (..), parseData)

-- | The first solution is simple: traverse the tree and accumulate the metadata entries.
part1 :: Node -> Int
part1 ns = ((sum . metadata) ns) + (sum (map part1 (children ns)))

-- | The second solution is nearly as simple: instead of traversing the whole tree, traverse it
--   in the manner described in the problem (visit child nodes indexed by the metadata, unless the
--   node is a leaf node in which case accumulate the metadata entries).
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

-- | The input to this problem is a tree where each node has a list of metadata.
--   To be fair, most of the problem lies in parsing the input for this one.
input :: Node
input = parseData (head . lines $ $(embedStringFile "Day8/input.txt"))
