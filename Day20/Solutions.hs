{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day20.Solutions
Description : Solutions for day 18 of the 2018 Advent of Code.
-}
module Day20.Solutions
  ( part1
  , part2
  , input
  , toGraph
  ) where

import           Algebra.Graph.AdjacencyMap
import           Control.Arrow
import           Data.FileEmbed
import           Data.List
import           Data.Maybe
import           Day20.Parsing

type Graph a = AdjacencyMap a

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) 'N' = (x, pred y)
move (x, y) 'E' = (succ x, y)
move (x, y) 'S' = (x, succ y)
move (x, y) 'W' = (pred x, y)

toGraph :: (Int, Int) -> Tree Char -> Graph (Int, Int)
toGraph o (Tree (l:ls) ts s) =
  overlay (edge o (move o l)) (toGraph (move o l) (Tree ls ts s))
toGraph o (Tree [] ts [s]) =
  overlays (map (\t -> overlay (toGraph o t) (toGraph o s)) ts)
toGraph o (Tree [] ts []) = overlays (map (toGraph o) ts)

depths :: (Eq a) => a -> Graph a -> [Int]
depths o = depth' o . adjacencyList
  where
    depth' _ [] = [(-1)]
    depth' o as =
      map succ . foldr (++) [(-1)] . map (flip depth' (purge o as)) $
      (fromMaybe [] . lookup o $ as)
    purge o as = map (second (delete o)) $ as

part1 :: Graph (Int, Int) -> Int
part1 = maximum . depths (0, 0)

part2 :: Graph (Int, Int) -> Int
part2 = length . filter (>= 1000) . depths (0, 0)

input :: Graph (Int, Int)
input = (toGraph (0, 0) . parseData $ $(embedStringFile "Day20/input.txt"))
