{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day18.Solutions
Description : Solutions for day 18 of the 2018 Advent of Code.
-}
module Day18.Solutions
  ( part1
  , part2
  , input
  ) where

import           Data.Array
import           Data.FileEmbed
import           Data.Function
import           Data.List
import           Day18.Parsing

neighbours :: Forest -> (Int, Int) -> (Char, [Char])
neighbours f (x, y) =
  ((,) (f ! (x, y))) .
  map snd .
  deleteBy ((==) `on` fst) ((x, y), ' ') . filter (neighbour . fst) . assocs $
  f
  where
    neighbour (x', y') = (abs (x' - x) < 2) && (abs (y' - y) < 2)

transform :: Char -> [Char] -> Char
transform '.' xs
  | 2 < length (filter (== '|') xs) = '|'
  | otherwise = '.'
transform '|' xs
  | 2 < length (filter (== '#') xs) = '#'
  | otherwise = '|'
transform '#' xs
  | 0 < length (filter (== '#') xs) && 0 < length (filter (== '|') xs) = '#'
  | otherwise = '.'

generate :: Forest -> Forest
generate f =
  f // (map (\x -> (x, uncurry transform . neighbours f $ x)) . indices $ f)

resources :: Forest -> [Int]
resources f = map (uncurry (*) . count) . iterate generate $ f
  where
    count f =
      ( length . filter (== '|') . elems $ f
      , length . filter (== '#') . elems $ f)

findcycle :: Eq a => [a] -> ([a], [a])
findcycle xxs = fCycle xxs xxs
  where
    fCycle (x:xs) (_:y:ys)
      | x == y = fStart xxs xs
      | otherwise = fCycle xs ys
    fCycle _ _ = (xxs, []) -- not cyclic
    fStart (x:xs) (y:ys)
      | x == y = ([], x : fLength x xs)
      | otherwise =
        let (as, bs) = fStart xs ys
         in (x : as, bs)
    fLength x (y:ys)
      | x == y = []
      | otherwise = y : fLength x ys

recycle :: ([a], [a]) -> [a]
recycle (xs, ys) = xs ++ (cycle ys)

part1 :: Forest -> Int
part1 = (!! 10) . resources

part2 :: Forest -> Int
part2 = (!! 1000000000) . (recycle $!) . findcycle . resources

input :: Forest
input = parseData (lines $(embedStringFile "Day18/input.txt"))
