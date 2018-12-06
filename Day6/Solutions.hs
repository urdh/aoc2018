{-# LANGUAGE TemplateHaskell #-}

module Day6.Solutions
  ( input
  , part1
  , part2
  ) where

import           Control.Arrow
import           Control.Monad
import           Data.FileEmbed
import           Data.Function
import           Data.List
import           Day6.Parsing   (Point, parseData)

taxicab :: (Int, Int) -> (Int, Int) -> Int
taxicab = ((uncurry (+) . (abs *** abs)) .) . sub
  where
    sub (a, b) (a', b') = (a - a', b - b')

boundingBox :: [Point] -> (Point, Point)
boundingBox =
  ap ((,) . (minimum *** minimum) . unzip) ((maximum *** maximum) . unzip)

expandBox :: Int -> (Point, Point) -> (Point, Point)
expandBox m = (move (negate m)) *** (move m)
  where
    move z = (+ z) *** (+ z)

pointsInBox :: (Point, Point) -> [Point]
pointsInBox (min', max') =
  [(x, y) | x <- [(fst min') .. (fst max')], y <- [(snd min') .. (snd max')]]

singleOr :: a -> [a] -> a
singleOr _ [x] = x
singleOr x _   = x

nearestPoint :: [Point] -> Point -> Point
nearestPoint ps p =
  (singleOr p) .
  (map fst) .
  head . groupBy ((==) `on` snd) . (sortOn snd) . map (ap (,) (taxicab p)) $
  ps

groupNearest :: [Point] -> [Point] -> [[Point]]
groupNearest ps = group . sort . (map (nearestPoint ps))

largestGroup :: Int -> [Point] -> (Point, Int)
largestGroup margin =
  last .
  (sortOn snd) .
  ap (intersect . areas . groups margin) (areas . groups (margin + 1))
  where
    groups m = (ap groupNearest (pointsInBox . (expandBox m) . boundingBox))
    areas = map (liftM2 (,) head length)

distanceSum :: [Point] -> [Point] -> [Int]
distanceSum ps cs = map (\p -> sum . map (taxicab p) $ ps) cs

part1 :: Int -> [Point] -> Int
part1 margin = snd . (largestGroup margin)

part2 :: Int -> [Point] -> Int
part2 limit =
  length . filter (< limit) . ap distanceSum (pointsInBox . boundingBox)

input :: [Point]
input = parseData (lines $(embedStringFile "Day6/input.txt"))
