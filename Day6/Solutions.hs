{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day6.Solutions
Description : Solutions for day 6 of the 2018 Advent of Code.
-}
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

-- | Calculate the distance between two points using L1 (Manhattan, Taxicab) distance norm.
taxicab :: (Int, Int) -> (Int, Int) -> Int
taxicab = ((uncurry (+) . (abs *** abs)) .) . sub
  where
    sub (a, b) (a', b') = (a - a', b - b')

-- | Given a set of points, return a minimal rectangle covering all these points.
boundingBox :: [Point] -> (Point, Point)
boundingBox =
  ap ((,) . (minimum *** minimum) . unzip) ((maximum *** maximum) . unzip)

-- | Expand the rectangle by 'm' elements in every direction.
expandBox :: Int -> (Point, Point) -> (Point, Point)
expandBox m = (move (negate m)) *** (move m)
  where
    move z = (+ z) *** (+ z)

-- | Generate a list of all coordinates inside the rectangle defined by the input point pair.
pointsInBox :: (Point, Point) -> [Point]
pointsInBox (min', max') =
  [(x, y) | x <- [(fst min') .. (fst max')], y <- [(snd min') .. (snd max')]]

-- | Return the first element of the list if it has length 1, otherwise return a default value.
singleOr :: a -> [a] -> a
singleOr _ [x] = x
singleOr x _   = x

-- | Find the point from 'ps' that is nearest 'p', using 'p' itself to break ties.
nearestPoint :: [Point] -> Point -> Point
nearestPoint ps p =
  (singleOr p) .
  (map fst) .
  head . groupBy ((==) `on` snd) . (sortOn snd) . map (ap (,) (taxicab p)) $
  ps

-- | Group points in input by closest point from 'ps'.
groupNearest :: [Point] -> [Point] -> [[Point]]
groupNearest ps = group . sort . (map (nearestPoint ps))

-- | Find the largest site in a Voronoi diagram with taxicab metric. See 'part1' for algorithm.
largestGroup :: Int -> [Point] -> (Point, Int)
largestGroup margin =
  last .
  (sortOn snd) .
  ap (intersect . areas . groups margin) (areas . groups (margin + 1))
  where
    groups m = (ap groupNearest (pointsInBox . (expandBox m) . boundingBox))
    areas = map (liftM2 (,) head length)

-- | For each coordinate in 'cs', calculate the sum of distances to points in 'ps'.
distanceSum :: [Point] -> [Point] -> [Int]
distanceSum ps cs = map (\p -> sum . map (taxicab p) $ ps) cs

-- | This is a bit tricky. What is basically being asked for is the largest non-infinite
--   site of a Voronoi diagram computed using the L1 (Manhattan/Taxicab) norm. The sort of
--   hacky solution is to simply pick a bounding box that is sufficiently large, and find
--   the closest point for each square inside this bounding box (breaking ties by assigning
--   the square itself as the closes point if there are multiple equidistant points). To
--   filter out the infinite areas, increase the size of the bounding box by one square in
--   each direction and repeat the process. All areas that grow will be infinite and can be
--   discarded. (This requires the original bounding box to be sufficiently large).
--   The area of each site in the diagram is found by sorting and grouping the list of
--   closest points.
part1 :: Int -> [Point] -> Int
part1 margin = snd . (largestGroup margin)

-- | The second problem is easier. Simply calculate the distance sum for each coordinate in
--   a suitable bounding box, and count the number of squares that are below the limit.
part2 :: Int -> [Point] -> Int
part2 limit =
  length . filter (< limit) . ap distanceSum (pointsInBox . boundingBox)

-- | The input to this problem is a set of points in the 2D plane.
input :: [Point]
input = parseData (lines $(embedStringFile "Day6/input.txt"))
