{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day10.Solutions
Description : Solutions for day 10 of the 2018 Advent of Code.
-}
module Day10.Solutions
  ( input
  , part1
  , part2
  ) where

import           Data.Array
import           Data.FileEmbed
import           Data.Function
import           Data.List
import           Data.Tuple
import           Day10.Parsing  (Point (..), parseData)

-- | Move a single point a number of time steps along the line defined by its velocity.
movept :: Int -> Point -> Point
movept n x = (Point (move n pos vel) vel)
  where
    move n (x, y) (u, v) = (x + (n * u), y + (n * v))
    pos = position x
    vel = velocity x

-- | Produce a string representation of a set of points plotted on a 2D ASCII rectangle.
display :: [(Int, Int)] -> String
display xs =
  unlines .
  map (map snd) .
  groupBy ((==) `on` fst . fst) .
  assocs .
  accumArray
    (const id)
    '.'
    ( (minimum $ map fst xs, minimum $ map snd xs)
    , (maximum $ map fst xs, maximum $ map snd xs)) $
  map (\x -> (x, '#')) xs

-- | Move a set of points so that their enclosing bounding box starts at '(0, 0)'.
offset :: [(Int, Int)] -> [(Int, Int)]
offset xs = map (sub (xmin, ymin)) xs
  where
    sub (u, v) (x, y) = (x - u, y - v)
    xmin = minimum $ map fst xs
    ymin = minimum $ map snd xs

-- | Find the area of the bounding box enclosing a list of points.
area :: [(Int, Int)] -> Int
area xs =
  ((maximum $ map fst xs) - (minimum $ map fst xs)) *
  ((maximum $ map snd xs) - (minimum $ map snd xs))

-- | Find the first local minimum of a list.
locmin :: Ord a => [a] -> (a, Int)
locmin xs =
  head .
  map (\(x, y, z) -> x) .
  filter (\(x, y, z) -> (fst x) < (fst y) && (fst x) < (fst z)) $
  zip3 (drop 1 pairs) pairs (drop 2 pairs)
  where
    pairs = zip xs [1 ..]

-- | Move the points along their line, until the area of their bounding box reaches a local minimum.
findmsg :: [Point] -> ((Int, [(Int, Int)]), Int)
findmsg pts =
  locmin . map (\x -> (area x, x)) $
  map (\n -> map (position . movept n) pts) [1 ..]

-- | The solution to the first part is fairly straight-forward. Expecting that the resulting text
--   will be fairly rectangular, we can conjecture that while moving all points in lockstep, their
--   area will define a (quadratic) function with a minimum at the point where the text appears.
--   Therefore, we can lazily move the points further and further until we find this minimum, and
--   then draw the points in a tight bounding box when we've found it.
part1 :: [Point] -> String
part1 = display . (map swap) . offset . snd . fst . findmsg

-- | The solution to the second problem is a by-product of the first; it is the number of iterations
--   before the minimum was found.
part2 :: [Point] -> Int
part2 = snd . findmsg

-- | The input to this problem is a set of points with accompanying velocities.
input :: [Point]
input = parseData (lines $(embedStringFile "Day10/input.txt"))
