{-|
Module      : Day11.Solutions
Description : Solutions for day 11 of the 2018 Advent of Code.
-}
module Day11.Solutions
  ( part1
  , part2
  , input
  ) where

import           Control.Arrow
import           Control.Monad
import           Data.Array
import           Data.Function
import           Data.List

type Coordinate = (Int, Int)

type Powers = Array Coordinate Int

-- | Calculate the power for a given grid serial number and fuel cell coordinate.
power :: Int -> Coordinate -> Int
power serial pos = hundreds ((rackid pos * snd pos + serial) * rackid pos) - 5
  where
    rackid = (10 +) . fst
    hundreds = (`mod` 10) . (`quot` 100)

-- | Calculate the integral image (summed-area table) of all fuel cell power values
--   given a grid serial number. The summed-area table allows cheap lookup of sums
--   across sub-rectangles of the larger grid. Output dimensions are [0..300]x[0..300].
powers :: Int -> Powers
powers n = a
  where
    a =
      array
        ((0, 0), (300, 300))
        [ (,)
          (x, y)
          (power n (x, y) + get a (x - 1, y) + get a (x, y - 1) -
           get a (x - 1, y - 1))
        | x <- [0 .. 300]
        , y <- [0 .. 300]
        ]
    get a (x, y)
      | x < 1 || y < 1 = 0
      | otherwise = a ! (x, y)

-- | Calculate the sum of fuel cell powers inside the NxN rectangle with its top-left
--   corner at the given coordinate, using the provided summed-area table.
sumNxN :: Powers -> Int -> Coordinate -> Int
sumNxN a n' (x, y) =
  a ! (x - 1, y - 1) + a ! (x + n, y + n) - a ! (x + n, y - 1) -
  a ! (x - 1, y + n)
  where
    n = n' - 1

-- | Generate all the coordinates in an NxN grid, [1..300]x[1..300].
grid :: Int -> Int -> [Coordinate]
grid m n = [(x, y) | x <- [m .. n], y <- [m .. n]]

-- | Calculate the maximum fuel cell power sum across NxN rectangles.
maxPower :: Int -> Powers -> (Coordinate, Int)
maxPower n a =
  maximumBy (compare `on` snd) . map (\c -> (c, sumNxN a n c)) $
  grid 1 (301 - n)

-- | The first problem is neat; the naive solution involves simply calculating the fuel cell
--   power sum for every 3x3 sub-rectangle of the 300x300 grid. This becomes infeasible to
--   brute-force in the second problem, though, so an optimization has to be made. By calculating
--   the NxN sums from an integral image (summed-area table), the execution time required for
--   finding the NxN square that maximizes the fuel cell power sum is greatly reduced.
part1 :: Int -> Coordinate
part1 s = fst . maxPower 3 $! (powers s)

-- | The second problem is straight-forward once the solution to the first has been optimized:
--   simply find the best NxN square for N = [1..300], then pick the best one of these.
part2 :: Int -> (Coordinate, Int)
part2 =
  (first fst) .
  maximumBy (compare `on` snd . fst) .
  flip map [1 .. 300] . ap (flip (,)) . (flip maxPower $!) . powers

-- | The input to this problem is a single integer "grid serial number".
input :: Int
input = 5153
