module Day6.Tests
  ( parsingtests
  , part1tests
  , part2tests
  ) where

import           Day6.Parsing     (Point (..), parseData, parseLine)
import           Day6.Solutions   (part1, part2)
import           Test.Tasty.HUnit (assertEqual, testCase)

parsingtests =
  testCase "Day 6 -- Parsing data" $ do
    assertEqual [] (278, 89) (parseLine "278, 89")

part1tests =
  testCase "Day 6 -- Calculating the distance-maximizing area" $
    -- Therefore, in this example, the size of the largest area is 17.
   do
    assertEqual
      []
      17
      (part1 15 [(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)])

part2tests =
  testCase "Day 6 -- Calculating the neighbour-maximizing area" $
    -- This region, which also includes coordinates D and E, has a total size of 16.
   do
    assertEqual
      []
      16
      (part2 32 [(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)])
