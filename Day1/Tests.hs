module Day1.Tests
  ( part1tests
  , part2tests
  ) where

import           Day1.Solutions   (part1, part2)
import           Test.Tasty.HUnit (assertEqual, testCase)

part1tests =
  testCase "Day 1 -- Calculating the final frequency" $
    -- +1, +1, +1 results in 3
   do
    assertEqual [] 3 (part1 [1, 1, 1])
    -- +1, +1, -2 results in 0
    assertEqual [] 0 (part1 [1, 1, (-2)])
    -- -1, -2, -3 results in -6
    assertEqual [] (-6) (part1 [(-1), (-2), (-3)])

part2tests =
  testCase "Day 1 -- Calculating the first frequency reached twice" $
    -- +1, -1 first reaches 0 twice
   do
    assertEqual [] 0 (part2 [1, (-1)])
    -- +3, +3, +4, -2, -4 first reaches 10 twice
    assertEqual [] 10 (part2 [3, 3, 4, (-2), (-4)])
    -- -6, +3, +8, +5, -6 first reaches 5 twice
    assertEqual [] 5 (part2 [(-6), 3, 8, 5, (-6)])
    -- +7, +7, -2, -7, -4 first reaches 14 twice
    assertEqual [] 14 (part2 [7, 7, (-2), (-7), (-4)])
