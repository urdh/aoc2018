module Day11.Tests
  ( part1tests
  , part2tests
  ) where

import           Day11.Solutions  (part1, part2)
import           Test.Tasty.HUnit (assertEqual, testCase)

part1tests =
  testCase "Day 11 -- Calculating the best 3x3 square" $
    -- For grid serial number 18, the largest total 3x3 square has a top-left corner
    -- of 33,45 (with a total power of 29).
   do assertEqual [] (33, 45) (part1 18)

part2tests =
  testCase "Day 11 -- Calculating the best NxN square" $
    -- For grid serial number 18, the largest total square (with a total power of 113)
    -- is 16x16 and has a top-left corner of 90,269, so its identifier is 90,269,16.
   do assertEqual [] ((90, 269), 16) (part2 18)
