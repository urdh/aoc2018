module Day9.Tests
  ( part1tests
  ) where

import           Day9.Solutions   (part1)
import           Test.Tasty.HUnit (assertEqual, testCase)

part1tests =
  testCase "Day 9 -- Calculating the winning score" $
    -- Assuming the example above ends after the marble numbered 25, the winning score is 23+9=32.
   do
    assertEqual [] 32 (part1 9 25)
    -- Here are a few more examples.
    assertEqual [] 8317 (part1 10 1618)
    assertEqual [] 146373 (part1 13 7999)
    assertEqual [] 2764 (part1 17 1104)
    assertEqual [] 54718 (part1 21 6111)
    assertEqual [] 37305 (part1 30 5807)
