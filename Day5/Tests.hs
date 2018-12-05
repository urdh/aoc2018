module Day5.Tests
  ( part1tests
  , part2tests
  ) where

import           Day5.Solutions   (part1, part2)
import           Test.Tasty.HUnit (assertEqual, testCase)

part1tests =
  testCase "Day 5 -- Calculating reacted polymer length" $
    -- After all possible reactions, the resulting polymer contains 10 units.
   do assertEqual [] 10 (part1 "dabAcCaCBAcCcaDA")

part2tests =
  testCase "Day 5 -- Calculating shortest possible polymer length" $
    -- In this example, removing all C/c units was best, producing the answer 4.
   do assertEqual [] 4 (part2 "dabAcCaCBAcCcaDA")
