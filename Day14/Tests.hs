module Day14.Tests
  ( part1tests
  , part2tests
  ) where

import           Day14.Solutions  (part1, part2)
import           Test.Tasty.HUnit (assertEqual, testCase)

part1tests =
  testCase "Day 14 -- Finding the scores of the interesting recipes" $ do
    assertEqual [] "5158916779" (part1 9)
    assertEqual [] "0124515891" (part1 5)
    assertEqual [] "9251071085" (part1 18)
    assertEqual [] "5941429882" (part1 2018)

part2tests =
  testCase "Day 14 -- Finding the position of the interesting scores" $ do
    assertEqual [] 9 (part2 "51589")
    assertEqual [] 5 (part2 "01245")
    assertEqual [] 18 (part2 "92510")
    assertEqual [] 2018 (part2 "59414")
