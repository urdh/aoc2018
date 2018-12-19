module Day17.Tests
  ( part1tests
  , part2tests
  , parsingtests
  ) where

import           Day17.Parsing    (parseLines)
import           Day17.Solutions  (part1, part2)
import           Test.Tasty.HUnit (assertEqual, testCase)

testinput1 :: [String]
testinput1 =
  [ "x=495, y=2..7"
  , "y=7, x=495..501"
  , "x=501, y=3..7"
  , "x=498, y=2..4"
  , "x=506, y=1..2"
  , "x=498, y=10..13"
  , "x=504, y=10..13"
  , "y=13, x=498..504"
  ]

testinput2 :: [String]
testinput2 =
  ["x=496, y=3..11", "x=504, y=3..11", "y=11, x=497..503", "x=500, y=6..8"]

parsingtests =
  testCase "Day 17 -- Parsing data" $ do
    assertEqual [] [(1, 1), (1, 2)] (parseLines ["x=1, y=1..2"])
    assertEqual [] [(2, 1), (3, 1)] (parseLines ["y=1, x=2..3"])

part1tests =
  testCase "Day 17 -- Finding the number of wet tiles" $ do
    assertEqual [] 57 (part1 (parseLines testinput1))
    assertEqual [] 71 (part1 (parseLines testinput2))

part2tests =
  testCase "Day 17 -- Finding the number of at-rest tiles" $ do
    assertEqual [] 29 (part2 (parseLines testinput1))
    assertEqual [] 53 (part2 (parseLines testinput2))
