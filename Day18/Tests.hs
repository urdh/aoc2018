module Day18.Tests
  ( part1tests
  ) where

import           Day18.Parsing    (Forest (..), parseData)
import           Day18.Solutions  (part1)
import           Test.Tasty.HUnit (assertEqual, testCase)

testinput :: [String]
testinput =
  [ ".#.#...|#."
  , ".....#|##|"
  , ".|..|...#."
  , "..|#.....#"
  , "#.#|||#|#|"
  , "...#.||..."
  , ".|....|..."
  , "||...#|.#|"
  , "|.||||..|."
  , "...#.|..|."
  ]

part1tests =
  testCase "Day 18 -- Finding the resource value after 10 minutes" $ do
    assertEqual [] 1147 (part1 (parseData testinput))
