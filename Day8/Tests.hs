module Day8.Tests
  ( parsingtests
  , part1tests
  , part2tests
  ) where

import           Day8.Parsing     (Node (..), parseData)
import           Day8.Solutions   (part1, part2)
import           Test.Tasty.HUnit (assertEqual, testCase)

parsingtests =
  testCase "Day 8 -- Parsing data" $ do
    assertEqual
      []
      (Node
         { children =
             [ Node {children = [], metadata = [10, 11, 12]}
             , Node
                 { children = [Node {children = [], metadata = [99]}]
                 , metadata = [2]
                 }
             ]
         , metadata = [1, 1, 2]
         })
      (parseData "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

part1tests =
  testCase "Day 8 -- Calculating the sum of all leaf nodes" $
    -- In this example, that sum is 1+1+2+10+11+12+2+99=138.
   do
    assertEqual [] 138 (part1 (parseData "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"))

part2tests =
  testCase "Day 8 -- Calculating the root node value" $
    -- So, in this example, the value of the root node is 66.
   do
    assertEqual [] 66 (part2 (parseData "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"))
