module Day3.Tests
  ( parsingtests
  , part1tests
  , part2tests
  ) where

import           Data.Time
import           Day3.Parsing     (Rectangle (..), parseLine)
import           Day3.Solutions   (part1, part2)
import           Test.Tasty.HUnit (assertEqual, testCase)

parsingtests =
  testCase "Day 3 -- Parsing data" $ do
    assertEqual [] (1, (Rectangle (1, 3) 4 4)) (parseLine "#1 @ 1,3: 4x4")
    assertEqual [] (2, (Rectangle (3, 1) 4 4)) (parseLine "#2 @ 3,1: 4x4")
    assertEqual [] (3, (Rectangle (5, 5) 2 2)) (parseLine "#3 @ 5,5: 2x2")
    assertEqual [] (1, (Rectangle (0, 0) 29 19)) (parseLine "#1 @ 0,0: 29x19")
    assertEqual [] (2, (Rectangle (0, 0) 15 26)) (parseLine "#2 @ 0,0: 15x26")

part1tests =
  testCase "Day 3 -- Calculating the contested area" $
    -- The four square inches marked with X are claimed by both 1 and 2.
   do
    assertEqual
      []
      4
      (part1
         [ (1, (Rectangle (1, 3) 4 4))
         , (2, (Rectangle (3, 1) 4 4))
         , (3, (Rectangle (5, 5) 2 2))
         ])

part2tests =
  testCase "Day 3 -- Finding the uncontested claim" $
    -- For example, in the claims above, only claim 3 is intact after all claims are made.
   do
    assertEqual
      []
      (Just 3)
      (part2
         [ (1, (Rectangle (1, 3) 4 4))
         , (2, (Rectangle (3, 1) 4 4))
         , (3, (Rectangle (5, 5) 2 2))
         ])
