module Day7.Tests
  ( parsingtests
  , part1tests
  , part2tests
  ) where

import           Day7.Parsing     (parseData, parseLine)
import           Day7.Solutions   (part1, part2)
import           Test.Tasty.HUnit (assertEqual, testCase)

parsingtests =
  testCase "Day 7 -- Parsing data" $ do
    assertEqual
      []
      [('A', "C"), ('B', "A"), ('C', ""), ('D', "A"), ('E', "FDB"), ('F', "C")]
      (parseData
         [ "Step C must be finished before step A can begin."
         , "Step C must be finished before step F can begin."
         , "Step A must be finished before step B can begin."
         , "Step A must be finished before step D can begin."
         , "Step B must be finished before step E can begin."
         , "Step D must be finished before step E can begin."
         , "Step F must be finished before step E can begin."
         ])

part1tests =
  testCase "Day 7 -- Calculating the correct build order" $
    -- So, in this example, the correct order is CABDFE.
   do
    assertEqual
      []
      "CABDFE"
      (part1
         [ ('A', "C")
         , ('B', "A")
         , ('C', "")
         , ('D', "A")
         , ('E', "FDB")
         , ('F', "C")
         ])

part2tests =
  testCase "Day 7 -- Calculating the total time to build" $
    -- In this example, it would take 15 seconds for two workers to complete these steps.
   do
    assertEqual
      []
      15
      (part2
         2
         0
         [ ('A', "C")
         , ('B', "A")
         , ('C', "")
         , ('D', "A")
         , ('E', "FDB")
         , ('F', "C")
         ])
