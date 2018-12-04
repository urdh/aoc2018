module Day4.Tests
  ( parsingtests
  , part1tests
  , part2tests
  ) where

import           Data.Time
import           Day4.Parsing     (Entry (..), Event (..), parseData, parseLine)
import           Day4.Solutions   (part1, part2)
import           Test.Tasty.HUnit (assertEqual, testCase)

date :: Integer -> Int -> Int -> Integer -> Integer -> UTCTime
date y m d hour minute =
  UTCTime (fromGregorian y m d) (secondsToDiffTime $ hour * 3600 + minute * 60)

parsingtests =
  testCase "Day 4 -- Parsing data" $ do
    assertEqual
      []
      (Entry (date 1518 11 1 0 0) BeginShift 10)
      (parseLine "[1518-11-01 00:00] Guard #10 begins shift")
    assertEqual
      []
      (Entry (date 1518 11 1 0 5) FallAsleep 0)
      (parseLine "[1518-11-01 00:05] falls asleep")
    assertEqual
      []
      (Entry (date 1518 11 1 0 25) WakeUp 0)
      (parseLine "[1518-11-01 00:25] wakes up")
    assertEqual
      []
      [ (Entry (date 1518 11 1 0 0) BeginShift 10)
      , (Entry (date 1518 11 1 0 5) FallAsleep 10)
      , (Entry (date 1518 11 1 0 25) WakeUp 10)
      ]
      (parseData
         [ "[1518-11-01 00:25] wakes up"
         , "[1518-11-01 00:00] Guard #10 begins shift"
         , "[1518-11-01 00:05] falls asleep"
         ])

part1tests =
  testCase "Day 4 -- Calculating the guard and minute combination (strategy 1)" $
    -- In the above example, the answer would be 10 * 24 = 240.
   do
    assertEqual
      []
      (10, 24)
      (part1 $
       parseData
         [ "[1518-11-01 00:00] Guard #10 begins shift"
         , "[1518-11-01 00:05] falls asleep"
         , "[1518-11-01 00:25] wakes up"
         , "[1518-11-01 00:30] falls asleep"
         , "[1518-11-01 00:55] wakes up"
         , "[1518-11-01 23:58] Guard #99 begins shift"
         , "[1518-11-02 00:40] falls asleep"
         , "[1518-11-02 00:50] wakes up"
         , "[1518-11-03 00:05] Guard #10 begins shift"
         , "[1518-11-03 00:24] falls asleep"
         , "[1518-11-03 00:29] wakes up"
         , "[1518-11-04 00:02] Guard #99 begins shift"
         , "[1518-11-04 00:36] falls asleep"
         , "[1518-11-04 00:46] wakes up"
         , "[1518-11-05 00:03] Guard #99 begins shift"
         , "[1518-11-05 00:45] falls asleep"
         , "[1518-11-05 00:55] wakes up"
         ])

part2tests =
  testCase "Day 4 -- Calculating the guard and minute combination (strategy 2)" $
    -- In the above example, the answer would be 10 * 24 = 240.
   do
    assertEqual
      []
      (99, 45)
      (part2 $
       parseData
         [ "[1518-11-01 00:00] Guard #10 begins shift"
         , "[1518-11-01 00:05] falls asleep"
         , "[1518-11-01 00:25] wakes up"
         , "[1518-11-01 00:30] falls asleep"
         , "[1518-11-01 00:55] wakes up"
         , "[1518-11-01 23:58] Guard #99 begins shift"
         , "[1518-11-02 00:40] falls asleep"
         , "[1518-11-02 00:50] wakes up"
         , "[1518-11-03 00:05] Guard #10 begins shift"
         , "[1518-11-03 00:24] falls asleep"
         , "[1518-11-03 00:29] wakes up"
         , "[1518-11-04 00:02] Guard #99 begins shift"
         , "[1518-11-04 00:36] falls asleep"
         , "[1518-11-04 00:46] wakes up"
         , "[1518-11-05 00:03] Guard #99 begins shift"
         , "[1518-11-05 00:45] falls asleep"
         , "[1518-11-05 00:55] wakes up"
         ])
