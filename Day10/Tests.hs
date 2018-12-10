module Day10.Tests
  ( parsingtests
  , part1tests
  , part2tests
  ) where

import           Day10.Parsing    (Point (..), parseData)
import           Day10.Solutions  (part1, part2)
import           Test.Tasty.HUnit (assertEqual, testCase)

testinput :: [String]
testinput =
  [ "position=< 9,  1> velocity=< 0,  2>"
  , "position=< 7,  0> velocity=<-1,  0>"
  , "position=< 3, -2> velocity=<-1,  1>"
  , "position=< 6, 10> velocity=<-2, -1>"
  , "position=< 2, -4> velocity=< 2,  2>"
  , "position=<-6, 10> velocity=< 2, -2>"
  , "position=< 1,  8> velocity=< 1, -1>"
  , "position=< 1,  7> velocity=< 1,  0>"
  , "position=<-3, 11> velocity=< 1, -2>"
  , "position=< 7,  6> velocity=<-1, -1>"
  , "position=<-2,  3> velocity=< 1,  0>"
  , "position=<-4,  3> velocity=< 2,  0>"
  , "position=<10, -3> velocity=<-1,  1>"
  , "position=< 5, 11> velocity=< 1, -2>"
  , "position=< 4,  7> velocity=< 0, -1>"
  , "position=< 8, -2> velocity=< 0,  1>"
  , "position=<15,  0> velocity=<-2,  0>"
  , "position=< 1,  6> velocity=< 1,  0>"
  , "position=< 8,  9> velocity=< 0, -1>"
  , "position=< 3,  3> velocity=<-1,  1>"
  , "position=< 0,  5> velocity=< 0, -1>"
  , "position=<-2,  2> velocity=< 2,  0>"
  , "position=< 5, -2> velocity=< 1,  2>"
  , "position=< 1,  4> velocity=< 2,  1>"
  , "position=<-2,  7> velocity=< 2, -2>"
  , "position=< 3,  6> velocity=<-1, -1>"
  , "position=< 5,  0> velocity=< 1,  0>"
  , "position=<-6,  0> velocity=< 2,  0>"
  , "position=< 5,  9> velocity=< 1, -2>"
  , "position=<14,  7> velocity=<-2,  0>"
  , "position=<-3,  6> velocity=< 2, -1>"
  ]

testdata :: [Point]
testdata =
  [ (Point (9, 1) (0, 2))
  , (Point (7, 0) (-1, 0))
  , (Point (3, -2) (-1, 1))
  , (Point (6, 10) (-2, -1))
  , (Point (2, -4) (2, 2))
  , (Point (-6, 10) (2, -2))
  , (Point (1, 8) (1, -1))
  , (Point (1, 7) (1, 0))
  , (Point (-3, 11) (1, -2))
  , (Point (7, 6) (-1, -1))
  , (Point (-2, 3) (1, 0))
  , (Point (-4, 3) (2, 0))
  , (Point (10, -3) (-1, 1))
  , (Point (5, 11) (1, -2))
  , (Point (4, 7) (0, -1))
  , (Point (8, -2) (0, 1))
  , (Point (15, 0) (-2, 0))
  , (Point (1, 6) (1, 0))
  , (Point (8, 9) (0, -1))
  , (Point (3, 3) (-1, 1))
  , (Point (0, 5) (0, -1))
  , (Point (-2, 2) (2, 0))
  , (Point (5, -2) (1, 2))
  , (Point (1, 4) (2, 1))
  , (Point (-2, 7) (2, -2))
  , (Point (3, 6) (-1, -1))
  , (Point (5, 0) (1, 0))
  , (Point (-6, 0) (2, 0))
  , (Point (5, 9) (1, -2))
  , (Point (14, 7) (-2, 0))
  , (Point (-3, 6) (2, -1))
  ]

testresult :: String
testresult =
  unlines
    [ "#...#..###"
    , "#...#...#."
    , "#...#...#."
    , "#####...#."
    , "#...#...#."
    , "#...#...#."
    , "#...#...#."
    , "#...#..###"
    ]

parsingtests =
  testCase "Day 10 -- Parsing data" $ do
    assertEqual [] testdata (parseData testinput)

part1tests =
  testCase "Day 10 -- Finding the first message" $
    -- After 3 seconds, the message appeared briefly: HI.
   do assertEqual [] testresult (part1 testdata)

part2tests =
  testCase "Day 10 -- Calculating the root node value" $
    -- After 3 seconds, the message appeared briefly: HI.
   do assertEqual [] 3 (part2 testdata)
