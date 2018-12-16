module Day16.Tests
  ( parsingtests
  ) where

import           Day16.Parsing    (Instruction (..), Registers (..),
                                   Sample (..), parseData)
import           Test.Tasty.HUnit (assertEqual, testCase)

testinput :: [String]
testinput =
  [ "Before: [1, 0, 2, 1]"
  , "7 0 1 1"
  , "After:  [1, 1, 2, 1]"
  , ""
  , "Before: [0, 0, 2, 1]"
  , "6 2 3 1"
  , "After:  [0, 6, 2, 1]"
  , ""
  , ""
  , ""
  , "5 0 2 3"
  , "5 1 3 1"
  , "5 3 2 2"
  , "14 3 2 2"
  , "6 2 3 2"
  , "4 0 2 0"
  ]

samples :: [Sample]
samples =
  [ ([1, 0, 2, 1], (7, 0, 1, 1), [1, 1, 2, 1])
  , ([0, 0, 2, 1], (6, 2, 3, 1), [0, 6, 2, 1])
  ]

program :: [Instruction]
program =
  [ (5, 0, 2, 3)
  , (5, 1, 3, 1)
  , (5, 3, 2, 2)
  , (14, 3, 2, 2)
  , (6, 2, 3, 2)
  , (4, 0, 2, 0)
  ]

parsingtests =
  testCase "Day 16 -- Parsing data" $ do
    assertEqual [] (samples, program) (parseData testinput)
