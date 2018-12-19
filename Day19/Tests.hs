module Day19.Tests
  ( parsingtests
  , part1tests
  ) where

import           Day19.Parsing    (Instruction (..), Registers (..), parseData)
import           Day19.Solutions  (part1)
import           Test.Tasty.HUnit (assertEqual, testCase)

testinput :: [String]
testinput =
  [ "#ip 0"
  , "seti 5 0 1"
  , "seti 6 0 2"
  , "addi 0 1 0"
  , "addr 1 2 3"
  , "setr 1 0 0"
  , "seti 8 0 4"
  , "seti 9 0 5"
  ]

program :: [Instruction]
program =
  [ ("seti", 5, 0, 1)
  , ("seti", 6, 0, 2)
  , ("addi", 0, 1, 0)
  , ("addr", 1, 2, 3)
  , ("setr", 1, 0, 0)
  , ("seti", 8, 0, 4)
  , ("seti", 9, 0, 5)
  ]

parsingtests =
  testCase "Day 19 -- Parsing data" $ do
    assertEqual [] (0, program) (parseData testinput)

part1tests =
  testCase "Day 19 -- Finding the output register values of the program" $ do
    assertEqual [] (Just [6, 5, 6, 0, 0, 9]) (part1 (parseData testinput))
