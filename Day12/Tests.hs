module Day12.Tests
  ( parsingtests
  , part1tests
  , part2tests
  ) where

import           Day12.Parsing    (Rule (..), Tape (..), parseData, parseRules,
                                   parseTape)
import           Day12.Solutions  (part1, part2)
import           Test.Tasty.HUnit (assertEqual, testCase)

testinput :: [String]
testinput =
  [ "initial state: #..#.#..##......###...###"
  , ""
  , "...## => #"
  , "..#.. => #"
  , ".#... => #"
  , ".#.#. => #"
  , ".#.## => #"
  , ".##.. => #"
  , ".#### => #"
  , "#.#.# => #"
  , "#.### => #"
  , "##.#. => #"
  , "##.## => #"
  , "###.. => #"
  , "###.# => #"
  , "####. => #"
  ]

parsingtests =
  testCase "Day 12 -- Parsing data" $ do
    assertEqual
      []
      [0, 3, 5, 8, 9, 16, 17, 18, 22, 23, 24]
      (parseTape "initial state: #..#.#..##......###...###")
    assertEqual
      []
      [[False, False, False, True, True]]
      (parseRules ["...## => #"])
    assertEqual
      []
      [[False, False, False, True, True]]
      (parseRules ["...## => #", "..#.. => ."])
    assertEqual [] [] (parseRules ["..#.. => .", ""])

part1tests =
  testCase "Day 12 -- Finding the plant position sum after 20 generations" $
    -- Adding up all the numbers of plant-containing pots after the 20th generation produces 325.
   do assertEqual [] 325 (part1 $ parseData testinput)

part2tests =
  testCase
    "Day 12 -- Finding the plant position sum after 5 billion generations" $
    -- You realize that 20 generations aren't enough.
   do assertEqual [] 999999999374 (part2 $ parseData testinput)
