module Day2.Tests
  ( part1tests
  , part2tests
  ) where

import           Day2.Solutions   (part1, part2)
import           Test.Tasty.HUnit (assertEqual, testCase)

part1tests =
  testCase "Day 2 -- Calculating the checksum" $
    -- Multiplying these together produces a checksum of 4 * 3 = 12.
   do
    assertEqual
      []
      12
      (part1
         ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"])

part2tests =
  testCase "Day 2 -- Calculating the common characters of the correct boxes" $
    -- The IDs fghij and fguij differ by exactly one character, the third (h and u).
   do
    assertEqual
      []
      "fgij"
      (part2 ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"])
