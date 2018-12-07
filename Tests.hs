module Main
  ( main
  ) where

import qualified Day1.Tests as Day1
import qualified Day2.Tests as Day2
import qualified Day3.Tests as Day3
import qualified Day4.Tests as Day4
import qualified Day5.Tests as Day5
import qualified Day6.Tests as Day6
import qualified Day7.Tests as Day7
import           Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
  testGroup
    "Unit tests"
    [ Day1.part1tests
    , Day1.part2tests
    , Day2.part1tests
    , Day2.part2tests
    , Day3.parsingtests
    , Day3.part1tests
    , Day3.part2tests
    , Day4.parsingtests
    , Day4.part1tests
    , Day4.part2tests
    , Day5.part1tests
    , Day5.part2tests
    , Day6.parsingtests
    , Day6.part1tests
    , Day6.part2tests
    , Day7.parsingtests
    , Day7.part1tests
    , Day7.part2tests
    ]
