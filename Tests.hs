module Main
  ( main
  ) where

import qualified Day1.Tests      as Day1
import qualified Day10.Tests     as Day10
import qualified Day11.Tests     as Day11
import qualified Day12.Tests     as Day12
import qualified Day13.Tests     as Day13
import qualified Day2.Tests      as Day2
import qualified Day3.Tests      as Day3
import qualified Day4.Tests      as Day4
import qualified Day5.Tests      as Day5
import qualified Day6.Tests      as Day6
import qualified Day7.Tests      as Day7
import qualified Day8.Tests      as Day8
import qualified Day9.Tests      as Day9
import           Test.Tasty      (defaultMain, testGroup)
import qualified Utilities.Tests as Utilities

main :: IO ()
main =
  defaultMain $
  testGroup
    "Unit tests"
    [ Utilities.integertests
    , Utilities.pairtests
    , Day1.part1tests
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
    , Day8.parsingtests
    , Day8.part1tests
    , Day8.part2tests
    , Day9.part1tests
    , Day10.parsingtests
    , Day10.part1tests
    , Day10.part2tests
    , Day11.part1tests
    , Day11.part2tests
    , Day12.parsingtests
    , Day12.part1tests
    , Day12.part2tests
    , Day13.parsingtests
    , Day13.part1tests
    , Day13.part2tests
    ]
