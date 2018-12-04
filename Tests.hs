module Main
  ( main
  ) where

import qualified Day1.Tests as Day1
import qualified Day2.Tests as Day2
import qualified Day4.Tests as Day4
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
    , Day4.parsingtests
    , Day4.part1tests
    , Day4.part2tests
    ]
