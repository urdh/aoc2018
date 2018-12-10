module Utilities.Tests
  ( integertests
  , pairtests
  ) where

import           Test.Tasty.HUnit             (assertEqual, testCase)
import           Text.ParserCombinators.ReadP
import           Utilities.Parsing            (integer, pair, parse)

integertests =
  testCase "Utilities -- parsing integers" $ do
    assertEqual [] 0 (parse integer "0")
    assertEqual [] 99 (parse integer "99")
    assertEqual [] 7 (parse integer " 7")
    assertEqual [] 23 (parse integer "+23")
    assertEqual [] (-23) (parse integer "-23")
    assertEqual [] 1234567890 (parse integer "1234567890")

pairtests =
  testCase "Utilities -- parsing pairs" $ do
    assertEqual [] (0, 0) (parse (pair '(' integer ')') "(0, 0)")
    assertEqual [] (0, 0) (parse (pair '(' integer ')') "( 0, 0)")
    assertEqual [] (0, 0) (parse (pair '(' integer ')') "(+0,+0)")
    assertEqual [] ((-2), (-3)) (parse (pair '(' integer ')') "(   -2,   -3)")
    assertEqual [] (0, 0) (parse (pair '<' integer '>') "<0, 0>")
    assertEqual [] ('A', 'B') (parse (pair '<' get '>') "<A,B>")
