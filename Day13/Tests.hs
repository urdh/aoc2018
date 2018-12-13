module Day13.Tests
  ( parsingtests
  , part1tests
  , part2tests
  ) where

import qualified Data.Map.Strict  as Map
import           Day13.Parsing    (Board (..), Cart (..), Coord (..), parseData)
import           Day13.Solutions  (part1, part2)
import           Test.Tasty.HUnit (assertEqual, testCase)

testinput1 :: [String]
testinput1 =
  [ "/->-\\" --           /->-\
  , "|   |  /----\\" --   |   |  /----\
  , "| /-+--+-\\  |" --   | /-+--+-\  |
  , "| | |  | v  |" --    | | |  | v  |
  , "\\-+-/  \\-+--/" --  \-+-/  \-+--/
  , "  \\------/" --        \------/
  ]

testinput2 :: [String]
testinput2 =
  [ "/>-<\\" --    />-<\
  , "|   |" --     |   |
  , "| /<+-\\" --  | /<+-\
  , "| | | v" --   | | | v
  , "\\>+</ |" --  \>+</ |
  , "  |   ^" --     |   ^
  , "  \\<->/" --    \<->/
  ]

carts :: [(Cart, Coord)]
carts = [(('>', 0), (2, 0)), (('v', 0), (9, 3))]

board :: Board
board =
  Map.fromList
    [ ((0, 0), '/')
    , ((4, 0), '\\')
    , ((0, 4), '\\')
    , ((4, 4), '/')
    , ((7, 1), '/')
    , ((12, 1), '\\')
    , ((7, 4), '\\')
    , ((12, 4), '/')
    , ((2, 2), '/')
    , ((9, 2), '\\')
    , ((2, 5), '\\')
    , ((9, 5), '/')
    , ((4, 2), '+')
    , ((7, 2), '+')
    , ((2, 4), '+')
    , ((9, 4), '+')
    ]

parsingtests =
  testCase "Day 13 -- Parsing data" $ do
    assertEqual [] (board, carts) (parseData testinput1)

part1tests =
  testCase "Day 13 -- Finding location of the first crash" $
    -- In this example, the location of the first crash is 7,3.
   do
    assertEqual [] (7, 3) (uncurry part1 $ parseData testinput1)
    -- Additional tests for edge cases.
    assertEqual
      []
      (0, 3)
      (uncurry part1 $ parseData ["|", "v", "|", "|", "|", "^", "|"])
    assertEqual [] (3, 0) (uncurry part1 $ parseData ["->---<-"])
    assertEqual
      []
      (132, 110)
      (part1
         Map.empty
         [ (('>', 2), (131, 110))
         , (('^', 0), (132, 110))
         , (('>', 0), (145, 139))
         ])
    assertEqual
      []
      (108, 49)
      (part1
         Map.empty
         [(('v', 2), (108, 48)), (('^', 0), (108, 49)), (('>', 2), (87, 64))])

part2tests =
  testCase "Day 13 -- Finding the location of the last surviving cart" $
    -- After four very expensive crashes, a tick ends with only one cart
    -- remaining; its final location is 6,4.
   do
    assertEqual [] (6, 4) (uncurry part2 $ parseData testinput2)
    -- Additional tests for edge cases.
    assertEqual
      []
      (4, 1)
      (uncurry part2 $
       parseData
         ["/---\\", "| />+<--\\", "| | ^   |", "\\-+-/   |", "  \\-----/"])
    assertEqual
      []
      (5, 1)
      (uncurry part2 $
       parseData
         [ "/-><-\\"
         , "| />-+-<\\"
         , "| |  |  |"
         , "| |  ^  |"
         , "\\-+--/  |"
         , "  \\-----/"
         ])
