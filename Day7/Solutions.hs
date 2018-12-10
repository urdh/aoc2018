{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day7.Solutions
Description : Solutions for day 7 of the 2018 Advent of Code.
-}
module Day7.Solutions
  ( input
  , part1
  , part2
  ) where

import           Control.Monad
import           Data.Char
import           Data.FileEmbed
import           Data.Function
import           Data.List
import           Day7.Parsing     (parseData)
import           Utilities.Tuples

-- | Add time remaining to all items in the list.
addTime :: Int -> [(Char, [Char])] -> [((Char, Int), [Char])]
addTime base = map (mapfst (ap (,) (subtract (64 - base) . ord)))

-- | Find a given number of dependency-less items, sorted alphabetically.
bestchars :: Int -> [((Char, Int), [Char])] -> [Char]
bestchars n =
  (map fst) .
  (take n) .
  (sortOn snd) .
  (map fst) . head . groupBy ((==) `on` (length . snd)) . sortOn (length . snd)

-- | Find all items with no time remaining.
donechars :: [((Char, Int), [Char])] -> [Char]
donechars = sort . (map fst) . (filter ((== 1) . snd)) . (map fst)

-- | The first problem may be solved by simply popping the first (alphabetically) item of the
--   input that has an empty list of dependencies, and removing that same item from the list
--   of dependencies of all the other items. This process is repeated until no more elements
--   remain, and the order in which element were popped is returned.
part1 :: [(Char, [Char])] -> [Char]
part1 [] = []
part1 m = [bestchar m] ++ (part1 (popchar (bestchar m) m))
  where
    bestchar =
      minimum .
      (map fst) .
      head . groupBy ((==) `on` (length . snd)) . sortOn (length . snd)
    popchar c = (delete (c, [])) . map (mapsnd (delete c))

-- | The second problem is more complicated. Now, we have to keep track of the time remaining
--   on each item. Additionally, instead of popping one element, we pop at most N items with
--   empty dependency lists, and instead of removing them directly we decrement the time remaining.
--   After doing this, we may pop all items that have no time remaining. The output is the number
--   of iterations we have to perform.
part2 :: Int -> Int -> [(Char, [Char])] -> Int
part2 n b = (helper n) . (addTime b)
  where
    helper _ [] = 0
    helper n m =
      1 +
      (helper
         n
         (foldr (.) id (map (popchar) (donechars m)) .
          (map (mapfst (decchar (bestchars n m)))) $
          m))
    popchar c = (delete ((c, 0), [])) . map (mapsnd (delete c))
    decchar cs (c, i)
      | elem c cs = (c, (i - 1))
      | otherwise = (c, i)

-- | The input of this problem is a set of dependencies forming a directed graph.
--   It is represented here as a pair of dependee and list of dependencies.
input :: [(Char, [Char])]
input = parseData (lines $(embedStringFile "Day7/input.txt"))
