{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day4.Solutions
Description : Solutions for day 4 of the 2018 Advent of Code.
-}
module Day4.Solutions
  ( input
  , part1
  , part2
  ) where

import           Data.FileEmbed
import           Data.List
import qualified Data.Map.Strict  as Map
import qualified Data.Set.Range   as Range
import           Data.Time
import           Day4.Parsing     (Entry (..), Event (..), parseData)
import           Utilities.Tuples

-- | Given a list of corresponding 'FallAsleep', 'WakeUp' pairs, construct a list of range sets.
makeRanges :: [Entry] -> [(Int, [Range.Range Int])]
makeRanges [] = []
makeRanges (x:y:xs)
  | isSameGuard x y && isSleepRange x y = (guard x, range x y) : makeRanges xs
  | otherwise = makeRanges (x : xs)
  where
    isSameGuard x y = (uncurry (==)) ((guard x), (guard y))
    isSleepRange x y = (==) (FallAsleep, WakeUp) ((evt x), (evt y))
    range x y = [((minute $ time x), ((minute $ time y) - 1))]
    minute x = read (formatTime defaultTimeLocale "%M" x) :: Int

-- | Fold the sorted problem input into a series of range sets with an accompanying guard ID.
mergeData :: [Entry] -> [(Int, [Range.Range Int])]
mergeData =
  Map.toList .
  Map.fromListWith (++) . makeRanges . filter ((BeginShift /=) . evt)

-- | Count the number of occurrences of each unique element in a list.
--
-- @
-- countElements [1, 1, 2, 2, 2, 3] == [(2, 1), (3, 2), (1, 3)]
-- @
countElements :: (Ord a, Eq a) => [a] -> [(Int, a)]
countElements = map (\l@(x:xs) -> (length l, x)) . groupBy (==) . sort

-- | The first problem concerns finding the guard whose total aggregated number of minutes
--   in the asleep state is largest, and returning the minute in which it is most likely
--   that this guard is asleep. The best guard can be found by constructing range sets using
--   only the minute part of the timestamps (since all events are guaranteed to occur in the
--   same hour), and finding the guard with the largest number of unique points in the range set.
--   The best minute is then given by collapsing the range set to a list of minutes, and finding
--   the most common one (the mode).
part1 :: [Entry] -> (Int, Int)
part1 = mapsnd commonMinute . bestGuard
  where
    bestGuard = last . sortOn (Range.size . snd) . mergeData
    commonMinute = snd . maximum . countElements . Range.toList

-- | The second problem, finding the guard which is most frequently asleep in the same minute,
--   can be solved by collapsing all range set into points and finding the guard with the largest
--   mode.
part2 :: [Entry] -> (Int, Int)
part2 = mapsnd snd . last . sortOn (fst . snd) . bestMinute
  where
    bestMinute =
      map (mapsnd (maximum . countElements . Range.toList)) . mergeData

-- | The input to this problem is a set of log entries describing the actions of a group
--   of guards. Each guard has an ID, and the (sorted) list has been pre-processed to assign
--   a guard ID to each event in the log. Events are 'FallAsleep', 'WakeUp', and 'BeginShift'.
input :: [Entry]
input = parseData (lines $(embedStringFile "Day4/input.txt"))
