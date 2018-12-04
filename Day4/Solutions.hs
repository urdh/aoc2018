{-# LANGUAGE TemplateHaskell #-}

module Day4.Solutions
  ( input
  , part1
  , part2
  ) where

import           Data.FileEmbed
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set.Range  as Range
import           Data.Time
import           Day4.Parsing    (Entry (..), Event (..), parseData)

makeRanges :: [Entry] -> [(Int, [Range.Range Int])]
makeRanges [] = []
makeRanges (x:y:xs)
  | (isSameGuard x y) && (isSleepRange x y) =
    [((guard x), (range x y))] ++ (makeRanges xs)
  | otherwise = makeRanges ([x] ++ xs)
  where
    isSameGuard x y = (uncurry (==)) ((guard x), (guard y))
    isSleepRange x y = (==) (FallAsleep, WakeUp) ((evt x), (evt y))
    range x y = [((minute $ time x), ((minute $ time y) - 1))]
    minute x = read (formatTime defaultTimeLocale "%M" x) :: Int

mergeData :: [Entry] -> [(Int, [Range.Range Int])]
mergeData =
  Map.toList .
  (Map.fromListWith (++)) . makeRanges . filter (((/=) BeginShift) . evt)

coveringRange :: [Range.Range Int] -> Range.RangeSet Int
coveringRange = foldr Range.union Range.empty . (map (\x -> [x]))

countElements :: (Ord a, Eq a) => [a] -> [(Int, a)]
countElements = (map (\l@(x:xs) -> (length l, x))) . (groupBy (==)) . sort

mapsnd :: (b -> c) -> (a, b) -> (a, c)
mapsnd f (x, y) = (x, (f y))

part1 :: [Entry] -> (Int, Int)
part1 = (mapsnd commonMinute) . bestGuard
  where
    bestGuard = last . (sortOn (Range.size . snd)) . mergeData
    commonMinute = snd . maximum . countElements . Range.toList

part2 :: [Entry] -> (Int, Int)
part2 = (mapsnd snd) . last . (sortOn (fst . snd)) . bestMinute
  where
    bestMinute =
      (map (mapsnd $ maximum . countElements . Range.toList)) . mergeData

input :: [Entry]
input = (parseData . lines) $(embedStringFile "Day4/input.txt")
