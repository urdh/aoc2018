{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day3.Solutions
Description : Solutions for day 3 of the 2018 Advent of Code.
-}
module Day3.Solutions
  ( input
  , part1
  , part2
  ) where

import           Control.Monad
import           Data.FileEmbed
import           Data.List
import           Day3.Parsing   (Rectangle (..), parseLine)

-- | Get the x coordinate of the leftmost edge of a 'Rectangle'.
left :: Rectangle -> Int
left = fst . position

-- | Get the y coordinate of the upper edge of a 'Rectangle'.
top :: Rectangle -> Int
top = snd . position

-- | Get the x coordinate of the rightmost edge of a 'Rectangle'.
right :: Rectangle -> Int
right = subtract 1 . liftM2 (+) left width

-- | Get the y coordinate of the lower edge of a 'Rectangle'.
bottom :: Rectangle -> Int
bottom = subtract 1 . liftM2 (+) top height

-- | Determine is a point is inside a given 'Rectangle'.
isInside :: (Int, Int) -> Rectangle -> Bool
isInside (x, y) r =
  ((left r) <= x) && (x <= (right r)) && ((top r) <= y) && (y <= (bottom r))

-- | Count how many 'Rectangle's contain a given point.
isInsideCount :: [Rectangle] -> (Int, Int) -> Int
isInsideCount = flip (((length . filter id) .) . map . isInside)

-- | Determine if two 'Rectangle's intersect.
intersects :: Rectangle -> Rectangle -> Bool
intersects r1 r2 =
  intersect1D (left r1, right r1) (left r2, right r2) &&
  intersect1D (top r1, bottom r1) (top r2, bottom r2)
  where
    intersect1D (x, y) (u, w) =
      not (x < min u w && y < min u w || x > max u w && y > max u w)

-- | Calculate the overlap area given a list of 'Rectangle's. See 'part1' for algorithm description.
overlapArea :: [Rectangle] -> Int
overlapArea = ((length . filter (1 <)) .) =<< (. coords) . map . isInsideCount
  where
    coords rs =
      [ (x, y)
      | x <- [((minimum . map left) rs) .. ((maximum . map right) rs)]
      , y <- [((minimum . map top) rs) .. ((maximum . map bottom) rs)]
      ]

-- | The solution to the first problem simply naively counts the number of 'Rectangle's
--   that overlap each individual square in a bounding box given by the top-left-most
--   and bottom-right-most coordinate among all the rectangles. The area of overlap is
--   then given by counting the number of squares that are inside more than one 'Rectangle'.
part1 :: [(Int, Rectangle)] -> Int
part1 = overlapArea . map snd

-- | The solution to the second problem is easier. For each rectangle, check if it intersects
--   any other rectangle in the set (not including itself). Find the first such rectangle, and
--   return its ID.
part2 :: [(Int, Rectangle)] -> Maybe Int
part2 = fmap fst . (find =<< flip ((not .) . any . contested))
  where
    contested r1 r2 = (intersects (snd r1) (snd r2)) && ((fst r1) /= (fst r2))

-- | The input to this problem is a set of IDs, coordinates, and sizes.
--   These are represented as a pair consisting of the ID and a 'Rectangle'.
input :: [(Int, Rectangle)]
input = map parseLine (lines $(embedStringFile "Day3/input.txt"))
