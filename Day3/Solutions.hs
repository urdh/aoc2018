{-# LANGUAGE TemplateHaskell #-}

module Day3.Solutions
  ( input
  , part1
  , part2
  ) where

import           Data.FileEmbed
import           Data.List
import           Day3.Parsing   (Rectangle (..), parseLine)

left :: Rectangle -> Int
left = fst . position

top :: Rectangle -> Int
top = snd . position

right :: Rectangle -> Int
right r = (left r) + (width r) - 1

bottom :: Rectangle -> Int
bottom r = (top r) + (height r) - 1

isInside :: (Int, Int) -> Rectangle -> Bool
isInside (x, y) r =
  ((left r) <= x) && (x <= (right r)) && ((top r) <= y) && (y <= (bottom r))

isInsideCount :: [Rectangle] -> (Int, Int) -> Int
isInsideCount rs pos = (length . (filter id) . map (isInside pos)) rs

intersects :: Rectangle -> Rectangle -> Bool
intersects r1 r2 =
  intersect1D (left r1, right r1) (left r2, right r2) &&
  intersect1D (top r1, bottom r1) (top r2, bottom r2)
  where
    intersect1D (x, y) (u, w) =
      not $ (x < min u w && y < min u w) || (x > max u w && y > max u w)

overlapArea :: [Rectangle] -> Int
overlapArea rs =
  (length . (filter ((<) 1)) . (map $ isInsideCount rs) . coords) rs
  where
    coords rs =
      [ (x, y)
      | x <- [((minimum . map left) rs) .. ((maximum . map right) rs)]
      , y <- [((minimum . map top) rs) .. ((maximum . map bottom) rs)]
      ]

part1 :: [(Int, Rectangle)] -> Int
part1 = overlapArea . (map snd)

part2 :: [(Int, Rectangle)] -> Maybe Int
part2 rs = fmap fst (find (flip ((not .) . any . contested) rs) rs)
  where
    contested r1 r2 = (intersects (snd r1) (snd r2)) && ((fst r1) /= (fst r2))

input :: [(Int, Rectangle)]
input = (map parseLine) . lines $ $(embedStringFile "Day3/input.txt")
