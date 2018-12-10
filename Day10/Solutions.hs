{-# LANGUAGE TemplateHaskell #-}

module Day10.Solutions
  ( input
  , part1
  , part2
  ) where

import           Data.Array
import           Data.FileEmbed
import           Data.Function
import           Data.List
import           Data.Tuple
import           Day10.Parsing  (Point (..), parseData)

movept :: Int -> Point -> Point
movept n x = (Point (move n pos vel) vel)
  where
    move n (x, y) (u, v) = (x + (n * u), y + (n * v))
    pos = position x
    vel = velocity x

display :: [(Int, Int)] -> String
display xs =
  unlines .
  map (map snd) .
  groupBy ((==) `on` fst . fst) .
  assocs .
  accumArray
    (const id)
    '.'
    ( (minimum $ map fst xs, minimum $ map snd xs)
    , (maximum $ map fst xs, maximum $ map snd xs)) $
  map (\x -> (x, '#')) xs

offset :: [(Int, Int)] -> [(Int, Int)]
offset xs = map (sub (xmin, ymin)) xs
  where
    sub (u, v) (x, y) = (x - u, y - v)
    xmin = minimum $ map fst xs
    ymin = minimum $ map snd xs

area :: [(Int, Int)] -> Int
area xs =
  ((maximum $ map fst xs) - (minimum $ map fst xs)) *
  ((maximum $ map snd xs) - (minimum $ map snd xs))

locmin :: Ord a => [a] -> (a, Int)
locmin xs =
  head .
  map (\(x, y, z) -> x) .
  filter (\(x, y, z) -> (fst x) < (fst y) && (fst x) < (fst z)) $
  zip3 (drop 1 pairs) pairs (drop 2 pairs)
  where
    pairs = zip xs [1 ..]

findmsg :: [Point] -> ((Int, [(Int, Int)]), Int)
findmsg pts =
  locmin . map (\x -> (area x, x)) $
  map (\n -> map (position . movept n) pts) [1 ..]

part1 :: [Point] -> String
part1 = display . (map swap) . offset . snd . fst . findmsg

part2 :: [Point] -> Int
part2 = snd . findmsg

input :: [Point]
input = parseData (lines $(embedStringFile "Day10/input.txt"))
