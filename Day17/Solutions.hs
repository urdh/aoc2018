{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day17.Solutions
Description : Solutions for day 17 of the 2018 Advent of Code.
-}
module Day17.Solutions
  ( part1
  , part2
  , input
  ) where

import           Control.Arrow   hiding (left, right)
import           Control.Monad
import           Data.FileEmbed
import           Data.List       hiding (delete, insert, lookup)
import           Data.Map.Strict (Map, delete, filterWithKey, fromList, insert,
                                  keys, lookup, member, size, (!))
import           Day17.Parsing   (parseLines)
import           Prelude         hiding (delete, foldr, lookup)

data Type
  = Clay
  | Still
  | Flowing
  | Unknown
  deriving (Eq, Show)

data Coord =
  Coord Int
        Int
  deriving (Eq, Ord, Show)

type Board = Map Coord Type

xc :: Coord -> Int
xc (Coord x _) = x

yc :: Coord -> Int
yc (Coord _ y) = y

maxyc :: Board -> Int
maxyc = maximum . map yc . keys

minyc :: Board -> Int
minyc = minimum . map yc . keys

down :: Coord -> Coord
down (Coord x y) = (Coord x (succ y))

right :: Coord -> Coord
right (Coord x y) = (Coord (succ x) y)

left :: Coord -> Coord
left (Coord x y) = (Coord (pred x) y)

scan :: (Coord -> Coord) -> Board -> Coord -> (Type, Coord)
scan next w c
  | lookup (next c) w == Just Clay = (Still, c) -- edge: wall reached
  | not (member (down c) w) = (Flowing, c) -- edge: nothing below
  | otherwise = scan next w (next c)

range :: (Type, Coord) -> (Type, Coord) -> (Type, [Coord])
range (t, c) (t', c') = (type' t t', (range' c c'))
  where
    type' Flowing _ = Flowing
    type' _ Flowing = Flowing
    type' _ _       = Still
    range' l r
      | xc r < xc l = range' r l
      | otherwise = takeWhile (<= r) . iterate right $ l

range' :: Board -> Coord -> (Type, [Coord])
range' w = liftM2 range (scan left w) (scan right w)

flow :: Coord -> Board -> (Type, [Coord], Board)
flow c w
  | maxyc w < yc c = (Flowing, [], w) -- leaf: below lower limit
  | member c w = (w ! c, [], w) -- leaf: clay or water floor
  | otherwise = branch c (flow (down c) w)
  where
    branch c (t, cs, w)
      | t == Flowing = (t, cs, insert c t w)
      | otherwise = recurse . commit w . range' w $ c
    commit w (t, cs) =
      (t, (sources w t cs), foldr (\c' w' -> insert c' t w') w $ cs)
    sources w t = filter (isSource w t) . ap ((++) . take 1) (take 1 . reverse)
    isSource w t c = (t == Flowing) && (not (member (down c) w))
    recurse (t, cs, w) =
      head . dropWhile (\(_, cs', _) -> not (null cs')) . iterate reduce $
      (t, cs, w)
    reduce (t, (c:cs), w) =
      (\(t', cs', w') -> (t', nub (cs ++ cs'), w')) . flow c . purgeLine c $ w
    purgeLine c w = foldr (\c' w' -> delete c' w') w . snd . range' w $ c

pour :: Board -> Board
pour w =
  filterWithKey (\k v -> (minyc w <= yc k)) .
  (\(_, _, w') -> w') . flow (Coord 500 0) $
  w

makeBoard :: [(Int, Int)] -> Board
makeBoard = fromList . flip zip (repeat Clay) . map (toCoord)
  where
    toCoord (x, y) = (Coord x y)

part1 :: [(Int, Int)] -> Int
part1 = size . filterWithKey (\k v -> (v /= Clay)) . pour . makeBoard

part2 :: [(Int, Int)] -> Int
part2 = size . filterWithKey (\k v -> (v == Still)) . pour . makeBoard

input :: [(Int, Int)]
input = parseLines (lines $(embedStringFile "Day17/input.txt"))
