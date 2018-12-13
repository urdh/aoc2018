{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day13.Solutions
Description : Solutions for day 13 of the 2018 Advent of Code.
-}
module Day13.Solutions
  ( part1
  , part2
  , input
  ) where

import           Control.Arrow
import           Control.Monad
import           Data.FileEmbed
import           Data.Function
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Tuple
import           Day13.Parsing   (Board (..), Cart (..), Coord (..), parseData)

rotmap :: [(Char, Char)]
rotmap = [('>', 'v'), ('v', '<'), ('<', '^'), ('^', '>'), ('x', 'x')]

ccw :: Char -> Char
ccw = fromJust . flip lookup (map swap rotmap)

cw :: Char -> Char
cw = fromJust . flip lookup rotmap

rotate :: Maybe Char -> Cart -> Cart
rotate (Just '+') (c, 0) = (ccw c, 1)
rotate (Just '+') (c, 1) = (c, 2)
rotate (Just '+') (c, 2) = (cw c, 0)
rotate (Just '/') (c, n)
  | c `elem` "<>" = (ccw c, n)
  | otherwise = (cw c, n)
rotate (Just '\\') (c, n)
  | c `elem` "<>" = (cw c, n)
  | otherwise = (ccw c, n)
rotate Nothing (c, n) = (c, n)

move :: Cart -> Coord -> Coord
move ('<', _) = first (subtract 1)
move ('^', _) = second (subtract 1)
move ('>', _) = first (+ 1)
move ('v', _) = second (+ 1)
move (_, _)   = id

mapnth :: (a -> a) -> Int -> [a] -> [a]
mapnth f n = zipWith ($) ((replicate (n - 1) id) ++ (f : repeat id))

crash :: [(Cart, Coord)] -> [(Cart, Coord)]
crash = map =<< kill . dups
  where
    kill cs (cart, pos)
      | pos `elem` cs = (('x', 0), pos)
      | otherwise = (cart, pos)
    dups = ap (\\) nub . map snd

remove :: [(Cart, Coord)] -> [(Cart, Coord)]
remove (x:xs)
  | fst (fst x) == 'x' = (second (negate . abs *** negate . abs) x) : remove xs
  | otherwise = x : remove xs
remove [] = []

tick' :: Board -> (Cart, Coord) -> (Cart, Coord)
tick' board = (rotate' board) . move'
  where
    rotate' b = uncurry (join . ((,) .) . flip (rotate . (flip Map.lookup b)))
    move' = uncurry (liftM2 (.) (,) move)

tick :: Board -> [(Cart, Coord)] -> [(Cart, Coord)]
tick board carts =
  foldr1 (.) (map (ticknth board) (reverse [1 .. (length carts)])) $
  sortOn (swap . snd) carts
  where
    ticknth b n = remove . crash . mapnth (tick' b) n

part1 :: Board -> [(Cart, Coord)] -> Coord
part1 board =
  (negate *** negate) .
  snd .
  head . filter isDead . head . filter (any isDead) . (iterate (tick board))
  where
    isDead = ((== 'x') . fst . fst)

part2 :: Board -> [(Cart, Coord)] -> Coord
part2 board =
  snd .
  head .
  filter isAlive .
  head . filter ((< 2) . length . filter isAlive) . (iterate (tick board))
  where
    isAlive = ((/= 'x') . fst . fst)

input :: (Board, [(Cart, Coord)])
input = parseData (lines $(embedStringFile "Day13/input.txt"))
