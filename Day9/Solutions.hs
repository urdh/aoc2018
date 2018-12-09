module Day9.Solutions
  ( input
  , part1
  , part2
  ) where

import           Data.List
import           Data.List.PointedList.Circular
import qualified Data.Map.Strict                as Map
import           Data.Maybe

type Marbles = PointedList Int

type Scores = Map.Map Int Int

place :: Int -> Marbles -> (Int, Marbles)
place n t
  | (n `mod` 23) == 0 =
    (n + (_focus . moveN (-7) $ t), (fromJust (deleteRight . moveN (-7) $ t)))
  | otherwise = (0, (insertLeft n . moveN 2) t)

play :: Int -> Int -> Scores
play n m = fst . foldl' go (Map.empty, singleton 0) $ zip players pieces
  where
    go (scores, t) (player, x) = do
      (Map.insertWith (+) player (fst (place x t)) scores, snd (place x t))
    players = (`mod` n) <$> [0 ..]
    pieces = [1 .. m]

part1 :: Int -> Int -> Int
part1 n m = maximum (Map.elems (play n m))

part2 :: Int -> Int -> Int
part2 n m = maximum (Map.elems (play n (m * 100)))

input :: (Int, Int)
input = (411, 71058)
