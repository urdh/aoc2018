{-|
Module      : Day9.Solutions
Description : Solutions for day 9 of the 2018 Advent of Code.
-}
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

-- | Place marbles as described by the problem description.
place :: Int -> Marbles -> (Int, Marbles)
place n t
  | (n `mod` 23) == 0 =
    (n + (_focus . moveN (-7) $ t), (fromJust (deleteRight . moveN (-7) $ t)))
  | otherwise = (0, (insertLeft n . moveN 2) t)

-- | See 'part1' for a description of the algorithm. This function keeps track of player scores
--   and pairs players with marbles (using 'zip'). See 'place' for the marble-placing routine.
play :: Int -> Int -> Scores
play n m = fst . foldl' go (Map.empty, singleton 0) $ zip players pieces
  where
    go (scores, t) (player, x) = do
      (Map.insertWith (+) player (fst (place x t)) scores, snd (place x t))
    players = (`mod` n) <$> [0 ..]
    pieces = [1 .. m]

-- | The solution to both problems is identical, since the second problem simply increases the
--   overall length of the game. The game is set up using a circular pointed list, where the current
--   marble is the focus element. The game is played by placing each marble as described in the
--   problem, and accumulating the score of the move into the correct player. Scores are stored in
--   a 'Map Int Int' where the key is the player number (zero-indexed).
part1 :: Int -> Int -> Int
part1 n m = maximum (Map.elems (play n m))

-- | See description for 'part1'.
part2 :: Int -> Int -> Int
part2 n m = maximum (Map.elems (play n (m * 100)))

-- | The input to this problem is a player count and the value of the marble on which to end.
input :: (Int, Int)
input = (411, 71058)
