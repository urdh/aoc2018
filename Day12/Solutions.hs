{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day12.Solutions
Description : Solutions for day 12 of the 2018 Advent of Code.
-}
module Day12.Solutions
  ( part1
  , part2
  , input
  ) where

import           Control.Arrow
import           Control.Monad
import           Data.FileEmbed
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Tuple
import           Day12.Parsing  (Rule (..), Tape (..), parseData)

-- | Check if each of the four closest neighbors of a point on the tape are also alive.
hood :: Tape -> Int -> [Bool]
hood t x = map (\x' -> x' `elem` t) $ map (+ x) [-2 .. 2]

-- | Run one generation of the automata.
evolve :: [Rule] -> Tape -> Tape
evolve rs t =
  filter (\x -> (hood t x) `elem` rs) [(minimum t - 2) .. (maximum t + 2)]

-- | Like evolve, but also shift the tape so the first live cell is at 0, accumulate the offset.
evolve' :: [Rule] -> (Int, Tape) -> (Int, Tape)
evolve' rs (o, t) = (o + minimum t', map (subtract (minimum t')) t')
  where
    t' = evolve rs t

-- | Find the first element matching the predicate, as well as its index.
findWithIndex :: (a -> Bool) -> [a] -> (Int, a)
findWithIndex p a = (\x -> (x, a !! x)) . fromJust . findIndex p $ a

-- | This is basically a one-dimensional cellular automaton. For 20 generations, just running the
--   automaton is sufficient. We represent the current state (tape) as a list of indices at which
--   the cells are alive, and each rule as a list of 5 booleans representing the neighborhood
--   centered around a point; True would indicate that this neighbor must be alive for the rule
--   to apply, and False the opposite. If a rule matches, the cell will be alive in the next
--   generation, otherwise not (i.e. only rules resulting in live cells are stored).
part1 :: ([Rule], Tape) -> Int
part1 (rs, t) = sum . (!! 20) $ iterate (evolve rs) t

-- | The second part is more challenging; the number of generations make brute-forcing impractical.
--   Fortunately, the automaton converges into a set of gliders after a number of generations, after
--   which the whole tape is just shifted to the right. We find the first generation for which this
--   is the case, then simply shift it to the final generation by adding the difference to the
--   indices representing the tape.
part2 :: ([Rule], Tape) -> Int
part2 (rs, t) =
  sum . map (+ ((fst . fst $ pair) + increment * remaining)) $
  (snd . fst $ pair)
  where
    (offset, pair) =
      findWithIndex (uncurry ((==) `on` snd)) . (zip <*> tail) $
      iterate (evolve' rs) (0, t)
    increment = uncurry (-) . swap . (fst *** fst) $ pair
    remaining = 50000000000 - offset

-- | The input to this problem is a starting tape for the automata, and a set of rules.
input :: ([Rule], Tape)
input = parseData (lines $(embedStringFile "Day12/input.txt"))
