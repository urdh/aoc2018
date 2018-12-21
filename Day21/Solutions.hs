{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day21.Solutions
Description : Solutions for day 21 of the 2018 Advent of Code.
-}
module Day21.Solutions
  ( part1
  , part2
  , input
  ) where

import           Control.Arrow
import           Control.Monad
import           Data.Bits
import           Data.FileEmbed
import           Data.List
import           Data.Maybe
import           Day19.Parsing

-- | Helper for setting a register to a value.
set' :: Int -> Int -> Registers -> Registers
set' n x = ap ((++) . take n) ((x :) . drop (n + 1))

-- | Helper for getting the value of a register.
get' :: Int -> Registers -> Int
get' = flip (!!)

-- | Helper for the gtXX instructions.
cmp' :: Int -> Int -> Int
cmp' a b
  | a > b = 1
  | otherwise = 0

-- | Helper for the eqXX instructions.
eq' :: Int -> Int -> Int
eq' a b
  | a == b = 1
  | otherwise = 0

-- | Applies an instruction to the given registers.
op :: Instruction -> Registers -> Registers
op ("addr", l, r, o) = set' o =<< ap ((+) . get' l) (get' r)
op ("addi", l, v, o) = set' o =<< (v +) . get' l
op ("mulr", l, r, o) = set' o =<< ap ((*) . get' l) (get' r)
op ("muli", l, v, o) = set' o =<< (v *) . get' l
op ("banr", l, r, o) = set' o =<< ap ((.&.) . get' l) (get' r)
op ("bani", l, v, o) = set' o =<< (v .&.) . get' l
op ("borr", l, r, o) = set' o =<< ap ((.|.) . get' l) (get' r)
op ("bori", l, v, o) = set' o =<< (v .|.) . get' l
op ("setr", s, _, o) = set' o =<< get' s
op ("seti", s, _, o) = set' o s
op ("gtir", v, r, o) = set' o =<< cmp' v . get' r
op ("gtri", l, v, o) = set' o =<< flip cmp' v . get' l
op ("gtrr", l, r, o) = set' o =<< ap (cmp' . get' l) (get' r)
op ("eqir", v, r, o) = set' o =<< eq' v . get' r
op ("eqri", l, v, o) = set' o =<< flip eq' v . get' l
op ("eqrr", l, r, o) = set' o =<< ap (eq' . get' l) (get' r)

callop :: Int -> Instruction -> Registers -> Registers
callop iptr = (op ("addi", iptr, 1, iptr) .) . op

nextop :: Int -> [Instruction] -> Registers -> Maybe Instruction
nextop iptr p = listToMaybe . flip drop p . (!! iptr)

tick ::
     Int
  -> [Instruction]
  -> (Maybe Instruction, Registers)
  -> (Maybe Instruction, Registers)
tick iptr p (Just i, r) = ((,) =<< nextop iptr p) . (callop iptr i) $ r

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p =
  foldr
    (\x ys ->
       if not (p x)
         then x : ys
         else [x])
    []

run :: (Int, [Instruction]) -> Registers -> [(Maybe Instruction, Registers)]
run (iptr, p) r =
  takeUntil (isNothing . fst) . iterate (tick iptr p) $ (Just (head p), r)

-- | Looking at the input, it's obvious that the only way the program will exit
--   is if the comparison at instruction 28 is true (this is also the only
--   instruction that inspects the modifiable register). Just run the program
--   up until the first time that instruction is hit, then return the registers
--   after this instruction has been run. Clever inspection gives the desired value.
part1 :: (Int, [Instruction]) -> Maybe Int
part1 (iptr, p) =
  fmap (getval) .
  fmap (first fromJust) .
  find (\x -> Just (p !! 28) == fst x) . flip run [0, 0, 0, 0, 0, 0] $
  (iptr, p)
  where
    getval (("eqrr", a, b, _), rs) = (rs !!) . head . filter (/= iptr) $ [a, b]

-- | The main loop of the program is basically this:
--
-- @
-- D = 0;
-- do {
--   C = D | 0x10000;
--   D = 0x5DE05D;
--   while (true)
--   {
--     D = (D + (C & 0x100)) & 0xFFFFFF;
--     D = (D * 0x01016B) & 0xFFFFFF;
--     if (C < 0x100) break;
--     C /= 0x100; // C >>= 8
--   }
-- } while (D != A)
-- @
--
-- The worst initial value is the value D takes on the last iteration before the first
-- (i.e. best) value reappears. This solution reimplements the elfcode in Haskell to solve.
part2 :: (Int, [Instruction]) -> Int
part2 (iptr, p) =
  foldr1 (flip const) . foldr (\x r -> x : takeWhile (/= x) r) [] $ values
  where
    loop (0, d) = d
    loop (c, d) =
      loop
        ( shift c (-8)
        , (((d + (c .&. 0xFF)) .&. 0xFFFFFF) * 0x01016B) .&. 0xFFFFFF)
    values =
      iterate (\d -> loop (d .|. 0x10000, 0x5DE05D)) . fromJust $!
      part1 (iptr, p)

input :: (Int, [Instruction])
input = parseData (lines $(embedStringFile "Day21/input.txt"))
