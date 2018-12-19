{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day19.Solutions
Description : Solutions for day 19 of the 2018 Advent of Code.
-}
module Day19.Solutions
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

part1 :: (Int, [Instruction]) -> Maybe Registers
part1 (iptr, p) =
  fmap (op ("addi", iptr, (-1), iptr)) .
  fmap snd . find (isNothing . fst) . iterate (tick iptr p) $
  (Just (head p), [0, 0, 0, 0, 0, 0])

-- | Manually reverse-engineered program:
-- @
-- #ip 1            Registers: A, ip, B, C, D, E
-- addi 1 16 1   00 goto 17
-- seti 1 5 3    01 C = 1        ; C = 1; do {
-- seti 1 7 5    02 E = 1        ;   E = 1; do {
-- mulr 3 5 4       D = C * E    ;     D = (C * E)
-- eqrr 4 2 4       D = (D == B)
-- addr 4 1 1    05 goto 06 + D  ;     if (D == B)
-- addi 1 1 1       goto 08
-- addr 3 0 0       A += C       ;       A += C
-- addi 5 1 5       E += 1       ;     E += 1
-- gtrr 5 2 4       D = (E > B)
-- addr 1 4 1    10 goto 11 + D  ;   } until (E > B)
-- seti 2 1 1       goto 03
-- addi 3 1 3       C += 1       ;   C += 1
-- gtrr 3 2 4       D = (C > B)
-- addr 4 1 1       goto 14 + D  ; } until (C > B)
-- seti 1 3 1    15 goto 02
-- mulr 1 1 1       goto 257     ; return A
-- addi 2 2 2    17 B += 2
-- mulr 2 2 2       B *= B
-- mulr 1 2 2       B *= 19
-- muli 2 11 2   20 B *= 11
-- addi 4 7 4       D += 7
-- mulr 4 1 4       D *= 22
-- addi 4 13 4      D += 13
-- addr 2 4 2       B += D       ; B = (2 * 2 * 19 * 11) + ((7 * 22) + 13) = 1003
-- addr 1 0 1    25 goto 26 + A
-- seti 0 9 1       goto 01
-- setr 1 0 4       D = 27
-- mulr 4 1 4       D *= 28
-- addr 1 4 4       D += 29
-- mulr 1 4 4    30 D *= 30
-- muli 4 14 4      D *= 14
-- mulr 4 1 4       D *= 32      ; D = (((27 * 28) + 29) * 30 * 14 * 32) = 10550400
-- addr 2 4 2       B += D       ; B = B + D = 10551403
-- seti 0 2 0       A = 0
-- seti 0 0 1    35 goto 01
-- @
--
-- In pseudo-code:
--
-- @
-- D = 10550400
-- B = 1003 + ((A == 0) ? 0 : D)
-- A = 0
-- for (C = 1; C <= B; C += 1)
-- {
--   for (E = 1; E <= B; E += 1)
--   {
--     if ((C * E) == B)
--     {
--       A += C
--     }
--   }
-- }
-- @
--
-- In essence this sums the divisors of B into A:
--
-- @
-- program b = sum . map snd . filter (\(c, e) -> (c * e) == b) $ [(c, e) | c <- [1..b], e <- [1..b]]
-- program b = sum . map (\c -> c + (b `div` c)) $
--             [c | c <- [1..(ceiling . sqrt . fromIntegral $ b)], b `rem` c == 0] -- equivalently
-- @
part2 :: (Int, [Instruction]) -> Int
part2 (_, _) = program (10550400 + 1003)
  where
    program b =
      sum . map (\c -> c + (b `div` c)) $
      [c | c <- [1 .. (ceiling . sqrt . fromIntegral $ b)], b `rem` c == 0]

input :: (Int, [Instruction])
input = parseData (lines $(embedStringFile "Day19/input.txt"))
