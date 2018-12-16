{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Day16.Solutions
Description : Solutions for day 16 of the 2018 Advent of Code.
-}
module Day16.Solutions
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
import           Day16.Parsing

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

-- | A map of instruction functors, keyed by their string name.
instructions :: [(String, Instruction -> Registers -> Registers)]
instructions =
  [ ("addr", addr)
  , ("addi", addi)
  , ("mulr", mulr)
  , ("muli", muli)
  , ("banr", banr)
  , ("bani", bani)
  , ("borr", borr)
  , ("bori", bori)
  , ("setr", setr)
  , ("seti", seti)
  , ("gtir", gtir)
  , ("gtri", gtri)
  , ("gtrr", gtrr)
  , ("eqir", eqir)
  , ("eqri", eqri)
  , ("eqrr", eqrr)
  ]
  where
    addr (_, l, r, o) = set' o =<< ap ((+) . get' l) (get' r)
    addi (_, l, v, o) = set' o =<< (v +) . get' l
    mulr (_, l, r, o) = set' o =<< ap ((*) . get' l) (get' r)
    muli (_, l, v, o) = set' o =<< (v *) . get' l
    banr (_, l, r, o) = set' o =<< ap ((.&.) . get' l) (get' r)
    bani (_, l, v, o) = set' o =<< (v .&.) . get' l
    borr (_, l, r, o) = set' o =<< ap ((.|.) . get' l) (get' r)
    bori (_, l, v, o) = set' o =<< (v .|.) . get' l
    setr (_, s, _, o) = set' o =<< get' s
    seti (_, s, _, o) = set' o s
    gtir (_, v, r, o) = set' o =<< cmp' v . get' r
    gtri (_, l, v, o) = set' o =<< flip cmp' v . get' l
    gtrr (_, l, r, o) = set' o =<< ap (cmp' . get' l) (get' r)
    eqir (_, v, r, o) = set' o =<< eq' v . get' r
    eqri (_, l, v, o) = set' o =<< flip eq' v . get' l
    eqrr (_, l, r, o) = set' o =<< ap (eq' . get' l) (get' r)

-- | Given an instruction and input registers, determine which instructions could have
--   resulted in the output registers. Return a list of the string names of these instructions.
matches :: Instruction -> Registers -> Registers -> [String]
matches instr input output =
  map fst . filter ((==) output . snd) $
  map (second (flip ($ instr) input)) instructions

-- | Given a map of instructions and candidate string names, find all instructions that can
--   be uniquely mapped to string names, and return the opcode and string name for all these.
certain :: [(Instruction, [String])] -> [(Int, String)]
certain =
  nub . map (\((i, _, _, _), s) -> (i, head s)) . filter ((== 1) . length . snd)

-- | Iteratively reduce the instruction/string name candidates until all instructions have
--   been uniquely assigned to an opcode.
reduce :: [(Int, String)] -> [(Instruction, [String])] -> [(Int, String)]
reduce known unknown
  | length (certain unknown) == 0 = known
  | otherwise = (reduce k (map (removeInstr) . filter (opcodeKnown) $ unknown))
  where
    k = known ++ (certain unknown)
    removeInstr (i, s) = (i, s \\ (map snd $ k))
    opcodeKnown ((i, _, _, _), s) = not $ i `elem` (map fst $ k)

-- | Apply an instruction to the registers, given the opcode-instruction map.
apply :: [(Int, String)] -> Registers -> Instruction -> Registers
apply map' input (i, a, b, c) = func (i, a, b, c) input
  where
    instr = fromJust (lookup i map')
    func = fromJust (lookup instr instructions)

-- | Part one is simple; just execute the instruction on the input as if it was every opcode,
--   and collect the result. Filter the output to keep only the ones matching the sample. After
--   that, the answer is calculated by counting the samples with three or more matchin outputs.
part1 :: ([Sample], [Instruction]) -> Int
part1 (s, _) =
  (length . filter (> 2) . map (\(b, i, a) -> length (matches i b a)) $ s)

-- | Part 2 is slightly more work; using the output from running the samples against all opcodes,
--   find the samples that have only one possible opcode. Save this to a map, and remove this
--   opcode and the corresponding instruction from all other samples; repeat until all instructions
--   have an opcode in the map. Then, simply run the instructions from the program in sequence.
part2 :: ([Sample], [Instruction]) -> Registers
part2 (s, p) = (foldl (apply map') [0, 0, 0, 0] p)
  where
    map' = reduce [] . map (\(b, i, a) -> (i, matches i b a)) $ s

-- | The input to this problem is a set of (input, instruction, output) samples, and a set of
--   instructions forming a program.
input :: ([Sample], [Instruction])
input = parseData (lines $(embedStringFile "Day16/input.txt"))
