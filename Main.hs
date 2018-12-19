module Main
  ( main
  ) where

import qualified Day1.Solutions     as Day1
import qualified Day10.Solutions    as Day10
import qualified Day11.Solutions    as Day11
import qualified Day12.Solutions    as Day12
import qualified Day13.Solutions    as Day13
import qualified Day14.Solutions    as Day14
import qualified Day16.Solutions    as Day16
import qualified Day18.Solutions    as Day18
import qualified Day19.Solutions    as Day19
import qualified Day2.Solutions     as Day2
import qualified Day3.Solutions     as Day3
import qualified Day4.Solutions     as Day4
import qualified Day5.Solutions     as Day5
import qualified Day6.Solutions     as Day6
import qualified Day7.Solutions     as Day7
import qualified Day8.Solutions     as Day8
import qualified Day9.Solutions     as Day9
import           System.Environment
import           System.Exit

day1 :: IO ()
day1 = do
  putStrLn ("Day 1 -- Final frequency: " ++ show (Day1.part1 Day1.input))
  putStrLn ("Day 1 -- First duplicate: " ++ show (Day1.part2 Day1.input))

day2 :: IO ()
day2 = do
  putStrLn ("Day 2 -- Checksum: " ++ show (Day2.part1 Day2.input))
  putStrLn ("Day 2 -- Common characters: " ++ show (Day2.part2 Day2.input))

day3 :: IO ()
day3 = do
  putStrLn ("Day 3 -- Contested area: " ++ show (Day3.part1 Day3.input))
  putStrLn ("Day 3 -- Uncontested claims: " ++ show (Day3.part2 Day3.input))

day4 :: IO ()
day4 = do
  putStrLn
    ("Day 4 -- Best guard/minute combination (strategy 1): " ++
     show (Day4.part1 Day4.input))
  putStrLn
    ("Day 4 -- Best guard/minute combination (strategy 2): " ++
     show (Day4.part2 Day4.input))

day5 :: IO ()
day5 = do
  putStrLn
    ("Day 5 -- Resulting polymer length: " ++ show (Day5.part1 Day5.input))
  putStrLn ("Day 5 -- Best polymer length: " ++ show (Day5.part2 Day5.input))

day6 :: IO ()
day6 = do
  putStrLn
    ("Day 6 -- Distance-maximizing area: " ++ show (Day6.part1 250 Day6.input))
  putStrLn
    ("Day 6 -- Neighbour-maximizing area: " ++
     show (Day6.part2 10000 Day6.input))

day7 :: IO ()
day7 = do
  putStrLn ("Day 7 -- Correct build order: " ++ show (Day7.part1 Day7.input))
  putStrLn
    ("Day 7 -- Total time to build: " ++ show (Day7.part2 15 60 Day7.input))

day8 :: IO ()
day8 = do
  putStrLn ("Day 8 -- Sum of all leaf nodes: " ++ show (Day8.part1 Day8.input))
  putStrLn ("Day 8 -- Root node value: " ++ show (Day8.part2 Day8.input))

day9 :: IO ()
day9 = do
  putStrLn
    ("Day 9 -- Winning score (1): " ++ show (uncurry Day9.part1 Day9.input))
  putStrLn
    ("Day 9 -- Winning score (2): " ++ show (uncurry Day9.part2 Day9.input))

day10 :: IO ()
day10 = do
  putStr ("Day 10 -- Revealed message:\n" ++ (Day10.part1 Day10.input))
  putStrLn
    ("Day 10 -- Time until message appears: " ++ show (Day10.part2 Day10.input))

day11 :: IO ()
day11 = do
  putStrLn ("Day 11 -- Best 3x3 square: " ++ show (Day11.part1 Day11.input))
  putStrLn ("Day 11 -- Best NxN square: " ++ show (Day11.part2 Day11.input))

day12 :: IO ()
day12 = do
  putStrLn
    ("Day 12 -- Plant position sum after 20 generations: " ++
     show (Day12.part1 Day12.input))
  putStrLn
    ("Day 12 -- Plant position sum after 5 billion generations: " ++
     show (Day12.part2 Day12.input))

day13 :: IO ()
day13 = do
  putStrLn
    ("Day 13 -- First collision: " ++ show (uncurry Day13.part1 Day13.input))
  putStrLn
    ("Day 13 -- Last remaining cart: " ++ show (uncurry Day13.part2 Day13.input))

day14 :: IO ()
day14 = do
  putStrLn ("Day 14 -- Recipe scores: " ++ show (Day14.part1 Day14.input))
  putStrLn
    ("Day 14 -- Number of recipes: " ++ show (Day14.part2 (show Day14.input)))

day16 :: IO ()
day16 = do
  putStrLn
    ("Day 16 -- Multi-opcode samples: " ++ show (Day16.part1 Day16.input))
  putStrLn ("Day 16 -- Program output: " ++ show (Day16.part2 Day16.input))

day18 :: IO ()
day18 = do
  putStrLn ("Day 18 -- Resource value: " ++ show (Day18.part1 Day18.input))
  putStrLn ("Day 18 -- Resource value: " ++ show (Day18.part2 Day18.input))

day19 :: IO ()
day19 = do
  putStrLn ("Day 19 -- Program output: " ++ show (Day19.part1 Day19.input))
  putStrLn ("Day 19 -- Program output: " ++ show (Day19.part2 Day19.input))

dispatch :: [String] -> IO ()
dispatch []     = exitSuccess
dispatch ["1"]  = day1
dispatch ["2"]  = day2
dispatch ["3"]  = day3
dispatch ["4"]  = day4
dispatch ["5"]  = day5
dispatch ["6"]  = day6
dispatch ["7"]  = day7
dispatch ["8"]  = day8
dispatch ["9"]  = day9
dispatch ["10"] = day10
dispatch ["11"] = day11
dispatch ["12"] = day12
dispatch ["13"] = day13
dispatch ["14"] = day14
dispatch ["16"] = day16
dispatch ["18"] = day18
dispatch ["19"] = day19
dispatch [_]    = exitFailure
dispatch (x:xs) = dispatch [x] >> dispatch xs

main :: IO ()
main = getArgs >>= dispatch >> exitSuccess
