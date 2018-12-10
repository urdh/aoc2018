module Main
  ( main
  ) where

import qualified Day1.Solutions     as Day1
import qualified Day10.Solutions    as Day10
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
dispatch [_]    = exitFailure
dispatch (x:xs) = dispatch [x] >> dispatch xs

main :: IO ()
main = getArgs >>= dispatch >> exitSuccess
