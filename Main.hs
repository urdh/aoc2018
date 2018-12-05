module Main
  ( main
  ) where

import qualified Day1.Solutions     as Day1
import qualified Day2.Solutions     as Day2
import qualified Day3.Solutions     as Day3
import qualified Day4.Solutions     as Day4
import qualified Day5.Solutions     as Day5
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

dispatch :: [String] -> IO ()
dispatch []     = exitSuccess
dispatch ["1"]  = day1
dispatch ["2"]  = day2
dispatch ["3"]  = day3
dispatch ["4"]  = day4
dispatch ["5"]  = day5
dispatch [_]    = exitFailure
dispatch (x:xs) = (dispatch [x]) >> (dispatch xs)

main :: IO ()
main = getArgs >>= dispatch >> exitSuccess
