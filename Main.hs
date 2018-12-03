module Main
  ( main
  ) where

import qualified Day1.Solutions     as Day1
import           System.Environment

day1 :: IO ()
day1 = do
  putStrLn ("Day 1 -- Final frequency: " ++ show (Day1.part1 Day1.input))
  putStrLn ("Day 1 -- First duplicate: " ++ show (Day1.part2 Day1.input))

help :: IO ()
help = do
  prog <- getProgName
  putStrLn ("Usage: " ++ prog ++ " <day>")

dispatch :: [(String, IO ())]
dispatch = [("1", day1), ("help", help)]

main :: IO ()
main = do
  args <- getArgs
  let (Just action) = lookup (head $ args ++ ["help"]) dispatch
  action
