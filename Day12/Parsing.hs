module Day12.Parsing
  ( Tape(..)
  , Rule(..)
  , parseTape
  , parseRules
  , parseData
  ) where

import           Control.Monad
import           Data.Array
import           Data.List
import           Text.ParserCombinators.ReadP

type Tape = [Int]

type Rule = [Bool]

rule :: ReadP Rule
rule = do
  s <- (count 5 (satisfy (flip elem "#.")))
  skipSpaces
  string "=>"
  skipSpaces
  char '#'
  eof
  return (map (== '#') s)

parseRules :: [String] -> [Rule]
parseRules =
  foldl (++) [] . map (\s -> take 1 [x | (x, "") <- (readP_to_S rule s)])

parseTape :: String -> Tape
parseTape = findIndices (== '#') . drop (length "initial state: ")

parseData :: [String] -> ([Rule], Tape)
parseData = ap ((,) . parseRules . tail) (parseTape . head)
