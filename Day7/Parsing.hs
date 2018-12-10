module Day7.Parsing
  ( parseLine
  , parseData
  ) where

import           Data.List
import qualified Data.Map.Strict              as Map
import           Text.ParserCombinators.ReadP
import           Utilities.Parsing

dependency :: ReadP [(Char, [Char])]
dependency = do
  string "Step "
  a <- get
  string " must be finished before step "
  b <- get
  string " can begin."
  eof
  return [(b, [a]), (a, [])]

parseLine :: String -> [(Char, [Char])]
parseLine = parse dependency

parseData :: [String] -> [(Char, [Char])]
parseData =
  Map.toList . (Map.fromListWith (++)) . (foldr (++) []) . map parseLine
