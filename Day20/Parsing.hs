module Day20.Parsing
  ( Tree(..)
  , parseData
  ) where

import           Text.ParserCombinators.ReadP
import           Utilities.Parsing

import           Debug.Trace

data Tree a = Tree
  { _prefix   :: [a]
  , _children :: [Tree a]
  , _suffix   :: [Tree a]
  } deriving (Eq, Show)

empty :: ReadP (Tree Char)
empty = return $! (Tree "" [] [])

branch :: ReadP [(Tree Char)]
branch = between (char '(') (char ')') (sepBy (node +++ empty) (char '|'))

node :: ReadP (Tree Char)
node = do
  p <- munch1 (`elem` "NWSE_")
  c <- option [] branch
  s <- option [] (count 1 node)
  return $! (Tree (filter (/= '_') p) c s)

tree :: ReadP (Tree Char)
tree = do
  char '^'
  t <- node
  char '$'
  skipSpaces
  eof
  return $! t

fixparens :: String -> String
fixparens (')':'(':xs) = ')' : '_' : '(' : fixparens xs
fixparens (x:xs)       = x : fixparens xs
fixparens ""           = ""

parseData :: String -> Tree Char
parseData = parse tree . fixparens
