module Day8.Parsing
  ( Node(..)
  , parseData
  ) where

import           Data.List
import           Text.ParserCombinators.ReadP
import           Utilities.Parsing

data Node = Node
  { children :: [Node]
  , metadata :: [Int]
  } deriving (Show, Eq)

meta :: ReadP Int
meta = do
  i <- integer
  char ' '
  return i

node :: ReadP Node
node = do
  c <- integer
  char ' '
  n <- integer
  char ' '
  children <- count c node
  metadata <- count n meta
  return (Node children metadata)

parseData :: String -> Node
parseData = parse node . (++ " ")
