module Day8.Parsing
  ( Node(..)
  , parseData
  ) where

import qualified Control.Monad                as Monad
import           Data.List
import           Text.ParserCombinators.ReadP

data Node = Node
  { children :: [Node]
  , metadata :: [Int]
  } deriving (Show, Eq)

integer :: ReadP Int
integer = fmap read (many1 digit)
  where
    digit = satisfy (Monad.liftM2 (&&) (>= '0') (<= '9'))

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
parseData = fst . head . readP_to_S node . (++ " ")
