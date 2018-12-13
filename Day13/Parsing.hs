module Day13.Parsing
  ( Cart(..)
  , Coord(..)
  , Board(..)
  , parseData
  ) where

import           Control.Monad
import           Data.List
import qualified Data.Map.Strict as Map

type Cart = (Char, Int)

type Coord = (Int, Int)

type Board = Map.Map Coord Char

parseData :: [String] -> (Board, [(Cart, Coord)])
parseData = liftM2 (,) board carts . items
  where
    carts = map (\(p, c) -> ((c, 0), p)) . filter (flip elem "<>^v" . snd)
    board = Map.fromList . filter (flip elem "/\\+" . snd)
    items =
      foldr (++) [] .
      (zipWith (\y l -> map (\(x, c) -> ((x, y), c)) l) [0 ..]) .
      map (zip [0 ..])
