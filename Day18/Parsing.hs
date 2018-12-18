module Day18.Parsing
  ( Forest(..)
  , parseData
  ) where

import           Data.Array
import           Data.List

type Forest = Array (Int, Int) Char

parseData :: [String] -> Forest
parseData f = array ((1, 1), (width, height)) (parse' f)
  where
    parse' =
      foldr (++) [] .
      (zipWith (\y l -> map (\(x, c) -> ((x, y), c)) l) [1 ..]) .
      map (zip [1 ..])
    width = length . head $ f
    height = length f
