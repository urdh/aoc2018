{-# LANGUAGE TemplateHaskell #-}

module Day7.Solutions
  ( input
  , part1
  , part2
  ) where

import           Control.Monad
import           Data.Char
import           Data.FileEmbed
import           Data.Function
import           Data.List
import           Day7.Parsing   (parseData)

mapfst :: (a -> c) -> (a, b) -> (c, b)
mapfst f (x, y) = ((f x), y)

mapsnd :: (b -> c) -> (a, b) -> (a, c)
mapsnd f (x, y) = (x, (f y))

addTime :: Int -> [(Char, [Char])] -> [((Char, Int), [Char])]
addTime base = map (mapfst (ap (,) (subtract (64 - base) . ord)))

bestchars :: Int -> [((Char, Int), [Char])] -> [Char]
bestchars n =
  (map fst) .
  (take n) .
  (sortOn snd) .
  (map fst) . head . groupBy ((==) `on` (length . snd)) . sortOn (length . snd)

donechars :: [((Char, Int), [Char])] -> [Char]
donechars = sort . (map fst) . (filter ((== 1) . snd)) . (map fst)

part1 :: [(Char, [Char])] -> [Char]
part1 [] = []
part1 m = [bestchar m] ++ (part1 (popchar (bestchar m) m))
  where
    bestchar =
      minimum .
      (map fst) .
      head . groupBy ((==) `on` (length . snd)) . sortOn (length . snd)
    popchar c = (delete (c, [])) . map (mapsnd (delete c))

part2 :: Int -> Int -> [(Char, [Char])] -> Int
part2 n b = (helper n) . (addTime b)
  where
    helper _ [] = 0
    helper n m =
      1 +
      (helper
         n
         (foldr (.) id (map (popchar) (donechars m)) .
          (map (mapfst (decchar (bestchars n m)))) $
          m))
    popchar c = (delete ((c, 0), [])) . map (mapsnd (delete c))
    decchar cs (c, i)
      | elem c cs = (c, (i - 1))
      | otherwise = (c, i)

input :: [(Char, [Char])]
input = parseData (lines $(embedStringFile "Day7/input.txt"))
