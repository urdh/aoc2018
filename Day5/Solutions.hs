{-# LANGUAGE TemplateHaskell #-}

module Day5.Solutions
  ( part1
  , part2
  , input
  ) where

import           Control.Monad.Fix
import           Data.Char
import           Data.FileEmbed
import           Data.List

invertCase :: Char -> Char
invertCase x
  | isUpper x = toLower x
  | isLower x = toUpper x

reduce :: String -> String
reduce [] = []
reduce [x] = [x]
reduce (x:y:xs)
  | y == (invertCase x) = reduce xs
  | otherwise = [x] ++ (reduce ([y] ++ xs))

converge :: (Eq a) => (a -> a) -> a -> a
converge h =
  fix $
  (\g f x ->
     if g x == x
       then x
       else (f . g) x)
    h

part1 :: String -> Int
part1 = length . (converge reduce)

part2 :: String -> Int
part2 = minimum . (map part1) . (\x -> map ($ x) ((map dropType) . types $ x))
  where
    dropType t = filter $ ((/=) t) . toLower
    types = nub . (map toLower)

input :: String
input = rstrip $(embedStringFile "Day5/input.txt")
  where
    rstrip = reverse . dropWhile isSpace . reverse
