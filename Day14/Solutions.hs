{-|
Module      : Day14.Solutions
Description : Solutions for day 14 of the 2018 Advent of Code.
-}
module Day14.Solutions
  ( part1
  , part2
  , input
  ) where

import           Control.Arrow
import           Control.Monad
import           Data.Char     (digitToInt)
import           Data.Foldable
import           Data.List
import qualified Data.Sequence as Seq

data Tape a = Tape
  { _pos  :: (Int, Int)
  , _list :: Seq.Seq a
  } deriving (Eq)

singleton :: a -> a -> Tape a
singleton a b = (Tape (0, 1) (Seq.fromList [a, b]))

focus :: Tape a -> (a, a)
focus t =
  ( (_list t) `Seq.index` (fst . _pos $ t)
  , (_list t) `Seq.index` (snd . _pos $ t))

append :: Tape a -> [a] -> Tape a
append t xs = (Tape (_pos t) ((Seq.><) (_list t) (Seq.fromList xs)))

move :: Tape Int -> Tape Int
move t = (Tape (add (_pos t) (focus t)) (_list t))
  where
    add a b = (msum *** msum) . unzip $ [a, b, (1, 1)]
    msum = (`mod` (length . _list $ t)) . sum

digits :: (Show a, Num b, Num a) => a -> [b]
digits = map (fromIntegral . digitToInt) . show . abs

generate :: Tape Int -> Tape Int
generate = move . ap append (digits . uncurry (+) . focus)

infixPos :: (Eq a) => [a] -> [a] -> Int
infixPos = infixPos' 0
  where
    infixPos' acc s l
      | isPrefixOf s l = acc
      | otherwise = infixPos' (succ acc) s (tail l)

part1 :: Int -> String
part1 n =
  foldr (++) "" .
  map show .
  toList .
  (Seq.take 10 . Seq.drop n . _list) .
  (!! 10) . filter ((> n) . length . _list) . (iterate generate) $
  singleton 3 7

part2 :: String -> Int
part2 s =
  infixPos l .
  toList . _list . head . filter (infixNearEnd l . _list) . (iterate generate) $
  singleton 3 7
  where
    infixNearEnd xs seq =
      (isInfixOf xs . toList . Seq.drop (length seq - length xs - 2) $ seq)
    l = map (\x -> read [x]) $ s

input :: Int
input = 170641
