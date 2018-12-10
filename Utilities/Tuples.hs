{-|
Module      : Utilities.Tuples
Description : Tuple utilities for the 2018 Advent of Code problems.
-}
module Utilities.Tuples
  ( mapfst
  , mapsnd
  ) where

-- | Maps the first element of a tuple:
--
-- @
-- mapfst f (a, b) == (f a, b)
-- @
mapfst :: (a -> a') -> (a, b) -> (a', b)
mapfst f (x, y) = ((f x), y)

-- | Maps the second element of a tuple:
--
-- @
-- mapsnd f (a, b) == (a, f b)
-- @
mapsnd :: (b -> b') -> (a, b) -> (a, b')
mapsnd f (x, y) = (x, (f y))
