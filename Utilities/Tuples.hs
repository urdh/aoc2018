module Utilities.Tuples
  ( mapfst
  , mapsnd
  ) where

mapfst :: (a -> a') -> (a, b) -> (a', b)
mapfst f (x, y) = ((f x), y)

mapsnd :: (b -> b') -> (a, b) -> (a, b')
mapsnd f (x, y) = (x, (f y))
