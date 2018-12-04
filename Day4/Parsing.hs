module Day4.Parsing
  ( Event(WakeUp, FallAsleep, BeginShift)
  , Entry(..)
  , parseLine
  , parseData
  ) where

import           Data.List
import           Data.Time
import           Text.ParserCombinators.ReadP

data Event
  = WakeUp
  | FallAsleep
  | BeginShift
  deriving (Show, Enum, Eq)

data Entry = Entry
  { time  :: UTCTime
  , evt   :: Event
  , guard :: Int
  } deriving (Show, Eq)

timestamp :: ReadP UTCTime
timestamp = readPTime False defaultTimeLocale "[%Y-%m-%d %H:%M]"

integer :: ReadP Int
integer = fmap read (many1 digit)
  where
    digit = satisfy (\char -> char >= '0' && char <= '9')

wakeup :: ReadP (Event, Int)
wakeup = do
  string "wakes up"
  return (WakeUp, 0)

sleep :: ReadP (Event, Int)
sleep = do
  string "falls asleep"
  return (FallAsleep, 0)

start :: ReadP (Event, Int)
start = do
  string "Guard #"
  g <- integer
  string " begins shift"
  return (BeginShift, g)

event :: ReadP Entry
event = do
  t <- timestamp
  skipSpaces
  e <- choice [wakeup, sleep, start]
  return (Entry t (fst e) (snd e))

parseLine :: String -> Entry
parseLine = fst . head . (readP_to_S event)

fillGuard :: Int -> [Entry] -> [Entry]
fillGuard _ [] = []
fillGuard g (x:xs)
  | (==) BeginShift (evt x) = [x] ++ (fillGuard (guard x) xs)
  | otherwise = [(Entry (time x) (evt x) g)] ++ (fillGuard g xs)

parseData :: [String] -> [Entry]
parseData = (fillGuard 0) . (sortOn time) . (map parseLine)
