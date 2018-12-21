module Day20.Tests
  ( part1tests
  ) where

import           Day20.Parsing    (Tree (..), parseData)
import           Day20.Solutions  (part1, toGraph)
import           Test.Tasty.HUnit (assertEqual, testCase)

part1tests =
  testCase "Day 20 -- Finding the room furthest away" $ do
    assertEqual [] 3 (part1 . toGraph (0, 0) . parseData $ "^WNE$")
    assertEqual
      []
      5
      (part1 . toGraph (0, 0) . parseData $ "^EEE(WW|EE)(NN|SS)$")
    assertEqual
      []
      10
      (part1 . toGraph (0, 0) . parseData $ "^ENWWW(NEEE|SSE(EE|N))$")
    assertEqual
      []
      18
      (part1 . toGraph (0, 0) . parseData $
       "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")
    assertEqual
      []
      23
      (part1 . toGraph (0, 0) . parseData $
       "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")
    assertEqual
      []
      31
      (part1 . toGraph (0, 0) . parseData $
       "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")
