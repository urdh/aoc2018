module Main
  ( main
  ) where

import           System.Environment

help :: IO ()
help = do
  prog <- getProgName
  putStrLn ("Usage: " ++ prog ++ " <day>")

dispatch :: [(String, IO ())]
dispatch = [("help", help)]

main :: IO ()
main = do
  args <- getArgs
  let (Just action) = lookup (head $ args ++ ["help"]) dispatch
  action
