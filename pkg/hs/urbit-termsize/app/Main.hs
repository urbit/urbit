module Main where

import Prelude

import Urbit.TermSize (liveTermSize)

main :: IO ()
main = do
  init <- liveTermSize (putStrLn . ("New Size: " <>) . show)
  putStrLn ("Initial Size: " <> show init)
  _ <- getLine
  pure ()
