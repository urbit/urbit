module Main where

import ClassyPrelude

main :: IO ()
main = do
  getArgs >>= \case
    ["repl"]     -> putStrLn "A supermoon!"
    ["exec", fn] -> putStrLn "A supermoon!"
    _            -> do
      putStrLn "usage: supermoon repl"
      putStrLn "       supermoon exec file"
