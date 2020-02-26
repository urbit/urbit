module Main (main) where

import ClassyPrelude
import Prelude (read)
import Moon.MoonToUruk (runFastAcker, runFastFib)
import Moon.Repl (repl, runFile, runText)

main :: IO ()
main = do
  getArgs >>= \case
    ["acker", readMay → Just x, readMay → Just y] -> do
      res <- runFastAcker x y
      print res
    ["fib", readMay → Just x] -> do
      res <- runFastFib x
      print res
    ["repl"]     -> repl
    ["exec", fn] -> runFile (unpack fn)
    _            -> do
      putStrLn "usage: urbit-uruk acker x y"
      putStrLn "       urbit-uruk fib x"
      putStrLn "       urbit-uruk repl"
      putStrLn "       urbit-uruk exec file"
