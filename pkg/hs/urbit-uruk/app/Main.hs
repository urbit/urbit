module Main (main) where

import ClassyPrelude

import Prelude         (read)
import Urbit.Moon.Repl (repl, replFast, runFile, runFileFast, runText)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  getArgs >>= \case
    ["repl"]               -> repl
    ["repl", "--fast"]     -> replFast
    ["exec", "--fast", fn] -> runFileFast (unpack fn)
    _                      -> do
      putStrLn "usage: urbit-uruk repl [--fast]"
      putStrLn "       urbit-uruk exec file [--fast]"
