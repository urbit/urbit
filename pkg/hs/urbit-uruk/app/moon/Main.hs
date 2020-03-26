{-# LANGUAGE CPP #-}

module Main (main) where

import ClassyPrelude

import Prelude         (read)
import Urbit.Moon.Repl (runFile, runFileFast, runText)
#if !defined(__GHCJS__)
import Urbit.Moon.Repl (repl, replFast)
#endif

--------------------------------------------------------------------------------

main :: IO ()
main = do
  getArgs >>= \case
#if !defined(__GHCJS__)
    ["repl"]               -> repl
    ["repl", "--fast"]     -> replFast
#endif
    ["exec", "--fast", fn] -> runFileFast (unpack fn)
    _                      -> do
      putStrLn "usage: urbit-uruk repl [--fast]"
      putStrLn "       urbit-uruk exec file [--fast]"
