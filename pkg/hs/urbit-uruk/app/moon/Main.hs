{-# LANGUAGE CPP #-}

module Main (main) where

import ClassyPrelude

import Prelude         (read)
import Urbit.Moon.Repl (runFileSlow, runFileFast, runText)
#if !defined(__GHCJS__)
import Urbit.Moon.Repl (replSlow, replFast)
#endif

--------------------------------------------------------------------------------

main :: IO ()
main = do
  getArgs >>= \case
#if !defined(__GHCJS__)
    ["repl"]               -> replSlow
    ["repl", "--fast"]     -> replFast
#endif
    ["exec", fn]           -> runFileSlow (unpack fn)
    ["exec", "--fast", fn] -> runFileFast (unpack fn)
    _                      -> do
      putStrLn "usage: urbit-uruk repl [--fast]"
      putStrLn "       urbit-uruk exec file [--fast]"
