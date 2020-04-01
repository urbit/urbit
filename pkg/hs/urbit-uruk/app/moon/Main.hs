{-# LANGUAGE CPP #-}

module Main (main) where

import ClassyPrelude

import Prelude         (read)
import Urbit.Moon.Repl (runFileSlow, runFileFast, runFileCompile, runFileNew, runText)
#if !defined(__GHCJS__)
import Urbit.Moon.Repl (replSlow, replFast, replCompile, replNew)
#endif

--------------------------------------------------------------------------------

main :: IO ()
main = do
  getArgs >>= \case
#if !defined(__GHCJS__)
    ["repl"]                  -> replSlow
    ["repl", "--fast"]        -> replFast
    ["repl", "--compile"]     -> replCompile
    ["repl", "--new"]         -> replNew
#endif
    ["exec", fn]              -> runFileSlow (unpack fn)
    ["exec", "--fast", fn]    -> runFileFast (unpack fn)
    ["exec", "--compile", fn] -> runFileCompile (unpack fn)
    ["exec", "--new", fn]     -> runFileNew (unpack fn)
    _                         -> do
      putStrLn "usage: urbit-uruk repl [--fast | --compile]"
      putStrLn "       urbit-uruk exec file [--fast | --compile]"
