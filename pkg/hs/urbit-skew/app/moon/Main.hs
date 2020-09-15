{-# LANGUAGE CPP #-}

module Main (main) where

import ClassyPrelude
import Urbit.Moon.Repl

import Prelude          (read)
import Text.Show.Pretty (pPrint)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  getArgs >>= \case
#if !defined(__GHCJS__)
    ["repl", "--oleg"]          -> replOleg
    ["repl", "--tromp"]         -> replTromp
    ["repl"]                    -> replNew
#endif
    ["exec", "--oleg", fn]      -> runFileOleg (unpack fn)
    ["exec", "--tromp", fn]     -> runFileTromp (unpack fn)
    ["exec", fn]                -> runFileNew (unpack fn)
    _                           -> do
      putStrLn "usage: moon repl [--(oleg|tromp)]"
      putStrLn "       moon exec [--(oleg|tromp)] file"
