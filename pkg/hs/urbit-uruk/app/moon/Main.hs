{-# LANGUAGE CPP #-}

module Main (main) where

import ClassyPrelude
import Urbit.Moon.Repl

import Prelude               (read)
import Text.Show.Pretty      (pPrint)
import Urbit.Uruk.DashParser (dashEnv, jetsMap)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn ""
  for_ (mapToList dashEnv) $ \(k,v) -> do
    putStrLn ("[" <> k <> "]")
    pPrint v
    putStrLn ""

  for_ (mapToList jetsMap) $ \(k,v) -> do
    putStrLn ("[" <> tshow k <> "]")
    pPrint v
    putStrLn ""

  getArgs >>= \case
#if !defined(__GHCJS__)
    ["repl"]                    -> replSlow
    ["repl", "--fast"]          -> replFast
    ["repl", "--oleg"]          -> replOleg
    ["repl", "--lazyoleg"]      -> replLazyOleg
    ["repl", "--tromp"]         -> replTromp
    ["repl", "--lazytromp"]     -> replLazyTromp
    ["repl", "--new"]           -> replNew
#endif
    ["exec", "--fast", fn]      -> runFileFast (unpack fn)
    ["exec", "--oleg", fn]      -> runFileOleg (unpack fn)
    ["exec", "--lazyoleg", fn]  -> runFileLazyOleg (unpack fn)
    ["exec", "--tromp", fn]     -> runFileTromp (unpack fn)
    ["exec", "--lazytromp", fn] -> runFileLazyTromp (unpack fn)
    ["exec", "--new", fn]       -> runFileNew (unpack fn)
    ["exec", fn]                -> runFileSlow (unpack fn)
    _                           -> do
      putStrLn "usage: urbit-uruk repl [--(fast|new|oleg)]"
      putStrLn "       urbit-uruk exec [--(fast|new|oleg)] file"
