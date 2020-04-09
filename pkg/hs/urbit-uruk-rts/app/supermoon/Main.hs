module Main where

import ClassyPrelude
import Control.Monad.Except

import Urbit.Moon.Repl (runFile')
import Urbit.UrukRTS   ()

import qualified Urbit.Moon.MoonToUruk as MU
import qualified Urbit.UrukRTS.Types   as RTS

--------------------------------------------------------------------------------

main :: IO ()
main = do
  getArgs >>= \case
    ["repl"]     -> putStrLn "TODO: No REPL yet"
    ["exec", fn] -> runFileFas (unpack fn)
    _            -> do
      putStrLn "usage: supermoon repl"
      putStrLn "       supermoon exec file"

runFileFas :: FilePath -> IO ()
runFileFas = runFile' (pure . MU.strictOlegFile' (id :: RTS.Val -> RTS.Val))
