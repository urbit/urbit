module Main where

import ClassyPrelude
import Control.Monad.Except

import Control.Concurrent (threadDelay)
import Urbit.Moon.Repl    (runFile')
import Urbit.SkewRTS      (dumpEventsFile, toJSON, vProfDone)

import qualified Urbit.Moon.MoonToSkew as MU
import qualified Urbit.SkewRTS.Types   as RTS

--------------------------------------------------------------------------------

main :: IO ()
main = do
  getArgs >>= \case
    ["repl"]     -> putStrLn "TODO: No REPL yet"
    ["exec", fn] -> runFileFas (unpack fn)
    ["prof"]     -> profToJson
    _            -> do
      putStrLn "usage: supermoon repl"
      putStrLn "       supermoon exec file"

runFileFas :: FilePath -> IO ()
runFileFas fp = do
  traceTid <- async (dumpEventsFile "/home/benjamin/trace.bin")
  runFile' (pure . MU.strictOlegFile' (id :: RTS.Val -> RTS.Val)) fp
  putStr "DONE! Waiting until profiling data finishes going to disk."
  threadDelay 10_000
  atomically (writeTVar vProfDone True)
  void $ wait traceTid
  putStr "DONE. Profiling data written."

profToJson :: IO ()
profToJson = toJSON "/home/benjamin/trace.bin" "/home/benjamin/trace.json"
