module Main where

import ClassyPrelude
import Control.Monad.Except

import Control.Concurrent (threadDelay)
import Urbit.Moon.Repl    (runFile')
import Urbit.UrukRTS      (dumpEventsFile, kVVV, toJSON, vProfDone)

import qualified Urbit.Moon.MoonToUruk as MU
import qualified Urbit.UrukRTS.Types   as RTS

import qualified System.Console.Haskeline as HL

--------------------------------------------------------------------------------

main :: IO ()
main = do
  getArgs >>= \case
    ["repl", fn] -> loadKernel (unpack fn)
    ["prof"]     -> profToJson
    _            -> do
      putStrLn "usage: arvomoon repl <kernel file>"

loadKernel :: FilePath -> IO ()
loadKernel fp = do
  traceTid <- async (dumpEventsFile "/home/benjamin/trace.bin")
  txt <- readFile fp
  let retVal = MU.strictOlegFile' (id :: RTS.Val -> RTS.Val) (decodeUtf8 txt)
  case retVal of
    Left err -> putStrLn err
    Right val -> do
      putStr "Kernel loaded."
      useKernel val

useKernel :: RTS.Val -> IO ()
useKernel kernel = HL.runInputT HL.defaultSettings (loop kernel)
  where
    loop :: RTS.Val -> HL.InputT IO ()
    loop kernel = do
      minput <- HL.getInputLine "arvo> "
      case minput of
        Nothing -> pure ()
        Just input -> do
          case readMay input :: Maybe (Int, Int) of
            Nothing -> do
              HL.outputStrLn "invalid string"
              loop kernel

            Just (w, h) -> do
              out <- liftIO $ kVVV kernel (RTS.VNat $ fromIntegral w) (RTS.VNat $ fromIntegral h)
              case out of
                (RTS.VCon ppm nuKernel) -> do
                  HL.outputStrLn (show ppm)
                  loop nuKernel

                _ -> do
                  HL.outputStrLn "kernel didn't return a pair"
                  pure ()




  -- runFile' (pure . MU.strictOlegFile' (id :: RTS.Val -> RTS.Val)) fp
  -- putStr "DONE! Waiting until profiling data finishes going to disk."
  -- threadDelay 10_000
  -- atomically (writeTVar vProfDone True)
  -- void $ wait traceTid
  -- putStr "DONE. Profiling data written."

profToJson :: IO ()
profToJson = toJSON "/home/benjamin/trace.bin" "/home/benjamin/trace.json"
