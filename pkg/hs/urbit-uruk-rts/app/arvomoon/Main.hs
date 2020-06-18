module Main where

import ClassyPrelude        hiding (readFile, writeFile)
import Control.Monad.Except

import Control.Concurrent  (threadDelay)
import Urbit.Moon.Repl     (runFile')
import Urbit.UrukRTS       (dumpEventsFile, kVVV, toJSON, vProfDone)
import Urbit.UrukSerialize

import Data.Text.IO (readFile, writeFile)

import qualified Urbit.Moon.MoonToUruk as MU
import qualified Urbit.UrukRTS.Types   as RTS

import qualified System.Console.Haskeline as HL

--------------------------------------------------------------------------------

main :: IO ()
main = do
  getArgs >>= \case
    ["boot", kernelFile, pierDir] ->
      loadKernel (unpack kernelFile) (unpack pierDir)
    ["run", pierDir] -> runPier (unpack pierDir)
    _            -> do
      putStrLn "usage: arvomoon boot <kernel file> <pier dir>"
      putStrLn "       arvomoon run <pier dir>"

loadKernel :: FilePath -> FilePath -> IO ()
loadKernel fp pier = do
  txt <- readFile fp
  let retVal = MU.strictOlegFile' (id :: RTS.Val -> RTS.Val) txt
  case retVal of
    Left err -> putStrLn err
    Right val -> do
      putStr "Kernel loaded. Writing initial pier..."
      persistToPier pier val
      putStr ("Pier booted in " ++ (tshow pier))

toVNat :: Int -> RTS.Val
toVNat = RTS.VNat . fromIntegral

runPier :: FilePath -> IO ()
runPier pierDir =
  do
    kernel <- loadFromPier pierDir
    HL.runInputT HL.defaultSettings (loop kernel)
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
              out <- liftIO $ kVVV kernel (toVNat w) (toVNat h)
              case out of
                (RTS.VCon ppm nuKernel) -> do
                  HL.outputStrLn (show ppm)
                  -- Write the kernel.
                  liftIO $ persistToPier pierDir nuKernel
                  loop nuKernel

                _ -> do
                  HL.outputStrLn "kernel didn't return a pair"
                  pure ()
