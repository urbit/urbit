module Main where

import ClassyPrelude

import Noun
import Vere.Pier.Types
import Vere.Pier
import Vere.Serf

import Control.Concurrent (threadDelay)
import System.Directory   (removeFile)
import Text.Show.Pretty   (pPrint)

import qualified Vere.Log     as Log
import qualified Vere.Persist as Persist
import qualified Vere.Pier    as Pier

--------------------------------------------------------------------------------

main :: IO ()
main = do
    p <- loadFile @Pill "/home/benjamin/r/urbit/bin/brass.pill" >>= \case
        Left l  -> error (show l)
        Right p -> pure p

    pPrint p

    let pillPath = "/home/benjamin/r/urbit/bin/brass.pill"
        shipPath = "/home/benjamin/r/urbit/zod/"
        ship     = 0 -- zod

    removeFile (shipPath <> ".urb/chk/north.bin")
    removeFile (shipPath <> ".urb/chk/south.bin")

    (s,l,e,m) <- Pier.boot pillPath shipPath ship
    print (e,m)
    threadDelay 500000
    kill s
    putStrLn "Booted!"

    removeFile (shipPath <> ".urb/chk/north.bin")
    removeFile (shipPath <> ".urb/chk/south.bin")

    (s,l,e,m) <- Pier.resume shipPath
    print (e,m)
    putStrLn "Resumed!"

    pure ()

--------------------------------------------------------------------------------

tryCopyLog :: IO ()
tryCopyLog = do
  let logPath      = "/Users/erg/src/urbit/zod/.urb/falselog/"
      falselogPath = "/Users/erg/src/urbit/zod/.urb/falselog2/"

  ----------------------------------------

  persistQ <- newTQueueIO
  releaseQ <- newTQueueIO
  log      <- Log.open logPath
  persist  <- Persist.start log persistQ (writeTQueue releaseQ)
  ident    <- Log.readIdent log

  ----------------------------------------

  lastEv <- Log.latestEventNumber log
  events <- Log.readEvents log 1 3142

  ----------------------------------------

  print ident
  print lastEv
  print (length events)

  ----------------------------------------

  persistQ2 <- newTQueueIO
  releaseQ2 <- newTQueueIO
  log2      <- Log.open falselogPath
  persist2  <- Persist.start log2 persistQ2 (writeTQueue releaseQ2)

  ----------------------------------------

  Log.writeIdent log2 ident

  let writs = events <&> \(id, a) ->
                Writ id Nothing (Jam a) []

  ----------------------------------------

  print "About to write"

  for_ writs $ \w ->
    atomically (writeTQueue persistQ2 w)

  ----------------------------------------

  print "About to wait"

  replicateM_ 100 $ do
    atomically $ readTQueue releaseQ2

  ----------------------------------------

  print "Done"
