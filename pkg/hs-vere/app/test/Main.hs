module Main where

import ClassyPrelude

import Noun
import Vere.Pier.Types
import Vere.Pier
import Vere.Serf

import Text.Show.Pretty (pPrint)

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

    seq@(BootSeq ident _ _) <- generateBootSeq 0 p
    pPrint seq

    serf <- startSerfProcess "/home/benjamin/r/urbit/zod/"
    bootFromSeq serf seq >>= pPrint

    -- (s,l,e,m) <- Pier.resume "/home/benjamin/r/urbit/zod/"
    -- putStrLn "Resumed!"

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
