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

wipeSnapshot :: FilePath -> IO ()
wipeSnapshot shipPath = do
    removeFile (shipPath <> ".urb/chk/north.bin")
    removeFile (shipPath <> ".urb/chk/south.bin")

tryBootFromPill :: FilePath -> FilePath -> Ship -> IO ()
tryBootFromPill pillPath shipPath ship = do
    wipeSnapshot shipPath
    (s,l,ss) <- Pier.boot pillPath shipPath ship
    print ss
    threadDelay 500000
    kill s
    putStrLn "Booted!"

tryResume :: FilePath -> IO ()
tryResume shipPath = do
    (s,l,ss) <- Pier.resume shipPath
    print ss
    threadDelay 500000
    kill s
    putStrLn "Resumed!"

tryFullReplay :: FilePath -> IO ()
tryFullReplay shipPath = do
    wipeSnapshot shipPath
    tryResume shipPath

zod :: Ship
zod = 0

main :: IO ()
main = do
    let pillPath = "/home/benjamin/r/urbit/bin/brass.pill"
        shipPath = "/home/benjamin/r/urbit/zod/"
        ship     = zod

    tryBootFromPill pillPath shipPath ship

    tryResume shipPath

    tryFullReplay shipPath

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
