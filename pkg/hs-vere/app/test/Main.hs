module Main where

import ClassyPrelude

import Noun
import Vere.Pier.Types
import Vere.Pier
import Vere.Serf
import Data.Acquire
import Data.Conduit
import Data.Conduit.List
import Control.Exception hiding (evaluate)

import Control.Concurrent (threadDelay)
import System.Directory   (removeFile, doesFileExist)
import Text.Show.Pretty   (pPrint)

import qualified Vere.Log     as Log
import qualified Vere.Persist as Persist
import qualified Vere.Pier    as Pier

--------------------------------------------------------------------------------

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists pax = do
  exists <- doesFileExist pax
  when exists $ do
      removeFile pax

wipeSnapshot :: FilePath -> IO ()
wipeSnapshot shipPath = do
    removeFileIfExists (shipPath <> ".urb/chk/north.bin")
    removeFileIfExists (shipPath <> ".urb/chk/south.bin")

tryBootFromPill :: FilePath -> FilePath -> Ship -> IO ()
tryBootFromPill pillPath shipPath ship = do
    wipeSnapshot shipPath
    Pier.boot pillPath shipPath ship $ \s l ss -> do
        print "lul"
        print ss
        threadDelay 500000
        shutdownAndWait s 0 >>= print
        putStrLn "Booted!"

tryResume :: FilePath -> IO ()
tryResume shipPath = do
    Pier.resume shipPath $ \s l ss -> do
        print ss
        threadDelay 500000
        shutdownAndWait s 0 >>= print
        putStrLn "Resumed!"

tryFullReplay :: FilePath -> IO ()
tryFullReplay shipPath = do
    wipeSnapshot shipPath
    tryResume shipPath

zod :: Ship
zod = 0

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

tryParseEvents :: FilePath -> EventId -> IO ()
tryParseEvents dir first = do
    vPax <- newIORef []
    with (Log.existing dir) $ \log -> do
        let ident = Log.identity log
        print ident
        runConduit $ Log.streamEvents log first
                  .| showEvents vPax first (fromIntegral $ lifecycleLen ident)
    paths <- sort . ordNub <$> readIORef vPax
    for_ paths print
  where
    showEvents :: IORef [Path] -> EventId -> EventId -> ConduitT Atom Void IO ()
    showEvents vPax eId cycle = await >>= \case
      Nothing -> print "Done!"
      Just at -> do
          -- print ("got event", eId)
          n <- liftIO $ cueExn at
          -- print ("done cue", eId)
          when (eId > cycle) $ do
              case fromNounErr n of
                Left err -> print err
                Right (mug, date, ovum) -> do
                  evaluate $ Job eId mug $ DateOvum date ovum
                  pure ()
                  paths <- readIORef vPax
                  let pax = case ovum of Ovum pax _ -> pax
                  writeIORef vPax (pax:paths)
                  -- print ("done from noun", eId)
                  -- print (Job eId mug $ DateOvum date ovum)
          unless (eId - first > 100000000) $
              showEvents vPax (succ eId) cycle

main :: IO ()
main = do
    let pillPath = "/home/benjamin/r/urbit/bin/brass.pill"
        shipPath = "/home/benjamin/r/urbit/zod/"
        ship     = zod

    tryParseEvents "/home/benjamin/r/urbit/testnet-zod/.urb/log" 1

    -- tryBootFromPill pillPath shipPath ship
    -- tryResume shipPath
    -- tryFullReplay shipPath

    pure ()

--------------------------------------------------------------------------------

tryCopyLog :: IO ()
tryCopyLog = do
  let logPath      = "/Users/erg/src/urbit/zod/.urb/falselog/"
      falselogPath = "/Users/erg/src/urbit/zod/.urb/falselog2/"

  (ident, nextEv, events) <-
      with (Log.existing logPath) $ \log -> do
          persistQ <- newTQueueIO
          releaseQ <- newTQueueIO
          persist  <- Persist.start log persistQ (writeTQueue releaseQ)
          ident    <- pure $ Log.identity log
          events   <- runConduit (Log.streamEvents log 1 .| consume)
          nextEv   <- Log.nextEv log
          pure (ident, nextEv, events)

  print ident
  print nextEv
  print (length events)

  with (Log.new falselogPath ident) $ \log2 -> do
      persistQ2 <- newTQueueIO
      releaseQ2 <- newTQueueIO
      persist2  <- Persist.start log2 persistQ2 (writeTQueue releaseQ2)

      let writs = zip [1..] events <&> \(id, a) ->
                      Writ id Nothing (Jam a) []

      print "About to write"

      for_ writs $ \w ->
        atomically (writeTQueue persistQ2 w)

      print "About to wait"

      replicateM_ 100 $ do
        atomically $ readTQueue releaseQ2

      print "Done"
