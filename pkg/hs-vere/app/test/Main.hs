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
import System.Directory   (doesFileExist, removeFile)
import Text.Show.Pretty   (pPrint)
import Urbit.Time         (Wen)

import qualified Vere.Log  as Log
import qualified Vere.Ovum as Ovum
import qualified Vere.Pier as Pier
import qualified Vere.Serf as Serf


--------------------------------------------------------------------------------

zod :: Ship
zod = 0

--------------------------------------------------------------------------------

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists pax = do
  exists <- doesFileExist pax
  when exists $ do
      removeFile pax

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

--------------------------------------------------------------------------------

wipeSnapshot :: FilePath -> IO ()
wipeSnapshot shipPath = do
    removeFileIfExists (shipPath <> "/.urb/chk/north.bin")
    removeFileIfExists (shipPath <> "/.urb/chk/south.bin")
    print (shipPath <> "/.urb/chk/north.bin")
    print (shipPath <> "/.urb/chk/south.bin")

tryBootFromPill :: FilePath -> FilePath -> Ship -> IO ()
tryBootFromPill pillPath shipPath ship = do
    wipeSnapshot shipPath
    with (Pier.booted pillPath shipPath ship) $ \(serf, log, ss) -> do
        print "lul"
        print ss
        threadDelay 500000
        shutdown serf 0 >>= print
        putStrLn "Booted!"

tryResume :: FilePath -> IO ()
tryResume shipPath = do
    with (Pier.resumed shipPath) $ \(serf, log, ss) -> do
        print ss
        threadDelay 500000
        shutdown serf 0 >>= print
        putStrLn "Resumed!"

tryFullReplay :: FilePath -> IO ()
tryFullReplay shipPath = do
    wipeSnapshot shipPath
    tryResume shipPath

--------------------------------------------------------------------------------

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
    showEvents :: IORef [Path] -> EventId -> EventId
               -> ConduitT ByteString Void IO ()
    showEvents vPax eId cycle = await >>= \case
      Nothing -> print "Done!"
      Just bs -> do
          -- print ("got event", eId)
          n <- liftIO $ cueBSExn bs
          -- print ("done cue", eId)
          when (eId <= cycle) $ do
              putStrLn ("lifecycle nock: " <> tshow eId)
          when (eId > cycle) $ liftIO $ do
              (mug, wen, ovumNoun) <- unpackJob n
              case fromNounErr ovumNoun of
                  Left err -> liftIO $ do
                      -- pPrint err
                      -- pPrint ovumNoun
                      pure ()
                  Right (ovum :: Ovum.Ovum) -> do
                      -- pPrint ovum
                      -- _ <- getLine
                      pure ()
                      -- pPrint ovum
                      -- paths <- readIORef vPax
                      -- let pax = case ovum of Ovum pax _ -> pax
                      -- writeIORef vPax (pax:paths)
                      -- print ("done from noun", eId)
                      -- print (Job eId mug $ DateOvum date ovum)
          -- unless (eId - first > 1000) $
          showEvents vPax (succ eId) cycle

    unpackJob :: Noun -> IO (Mug, Wen, Noun)
    unpackJob n = fromNounExn n

--------------------------------------------------------------------------------

collectedFX :: FilePath -> Acquire ()
collectedFX top = do
    log    <- Log.existing (top <> "/.urb/log")
    serf   <- Serf.run top
    liftIO (Serf.collectFX serf log)

collectAllFx :: FilePath -> IO ()
collectAllFx top = do
    wipeSnapshot top
    with (collectedFX top) $ \() ->
        putStrLn "Done collecting effects!"

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let pillPath = "/home/benjamin/r/urbit/bin/brass.pill"
        shipPath = "/home/benjamin/r/urbit/zod/"
        ship     = zod

    collectAllFx "/home/benjamin/r/urbit/testnet-zod"

    -- tryParseEvents "/home/benjamin/r/urbit/zod/.urb/log" 1
    -- tryParseEvents "/home/benjamin/r/urbit/testnet-zod/.urb/log" 1

    -- tryBootFromPill pillPath shipPath ship
    -- tryResume shipPath
    -- tryFullReplay shipPath

    pure ()

--------------------------------------------------------------------------------

tryCopyLog :: IO ()
tryCopyLog = do
  let logPath      = "/Users/erg/src/urbit/zod/.urb/falselog/"
      falselogPath = "/Users/erg/src/urbit/zod/.urb/falselog2/"

  persistQ <- newTQueueIO
  releaseQ <- newTQueueIO
  (ident, nextEv, events) <-
      with (do { log <- Log.existing logPath
               ; Pier.runPersist log persistQ (writeTQueue releaseQ)
               ; pure log
               })
        \log -> do
          ident  <- pure $ Log.identity log
          events <- runConduit (Log.streamEvents log 1 .| consume)
          nextEv <- Log.nextEv log
          pure (ident, nextEv, events)

  print ident
  print nextEv
  print (length events)

  persistQ2 <- newTQueueIO
  releaseQ2 <- newTQueueIO
  with (do { log <- Log.new falselogPath ident
           ; Pier.runPersist log persistQ2 (writeTQueue releaseQ2)
           ; pure log
           })
    $ \log2 -> do
      let writs = zip [1..] events <&> \(id, a) ->
                      (Writ id Nothing a, [])

      print "About to write"

      for_ writs $ \w ->
        atomically (writeTQueue persistQ2 w)

      print "About to wait"

      replicateM_ 100 $ do
        atomically $ readTQueue releaseQ2

      print "Done"
