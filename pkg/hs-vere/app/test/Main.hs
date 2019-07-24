module Main where

import ClassyPrelude

import Arvo
import Control.Exception hiding (evaluate)
import Data.Acquire
import Data.Conduit
import Data.Conduit.List
import Noun
import Vere.Pier
import Vere.Pier.Types
import Vere.Serf

import Control.Concurrent (runInBoundThread, threadDelay)
import Control.Lens       ((&))
import System.Directory   (doesFileExist, removeFile)
import Text.Show.Pretty   (pPrint)
import Urbit.Time         (Wen)

import qualified Data.Set  as Set
import qualified Vere.Log  as Log
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
    with (Pier.booted pillPath shipPath serfFlags ship) $ \(serf, log, ss) -> do
        print "lul"
        print ss
        threadDelay 500000
        shutdown serf 0 >>= print
        putStrLn "[tryBootFromPill] Booted!"

tryResume :: FilePath -> IO ()
tryResume shipPath = do
    with (Pier.resumed shipPath serfFlags) $ \(serf, log, ss) -> do
        print ss
        threadDelay 500000
        shutdown serf 0 >>= print
        putStrLn "[tryResume] Resumed!"

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
              putStrLn ("[tryParseEvents] lifecycle nock: " <> tshow eId)
          when (eId > cycle) $ liftIO $ do
              (mug, wen, evNoun) <- unpackJob n
              case fromNounErr evNoun of
                  Left err -> liftIO $ do
                      -- pPrint err
                      -- pPrint evNoun
                      print err
                  Right (ev :: Ev) -> do
                      -- print ev
                      pure ()
                      -- pPrint ev
                      -- paths <- readIORef vPax
                      -- let pax = case ev of Ovum pax _ -> pax
                      -- writeIORef vPax (pax:paths)
                      -- print ("done from noun", eId)
                      -- print (Job eId mug $ DateOvum date ev)
          -- unless (eId - first > 1000) $
          showEvents vPax (succ eId) cycle

    unpackJob :: Noun -> IO (Mug, Wen, Noun)
    unpackJob n = fromNounExn n

--------------------------------------------------------------------------------

serfFlags :: Serf.Flags
serfFlags = [Serf.Hashless, Serf.DryRun] -- [Serf.Verbose, Serf.Trace]

    -- = DebugRam
    -- | DebugCpu
    -- | CheckCorrupt
    -- | CheckFatal
    -- | Verbose
    -- | DryRun
    -- | Quiet
    -- | Hashless
    -- | Trace

collectedFX :: FilePath -> Acquire ()
collectedFX top = do
    log    <- Log.existing (top <> "/.urb/log")
    serf   <- Serf.run (Serf.Config top serfFlags)
    liftIO (Serf.collectFX serf log)

collectAllFx :: FilePath -> IO ()
collectAllFx top = do
    wipeSnapshot top
    with (collectedFX top) $ \() ->
        putStrLn "[collectAllFx] Done collecting effects!"

--------------------------------------------------------------------------------

main :: IO ()
main = runInBoundThread $ do
    let pillPath = "/home/benjamin/r/urbit/bin/solid.pill"
        shipPath = "/home/benjamin/r/urbit/zod/"
        ship     = zod

    -- collectAllFx "/home/benjamin/r/urbit/testnet-zod/"

    tryParseEvents "/home/benjamin/r/urbit/zod/.urb/log" 1
    tryParseEvents "/home/benjamin/r/urbit/testnet-zod/.urb/log" 1

    tryParseFX "/home/benjamin/zod-fx"         1 100000000
    tryParseFX "/home/benjamin/testnet-zod-fx" 1 100000000

    -- tryBootFromPill pillPath shipPath ship
    -- tryResume shipPath
    -- tryFullReplay shipPath

    pure ()

--------------------------------------------------------------------------------

tryParseFX :: FilePath -> Word -> Word -> IO ()
tryParseFX pax first last =
  runConduit $ streamFX pax first last
            .| tryParseFXStream

streamFX :: FilePath -> Word -> Word -> ConduitT () ByteString IO ()
streamFX dir first last = loop first
  where
    loop n = do
      -- when (n `mod` 1000 == 0) $ do
        -- print n
      let fil = dir <> "/" <> show n
      exists <- liftIO (doesFileExist fil)
      when (exists && n <= last) $ do
          liftIO (readFile fil) >>= yield
          loop (n+1)

tryParseFXStream :: ConduitT ByteString Void IO ()
tryParseFXStream = loop 0 (mempty :: Set (Text, Noun))
  where
    loop 1 pax = for_ (setToList pax) print
    loop errors pax =
      await >>= \case
        Nothing -> for_ (setToList pax) $ \(t,n) ->
                     putStrLn (t <> ": " <> tshow n)
        Just bs -> do
          n <- liftIO (cueBSExn bs)
          fromNounErr n & \case
            Left err            -> print err >> loop (errors + 1) pax
            Right []            -> loop errors pax
            Right (fx :: FX) -> do
              -- pax <- pure $ Set.union pax
                          -- $ setFromList
                          -- $ fx <&> \(Effect p v) -> (getTag v, toNoun p)
              loop errors pax

-- getTag :: Effect -> Text
-- getTag fx =
  -- let n = toNoun fx
  -- in case n of
       -- A _   -> maybe "ERR" unCord (fromNoun n)
       -- C h _ -> maybe "ERR" unCord (fromNoun h)

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
