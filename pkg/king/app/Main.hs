{-
    # Booting a Ship

    - TODO Correctly setup the Pier directory.
    - TODO Hook up CLI command.
    - TODO Don't just boot, also run the ship (unless `-x` is set).
    - TODO Figure out why ships booted by us don't work.

    # Event Pruning

    - `king discard-events NUM_EVENTS`: Delete the last `n` events from
      the event log.

    - `king discard-events-interactive`: Iterate through the events in
      the event log, from last to first, pretty-print each event, and
      ask if it should be pruned.


    # `-L` -- Local-Only Networking

    Localhost-only networking, even on real ships.


    # `-O` -- Networking Disabled

    Run networking drivers, but configure them to never send any packages
    and to never open any ports.


    # `-N` -- Dry Run

    Disable all persistence and use no-op networking.


    # `-x` -- Exit Immediately

    When creating a new ship, or booting an existing one, simply get to
    a good state, snapshot, and then exit. Don't do anything that has
    any effect on the outside world, just boot or catch the snapshot up
    to the event log.


    # Proper Logging

    - TODO Consider using RIO's logging infrastructure.
    - TODO If that's too heavy, figure out what the best way to do
      logging is now.
    - TODO Convert all existing logging to the chosen logging system.
    - TODO Add more logging to all the places. Logging is super useful.


    # Implement subcommands to test event and effect parsing.

    - `king * --collect-fx`: All effects that come from the serf get
      written into the `effects` LMDB database.

    - `king parse-events PIER`: Run through the event log, and parse all
      events, print failures.

    - `king parse-effects PIER`: Run through the event log, and parse all
      effects, print any failures.

    - `king clear-fx PIER`: Deletes all collected effects.

    - `king full-replay PIER`: Replays the whole event log events, print
      any failures. On success, replace the snapshot.


    # Validate Pill Files

    - `king validate-pill PILL`: Parse a pill file. Print an error on
      exit, and print a description of the pill on success.


    # Full Replay -- An Integration Test

    - Copy the event log:

      - Create a new event log at the destination.
      - Stream events from the first event log.
      - Parse each event.
      - Re-Serialize each event.
      - Verify that the round-trip was successful.
      - Write the event into the new database.

    - Replay the event log at the destination.
      - If `--collect-fx` is set, then record effects as well.

    - Snapshot.

    - Verify that the final mug is the same as it was before.

    # Implement Remaining Serf Flags

    - `DebugRam`: Memory debugging.
    - `DebugCpu`: Profiling
    - `CheckCorrupt`: Heap Corruption Tests
    - `CheckFatal`: TODO What is this?
    - `Verbose`: TODO Just the `-v` flag?
    - `DryRun`: TODO Just the `-N` flag?
    - `Quiet`: TODO Just the `-q` flag?
    - `Hashless`: Don't use hashboard for jets.
    - `Trace`: TODO What does this do?
-}

module Main where

import ClassyPrelude

import Options.Applicative
import Options.Applicative.Help.Pretty

import Arvo
import Control.Exception hiding (evaluate, throwIO)
import Data.Acquire
import Data.Conduit
import Data.Conduit.List hiding (replicate, take)
import Noun hiding (Parser)
import Vere.Pier
import Vere.Pier.Types
import Vere.Serf

import Control.Concurrent (runInBoundThread, threadDelay)
import Control.Lens       ((&))
import System.Directory   (doesFileExist, removeFile)
import System.Environment (getProgName)
import Text.Show.Pretty   (pPrint)
import Urbit.Time         (Wen)

import qualified CLI
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
    putStrLn "wipeSnapshot"
    removeFileIfExists (shipPath <> "/.urb/chk/north.bin")
    removeFileIfExists (shipPath <> "/.urb/chk/south.bin")
    print (shipPath <> "/.urb/chk/north.bin")
    print (shipPath <> "/.urb/chk/south.bin")
    putStrLn "SNAPSHOT WIPED"

tryBootFromPill :: FilePath -> FilePath -> Ship -> IO ()
tryBootFromPill pillPath shipPath ship = do
    wipeSnapshot shipPath
    with (Pier.booted pillPath shipPath [] ship) $ \(serf, log, ss) -> do
        print "lul"
        print ss
        threadDelay 500000
        shutdown serf 0 >>= print
        putStrLn "[tryBootFromPill] Booted!"

runAcquire act = with act pure

tryPlayShip :: FilePath -> IO ()
tryPlayShip shipPath = do
    runAcquire $ do
        putStrLn "RESUMING SHIP"
        sls <- Pier.resumed shipPath []
        putStrLn "SHIP RESUMED"
        Pier.pier shipPath Nothing sls

tryResume :: FilePath -> IO ()
tryResume shipPath = do
    with (Pier.resumed shipPath []) $ \(serf, log, ss) -> do
        print ss
        threadDelay 500000
        shutdown serf 0 >>= print
        putStrLn "[tryResume] Resumed!"

tryFullReplay :: FilePath -> IO ()
tryFullReplay shipPath = do
    wipeSnapshot shipPath
    tryResume shipPath

--------------------------------------------------------------------------------

checkEvs :: FilePath -> Word64 -> IO ()
checkEvs pier first = do
    vPax <- newIORef []
    with (Log.existing dir) $ \log -> do
        let ident = Log.identity log
        print ident
        runConduit $ Log.streamEvents log first
                  .| showEvents vPax first (fromIntegral $ lifecycleLen ident)
    paths <- sort . ordNub <$> readIORef vPax
    for_ paths print
  where
    dir :: FilePath
    dir = pier <> "/.urb/log"

    showEvents :: IORef [Path] -> EventId -> EventId
               -> ConduitT ByteString Void IO ()
    showEvents vPax eId cycle = await >>= \case
      Nothing -> print "Everything checks out."
      Just bs -> do
          -- print ("got event", eId)
          n <- liftIO $ cueBSExn bs
          -- print ("done cue", eId)
          when (eId <= cycle) $ do
              -- putStrLn ("[tryParseEvents] lifecycle nock: " <> tshow eId)
              pure ()
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

{-
    This runs the serf at `$top/.tmpdir`, but we disable snapshots,
    so this should never actually be created. We just do this to avoid
    letting the serf use an existing snapshot.
-}
collectAllFx :: FilePath -> IO ()
collectAllFx top = do
    putStrLn (pack top)
    with collectedFX $ \() ->
        putStrLn "[collectAllFx] Done collecting effects!"
  where
    tmpDir :: FilePath
    tmpDir = top <> "/.tmpdir"

    collectedFX :: Acquire ()
    collectedFX = do
        log  <- Log.existing (top <> "/.urb/log")
        serf <- Serf.run (Serf.Config tmpDir serfFlags)
        liftIO (Serf.collectFX serf log)

    serfFlags :: Serf.Flags
    serfFlags = [Serf.Hashless, Serf.DryRun]

--------------------------------------------------------------------------------

tryDoStuff :: FilePath -> IO ()
tryDoStuff shipPath = runInBoundThread $ do
    let pillPath = "/home/benjamin/r/urbit/bin/solid.pill"
        ship     = zod

    -- tryParseEvents "/home/benjamin/r/urbit/s/zod/.urb/log" 1
    -- tryParseEvents "/home/benjamin/r/urbit/s/testnet-zod/.urb/log" 1

    -- tryParseFX "/home/benjamin/zod-fx"         1 100000000
    -- tryParseFX "/home/benjamin/testnet-zod-fx" 1 100000000

    -- tryResume shipPath
    tryPlayShip shipPath
    -- tryFullReplay shipPath

    pure ()

--------------------------------------------------------------------------------

{-
    Interesting
-}
testPill :: FilePath -> Bool -> Bool -> IO ()
testPill pax showPil showSeq = do
  putStrLn "Reading pill file."
  pillBytes <- readFile pax

  putStrLn "Cueing pill file."
  pillNoun <- cueBS pillBytes & either throwIO pure

  putStrLn "Parsing pill file."
  pill <- fromNounErr pillNoun & either (throwIO . uncurry ParseErr) pure

  putStrLn "Using pill to generate boot sequence."
  bootSeq <- generateBootSeq zod pill

  putStrLn "Validate jam/cue and toNoun/fromNoun on pill value"
  reJam <- validateNounVal pill

  putStrLn "Checking if round-trip matches input file:"
  unless (reJam == pillBytes) $ do
    putStrLn "    Our jam does not match the file...\n"
    putStrLn "    This is surprising, but it is probably okay."

  when showPil $ do
      putStrLn "\n\n== Pill ==\n"
      pPrint pill

  when showSeq $ do
      putStrLn "\n\n== Boot Sequence ==\n"
      pPrint bootSeq

validateNounVal :: (Eq a, ToNoun a, FromNoun a) => a -> IO ByteString
validateNounVal inpVal = do
    putStrLn "  jam"
    inpByt <- evaluate $ jamBS $ toNoun inpVal

    putStrLn "  cue"
    outNon <- cueBS inpByt & either throwIO pure

    putStrLn "  fromNoun"
    outVal <- fromNounErr outNon & either (throwIO . uncurry ParseErr) pure

    putStrLn "  toNoun"
    outNon <- evaluate (toNoun outVal)

    putStrLn "  jam"
    outByt <- evaluate $ jamBS outNon

    putStrLn "Checking if: x == cue (jam x)"
    unless (inpVal == outVal) $
        error "Value fails test: x == cue (jam x)"

    putStrLn "Checking if: jam x == jam (cue (jam x))"
    unless (inpByt == outByt) $
        error "Value fails test: jam x == jam (cue (jam x))"

    pure outByt

--------------------------------------------------------------------------------

newShip :: CLI.New -> CLI.Opts -> IO ()
newShip CLI.New{..} _ = do
    tryBootFromPill nPillPath pierPath (Ship 0)
  where
    pierPath = fromMaybe ("./" <> unpack nShipAddr) nPierPath

runShip :: CLI.Run -> CLI.Opts -> IO ()
runShip (CLI.Run pierPath) _ = tryPlayShip pierPath

main :: IO ()
main = CLI.parseArgs >>= \case
    CLI.CmdRun r o                            -> runShip r o
    CLI.CmdNew n o                            -> newShip n o
    CLI.CmdBug (CLI.CollectAllFX pax)         -> collectAllFx pax
    CLI.CmdBug (CLI.ValidatePill pax pil seq) -> testPill pax pil seq
    CLI.CmdBug (CLI.ValidateFX pax)           -> print "validate-fx"
    CLI.CmdBug (CLI.ValidateEvents pax start) -> checkEvs pax start

validatePill :: FilePath -> IO ()
validatePill = const (pure ())

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


{-
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
-}
