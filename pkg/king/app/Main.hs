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

module Main (main) where

import UrbitPrelude

import Data.RAcquire

import Arvo
import Control.Exception hiding (evaluate, throwIO)
import Data.Acquire
import Data.Conduit
import Data.Conduit.List hiding (replicate, take)
import Noun hiding (Parser)
import Vere.Pier
import Vere.Pier.Types
import Vere.Serf

import Control.Concurrent (runInBoundThread)
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

import RIO (RIO, runRIO)
import RIO (Utf8Builder, display, displayShow)
import RIO (threadDelay)

import RIO ( HasLogFunc
           , LogFunc
           , logError
           , logInfo
           , logWarn
           , logDebug
           , logOther
           , logFuncL
           , logOptionsHandle
           , withLogFunc
           , setLogUseTime
           , setLogUseLoc
           )

--------------------------------------------------------------------------------

logTrace :: HasLogFunc e => Utf8Builder -> RIO e ()
logTrace = logOther "trace"

--------------------------------------------------------------------------------

class HasAppName env where
    appNameL :: Lens' env Utf8Builder

data App = App
    { _appLogFunc :: !LogFunc
    , _appName    :: !Utf8Builder
    }

makeLenses ''App

instance HasLogFunc App where
    logFuncL = appLogFunc

instance HasAppName App where
    appNameL = appName

runApp :: RIO App a -> IO a
runApp inner = do
    logOptions <- logOptionsHandle stderr True
        <&> setLogUseTime True
        <&> setLogUseLoc False

    withLogFunc logOptions $ \logFunc -> do
        let app = App { _appLogFunc = logFunc
                      , _appName    = "Alice"
                      }
        runRIO app inner

--------------------------------------------------------------------------------

io :: MonadIO m => IO a -> m a
io = liftIO

rio :: MonadRIO m => RIO e a -> m e a
rio = liftRIO

--------------------------------------------------------------------------------

example :: IO ()
example = runApp sayHello

sayHello :: RIO App ()
sayHello = do
    name <- view appName
    logDebug $ "Hello, " <> name
    logInfo  $ "Hello, " <> name
    logWarn  $ "Hello, " <> name
    logError $ "Hello, " <> name

--------------------------------------------------------------------------------

zod :: Ship
zod = 0

--------------------------------------------------------------------------------

removeFileIfExists :: HasLogFunc env => FilePath -> RIO env ()
removeFileIfExists pax = do
  exists <- io $ doesFileExist pax
  when exists $ do
      io $ removeFile pax

--------------------------------------------------------------------------------

wipeSnapshot :: HasLogFunc env => FilePath -> RIO env ()
wipeSnapshot shipPath = do
    logTrace "wipeSnapshot"
    logDebug $ display $ pack @Text ("Wiping " <> north)
    logDebug $ display $ pack @Text ("Wiping " <> south)
    removeFileIfExists north
    removeFileIfExists south
  where
    north = shipPath <> "/.urb/chk/north.bin"
    south = shipPath <> "/.urb/chk/south.bin"

--------------------------------------------------------------------------------

tryBootFromPill :: HasLogFunc e => FilePath -> FilePath -> Ship -> RIO e ()
tryBootFromPill pillPath shipPath ship = do
    wipeSnapshot shipPath
    with (Pier.booted pillPath shipPath [] ship) $ \(serf, log, ss) -> do
        logTrace "Booting"
        logTrace $ displayShow ss
        io $ threadDelay 500000
        ss <- io $ shutdown serf 0
        logTrace $ displayShow ss
        logTrace "Booted!"

runAcquire :: (MonadUnliftIO m,  MonadIO m)
           => Acquire a -> m a
runAcquire act = with act pure

runRAcquire :: (MonadUnliftIO (m e),  MonadIO (m e), MonadReader e (m e))
            => RAcquire e a -> m e a
runRAcquire act = rwith act pure

tryPlayShip :: HasLogFunc e => FilePath -> RIO e ()
tryPlayShip shipPath = do
    runRAcquire $ do
        liftRIO $ logTrace "RESUMING SHIP"
        sls <- liftAcquire $ Pier.resumed shipPath []
        liftRIO $ logTrace "SHIP RESUMED"
        liftAcquire $ Pier.pier shipPath Nothing sls

tryResume :: HasLogFunc e => FilePath -> RIO e ()
tryResume shipPath = do
    rwith (liftAcquire $ Pier.resumed shipPath []) $ \(serf, log, ss) -> do
        logTrace (displayShow ss)
        threadDelay 500000
        ss <- io (shutdown serf 0)
        logTrace (displayShow ss)
        logTrace "Resumed!"

tryFullReplay :: HasLogFunc e => FilePath -> RIO e ()
tryFullReplay shipPath = do
    wipeSnapshot shipPath
    tryResume shipPath

--------------------------------------------------------------------------------

checkEvs :: forall e. HasLogFunc e => FilePath -> Word64 -> Word64 -> RIO e ()
checkEvs pierPath first last = do
    rwith (liftAcquire $ Log.existing logPath) $ \log -> do
        let ident = Log.identity log
        logTrace (displayShow ident)
        runConduit $ Log.streamEvents log first
                  .| showEvents first (fromIntegral $ lifecycleLen ident)
  where
    logPath :: FilePath
    logPath = pierPath <> "/.urb/log"

    showEvents :: EventId -> EventId -> ConduitT ByteString Void (RIO e) ()
    showEvents eId _ | eId > last = pure ()
    showEvents eId cycle          =
        await >>= \case
            Nothing -> lift $ logTrace "Everything checks out."
            Just bs -> do
                lift $ do
                    n <- io $ cueBSExn bs
                    when (eId > cycle) $ do
                        (mug, wen, evNoun) <- unpackJob n
                        fromNounErr evNoun &
                            either (logError . displayShow) pure
                showEvents (succ eId) cycle

    unpackJob :: Noun -> RIO e (Mug, Wen, Noun)
    unpackJob = io . fromNounExn

--------------------------------------------------------------------------------

{-
    This runs the serf at `$top/.tmpdir`, but we disable snapshots,
    so this should never actually be created. We just do this to avoid
    letting the serf use an existing snapshot.
-}
collectAllFx :: ∀e. HasLogFunc e => FilePath -> RIO e ()
collectAllFx top = do
    logTrace $ display $ pack @Text top
    rwith collectedFX $ \() ->
        logTrace "Done collecting effects!"
  where
    tmpDir :: FilePath
    tmpDir = top <> "/.tmpdir"

    collectedFX :: RAcquire e ()
    collectedFX = do
        log  <- liftAcquire $ Log.existing (top <> "/.urb/log")
        serf <- liftAcquire $ Serf.run (Serf.Config tmpDir serfFlags)
        liftIO (Serf.collectFX serf log)

    serfFlags :: Serf.Flags
    serfFlags = [Serf.Hashless, Serf.DryRun]

--------------------------------------------------------------------------------

{-
    Interesting
-}
testPill :: HasLogFunc e => FilePath -> Bool -> Bool -> RIO e ()
testPill pax showPil showSeq = do
  putStrLn "Reading pill file."
  pillBytes <- readFile pax

  putStrLn "Cueing pill file."
  pillNoun <- io $ cueBS pillBytes & either throwIO pure

  putStrLn "Parsing pill file."
  pill <- io $ fromNounErr pillNoun & either (throwIO . uncurry ParseErr) pure

  putStrLn "Using pill to generate boot sequence."
  bootSeq <- io $ generateBootSeq zod pill

  putStrLn "Validate jam/cue and toNoun/fromNoun on pill value"
  reJam <- validateNounVal pill

  putStrLn "Checking if round-trip matches input file:"
  unless (reJam == pillBytes) $ do
    putStrLn "    Our jam does not match the file...\n"
    putStrLn "    This is surprising, but it is probably okay."

  when showPil $ do
      putStrLn "\n\n== Pill ==\n"
      io $ pPrint pill

  when showSeq $ do
      putStrLn "\n\n== Boot Sequence ==\n"
      io $ pPrint bootSeq

validateNounVal :: (HasLogFunc e, Eq a, ToNoun a, FromNoun a)
                => a -> RIO e ByteString
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

newShip :: HasLogFunc e => CLI.New -> CLI.Opts -> RIO e ()
newShip CLI.New{..} _ = do
    tryBootFromPill nPillPath pierPath (Ship 0)
  where
    pierPath = fromMaybe ("./" <> unpack nShipAddr) nPierPath

runShip :: HasLogFunc e => CLI.Run -> CLI.Opts -> RIO e ()
runShip (CLI.Run pierPath) _ = tryPlayShip pierPath

main :: IO ()
main = CLI.parseArgs >>= runApp . \case
    CLI.CmdRun r o                             -> runShip r o
    CLI.CmdNew n o                             -> newShip n o
    CLI.CmdBug (CLI.CollectAllFX pax)          -> collectAllFx pax
    CLI.CmdBug (CLI.ValidatePill pax pil seq)  -> testPill pax pil seq
    CLI.CmdBug (CLI.ValidateEvents pax f l)    -> checkEvs pax f l
    CLI.CmdBug (CLI.ValidateFX pax f l)        -> checkFx  pax f l

--------------------------------------------------------------------------------

checkFx :: HasLogFunc e
        => FilePath -> Word64 -> Word64 -> RIO e ()
checkFx pierPath first last =
    rwith (liftAcquire $ Log.existing logPath) $ \log ->
        runConduit $ streamFX log first last
                  .| tryParseFXStream
  where
    logPath = pierPath <> "/.urb/log"

streamFX :: MonadIO m
         => Log.EventLog -> Word64 -> Word64 -> ConduitT () ByteString m ()
streamFX log first last = do
    Log.streamEffectsRows log first .| loop
  where
    loop = await >>= \case Nothing                     -> pure ()
                           Just (eId, bs) | eId > last -> pure ()
                           Just (eId, bs)              -> yield bs >> loop

tryParseFXStream :: HasLogFunc e => ConduitT ByteString Void (RIO e) ()
tryParseFXStream = loop
  where
    loop = await >>= \case
        Nothing -> pure ()
        Just bs -> do
            n <- liftIO (cueBSExn bs)
            fromNounErr n & either (logError . displayShow) pure
            loop


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
