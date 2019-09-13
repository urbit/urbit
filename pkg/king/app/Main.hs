{-
    # Booting a Ship

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


    # Implement subcommands to test event and effect parsing.

    - `king * --collect-fx`: All effects that come from the serf get
      written into the `effects` LMDB database.

    - `king clear-fx PIER`: Deletes all collected effects.

    - `king full-replay PIER`: Replays the whole event log events, print
      any failures. On success, replace the snapshot.


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
import Control.Exception (AsyncException(UserInterrupt))
import Data.Acquire
import Data.Conduit
import Data.Conduit.List hiding (replicate, take)
import Noun hiding (Parser)
import Vere.Pier
import Vere.Pier.Types
import Vere.Serf

import Control.Concurrent   (myThreadId, runInBoundThread)
import Control.Lens         ((&))
import Data.Default         (def)
import System.Directory     (doesFileExist, removeFile)
import System.Directory     (createDirectoryIfMissing, getHomeDirectory)
import System.Environment   (getProgName)
import System.Posix.Signals (Handler(Catch), installHandler, sigTERM)
import Text.Show.Pretty     (pPrint)
import Urbit.Time           (Wen)

import qualified CLI
import qualified Data.Set                    as Set
import qualified EventBrowser
import qualified System.IO.LockFile.Internal as Lock
import qualified Vere.Log                    as Log
import qualified Vere.Pier                   as Pier
import qualified Vere.Serf                   as Serf

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
    home <- getHomeDirectory
    let logDir = home <> "/log"
    createDirectoryIfMissing True logDir
    withTempFile logDir "king-" $ \tmpFile hFile -> do
        hSetBuffering hFile LineBuffering

        logOptions <- logOptionsHandle hFile True
            <&> setLogUseTime True
            <&> setLogUseLoc False

        withLogFunc logOptions $ \logFunc -> do
            let app = App { _appLogFunc = logFunc
                          , _appName    = "Alice"
                          }
            runRIO app inner

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

tryBootFromPill :: HasLogFunc e => FilePath -> FilePath -> Ship -> RIO e ()
tryBootFromPill pillPath shipPath ship = do
    rwith bootedPier $ \(serf, log, ss) -> do
        logTrace "Booting"
        logTrace $ displayShow ss
        io $ threadDelay 500000
        ss <- shutdown serf 0
        logTrace $ displayShow ss
        logTrace "Booted!"
  where
    bootedPier = do
        lockFile shipPath
        Pier.booted pillPath shipPath [] ship

runAcquire :: (MonadUnliftIO m,  MonadIO m)
           => Acquire a -> m a
runAcquire act = with act pure

runRAcquire :: (MonadUnliftIO (m e),  MonadIO (m e), MonadReader e (m e))
            => RAcquire e a -> m e a
runRAcquire act = rwith act pure

tryPlayShip :: HasLogFunc e => FilePath -> RIO e ()
tryPlayShip shipPath = do
    runRAcquire $ do
        lockFile shipPath
        rio $ logTrace "RESUMING SHIP"
        sls <- Pier.resumed shipPath []
        rio $ logTrace "SHIP RESUMED"
        Pier.pier shipPath Nothing sls

lockFile :: HasLogFunc e => FilePath -> RAcquire e ()
lockFile pax = void $ mkRAcquire start stop
  where
    fil = pax <> "/.vere.lock"

    stop handle = do
        logInfo $ display @Text $ ("Releasing lock file: " <> pack fil)
        io $ Lock.unlock fil handle

    params = def { Lock.retryToAcquireLock = Lock.No }

    start = do
        logInfo $ display @Text $ ("Taking lock file: " <> pack fil)
        io (Lock.lock params fil)

tryResume :: HasLogFunc e => FilePath -> RIO e ()
tryResume shipPath = do
    rwith resumedPier $ \(serf, log, ss) -> do
        logTrace (displayShow ss)
        threadDelay 500000
        ss <- shutdown serf 0
        logTrace (displayShow ss)
        logTrace "Resumed!"
  where
    resumedPier = do
        lockFile shipPath
        Pier.resumed shipPath []

tryFullReplay :: HasLogFunc e => FilePath -> RIO e ()
tryFullReplay shipPath = do
    wipeSnapshot
    tryResume shipPath
  where
    wipeSnapshot = do
        logTrace "wipeSnapshot"
        logDebug $ display $ pack @Text ("Wiping " <> north)
        logDebug $ display $ pack @Text ("Wiping " <> south)
        removeFileIfExists north
        removeFileIfExists south

    north = shipPath <> "/.urb/chk/north.bin"
    south = shipPath <> "/.urb/chk/south.bin"


--------------------------------------------------------------------------------

checkEvs :: forall e. HasLogFunc e => FilePath -> Word64 -> Word64 -> RIO e ()
checkEvs pierPath first last = do
    rwith (Log.existing logPath) $ \log -> do
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
        lockFile top
        log  <- Log.existing (top <> "/.urb/log")
        serf <- Serf.run (Serf.Config tmpDir serfFlags)
        rio $ Serf.collectFX serf log

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

startBrowser :: HasLogFunc e => FilePath -> RIO e ()
startBrowser pierPath = runRAcquire $ do
    lockFile pierPath
    log <- Log.existing (pierPath <> "/.urb/log")
    rio $ EventBrowser.run log

main :: IO ()
main = do
    mainTid <- myThreadId

    let onTermSig = throwTo mainTid UserInterrupt

    installHandler sigTERM (Catch onTermSig) Nothing

    CLI.parseArgs >>= runApp . \case
        CLI.CmdRun r o                            -> runShip r o
        CLI.CmdNew n o                            -> newShip n o
        CLI.CmdBug (CLI.CollectAllFX pax)         -> collectAllFx pax
        CLI.CmdBug (CLI.EventBrowser pax)         -> startBrowser pax
        CLI.CmdBug (CLI.ValidatePill pax pil seq) -> testPill pax pil seq
        CLI.CmdBug (CLI.ValidateEvents pax f l)   -> checkEvs pax f l
        CLI.CmdBug (CLI.ValidateFX pax f l)       -> checkFx  pax f l

--------------------------------------------------------------------------------

checkFx :: HasLogFunc e
        => FilePath -> Word64 -> Word64 -> RIO e ()
checkFx pierPath first last =
    rwith (Log.existing logPath) $ \log ->
        runConduit $ streamFX log first last
                  .| tryParseFXStream
  where
    logPath = pierPath <> "/.urb/log"

streamFX :: HasLogFunc e
         => Log.EventLog -> Word64 -> Word64
         -> ConduitT () ByteString (RIO e) ()
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
