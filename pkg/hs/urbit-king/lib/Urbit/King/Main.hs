{-|
    King Haskell Entry Point

    # Event Pruning

    - `king discard-events NUM_EVENTS`: Delete the last `n` events from
      the event log.

    - `king discard-events-interactive`: Iterate through the events in
      the event log, from last to first, pretty-print each event, and
      ask if it should be pruned.

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
-}

module Urbit.King.Main (main) where

import Urbit.Prelude

import Data.Conduit
import Network.HTTP.Client.TLS
import RIO.Directory
import Urbit.Arvo
import Urbit.King.Config
import Urbit.Vere.Dawn
import Urbit.Vere.Pier
import Urbit.Vere.Pier.Types
import Urbit.Vere.Serf

import Control.Concurrent     (myThreadId)
import Control.Exception      (AsyncException(UserInterrupt))
import Control.Lens           ((&))
import System.Process         (system)
import Text.Show.Pretty       (pPrint)
import Urbit.King.App         (runApp, runAppLogFile, runAppNoLog, runPierApp)
import Urbit.King.App         (HasConfigDir(..), HasStderrLogFunc(..))
import Urbit.Noun.Conversions (cordToUW)
import Urbit.Time             (Wen)
import Urbit.Vere.LockFile    (lockFile)

import qualified Data.Set                as Set
import qualified Data.Text               as T
import qualified Network.HTTP.Client     as C
import qualified System.Posix.Signals    as Sys
import qualified System.ProgressBar      as PB
import qualified System.Random           as Sys
import qualified Urbit.King.CLI          as CLI
import qualified Urbit.King.EventBrowser as EventBrowser
import qualified Urbit.Ob                as Ob
import qualified Urbit.Vere.Log          as Log
import qualified Urbit.Vere.Pier         as Pier
import qualified Urbit.Vere.Serf         as Serf
import qualified Urbit.Vere.Term         as Term

--------------------------------------------------------------------------------

zod :: Ship
zod = 0

--------------------------------------------------------------------------------

removeFileIfExists :: HasLogFunc env => FilePath -> RIO env ()
removeFileIfExists pax = do
  exists <- doesFileExist pax
  when exists $ do
      removeFile pax

--------------------------------------------------------------------------------

toSerfFlags :: CLI.Opts -> Serf.Flags
toSerfFlags CLI.Opts{..} = catMaybes m
  where
    -- TODO: This is not all the flags.
    m = [ from oQuiet Serf.Quiet
        , from oTrace Serf.Trace
        , from oHashless Serf.Hashless
        , from oQuiet Serf.Quiet
        , from oVerbose Serf.Verbose
        , from (oDryRun || isJust oDryFrom) Serf.DryRun
        ]
    from True flag = Just flag
    from False _   = Nothing


toPierConfig :: FilePath -> CLI.Opts -> PierConfig
toPierConfig pierPath CLI.Opts {..} = PierConfig { .. }
 where
  _pcPierPath = pierPath
  _pcDryRun   = oDryRun || isJust oDryFrom

toNetworkConfig :: CLI.Opts -> NetworkConfig
toNetworkConfig CLI.Opts {..} = NetworkConfig { .. }
 where
  dryRun     = oDryRun || isJust oDryFrom
  offline    = dryRun || oOffline

  mode = case (dryRun, offline, oLocalhost) of
    (True, _   , _   ) -> NMNone
    (_   , True, _   ) -> NMNone
    (_   , _   , True) -> NMLocalhost
    (_   , _   , _   ) -> NMNormal

  _ncNetMode   = mode
  _ncAmesPort  = oAmesPort
  _ncHttpPort  = oHttpPort
  _ncHttpsPort = oHttpsPort
  _ncLocalPort = oLoopbackPort

tryBootFromPill :: ( HasLogFunc e, HasNetworkConfig e, HasPierConfig e
                   , HasConfigDir e, HasStderrLogFunc e
                   )
                => Bool -> Pill -> Bool -> Serf.Flags -> Ship
                -> LegacyBootEvent
                -> RIO e ()
tryBootFromPill oExit pill lite flags ship boot = do
    mStart <- newEmptyMVar
    runOrExitImmediately bootedPier oExit mStart
  where
    bootedPier = do
        view pierPathL >>= lockFile
        rio $ logTrace "Starting boot"
        sls <- Pier.booted pill lite flags ship boot
        rio $ logTrace "Completed boot"
        pure sls

runOrExitImmediately :: ( HasLogFunc e, HasNetworkConfig e, HasPierConfig e
                        , HasConfigDir e
                        )
                     => RAcquire e (Serf e, Log.EventLog, SerfState)
                     -> Bool
                     -> MVar ()
                     -> RIO e ()
runOrExitImmediately getPier oExit mStart =
    rwith getPier $ if oExit then shutdownImmediately else runPier
  where
    shutdownImmediately (serf, log, ss) = do
        logTrace "Sending shutdown signal"
        logTrace $ displayShow ss

        -- Why is this here? Do I need to force a snapshot to happen?
        io $ threadDelay 500000

        ss <- shutdown serf 0
        logTrace $ displayShow ss
        logTrace "Shutdown!"

    runPier sls = do
        runRAcquire $ Pier.pier sls mStart

tryPlayShip :: ( HasStderrLogFunc e, HasLogFunc e, HasNetworkConfig e
               , HasPierConfig e, HasConfigDir e
               )
            => Bool -> Bool -> Maybe Word64 -> Serf.Flags -> MVar () -> RIO e ()
tryPlayShip exitImmediately fullReplay playFrom flags mStart = do
    when fullReplay wipeSnapshot
    runOrExitImmediately resumeShip exitImmediately mStart
  where
    wipeSnapshot = do
        shipPath <- view pierPathL
        logTrace "wipeSnapshot"
        logDebug $ display $ pack @Text ("Wiping " <> north shipPath)
        logDebug $ display $ pack @Text ("Wiping " <> south shipPath)
        removeFileIfExists (north shipPath)
        removeFileIfExists (south shipPath)

    north shipPath = shipPath <> "/.urb/chk/north.bin"
    south shipPath = shipPath <> "/.urb/chk/south.bin"

    resumeShip = do
        view pierPathL >>= lockFile
        rio $ logTrace "RESUMING SHIP"
        sls <- Pier.resumed playFrom flags
        rio $ logTrace "SHIP RESUMED"
        pure sls

runRAcquire :: (MonadUnliftIO (m e),  MonadIO (m e), MonadReader e (m e))
            => RAcquire e a -> m e a
runRAcquire act = rwith act pure

--------------------------------------------------------------------------------

checkEvs :: forall e. HasLogFunc e => FilePath -> Word64 -> Word64 -> RIO e ()
checkEvs pierPath first last = do
    rwith (Log.existing logPath) $ \log -> do
        let ident = Log.identity log
        let pbSty = PB.defStyle { PB.stylePostfix = PB.exact }
        logTrace (displayShow ident)

        last <- Log.lastEv log <&> \lastReal -> min last lastReal

        let evCount = fromIntegral (last - first)

        pb <- PB.newProgressBar pbSty 10 (PB.Progress 1 evCount ())

        runConduit $ Log.streamEvents log first
                  .| showEvents pb first (fromIntegral $ lifecycleLen ident)
  where
    logPath :: FilePath
    logPath = pierPath <> "/.urb/log"

    showEvents :: PB.ProgressBar () -> EventId -> EventId
               -> ConduitT ByteString Void (RIO e) ()
    showEvents pb eId _ | eId > last = pure ()
    showEvents pb eId cycle          = await >>= \case
        Nothing -> do
            lift $ PB.killProgressBar pb
            lift $ logTrace "Everything checks out."
        Just bs -> do
            lift $ PB.incProgress pb 1
            lift $ do
                n <- io $ cueBSExn bs
                when (eId > cycle) $ do
                    (mug, wen, evNoun) <- unpackJob n
                    fromNounErr evNoun & \case
                        Left err       -> logError (displayShow (eId, err))
                        Right (_ ∷ Ev) -> pure ()
            showEvents pb (succ eId) cycle

    unpackJob :: Noun -> RIO e (Mug, Wen, Noun)
    unpackJob = io . fromNounExn

--------------------------------------------------------------------------------

{-|
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
    tmpDir = top </> ".tmpdir"

    collectedFX :: RAcquire e ()
    collectedFX = do
        lockFile top
        log  <- Log.existing (top <> "/.urb/log")
        serf <- Serf.run (Serf.Config tmpDir serfFlags)
        rio $ Serf.collectFX serf log

    serfFlags :: Serf.Flags
    serfFlags = [Serf.Hashless, Serf.DryRun]

--------------------------------------------------------------------------------

replayPartEvs :: ∀e. (HasStderrLogFunc e, HasLogFunc e)
              => FilePath -> Word64 -> RIO e ()
replayPartEvs top last = do
    logTrace $ display $ pack @Text top
    fetchSnapshot
    rwith replayedEvs $ \() ->
        logTrace "Done replaying events!"
  where
    fetchSnapshot :: RIO e ()
    fetchSnapshot = do
      snap <- Pier.getSnapshot top last
      case snap of
        Nothing -> pure ()
        Just sn -> do
          liftIO $ system $ "cp -r \"" <> sn <> "\" \"" <> tmpDir <> "\""
          pure ()

    tmpDir :: FilePath
    tmpDir = top </> ".partial-replay" </> show last

    replayedEvs :: RAcquire e ()
    replayedEvs = do
        lockFile top
        log  <- Log.existing (top <> "/.urb/log")
        serf <- Serf.run (Serf.Config tmpDir serfFlags)
        rio $ do
          ss <- Serf.replay serf log $ Just last
          Serf.snapshot serf ss
          io $ threadDelay 500000 -- Copied from runOrExitImmediately
          pure ()

    serfFlags :: Serf.Flags
    serfFlags = [Serf.Hashless]

--------------------------------------------------------------------------------

{-|
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
  bootSeq <- generateBootSeq zod pill False (Fake $ Ship 0)

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

pillFrom :: CLI.PillSource -> RIO e Pill

pillFrom (CLI.PillSourceFile pillPath) = do
  putStrLn $ "boot: reading pill from " ++ pack pillPath
  io (loadFile pillPath >>= either throwIO pure)

pillFrom (CLI.PillSourceURL url) = do
  putStrLn $ "boot: retrieving pill from " ++ pack url
  -- Get the jamfile with the list of stars accepting comets right now.
  manager <- io $ C.newManager tlsManagerSettings
  request <- io $ C.parseRequest url
  response <- io $ C.httpLbs (C.setRequestCheckStatus request) manager
  let body = toStrict $ C.responseBody response

  noun <- cueBS body & either throwIO pure
  fromNounErr noun & either (throwIO . uncurry ParseErr) pure

newShip :: forall e. HasLogFunc e => CLI.New -> CLI.Opts -> RIO e ()
newShip CLI.New{..} opts
  | CLI.BootComet <- nBootType = do
      pill <- pillFrom nPillSource
      putStrLn "boot: retrieving list of stars currently accepting comets"
      starList <- dawnCometList
      putStrLn ("boot: " ++ (tshow $ length starList) ++
                " star(s) currently accepting comets")
      putStrLn "boot: mining a comet"
      eny <- io $ Sys.randomIO
      let seed = mineComet (Set.fromList starList) eny
      putStrLn ("boot: found comet " ++ renderShip (sShip seed))
      bootFromSeed pill seed

  | CLI.BootFake name <- nBootType = do
      pill <- pillFrom nPillSource
      ship <- shipFrom name
      runTryBootFromPill pill name ship (Fake ship)

  | CLI.BootFromKeyfile keyFile <- nBootType = do
      text <- readFileUtf8 keyFile
      asAtom <- case cordToUW (Cord $ T.strip text) of
        Nothing -> error "Couldn't parse keyfile. Hint: keyfiles start with 0w?"
        Just (UW a) -> pure a

      asNoun <- cueExn asAtom
      seed :: Seed <- case fromNoun asNoun of
        Nothing -> error "Keyfile does not seem to contain a seed."
        Just s  -> pure s

      pill <- pillFrom nPillSource

      bootFromSeed pill seed

  where
    shipFrom :: Text -> RIO e Ship
    shipFrom name = case Ob.parsePatp name of
      Left x  -> error "Invalid ship name"
      Right p -> pure $ Ship $ fromIntegral $ Ob.fromPatp p

    pierPath :: Text -> FilePath
    pierPath name = case nPierPath of
      Just x  -> x
      Nothing -> "./" <> unpack name

    nameFromShip :: Ship -> RIO e Text
    nameFromShip s = name
      where
        nameWithSig = Ob.renderPatp $ Ob.patp $ fromIntegral s
        name = case stripPrefix "~" nameWithSig of
          Nothing -> error "Urbit.ob didn't produce string with ~"
          Just x  -> pure x

    bootFromSeed :: Pill -> Seed -> RIO e ()
    bootFromSeed pill seed = do
      ethReturn <- dawnVent seed

      case ethReturn of
        Left x -> error $ unpack x
        Right dawn -> do
          let ship = sShip $ dSeed dawn
          name <- nameFromShip ship
          runTryBootFromPill pill name ship (Dawn dawn)

    flags = toSerfFlags opts

    -- Now that we have all the information for running an application with a
    -- PierConfig, do so.
    runTryBootFromPill pill name ship bootEvent = do
      let pierConfig = toPierConfig (pierPath name) opts
      let networkConfig = toNetworkConfig opts
      io $ runPierApp pierConfig networkConfig True $
        tryBootFromPill True pill nLite flags ship bootEvent
------  tryBootFromPill (CLI.oExit opts) pill nLite flags ship bootEvent


runShip :: CLI.Run -> CLI.Opts -> Bool -> IO ()
runShip (CLI.Run pierPath) opts daemon = do
    tid <- myThreadId
    let onTermExit = throwTo tid UserInterrupt
    mStart <- newEmptyMVar
    if daemon
    then runPier mStart
    else do
      connectionThread <- async $ do
        readMVar mStart
        finally (runAppNoLog $ connTerm pierPath) onTermExit
      finally (runPier mStart) (cancel connectionThread)
  where
    runPier mStart =
          runPierApp pierConfig networkConfig daemon $
            tryPlayShip
              (CLI.oExit opts)
              (CLI.oFullReplay opts)
              (CLI.oDryFrom opts)
              (toSerfFlags opts)
              mStart
    pierConfig = toPierConfig pierPath opts
    networkConfig = toNetworkConfig opts


startBrowser :: HasLogFunc e => FilePath -> RIO e ()
startBrowser pierPath = runRAcquire $ do
    -- lockFile pierPath
    log <- Log.existing (pierPath <> "/.urb/log")
    rio $ EventBrowser.run log

checkDawn :: HasLogFunc e => FilePath -> RIO e ()
checkDawn keyfilePath = do
  -- The keyfile is a jammed Seed then rendered in UW format
  text <- readFileUtf8 keyfilePath
  asAtom <- case cordToUW (Cord $ T.strip text) of
    Nothing -> error "Couldn't parse keyfile. Hint: keyfiles start with 0w?"
    Just (UW a) -> pure a

  asNoun <- cueExn asAtom
  seed :: Seed <- case fromNoun asNoun of
    Nothing -> error "Keyfile does not seem to contain a seed."
    Just s  -> pure s

  print $ show seed

  e <- dawnVent seed
  print $ show e


checkComet :: HasLogFunc e => RIO e ()
checkComet = do
  starList <- dawnCometList
  putStrLn "Stars currently accepting comets:"
  let starNames = map (Ob.renderPatp . Ob.patp . fromIntegral) starList
  print starNames
  putStrLn "Trying to mine a comet..."
  eny <- io $ Sys.randomIO
  let s = mineComet (Set.fromList starList) eny
  print s

main :: IO ()
main = do
    mainTid <- myThreadId

    hSetBuffering stdout NoBuffering

    let onTermSig = throwTo mainTid UserInterrupt

    Sys.installHandler Sys.sigTERM (Sys.Catch onTermSig) Nothing

    CLI.parseArgs >>= \case
        CLI.CmdRun r o d                        -> runShip r o d
        CLI.CmdNew n o                          -> runApp $ newShip n o
        CLI.CmdBug (CLI.CollectAllFX pax)       -> runApp $ collectAllFx pax
        CLI.CmdBug (CLI.EventBrowser pax)       -> runApp $ startBrowser pax
        CLI.CmdBug (CLI.ValidatePill pax pil s) -> runApp $ testPill pax pil s
        CLI.CmdBug (CLI.ValidateEvents pax f l) -> runApp $ checkEvs pax f l
        CLI.CmdBug (CLI.ValidateFX pax f l)     -> runApp $ checkFx  pax f l
        CLI.CmdBug (CLI.ReplayEvents pax l)     -> runApp $ replayPartEvs pax l
        CLI.CmdBug (CLI.CheckDawn pax)          -> runApp $ checkDawn pax
        CLI.CmdBug CLI.CheckComet               -> runApp $ checkComet
        CLI.CmdCon pier                         -> runAppLogFile $ connTerm pier


--------------------------------------------------------------------------------

connTerm :: ∀e. HasLogFunc e => FilePath -> RIO e ()
connTerm pier =
    Term.runTerminalClient pier

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
