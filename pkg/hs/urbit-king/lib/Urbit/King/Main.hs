{- |
  # Signal Handling (SIGTERM, SIGINT)

  We handle SIGTERM by causing the main thread to raise a `UserInterrupt`
  exception. This is the same behavior as SIGINT (the signal sent upon
  `CTRL-C`).

  The main thread is therefore responsible for handling this exception
  and causing everything to shut down properly.

  # Crashing and Shutting Down

  Rule number one: The King never crashes.

  This rule is asperational at the moment, but it needs to become as
  close to truth as possible. Shut down ships in extreme cases, but
  never let the king go down.
-}

{-
    TODO These some old scribbled notes. They don't belong here
    anymore. Do something about it.

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
import Urbit.Vere.Eyre.Multi (multiEyre, MultiEyreApi, MultiEyreConf(..))
import Urbit.Vere.Pier.Types
import Urbit.Vere.Serf

import Control.Concurrent     (myThreadId)
import Control.Exception      (AsyncException(UserInterrupt))
import Control.Lens           ((&))
import System.Process         (system)
import Text.Show.Pretty       (pPrint)
import Urbit.King.App         (App)
import Urbit.King.App         (runAppLogFile, runAppStderr, runPierApp)
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
  _ncNoAmes    = oNoAmes
  _ncNoHttp    = oNoHttp
  _ncNoHttps   = oNoHttps

tryBootFromPill :: ( HasLogFunc e, HasNetworkConfig e, HasPierConfig e
                   , HasConfigDir e, HasStderrLogFunc e
                   )
                => Bool -> Pill -> Bool -> Serf.Flags -> Ship
                -> LegacyBootEvent
                -> MultiEyreApi
                -> RIO e ()
tryBootFromPill oExit pill lite flags ship boot multi = do
    mStart <- newEmptyMVar
    runOrExitImmediately bootedPier oExit mStart multi
  where
    bootedPier = do
        view pierPathL >>= lockFile
        rio $ logTrace "Starting boot"
        sls <- Pier.booted pill lite flags ship boot
        rio $ logTrace "Completed boot"
        pure sls

runOrExitImmediately
  :: (HasLogFunc e, HasNetworkConfig e, HasPierConfig e, HasConfigDir e)
  => RAcquire e (Serf e, Log.EventLog, SerfState)
  -> Bool
  -> MVar ()
  -> MultiEyreApi
  -> RIO e ()
runOrExitImmediately getPier oExit mStart multi =
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
        runRAcquire $ Pier.pier sls mStart multi

tryPlayShip
  :: ( HasStderrLogFunc e
     , HasLogFunc e
     , HasNetworkConfig e
     , HasPierConfig e
     , HasConfigDir e
     )
  => Bool
  -> Bool
  -> Maybe Word64
  -> Serf.Flags
  -> MVar ()
  -> MultiEyreApi
  -> RIO e ()
tryPlayShip exitImmediately fullReplay playFrom flags mStart multi = do
    when fullReplay wipeSnapshot
    runOrExitImmediately resumeShip exitImmediately mStart multi
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
  logTrace "Reading pill file."
  pillBytes <- readFile pax

  logTrace "Cueing pill file."
  pillNoun <- io $ cueBS pillBytes & either throwIO pure

  logTrace "Parsing pill file."
  pill <- fromNounErr pillNoun & either (throwIO . uncurry ParseErr) pure

  logTrace "Using pill to generate boot sequence."
  bootSeq <- generateBootSeq zod pill False (Fake $ Ship 0)

  logTrace "Validate jam/cue and toNoun/fromNoun on pill value"
  reJam <- validateNounVal pill

  logTrace "Checking if round-trip matches input file:"
  unless (reJam == pillBytes) $ do
    logTrace "    Our jam does not match the file...\n"
    logTrace "    This is surprising, but it is probably okay."

  when showPil $ do
      logTrace "\n\n== Pill ==\n"
      io $ pPrint pill

  when showSeq $ do
      logTrace "\n\n== Boot Sequence ==\n"
      io $ pPrint bootSeq

validateNounVal :: (HasLogFunc e, Eq a, ToNoun a, FromNoun a)
                => a -> RIO e ByteString
validateNounVal inpVal = do
    logTrace "  jam"
    inpByt <- evaluate $ jamBS $ toNoun inpVal

    logTrace "  cue"
    outNon <- cueBS inpByt & either throwIO pure

    logTrace "  fromNoun"
    outVal <- fromNounErr outNon & either (throwIO . uncurry ParseErr) pure

    logTrace "  toNoun"
    outNon <- evaluate (toNoun outVal)

    logTrace "  jam"
    outByt <- evaluate $ jamBS outNon

    logTrace "Checking if: x == cue (jam x)"
    unless (inpVal == outVal) $
        error "Value fails test: x == cue (jam x)"

    logTrace "Checking if: jam x == jam (cue (jam x))"
    unless (inpByt == outByt) $
        error "Value fails test: jam x == jam (cue (jam x))"

    pure outByt

--------------------------------------------------------------------------------

pillFrom :: CLI.PillSource -> RIO App Pill
pillFrom = \case
  CLI.PillSourceFile pillPath -> do
    logTrace $ display $ "boot: reading pill from " ++ (pack pillPath :: Text)
    io (loadFile pillPath >>= either throwIO pure)

  CLI.PillSourceURL url -> do
    logTrace $ display $ "boot: retrieving pill from " ++ (pack url :: Text)
    -- Get the jamfile with the list of stars accepting comets right now.
    manager <- io $ C.newManager tlsManagerSettings
    request <- io $ C.parseRequest url
    response <- io $ C.httpLbs (C.setRequestCheckStatus request) manager
    let body = toStrict $ C.responseBody response

    noun <- cueBS body & either throwIO pure
    fromNounErr noun & either (throwIO . uncurry ParseErr) pure

newShip :: CLI.New -> CLI.Opts -> RIO App ()
newShip CLI.New{..} opts = do
  {-
    TODO XXX HACK

    Because the "new ship" flow *may* automatically start the ship,
    we need to create this, but it's not actually correct.

    The right solution is to separate out the "new ship" flow from the
    "run ship" flow, and possibly sequence them from the outside if
    that's really needed.
  -}
  multi <- multiEyre (MultiEyreConf Nothing Nothing True)

  case nBootType of
    CLI.BootComet -> do
      pill <- pillFrom nPillSource
      putStrLn "boot: retrieving list of stars currently accepting comets"
      starList <- dawnCometList
      putStrLn ("boot: " ++ (tshow $ length starList) ++
                " star(s) currently accepting comets")
      putStrLn "boot: mining a comet"
      eny <- io $ Sys.randomIO
      let seed = mineComet (Set.fromList starList) eny
      putStrLn ("boot: found comet " ++ renderShip (sShip seed))
      bootFromSeed multi pill seed

    CLI.BootFake name -> do
      pill <- pillFrom nPillSource
      ship <- shipFrom name
      runTryBootFromPill multi pill name ship (Fake ship)

    CLI.BootFromKeyfile keyFile -> do
      text <- readFileUtf8 keyFile
      asAtom <- case cordToUW (Cord $ T.strip text) of
        Nothing -> error "Couldn't parse keyfile. Hint: keyfiles start with 0w?"
        Just (UW a) -> pure a

      asNoun <- cueExn asAtom
      seed :: Seed <- case fromNoun asNoun of
        Nothing -> error "Keyfile does not seem to contain a seed."
        Just s  -> pure s

      pill <- pillFrom nPillSource

      bootFromSeed multi pill seed

  where
    shipFrom :: Text -> RIO App Ship
    shipFrom name = case Ob.parsePatp name of
      Left x  -> error "Invalid ship name"
      Right p -> pure $ Ship $ fromIntegral $ Ob.fromPatp p

    pierPath :: Text -> FilePath
    pierPath name = case nPierPath of
      Just x  -> x
      Nothing -> "./" <> unpack name

    nameFromShip :: Ship -> RIO App Text
    nameFromShip s = name
      where
        nameWithSig = Ob.renderPatp $ Ob.patp $ fromIntegral s
        name = case stripPrefix "~" nameWithSig of
          Nothing -> error "Urbit.ob didn't produce string with ~"
          Just x  -> pure x

    bootFromSeed :: MultiEyreApi -> Pill -> Seed -> RIO App ()
    bootFromSeed multi pill seed = do
      ethReturn <- dawnVent seed

      case ethReturn of
        Left x -> error $ unpack x
        Right dawn -> do
          let ship = sShip $ dSeed dawn
          name <- nameFromShip ship
          runTryBootFromPill multi pill name ship (Dawn dawn)

    flags = toSerfFlags opts

    -- Now that we have all the information for running an application with a
    -- PierConfig, do so.
    runTryBootFromPill multi pill name ship bootEvent = do
      let pierConfig = toPierConfig (pierPath name) opts
      let networkConfig = toNetworkConfig opts
      runPierApp pierConfig networkConfig $
        tryBootFromPill True pill nLite flags ship bootEvent multi
------  tryBootFromPill (CLI.oExit opts) pill nLite flags ship bootEvent

runShip :: CLI.Run -> CLI.Opts -> Bool -> MultiEyreApi -> RIO App ()
runShip (CLI.Run pierPath) opts daemon multi = do
    tid <- io myThreadId
    let onTermExit = throwTo tid UserInterrupt
    mStart <- newEmptyMVar
    if daemon
    then runPier mStart
    else do
      connectionThread <- async $ do
        readMVar mStart
        finally (connTerm pierPath) onTermExit
      finally (runPier mStart) (cancel connectionThread)
  where
    runPier mStart =
          runPierApp pierConfig networkConfig $
            tryPlayShip
              (CLI.oExit opts)
              (CLI.oFullReplay opts)
              (CLI.oDryFrom opts)
              (toSerfFlags opts)
              mStart
              multi
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
  args <- CLI.parseArgs
  hSetBuffering stdout NoBuffering
  setupSignalHandlers

  runApp args $ case args of
    CLI.CmdRun ko ships                       -> runShips ko ships
    CLI.CmdNew n  o                           -> newShip n o
    CLI.CmdBug (CLI.CollectAllFX pax        ) -> collectAllFx pax
    CLI.CmdBug (CLI.EventBrowser pax        ) -> startBrowser pax
    CLI.CmdBug (CLI.ValidatePill   pax pil s) -> testPill pax pil s
    CLI.CmdBug (CLI.ValidateEvents pax f   l) -> checkEvs pax f l
    CLI.CmdBug (CLI.ValidateFX     pax f   l) -> checkFx pax f l
    CLI.CmdBug (CLI.ReplayEvents pax l      ) -> replayPartEvs pax l
    CLI.CmdBug (CLI.CheckDawn pax           ) -> checkDawn pax
    CLI.CmdBug CLI.CheckComet                 -> checkComet
    CLI.CmdCon pier                           -> connTerm pier

 where
  runApp args | willRunTerminal args = runAppLogFile
  runApp args | otherwise            = runAppStderr

  setupSignalHandlers = do
    mainTid <- myThreadId
    let onKillSig = throwTo mainTid UserInterrupt
    for_ [Sys.sigTERM, Sys.sigINT] $ \sig -> do
      Sys.installHandler sig (Sys.Catch onKillSig) Nothing

  willRunTerminal :: CLI.Cmd -> Bool
  willRunTerminal = \case
    CLI.CmdCon _                 -> True
    CLI.CmdRun ko [(_,_,daemon)] -> not daemon
    CLI.CmdRun ko _              -> False
    _                            -> False


{-
  Runs a ship but restarts it if it crashes or shuts down on it's own.

  Once `waitForKillRequ` returns, the ship will be terminated and this
  routine will exit.

  TODO Use logging system instead of printing.
-}
runShipRestarting :: STM () -> CLI.Run -> CLI.Opts -> MultiEyreApi -> RIO App ()
runShipRestarting waitForKillRequ r o multi = do
  let pier = pack (CLI.rPierPath r)
      loop = runShipRestarting waitForKillRequ r o multi

  tid <- asyncBound (runShip r o True multi)

  let onShipExit = Left <$> waitCatchSTM tid
      onKillRequ = Right <$> waitForKillRequ

  atomically (onShipExit <|> onKillRequ) >>= \case
    Left exit -> do
      case exit of
        Left err -> logError $ display (tshow err <> ": " <> pier)
        Right () ->
          logError $ display ("Ship exited on it's own. Why? " <> pier)
      threadDelay 250_000
      loop
    Right () -> do
      logTrace $ display ("King Shutdown requested. Killing: " <> pier)
      cancel tid
      logTrace $ display ("Ship terminated: " <> pier)


runShips :: CLI.KingOpts -> [(CLI.Run, CLI.Opts, Bool)] -> RIO App ()
runShips CLI.KingOpts {..} ships = do
  let meConf = MultiEyreConf
        { mecHttpPort      = fromIntegral <$> koSharedHttpPort
        , mecHttpsPort     = fromIntegral <$> koSharedHttpsPort
        , mecLocalhostOnly = False -- TODO Localhost-only needs to be
                                   -- a king-wide option.
        }

  {-
    TODO Need to rework RIO environment to fix this. Should have a
    bunch of nested contexts:

      - King has started. King has Id. Logging available.
      - In running environment. MultiEyre and global config available.
      - In pier environment: pier path and config available.
      - In running ship environment: serf state, event queue available.
  -}
  multi <- multiEyre meConf

  go multi ships
 where
  go me = \case
    []          -> pure ()
    [(r, o, d)] -> runShip r o d me
    ships       -> runMultipleShips (ships <&> \(r, o, _) -> (r, o)) me

runMultipleShips :: [(CLI.Run, CLI.Opts)] -> MultiEyreApi -> RIO App ()
runMultipleShips ships multi = do
  killSignal <- newEmptyTMVarIO

  let waitForKillRequ = readTMVar killSignal

  shipThreads <- for ships $ \(r, o) -> do
    async (runShipRestarting waitForKillRequ r o multi)

  {-
    Since `spin` never returns, this will run until the main
    thread is killed with an async exception.  The one we expect is
    `UserInterrupt` which will be raised on this thread upon SIGKILL
    or SIGTERM.

    Once that happens, we write to `killSignal` which will cause
    all ships to be shut down, and then we `wait` for them to finish
    before returning.
  -}
  let spin = forever (threadDelay maxBound)
  finally spin $ do
    logTrace "KING IS GOING DOWN"
    atomically (putTMVar killSignal ())
    for_ shipThreads waitCatch


--------------------------------------------------------------------------------

connTerm :: ∀e. HasLogFunc e => FilePath -> RIO e ()
connTerm = Term.runTerminalClient

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
