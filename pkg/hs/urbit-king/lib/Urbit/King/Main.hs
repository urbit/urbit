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
import Urbit.Vere.Ports
import Urbit.Vere.Eyre.Multi (multiEyre, MultiEyreConf(..))
import Urbit.Vere.Pier.Types
import Urbit.Vere.Serf
import Urbit.King.App

import Control.Concurrent     (myThreadId)
import Control.Exception      (AsyncException(UserInterrupt))
import System.Process         (system)
import System.IO              (hPutStrLn)
import Urbit.Noun.Time        (Wen)
import Urbit.Vere.LockFile    (lockFile)

import qualified Data.Set                as Set
import qualified Data.Text               as T
import qualified Network.HTTP.Client     as C
import qualified System.Posix.Signals    as Sys
import qualified System.Posix.Resource   as Sys
import qualified System.ProgressBar      as PB
import qualified System.Random           as Sys
import qualified Urbit.EventLog.LMDB     as Log
import qualified Urbit.King.CLI          as CLI
import qualified Urbit.King.EventBrowser as EventBrowser
import qualified Urbit.Ob                as Ob
import qualified Urbit.Vere.Pier         as Pier
import qualified Urbit.Vere.Serf         as Serf
import qualified Urbit.Vere.Term         as Term


--------------------------------------------------------------------------------

removeFileIfExists :: HasLogFunc env => FilePath -> RIO env ()
removeFileIfExists pax = do
  exists <- doesFileExist pax
  when exists $ do
      removeFile pax


-- Compile CLI Flags to Pier Configuration -------------------------------------

{-
  TODO: This is not all of the flags.
  Urbit is basically useless with hashboard, so we ignore that flag.
-}
toSerfFlags :: CLI.Opts -> [Serf.Flag]
toSerfFlags CLI.Opts{..} = catMaybes m
  where
    m = [ setFrom oQuiet Serf.Quiet
        , setFrom oTrace Serf.Trace
        , setFrom (oHashless || True) Serf.Hashless
        , setFrom oQuiet Serf.Quiet
        , setFrom oVerbose Serf.Verbose
        , setFrom (oDryRun || isJust oDryFrom) Serf.DryRun
        ]
    setFrom True flag = Just flag
    setFrom False _   = Nothing

toPierConfig :: FilePath -> Maybe Text -> CLI.Opts -> PierConfig
toPierConfig pierPath serfExe o@(CLI.Opts{..}) = PierConfig { .. }
 where
  _pcPierPath  = pierPath
  _pcDryRun    = oDryRun || isJust oDryFrom
  _pcSerfExe   = serfExe
  _pcSerfFlags = toSerfFlags o

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

logStderr :: HasStderrLogFunc e => RIO LogFunc a -> RIO e a
logStderr action = do
  logFunc <- view stderrLogFuncL
  runRIO logFunc action

logSlogs :: HasStderrLogFunc e => RIO e (TVar ((Atom, Tank) -> IO ()))
logSlogs = logStderr $ do
  env <- ask
  newTVarIO (runRIO env . logOther "serf" . display . T.strip . tankToText . snd)

tryBootFromPill
  :: Bool
  -> Pill
  -> Bool
  -> Ship
  -> LegacyBootEvent
  -> Feed
  -> RIO PierEnv ()
tryBootFromPill oExit pill lite ship boot feed = do
  mStart <- newEmptyMVar
  vSlog  <- logSlogs
  runOrExitImmediately vSlog (bootedPier vSlog) oExit mStart []
 where
  bootedPier vSlog = do
    view pierPathL >>= lockFile
    rio $ logInfo "Starting boot"
    sls <- Pier.booted vSlog pill lite ship boot feed
    rio $ logInfo "Completed boot"
    pure sls

runOrExitImmediately
  :: TVar ((Atom, Tank) -> IO ())
  -> RAcquire PierEnv (Serf, Log.EventLog)
  -> Bool
  -> MVar ()
  -> [Ev]
  -> RIO PierEnv ()
runOrExitImmediately vSlog getPier oExit mStart injected = do
  rwith getPier (if oExit then shutdownImmediately else runPier)
 where
  shutdownImmediately :: (Serf, Log.EventLog) -> RIO PierEnv ()
  shutdownImmediately (serf, log) = do
    logInfo "Sending shutdown signal"
    Serf.stop serf
    logInfo "Shutdown!"

  runPier :: (Serf, Log.EventLog) -> RIO PierEnv ()
  runPier serfLog = do
    runRAcquire (Pier.pier serfLog vSlog mStart injected)

tryPlayShip
  :: Bool
  -> Bool
  -> Maybe Word64
  -> MVar ()
  -> [Ev]
  -> RIO PierEnv ()
tryPlayShip exitImmediately fullReplay playFrom mStart injected = do
  when fullReplay wipeSnapshot
  vSlog <- logSlogs
  runOrExitImmediately vSlog (resumeShip vSlog) exitImmediately mStart injected
 where
  wipeSnapshot = do
    shipPath <- view pierPathL
    logInfo "wipeSnapshot"
    logInfo $ display $ pack @Text ("Wiping " <> north shipPath)
    logInfo $ display $ pack @Text ("Wiping " <> south shipPath)
    removeFileIfExists (north shipPath)
    removeFileIfExists (south shipPath)

  north shipPath = shipPath <> "/.urb/chk/north.bin"
  south shipPath = shipPath <> "/.urb/chk/south.bin"

  resumeShip :: TVar ((Atom, Tank) -> IO ())
             -> RAcquire PierEnv (Serf, Log.EventLog)
  resumeShip vSlog = do
    view pierPathL >>= lockFile
    rio $ logInfo "RESUMING SHIP"
    sls <- Pier.resumed vSlog playFrom
    rio $ logInfo "SHIP RESUMED"
    pure sls

runRAcquire :: (MonadUnliftIO (m e),  MonadIO (m e), MonadReader e (m e))
            => RAcquire e a -> m e a
runRAcquire act = rwith act pure


--------------------------------------------------------------------------------

checkEvs :: FilePath -> Word64 -> Word64 -> RIO KingEnv ()
checkEvs pierPath first last = do
  rwith (Log.existing logPath) $ \log -> do
    let ident = Log.identity log
    let pbSty = PB.defStyle { PB.stylePostfix = PB.exact }
    logInfo (displayShow ident)

    last <- atomically $ Log.lastEv log <&> \lastReal -> min last lastReal

    let evCount = fromIntegral (last - first)

    pb <- PB.newProgressBar pbSty 10 (PB.Progress 1 evCount ())

    runConduit $ Log.streamEvents log first .| showEvents
      pb
      first
      (fromIntegral $ lifecycleLen ident)
 where
  logPath :: FilePath
  logPath = pierPath <> "/.urb/log"

  showEvents
    :: PB.ProgressBar ()
    -> EventId
    -> EventId
    -> ConduitT ByteString Void (RIO KingEnv) ()
  showEvents pb eId _ | eId > last = pure ()
  showEvents pb eId cycle          = await >>= \case
    Nothing -> do
      lift $ PB.killProgressBar pb
      lift $ logInfo "Everything checks out."
    Just bs -> do
      lift $ PB.incProgress pb 1
      lift $ do
        n <- io $ cueBSExn bs
        when (eId > cycle) $ do
          (mug, wen, evNoun) <- unpackJob n
          fromNounErr evNoun & \case
            Left  err       -> logError (displayShow (eId, err))
            Right (_ :: Ev) -> pure ()
      showEvents pb (succ eId) cycle

  unpackJob :: Noun -> RIO KingEnv (Mug, Wen, Noun)
  unpackJob = io . fromNounExn


--------------------------------------------------------------------------------

collectAllFx :: FilePath -> RIO KingEnv ()
collectAllFx = error "TODO"

{-
{-|
    This runs the serf at `$top/.tmpdir`, but we disable snapshots,
    so this should never actually be created. We just do this to avoid
    letting the serf use an existing snapshot.
-}
collectAllFx :: FilePath -> RIO KingEnv ()
collectAllFx top = do
    logInfo $ display $ pack @Text top
    vSlog <- logSlogs
    rwith (collectedFX vSlog) $ \() ->
        logInfo "Done collecting effects!"
  where
    tmpDir :: FilePath
    tmpDir = top </> ".tmpdir"

    collectedFX :: TVar (Text -> IO ()) -> RAcquire KingEnv ()
    collectedFX vSlog = do
        lockFile top
        log  <- Log.existing (top <> "/.urb/log")
        serf <- Pier.runSerf vSlog tmpDir serfFlags
        rio $ Serf.collectFX serf log

    serfFlags :: [Serf.Flag]
    serfFlags = [Serf.Hashless, Serf.DryRun]
-}


--------------------------------------------------------------------------------

replayPartEvs :: FilePath -> Word64 -> RIO KingEnv ()
replayPartEvs top last = do
    logInfo $ display $ pack @Text top
    fetchSnapshot
    rwith replayedEvs $ \() ->
        logInfo "Done replaying events!"
  where
    fetchSnapshot :: RIO KingEnv ()
    fetchSnapshot = do
      snap <- Pier.getSnapshot top last
      case snap of
        Nothing -> pure ()
        Just sn -> do
          liftIO $ system $ "cp -r \"" <> sn <> "\" \"" <> tmpDir <> "\""
          pure ()

    tmpDir :: FilePath
    tmpDir = top </> ".partial-replay" </> show last

    replayedEvs :: RAcquire KingEnv ()
    replayedEvs = do
        lockFile top
        log  <- Log.existing (top <> "/.urb/log")
        let onSlog = print
        let onStdr = print
        let onDead = error "DIED"
        let config = Serf.Config "urbit-worker" tmpDir serfFlags onSlog onStdr onDead
        (serf, info) <- io (Serf.start config)
        rio $ do
          eSs <- Serf.execReplay serf log (Just last)
          case eSs of
            Left bail -> error (show bail)
            Right 0   -> io (Serf.snapshot serf)
            Right num -> pure ()
          io $ threadDelay 500000 -- Copied from runOrExitImmediately
          pure ()

    serfFlags :: [Serf.Flag]
    serfFlags = [Serf.Hashless]


--------------------------------------------------------------------------------

{-|
    Interesting
-}
testPill :: HasKingEnv e => FilePath -> Bool -> Bool -> RIO e ()
testPill pax showPil showSeq = do
  logInfo "Reading pill file."
  pillBytes <- readFile pax

  logInfo "Cueing pill file."
  pillNoun <- io $ cueBS pillBytes & either throwIO pure

  logInfo "Parsing pill file."
  pill <- fromNounErr pillNoun & either (throwIO . uncurry ParseErr) pure

  logInfo "Using pill to generate boot sequence."
  bootSeq <- genBootSeq
               (Ship 0)
               pill
               False
               (Fake (Ship 0))
               (Feed1 $ Germs (Ship 0) [])

  logInfo "Validate jam/cue and toNoun/fromNoun on pill value"
  reJam <- validateNounVal pill

  logInfo "Checking if round-trip matches input file:"
  unless (reJam == pillBytes) $ do
    logInfo "    Our jam does not match the file...\n"
    logInfo "    This is surprising, but it is probably okay."

  when showPil $ do
      logInfo "\n\n== Pill ==\n"
      io $ pPrint pill

  when showSeq $ do
      logInfo "\n\n== Boot Sequence ==\n"
      io $ pPrint bootSeq

validateNounVal :: (HasLogFunc e, Eq a, ToNoun a, FromNoun a)
                => a -> RIO e ByteString
validateNounVal inpVal = do
    logInfo "  jam"
    inpByt <- evaluate $ jamBS $ toNoun inpVal

    logInfo "  cue"
    outNon <- cueBS inpByt & either throwIO pure

    logInfo "  fromNoun"
    outVal <- fromNounErr outNon & either (throwIO . uncurry ParseErr) pure

    logInfo "  toNoun"
    outNon <- evaluate (toNoun outVal)

    logInfo "  jam"
    outByt <- evaluate $ jamBS outNon

    logInfo "Checking if: x == cue (jam x)"
    unless (inpVal == outVal) $
        error "Value fails test: x == cue (jam x)"

    logInfo "Checking if: jam x == jam (cue (jam x))"
    unless (inpByt == outByt) $
        error "Value fails test: jam x == jam (cue (jam x))"

    pure outByt


--------------------------------------------------------------------------------

pillFrom :: CLI.PillSource -> RIO HostEnv Pill
pillFrom = \case
  CLI.PillSourceFile pillPath -> do
    logInfo $ display $ "boot: reading pill from " ++ (pack pillPath :: Text)
    io (loadFile pillPath >>= either throwIO pure)

  CLI.PillSourceURL url -> do
    logInfo $ display $ "boot: retrieving pill from " ++ (pack url :: Text)
    -- Get the jamfile with the list of stars accepting comets right now.
    manager <- io $ C.newManager tlsManagerSettings
    request <- io $ C.parseRequest url
    response <- io $ C.httpLbs (C.setRequestCheckStatus request) manager
    let body = toStrict $ C.responseBody response

    noun <- cueBS body & either throwIO pure
    fromNounErr noun & either (throwIO . uncurry ParseErr) pure

multiOnFatal :: HasKingEnv e => e -> IO ()
multiOnFatal env = runRIO env $ do
  (view stderrLogFuncL >>=) $ flip runRIO $ logError
    ("Urbit is shutting down because of a problem with the HTTP server.\n"
    <> "Please restart it at your leisure.")
  view killKingActionL >>= atomically

newShip :: CLI.New -> CLI.Opts -> RIO KingEnv ()
newShip CLI.New{..} opts = do
  {-
    TODO XXX HACK

    Because the "new ship" flow *may* automatically start the ship,
    we need to create this, but it's not actually correct.

    The right solution is to separate out the "new ship" flow from the
    "run ship" flow, and possibly sequence them from the outside if
    that's really needed.
  -}
  env <- ask
  multi <- multiEyre (multiOnFatal env) (MultiEyreConf Nothing Nothing True)

  -- TODO: We hit the same problem as above: we need a host env to boot a ship
  -- because it may autostart the ship, so build an inactive port configuration.
  let ports = buildInactivePorts

  -- here we are with a king env, and we now need a multi env.
  runHostEnv multi ports $ case nBootType of
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
      putStrLn ("code: " ++ (tshow $ deriveCode $ sRing seed))
      bootFromSeed pill $ Feed0 seed

    CLI.BootFake name -> do
      pill <- pillFrom nPillSource
      ship <- shipFrom name
      runTryBootFromPill pill name ship (Fake ship) (Feed1 $ Germs ship [])

    CLI.BootFromKeyfile keyFile -> do
      text <- readFileUtf8 keyFile
      asAtom <- case cordToUW (Cord $ T.strip text) of
        Nothing -> error "Couldn't parse keyfile. Hint: keyfiles start with 0w?"
        Just (UW a) -> pure a

      asNoun <- cueExn asAtom
      feed :: Feed <- case fromNoun asNoun of
        Nothing -> error "Keyfile does not seem to contain a seed."
        Just s  -> pure s

      pill <- pillFrom nPillSource

      bootFromSeed pill feed

  where
    shipFrom :: Text -> RIO HostEnv Ship
    shipFrom name = case Ob.parsePatp name of
      Left x  -> error "Invalid ship name"
      Right p -> pure $ Ship $ fromIntegral $ Ob.fromPatp p

    pierPath :: Text -> FilePath
    pierPath name = case nPierPath of
      Just x  -> x
      Nothing -> "./" <> unpack name

    nameFromShip :: HasKingEnv e => Ship -> RIO e Text
    nameFromShip s = name
      where
        nameWithSig = Ob.renderPatp $ Ob.patp $ fromIntegral s
        name = case stripPrefix "~" nameWithSig of
          Nothing -> error "Urbit.ob didn't produce string with ~"
          Just x  -> pure x

    bootFromSeed :: Pill -> Feed -> RIO HostEnv ()
    bootFromSeed pill feed = do
      ethReturn <- dawnVent nEthNode feed

      case ethReturn of
        Left x -> error $ unpack x
        Right dawn -> do
          let ship = sShip $ dSeed dawn
          name <- nameFromShip ship
          runTryBootFromPill pill name ship (Dawn dawn) feed

    -- Now that we have all the information for running an application with a
    -- PierConfig, do so.
    runTryBootFromPill :: Pill
                       -> Text
                       -> Ship
                       -> LegacyBootEvent
                       -> Feed
                       -> RIO HostEnv ()
    runTryBootFromPill pill name ship bootEvent feed = do
      vKill <- view (kingEnvL . kingEnvKillSignal)
      let pierConfig = toPierConfig (pierPath name) nSerfExe opts
      let networkConfig = toNetworkConfig opts
      runPierEnv pierConfig networkConfig vKill $
        tryBootFromPill True pill nLite ship bootEvent feed

runShipEnv :: Maybe Text -> CLI.Run -> CLI.Opts -> TMVar () -> RIO PierEnv a
           -> RIO HostEnv a
runShipEnv serfExe (CLI.Run pierPath) opts vKill act = do
  runPierEnv pierConfig netConfig vKill act
 where
  pierConfig = toPierConfig pierPath serfExe opts
  netConfig = toNetworkConfig opts

runShip
  :: CLI.Run -> CLI.Opts -> Bool -> RIO PierEnv ()
runShip (CLI.Run pierPath) opts daemon = do
    mStart  <- newEmptyMVar
    if daemon
    then runPier mStart
    else do
      -- Wait until the pier has started up, then connect a terminal. If
      -- the terminal ever shuts down, ask the ship to go down.
      connectionThread <- async $ do
        readMVar mStart
        finally (connTerm pierPath) $ do
          view killPierActionL >>= atomically

      -- Run the pier until it finishes, and then kill the terminal.
      finally (runPier mStart) $ do
        cancel connectionThread
  where
    runPier :: MVar () -> RIO PierEnv ()
    runPier mStart = do
      injections <- loadInjections (CLI.oInjectEvents opts)
      tryPlayShip
        (CLI.oExit opts)
        (CLI.oFullReplay opts)
        (CLI.oDryFrom opts)
        mStart
        injections

    loadInjections :: [CLI.Injection] -> RIO PierEnv [Ev]
    loadInjections injections = do
      perInjection :: [[Ev]] <- for injections $ \case
          CLI.InjectOneEvent filePath -> do
            logInfo $ display $ "boot: reading injected event from " ++
              (pack filePath :: Text)
            io (loadFile filePath >>= either throwIO (pure . singleton))

          CLI.InjectManyEvents filePath -> do
            logInfo $ display $ "boot: reading injected event list from " ++
              (pack filePath :: Text)
            io (loadFile filePath >>= either throwIO pure)
      pure $ concat perInjection



buildPortHandler :: HasLogFunc e => CLI.Nat -> RIO e PortControlApi
buildPortHandler CLI.NatNever  = pure buildInactivePorts
-- TODO: Figure out what to do about logging here. The "port: " messages are
-- the sort of thing that should be put on the muxed terminal log, but we don't
-- have that at this layer.
buildPortHandler CLI.NatAlways = buildNatPorts (io . hPutStrLn stderr . unpack)
buildPortHandler CLI.NatWhenPrivateNetwork =
  buildNatPortsWhenPrivate (io . hPutStrLn stderr . unpack)

startBrowser :: HasLogFunc e => FilePath -> RIO e ()
startBrowser pierPath = runRAcquire $ do
    -- lockFile pierPath
    log <- Log.existing (pierPath <> "/.urb/log")
    rio $ EventBrowser.run log

checkDawn :: HasLogFunc e => String -> FilePath -> RIO e ()
checkDawn provider keyfilePath = do
  -- The keyfile is a jammed Seed then rendered in UW format
  text <- readFileUtf8 keyfilePath
  asAtom <- case cordToUW (Cord $ T.strip text) of
    Nothing -> error "Couldn't parse keyfile. Hint: keyfiles start with 0w?"
    Just (UW a) -> pure a

  asNoun <- cueExn asAtom
  feed :: Feed <- case fromNoun asNoun of
    Nothing -> error "Keyfile does not seem to contain a seed."
    Just s  -> pure s

  print $ show feed

  e <- dawnVent provider feed
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
  (args, log) <- CLI.parseArgs

  hSetBuffering stdout NoBuffering
  setupSignalHandlers
  setRLimits

  runKingEnv args log $ case args of
    CLI.CmdRun ko ships                       -> runShips ko ships
    CLI.CmdNew n  o                           -> newShip n o
    CLI.CmdBug (CLI.CollectAllFX pax        ) -> collectAllFx pax
    CLI.CmdBug (CLI.EventBrowser pax        ) -> startBrowser pax
    CLI.CmdBug (CLI.ValidatePill   pax pil s) -> testPill pax pil s
    CLI.CmdBug (CLI.ValidateEvents pax f   l) -> checkEvs pax f l
    CLI.CmdBug (CLI.ValidateFX     pax f   l) -> checkFx pax f l
    CLI.CmdBug (CLI.ReplayEvents pax l      ) -> replayPartEvs pax l
    CLI.CmdBug (CLI.CheckDawn provider pax  ) -> checkDawn provider pax
    CLI.CmdBug CLI.CheckComet                 -> checkComet
    CLI.CmdCon pier                           -> connTerm pier

 where
  runKingEnv args log =
    let
      verb = verboseLogging args
      runStderr = case args of
        CLI.CmdRun {} -> runKingEnvStderrRaw
        _             -> runKingEnvStderr
      CLI.Log {..} = log
    in case logTarget lTarget args of
       CLI.LogFile f -> runKingEnvLogFile verb lLevel f
       CLI.LogStderr -> runStderr         verb lLevel
       CLI.LogOff    -> runKingEnvNoLog

  setupSignalHandlers = do
    mainTid <- myThreadId
    let onKillSig = throwTo mainTid UserInterrupt
    for_ [Sys.sigTERM, Sys.sigINT] $ \sig -> do
      Sys.installHandler sig (Sys.Catch onKillSig) Nothing

  setRLimits = do
    openFiles <- Sys.getResourceLimit Sys.ResourceOpenFiles
    let soft = case Sys.hardLimit openFiles of
          Sys.ResourceLimit lim     -> Sys.ResourceLimit lim
          Sys.ResourceLimitInfinity -> Sys.ResourceLimit 10240  -- macOS
          Sys.ResourceLimitUnknown  -> Sys.ResourceLimit 10240
    Sys.setResourceLimit Sys.ResourceOpenFiles
      openFiles { Sys.softLimit = soft }

  verboseLogging :: CLI.Cmd -> Bool
  verboseLogging = \case
    CLI.CmdRun ko ships -> any CLI.oVerbose (ships <&> \(_, o, _) -> o)
    _                   -> False

  -- If the user hasn't specified where to log, what we do depends on what
  -- command she has issued. Notably, the LogFile Nothing outcome means that
  -- runKingEnvLogFile should run an IO action to get the official app data
  -- directory and open a canonically named log file there.
  logTarget :: Maybe (CLI.LogTarget FilePath)
            -> CLI.Cmd
            -> CLI.LogTarget (Maybe FilePath)
  logTarget = \case
    Just (CLI.LogFile f) -> const $ CLI.LogFile (Just f)
    Just CLI.LogStderr   -> const $ CLI.LogStderr
    Just CLI.LogOff      -> const $ CLI.LogOff
    Nothing              -> \case
      CLI.CmdCon _                             -> CLI.LogFile Nothing
      CLI.CmdRun ko [(_,_,daemon)] | daemon    -> CLI.LogStderr
                                   | otherwise -> CLI.LogFile Nothing
      CLI.CmdRun ko _                          -> CLI.LogStderr
      _                                        -> CLI.LogStderr

{-
  Runs a ship but restarts it if it crashes or shuts down on it's own.

  Once `waitForKillRequ` returns, the ship will be terminated and this
  routine will exit.
-}
runShipRestarting
  :: Maybe Text -> CLI.Run -> CLI.Opts -> RIO HostEnv ()
runShipRestarting serfExe r o = do
  let pier = pack (CLI.rPierPath r)
      loop = runShipRestarting serfExe r o

  onKill    <- view onKillKingSigL
  vKillPier <- newEmptyTMVarIO

  tid <- asyncBound $ runShipEnv serfExe r o vKillPier $ runShip r o True

  let onShipExit = Left <$> waitCatchSTM tid
      onKillRequ = Right <$> onKill

  atomically (onShipExit <|> onKillRequ) >>= \case
    Left exit -> do
      case exit of
        Left err -> logError $ display (tshow err <> ": " <> pier)
        Right () ->
          logError $ display ("Ship exited on it's own. Why? " <> pier)
      threadDelay 250_000
      loop
    Right () -> do
      logTrace $ display (pier <> " shutdown requested")
      atomically $ putTMVar vKillPier ()
      race_ (wait tid) $ do
        threadDelay 5_000_000
        logInfo $ display (pier <> " not down after 5s, killing with fire.")
        cancel tid
      logTrace $ display ("Ship terminated: " <> pier)

{-
  TODO This is messy and shared a lot of logic with `runShipRestarting`.
-}
runShipNoRestart
  :: Maybe Text -> CLI.Run -> CLI.Opts -> Bool -> RIO HostEnv ()
runShipNoRestart serfExe r o d = do
  -- killing ship same as killing king
  vKill  <- view (kingEnvL . kingEnvKillSignal)
  tid    <- asyncBound (runShipEnv serfExe r o vKill $ runShip r o d)
  onKill <- view onKillKingSigL

  let pier = pack (CLI.rPierPath r)

  let onShipExit = Left <$> waitCatchSTM tid
      onKillRequ = Right <$> onKill

  atomically (onShipExit <|> onKillRequ) >>= \case
    Left (Left err) -> do
      logError $ display (tshow err <> ": " <> pier)
    Left (Right ()) -> do
      logError $ display (pier <> " exited on it's own. Why?")
    Right () -> do
      logTrace $ display (pier <> " shutdown requested")
      race_ (wait tid) $ do
        threadDelay 5_000_000
        logTrace $ display (pier <> " not down after 5s, killing with fire.")
        cancel tid
      logTrace $ display (pier <> " terminated.")

runShips :: CLI.Host -> [(CLI.Run, CLI.Opts, Bool)] -> RIO KingEnv ()
runShips CLI.Host {..} ships = do
  let meConf = MultiEyreConf
        { mecHttpPort      = fromIntegral <$> hSharedHttpPort
        , mecHttpsPort     = fromIntegral <$> hSharedHttpsPort
        , mecLocalhostOnly = False -- TODO Localhost-only needs to be
                                   -- a king-wide option.
        }

  env <- ask
  multi <- multiEyre (multiOnFatal env) meConf

  ports <- buildPortHandler hUseNatPmp

  runHostEnv multi ports (go ships)
 where
  go :: [(CLI.Run, CLI.Opts, Bool)] ->  RIO HostEnv ()
  go = \case
    []    -> pure ()
    [rod] -> runSingleShip hSerfExe rod
    ships -> runMultipleShips hSerfExe (ships <&> \(r, o, _) -> (r, o))


-- TODO Duplicated logic.
runSingleShip :: Maybe Text -> (CLI.Run, CLI.Opts, Bool) -> RIO HostEnv ()
runSingleShip serfExe (r, o, d) = do
  shipThread <- async (runShipNoRestart serfExe r o d)

  {-
    Wait for the ship to go down.

    Since `waitCatch` will never throw an exception, the `onException`
    block will only happen if this thread is killed with an async
    exception.  The one we expect is `UserInterrupt` which will be raised
    on this thread upon SIGKILL or SIGTERM.

    If this thread is killed, we first ask the ship to go down, wait
    for the ship to actually go down, and then go down ourselves.
  -}
  onException (void $ waitCatch shipThread) $ do
    logTrace "KING IS GOING DOWN"
    atomically =<< view killKingActionL
    waitCatch shipThread
    pure ()


runMultipleShips :: Maybe Text -> [(CLI.Run, CLI.Opts)] -> RIO HostEnv ()
runMultipleShips serfExe ships = do
  shipThreads <- for ships $ \(r, o) -> do
    async (runShipRestarting serfExe r o)

  {-
    Since `spin` never returns, this will run until the main
    thread is killed with an async exception.  The one we expect is
    `UserInterrupt` which will be raised on this thread upon SIGKILL
    or SIGTERM.

    Once that happens, we send a shutdown signal which will cause all
    ships to be shut down, and then we `wait` for them to finish before
    returning.

    This is different than the single-ship flow, because ships never
    go down on their own in this flow. If they go down, they just bring
    themselves back up.
  -}
  let spin = forever (threadDelay maxBound)
  finally spin $ do
    logTrace "KING IS GOING DOWN"
    view killKingActionL >>= atomically
    for_ shipThreads waitCatch


--------------------------------------------------------------------------------

connTerm :: forall e. HasLogFunc e => FilePath -> RIO e ()
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
