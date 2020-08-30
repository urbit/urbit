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
import Urbit.King.App

import Control.Concurrent     (myThreadId)
import Control.Exception      (AsyncException(UserInterrupt))
import Control.Lens           ((&))
import System.Process         (system)
import Text.Show.Pretty       (pPrint)
import Urbit.Noun.Conversions (cordToUW)
import Urbit.Noun.Time        (Wen)
import Urbit.Vere.LockFile    (lockFile)

import qualified Data.Set                as Set
import qualified Data.Text               as T
import qualified Network.HTTP.Client     as C
import qualified System.Posix.Signals    as Sys
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

toPierConfig :: FilePath -> CLI.Opts -> PierConfig
toPierConfig pierPath o@(CLI.Opts{..}) = PierConfig { .. }
 where
  _pcPierPath  = pierPath
  _pcDryRun    = oDryRun || isJust oDryFrom
  _pcSerfExe   = fromMaybe "urbit-worker" oSerfExe
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

logSlogs :: HasStderrLogFunc e => RIO e (TVar (Text -> IO ()))
logSlogs = logStderr $ do
  env <- ask
  newTVarIO (runRIO env . logOther "serf" . display . T.strip)

tryBootFromPill
  :: Bool
  -> Pill
  -> Bool
  -> Ship
  -> LegacyBootEvent
  -> MultiEyreApi
  -> RIO PierEnv ()
tryBootFromPill oExit pill lite ship boot multi = do
  mStart <- newEmptyMVar
  vSlog  <- logSlogs
  runOrExitImmediately vSlog (bootedPier vSlog) oExit mStart multi
 where
  bootedPier vSlog = do
    view pierPathL >>= lockFile
    rio $ logDebug "Starting boot"
    sls <- Pier.booted vSlog pill lite ship boot
    rio $ logDebug "Completed boot"
    pure sls

runOrExitImmediately
  :: TVar (Text -> IO ())
  -> RAcquire PierEnv (Serf, Log.EventLog)
  -> Bool
  -> MVar ()
  -> MultiEyreApi
  -> RIO PierEnv ()
runOrExitImmediately vSlog getPier oExit mStart multi = do
  rwith getPier (if oExit then shutdownImmediately else runPier)
 where
  shutdownImmediately :: (Serf, Log.EventLog) -> RIO PierEnv ()
  shutdownImmediately (serf, log) = do
    logDebug "Sending shutdown signal"
    Serf.stop serf
    logDebug "Shutdown!"

  runPier :: (Serf, Log.EventLog) -> RIO PierEnv ()
  runPier serfLog = do
    runRAcquire (Pier.pier serfLog vSlog mStart multi)

tryPlayShip
  :: Bool
  -> Bool
  -> Maybe Word64
  -> MVar ()
  -> MultiEyreApi
  -> RIO PierEnv ()
tryPlayShip exitImmediately fullReplay playFrom mStart multi = do
  when fullReplay wipeSnapshot
  vSlog <- logSlogs
  runOrExitImmediately vSlog (resumeShip vSlog) exitImmediately mStart multi
 where
  wipeSnapshot = do
    shipPath <- view pierPathL
    logDebug "wipeSnapshot"
    logDebug $ display $ pack @Text ("Wiping " <> north shipPath)
    logDebug $ display $ pack @Text ("Wiping " <> south shipPath)
    removeFileIfExists (north shipPath)
    removeFileIfExists (south shipPath)

  north shipPath = shipPath <> "/.urb/chk/north.bin"
  south shipPath = shipPath <> "/.urb/chk/south.bin"

  resumeShip :: TVar (Text -> IO ()) -> RAcquire PierEnv (Serf, Log.EventLog)
  resumeShip vSlog = do
    view pierPathL >>= lockFile
    rio $ logDebug "RESUMING SHIP"
    sls <- Pier.resumed vSlog playFrom
    rio $ logDebug "SHIP RESUMED"
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
    logDebug (displayShow ident)

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
      lift $ logDebug "Everything checks out."
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
    logDebug $ display $ pack @Text top
    vSlog <- logSlogs
    rwith (collectedFX vSlog) $ \() ->
        logDebug "Done collecting effects!"
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
    logDebug $ display $ pack @Text top
    fetchSnapshot
    rwith replayedEvs $ \() ->
        logDebug "Done replaying events!"
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
testPill :: HasLogFunc e => FilePath -> Bool -> Bool -> RIO e ()
testPill pax showPil showSeq = do
  logDebug "Reading pill file."
  pillBytes <- readFile pax

  logDebug "Cueing pill file."
  pillNoun <- io $ cueBS pillBytes & either throwIO pure

  logDebug "Parsing pill file."
  pill <- fromNounErr pillNoun & either (throwIO . uncurry ParseErr) pure

  logDebug "Using pill to generate boot sequence."
  bootSeq <- genBootSeq (Ship 0) pill False (Fake (Ship 0))

  logDebug "Validate jam/cue and toNoun/fromNoun on pill value"
  reJam <- validateNounVal pill

  logDebug "Checking if round-trip matches input file:"
  unless (reJam == pillBytes) $ do
    logDebug "    Our jam does not match the file...\n"
    logDebug "    This is surprising, but it is probably okay."

  when showPil $ do
      logDebug "\n\n== Pill ==\n"
      io $ pPrint pill

  when showSeq $ do
      logDebug "\n\n== Boot Sequence ==\n"
      io $ pPrint bootSeq

validateNounVal :: (HasLogFunc e, Eq a, ToNoun a, FromNoun a)
                => a -> RIO e ByteString
validateNounVal inpVal = do
    logDebug "  jam"
    inpByt <- evaluate $ jamBS $ toNoun inpVal

    logDebug "  cue"
    outNon <- cueBS inpByt & either throwIO pure

    logDebug "  fromNoun"
    outVal <- fromNounErr outNon & either (throwIO . uncurry ParseErr) pure

    logDebug "  toNoun"
    outNon <- evaluate (toNoun outVal)

    logDebug "  jam"
    outByt <- evaluate $ jamBS outNon

    logDebug "Checking if: x == cue (jam x)"
    unless (inpVal == outVal) $
        error "Value fails test: x == cue (jam x)"

    logDebug "Checking if: jam x == jam (cue (jam x))"
    unless (inpByt == outByt) $
        error "Value fails test: jam x == jam (cue (jam x))"

    pure outByt


--------------------------------------------------------------------------------

pillFrom :: CLI.PillSource -> RIO KingEnv Pill
pillFrom = \case
  CLI.PillSourceFile pillPath -> do
    logDebug $ display $ "boot: reading pill from " ++ (pack pillPath :: Text)
    io (loadFile pillPath >>= either throwIO pure)

  CLI.PillSourceURL url -> do
    logDebug $ display $ "boot: retrieving pill from " ++ (pack url :: Text)
    -- Get the jamfile with the list of stars accepting comets right now.
    manager <- io $ C.newManager tlsManagerSettings
    request <- io $ C.parseRequest url
    response <- io $ C.httpLbs (C.setRequestCheckStatus request) manager
    let body = toStrict $ C.responseBody response

    noun <- cueBS body & either throwIO pure
    fromNounErr noun & either (throwIO . uncurry ParseErr) pure

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
    shipFrom :: Text -> RIO KingEnv Ship
    shipFrom name = case Ob.parsePatp name of
      Left x  -> error "Invalid ship name"
      Right p -> pure $ Ship $ fromIntegral $ Ob.fromPatp p

    pierPath :: Text -> FilePath
    pierPath name = case nPierPath of
      Just x  -> x
      Nothing -> "./" <> unpack name

    nameFromShip :: Ship -> RIO KingEnv Text
    nameFromShip s = name
      where
        nameWithSig = Ob.renderPatp $ Ob.patp $ fromIntegral s
        name = case stripPrefix "~" nameWithSig of
          Nothing -> error "Urbit.ob didn't produce string with ~"
          Just x  -> pure x

    bootFromSeed :: MultiEyreApi -> Pill -> Seed -> RIO KingEnv ()
    bootFromSeed multi pill seed = do
      ethReturn <- dawnVent seed

      case ethReturn of
        Left x -> error $ unpack x
        Right dawn -> do
          let ship = sShip $ dSeed dawn
          name <- nameFromShip ship
          runTryBootFromPill multi pill name ship (Dawn dawn)

    -- Now that we have all the information for running an application with a
    -- PierConfig, do so.
    runTryBootFromPill multi pill name ship bootEvent = do
      vKill <- view kingEnvKillSignal
      let pierConfig = toPierConfig (pierPath name) opts
      let networkConfig = toNetworkConfig opts
      runPierEnv pierConfig networkConfig vKill $
        tryBootFromPill True pill nLite ship bootEvent multi
------  tryBootFromPill (CLI.oExit opts) pill nLite flags ship bootEvent

runShipEnv :: CLI.Run -> CLI.Opts -> TMVar () -> RIO PierEnv a -> RIO KingEnv a
runShipEnv (CLI.Run pierPath) opts vKill act = do
  runPierEnv pierConfig netConfig vKill act
 where
  pierConfig = toPierConfig pierPath opts
  netConfig = toNetworkConfig opts

runShip
  :: CLI.Run -> CLI.Opts -> Bool -> MultiEyreApi -> RIO PierEnv ()
runShip (CLI.Run pierPath) opts daemon multi = do
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
      tryPlayShip
        (CLI.oExit opts)
        (CLI.oFullReplay opts)
        (CLI.oDryFrom opts)
        mStart
        multi


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

  runKingEnv args $ case args of
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
  runKingEnv args =
    let verb = verboseLogging args
    in if willRunTerminal args
       then runKingEnvLogFile verb
       else runKingEnvStderr verb

  setupSignalHandlers = do
    mainTid <- myThreadId
    let onKillSig = throwTo mainTid UserInterrupt
    for_ [Sys.sigTERM, Sys.sigINT] $ \sig -> do
      Sys.installHandler sig (Sys.Catch onKillSig) Nothing

  verboseLogging :: CLI.Cmd -> Bool
  verboseLogging = \case
    CLI.CmdRun ko ships -> any CLI.oVerbose (ships <&> \(_, o, _) -> o)
    _                   -> False

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
runShipRestarting
  :: CLI.Run -> CLI.Opts -> MultiEyreApi -> RIO KingEnv ()
runShipRestarting r o multi = do
  let pier = pack (CLI.rPierPath r)
      loop = runShipRestarting r o multi

  onKill    <- view onKillKingSigL
  vKillPier <- newEmptyTMVarIO

  tid <- asyncBound $ runShipEnv r o vKillPier $ runShip r o True multi

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
      race_ (wait tid) $ do
        threadDelay 5_000_000
        logDebug $ display (pier <> " not down after 5s, killing with fire.")
        cancel tid
      logTrace $ display ("Ship terminated: " <> pier)

{-
  TODO This is messy and shared a lot of logic with `runShipRestarting`.
-}
runShipNoRestart
  :: CLI.Run -> CLI.Opts -> Bool -> MultiEyreApi -> RIO KingEnv ()
runShipNoRestart r o d multi = do
  vKill  <- view kingEnvKillSignal -- killing ship same as killing king
  tid    <- asyncBound (runShipEnv r o vKill $ runShip r o d multi)
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

runShips :: CLI.KingOpts -> [(CLI.Run, CLI.Opts, Bool)] -> RIO KingEnv ()
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
  go :: MultiEyreApi -> [(CLI.Run, CLI.Opts, Bool)] ->  RIO KingEnv ()
  go me = \case
    []    -> pure ()
    [rod] -> runSingleShip rod me
    ships -> runMultipleShips (ships <&> \(r, o, _) -> (r, o)) me


-- TODO Duplicated logic.
runSingleShip :: (CLI.Run, CLI.Opts, Bool) -> MultiEyreApi -> RIO KingEnv ()
runSingleShip (r, o, d) multi = do
  shipThread <- async (runShipNoRestart r o d multi)

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


runMultipleShips :: [(CLI.Run, CLI.Opts)] -> MultiEyreApi -> RIO KingEnv ()
runMultipleShips ships multi = do
  shipThreads <- for ships $ \(r, o) -> do
    async (runShipRestarting r o multi)

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
