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

module Urbit.King.Main (main) where

import Urbit.Prelude

import Network.HTTP.Client.TLS
import Urbit.Arvo
import Urbit.King.Config
import Urbit.Vere.Dawn
import Urbit.Vere.Ports
import Urbit.Vere.Eyre.Multi (multiEyre, MultiEyreConf(..))
import Urbit.Vere.Pier.Types
import Urbit.Vere.Serf
import Urbit.King.App

import Control.Concurrent     (myThreadId)
import Control.Exception      (AsyncException(UserInterrupt))
import System.Exit            (ExitCode(..))
import System.IO              (hPutStrLn)
import Urbit.Vere.LockFile    (lockFile)

import qualified Data.Set                as Set
import qualified Data.Text               as T
import qualified Network.HTTP.Client     as C
import qualified System.Posix.Signals    as Sys
import qualified System.Posix.Resource   as Sys
import qualified System.Random           as Sys
import qualified Urbit.King.CLI          as CLI
import qualified Urbit.Ob                as Ob
import qualified Urbit.Vere.Pier         as Pier
import qualified Urbit.Vere.Serf         as Serf
import qualified Urbit.Vere.Term         as Term


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
        , setFrom oDryRun Serf.DryRun
        ]
    setFrom True flag = Just flag
    setFrom False _   = Nothing

toPierConfig :: FilePath -> Maybe Text -> CLI.Opts -> PierConfig
toPierConfig pierPath serfExe o@(CLI.Opts{..}) = PierConfig { .. }
 where
  _pcPierPath  = pierPath
  _pcDryRun    = oDryRun
  _pcSerfExe   = serfExe
  _pcSerfFlags = toSerfFlags o

toNetworkConfig :: CLI.Opts -> NetworkConfig
toNetworkConfig CLI.Opts {..} = NetworkConfig { .. }
 where
  offline    = oDryRun || oOffline

  mode = case (oDryRun, offline, oLocalhost) of
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
  -> RIO PierEnv ()
tryBootFromPill oExit pill lite ship boot = do
  mStart <- newEmptyMVar
  vSlog  <- logSlogs
  exitCode <- rwith (view pierPathL >>= lockFile) $ \_ -> do
    rio $ logInfo "Starting boot"
    Pier.booted vSlog pill lite ship boot
  rio $ logInfo "Completed boot"
  case exitCode of
    ExitSuccess   -> pure ()
    ExitFailure c -> logStderr $ logError
                               $ "Mars terminated with exit code: " <> display c

runOrExitImmediately
  :: TVar ((Atom, Tank) -> IO ())
  -> RAcquire PierEnv (Serf, Ripe)
  -> Bool
  -> MVar ()
  -> [Ev]
  -> RIO PierEnv ()
runOrExitImmediately vSlog getPier oExit mStart injected = do
  rwith getPier (if oExit then shutdownImmediately else runPier)
 where
  shutdownImmediately :: (Serf, Ripe) -> RIO PierEnv ()
  shutdownImmediately (serf, _) = do
    logInfo "Sending shutdown signal"
    Serf.stop serf
    logInfo "Shutdown!"

  runPier :: (Serf, Ripe) -> RIO PierEnv ()
  runPier serfRipe = do
    runRAcquire (Pier.pier serfRipe vSlog mStart injected)

tryPlayShip
  :: Bool
  -> MVar ()
  -> [Ev]
  -> RIO PierEnv ()
tryPlayShip exitImmediately mStart injected = do
  vSlog <- logSlogs
  runOrExitImmediately vSlog (resumeShip vSlog) exitImmediately mStart injected
 where
  resumeShip :: TVar ((Atom, Tank) -> IO ())
             -> RAcquire PierEnv (Serf, Ripe)
  resumeShip vSlog = do
    view pierPathL >>= lockFile
    rio $ logInfo "RESUMING SHIP"
    sir <- Pier.resumed vSlog
    rio $ logInfo "SHIP RESUMED"
    pure sir

runRAcquire :: (MonadUnliftIO (m e),  MonadIO (m e), MonadReader e (m e))
            => RAcquire e a -> m e a
runRAcquire act = rwith act pure


--------------------------------------------------------------------------------

pillFrom :: CLI.PillSource -> RIO HostEnv Pill
pillFrom = \case
  CLI.PillSourceFile pillPath -> do
    logInfo $ display $ "boot: reading pill from " ++ (pack pillPath :: Text)
    io (loadFileAtom pillPath >>= either throwIO pure)

  CLI.PillSourceURL url -> do
    logInfo $ display $ "boot: retrieving pill from " ++ (pack url :: Text)
    -- Get the jamfile with the list of stars accepting comets right now.
    manager <- io $ C.newManager tlsManagerSettings
    request <- io $ C.parseRequest url
    response <- io $ C.httpLbs (C.setRequestCheckStatus request) manager
    pure $ bytesAtom $ toStrict $ C.responseBody response

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
      bootFromSeed pill seed

    CLI.BootFake name -> do
      pill <- pillFrom nPillSource
      ship <- shipFrom name
      runTryBootFromPill pill name ship (Fake ship)

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

      bootFromSeed pill seed

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

    bootFromSeed :: Pill -> Seed -> RIO HostEnv ()
    bootFromSeed pill seed = do
      ethReturn <- dawnVent nEthNode seed

      case ethReturn of
        Left x -> error $ unpack x
        Right dawn -> do
          let ship = sShip $ dSeed dawn
          name <- nameFromShip ship
          runTryBootFromPill pill name ship (Dawn dawn)

    -- Now that we have all the information for running an application with a
    -- PierConfig, do so.
    runTryBootFromPill :: Pill
                       -> Text
                       -> Ship
                       -> LegacyBootEvent
                       -> RIO HostEnv ()
    runTryBootFromPill pill name ship bootEvent = do
      vKill <- view (kingEnvL . kingEnvKillSignal)
      let pierConfig = toPierConfig (pierPath name) nSerfExe opts
      let networkConfig = toNetworkConfig opts
      runPierEnv pierConfig networkConfig vKill $
        tryBootFromPill True pill nLite ship bootEvent

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

checkDawn :: HasLogFunc e => String -> FilePath -> RIO e ()
checkDawn provider keyfilePath = do
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

  e <- dawnVent provider seed
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
    CLI.CmdRun ko ships                     -> runShips ko ships
    CLI.CmdNew n  o                         -> newShip n o
    CLI.CmdBug (CLI.CheckDawn provider pax) -> checkDawn provider pax
    CLI.CmdBug CLI.CheckComet               -> checkComet
    CLI.CmdCon pier                         -> connTerm pier

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
