{-|
    Code for setting up the RIO environment.
-}
module Urbit.King.App
  ( KingEnv
  , runKingEnvStderr
  , runKingEnvStderrRaw
  , runKingEnvLogFile
  , runKingEnvNoLog
  , kingEnvKillSignal
  , killKingActionL
  , onKillKingSigL
  , HostEnv
  , runHostEnv
  , PierEnv
  , runPierEnv
  , killPierActionL
  , onKillPierSigL
  , HasStderrLogFunc(..)
  , HasKingId(..)
  , HasProcId(..)
  , HasKingEnv(..)
  , HasMultiEyreApi(..)
  , HasHostEnv(..)
  , HasPierEnv(..)
  , module Urbit.King.Config
  )
where

import Urbit.King.Config
import Urbit.Prelude

import RIO                    (logGeneric)
import System.Directory       ( createDirectoryIfMissing
                              , getXdgDirectory
                              , XdgDirectory(XdgCache)
                              )
import System.Posix.Internals (c_getpid)
import System.Posix.Types     (CPid(..))
import System.Random          (randomIO)
import Urbit.King.App.Class   (HasStderrLogFunc(..))
import Urbit.Vere.Eyre.Multi  (MultiEyreApi)
import Urbit.Vere.Ports       (PortControlApi, HasPortControlApi(..))


-- KingEnv ---------------------------------------------------------------------

class HasKingId a where
  kingIdL :: Lens' a Word16

class HasProcId a where
  procIdL :: Lens' a Int32

class (HasLogFunc a, HasStderrLogFunc a, HasKingId a, HasProcId a)
   => HasKingEnv a
 where
  kingEnvL :: Lens' a KingEnv

data KingEnv = KingEnv
  { _kingEnvLogFunc       :: !LogFunc
  , _kingEnvStderrLogFunc :: !LogFunc
  , _kingEnvKingId        :: !Word16
  , _kingEnvProcId        :: !Int32
  , _kingEnvKillSignal    :: !(TMVar ())
  }

makeLenses ''KingEnv

instance HasKingEnv KingEnv where
  kingEnvL = id

instance HasLogFunc KingEnv where
  logFuncL = kingEnvLogFunc

instance HasStderrLogFunc KingEnv where
  stderrLogFuncL = kingEnvStderrLogFunc

instance HasProcId KingEnv where
  procIdL = kingEnvProcId

instance HasKingId KingEnv where
  kingIdL = kingEnvKingId

-- Running KingEnvs ------------------------------------------------------------

runKingEnvStderr :: Bool -> LogLevel -> RIO KingEnv a -> IO a
runKingEnvStderr verb lvl inner = do
  logOptions <-
    logOptionsHandle stderr verb
      <&> setLogUseTime True
      <&> setLogUseLoc False
      <&> setLogMinLevel lvl
  withLogFunc logOptions $ \logFunc -> runKingEnv logFunc logFunc inner

runKingEnvStderrRaw :: Bool -> LogLevel -> RIO KingEnv a -> IO a
runKingEnvStderrRaw verb lvl inner = do
  logOptions <-
    logOptionsHandle stderr verb
      <&> setLogUseTime True
      <&> setLogUseLoc False
      <&> setLogMinLevel lvl
  withLogFunc logOptions $ \logFunc ->
    let lf = wrapCarriage logFunc
    in runKingEnv lf lf inner

-- XX loses callstack
wrapCarriage :: LogFunc -> LogFunc
wrapCarriage lf = mkLogFunc $ \_ ls ll bldr ->
  runRIO lf $ logGeneric ls ll (bldr <> "\r")

runKingEnvLogFile :: Bool -> LogLevel -> Maybe FilePath -> RIO KingEnv a -> IO a
runKingEnvLogFile verb lvl fileM inner = do
  logFile <- case fileM of
    Just f  -> pure f
    Nothing -> defaultLogFile
  withLogFileHandle logFile $ \h -> do
    logOptions <-
      logOptionsHandle h verb
        <&> setLogUseTime True
        <&> setLogUseLoc False
        <&> setLogMinLevel lvl
    stderrLogOptions <-
      logOptionsHandle stderr verb
        <&> setLogUseTime False
        <&> setLogUseLoc False
        <&> setLogMinLevel lvl
    withLogFunc stderrLogOptions $ \stderrLogFunc -> withLogFunc logOptions
      $ \logFunc -> runKingEnv logFunc stderrLogFunc inner

withLogFileHandle :: FilePath -> (Handle -> IO a) -> IO a
withLogFileHandle f act =
  withFile f AppendMode $ \handle -> do
    hSetBuffering handle LineBuffering
    act handle

defaultLogFile :: IO FilePath
defaultLogFile = do
  logDir <- getXdgDirectory XdgCache "urbit"
  createDirectoryIfMissing True logDir
  logId :: Word32 <- randomIO
  pure (logDir </> "king-" <> show logId <> ".log")

runKingEnvNoLog :: RIO KingEnv a -> IO a
runKingEnvNoLog act = runKingEnv mempty mempty act

runKingEnv :: LogFunc -> LogFunc -> RIO KingEnv a -> IO a
runKingEnv logFunc stderr action = do
  kid      <- randomIO
  CPid pid <- c_getpid
  kil      <- newEmptyTMVarIO
  runRIO (KingEnv logFunc stderr kid pid kil) action


-- KingEnv Utils ---------------------------------------------------------------

onKillKingSigL :: HasKingEnv e => Getter e (STM ())
onKillKingSigL = kingEnvL . kingEnvKillSignal . to readTMVar

killKingActionL :: HasKingEnv e => Getter e (STM ())
killKingActionL =
  kingEnvL . kingEnvKillSignal . to (\kil -> void (tryPutTMVar kil ()))

-- HostEnv ------------------------------------------------------------------

-- The host environment is everything in King, eyre configuration shared
-- across ships, and nat punching data.

class HasMultiEyreApi a where
  multiEyreApiL :: Lens' a MultiEyreApi

class (HasKingEnv a, HasMultiEyreApi a, HasPortControlApi a) =>
      HasHostEnv a where
  hostEnvL :: Lens' a HostEnv

data HostEnv = HostEnv
  { _hostEnvKingEnv        :: !KingEnv
  , _hostEnvMultiEyreApi   :: !MultiEyreApi
  , _hostEnvPortControlApi :: !PortControlApi
  }

makeLenses ''HostEnv

instance HasKingEnv HostEnv where
  kingEnvL = hostEnvKingEnv

instance HasLogFunc HostEnv where
  logFuncL = kingEnvL . logFuncL

instance HasStderrLogFunc HostEnv where
  stderrLogFuncL = kingEnvL . stderrLogFuncL

instance HasProcId HostEnv where
  procIdL = kingEnvL . procIdL

instance HasKingId HostEnv where
  kingIdL = kingEnvL . kingEnvKingId

instance HasMultiEyreApi HostEnv where
  multiEyreApiL = hostEnvMultiEyreApi

instance HasPortControlApi HostEnv where
  portControlApiL = hostEnvPortControlApi

-- Running Running Envs --------------------------------------------------------

runHostEnv :: MultiEyreApi -> PortControlApi -> RIO HostEnv a
           -> RIO KingEnv a
runHostEnv multi ports action = do
    king <- ask

    let hostEnv = HostEnv { _hostEnvKingEnv        = king
                          , _hostEnvMultiEyreApi   = multi
                          , _hostEnvPortControlApi = ports
                          }

    io (runRIO hostEnv action)

-- PierEnv ---------------------------------------------------------------------

class (HasKingEnv a, HasHostEnv a, HasPierConfig a, HasNetworkConfig a) =>
      HasPierEnv a where
  pierEnvL :: Lens' a PierEnv

data PierEnv = PierEnv
  { _pierEnvHostEnv       :: !HostEnv
  , _pierEnvPierConfig    :: !PierConfig
  , _pierEnvNetworkConfig :: !NetworkConfig
  , _pierEnvKillSignal    :: !(TMVar ())
  }

makeLenses ''PierEnv

instance HasKingEnv PierEnv where
  kingEnvL = pierEnvHostEnv . kingEnvL

instance HasHostEnv PierEnv where
  hostEnvL = pierEnvHostEnv

instance HasMultiEyreApi PierEnv where
  multiEyreApiL = pierEnvHostEnv . multiEyreApiL

instance HasPortControlApi PierEnv where
  portControlApiL = pierEnvHostEnv . portControlApiL

instance HasPierEnv PierEnv where
  pierEnvL = id

instance HasKingId PierEnv where
  kingIdL = kingEnvL . kingEnvKingId

instance HasStderrLogFunc PierEnv where
  stderrLogFuncL = kingEnvL . stderrLogFuncL

instance HasLogFunc PierEnv where
  logFuncL = kingEnvL . logFuncL

instance HasPierPath PierEnv where
  pierPathL = pierEnvPierConfig . pierPathL

instance HasDryRun PierEnv where
  dryRunL = pierEnvPierConfig . dryRunL

instance HasPierConfig PierEnv where
  pierConfigL = pierEnvPierConfig

instance HasNetworkConfig PierEnv where
  networkConfigL = pierEnvNetworkConfig

instance HasProcId PierEnv where
  procIdL = kingEnvL . kingEnvProcId


-- PierEnv Utils ---------------------------------------------------------------

onKillPierSigL :: HasPierEnv e => Getter e (STM ())
onKillPierSigL = pierEnvL . pierEnvKillSignal . to readTMVar

killPierActionL :: HasPierEnv e => Getter e (STM ())
killPierActionL =
  pierEnvL . pierEnvKillSignal . to (\kil -> void (tryPutTMVar kil ()))


-- Running Pier Envs -----------------------------------------------------------

runPierEnv
  :: PierConfig -> NetworkConfig -> TMVar () -> RIO PierEnv a -> RIO HostEnv a
runPierEnv pierConfig networkConfig vKill action = do
  host <- ask

  let pierEnv = PierEnv { _pierEnvHostEnv       = host
                        , _pierEnvPierConfig    = pierConfig
                        , _pierEnvNetworkConfig = networkConfig
                        , _pierEnvKillSignal    = vKill
                        }
  putStrLn "runPierEnv"
  io (runRIO pierEnv action)
