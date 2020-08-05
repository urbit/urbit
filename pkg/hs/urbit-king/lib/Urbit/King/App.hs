{-|
    Code for setting up the RIO environment.
-}
module Urbit.King.App
  ( KingEnv
  , runKingEnvStderr
  , runKingEnvLogFile
  , runKingEnvNoLog
  , kingEnvKillSignal
  , killKingActionL
  , onKillKingSigL
  , RunningEnv
  , runRunningEnv
  , PierEnv
  , runPierEnv
  , killPierActionL
  , onKillPierSigL
  , HasStderrLogFunc(..)
  , HasKingId(..)
  , HasProcId(..)
  , HasKingEnv(..)
  , HasMultiEyreApi(..)
  , HasRunningEnv(..)
  , HasPierEnv(..)
  , module Urbit.King.Config
  )
where

import Urbit.King.Config
import Urbit.Prelude

import System.Directory       (createDirectoryIfMissing, getHomeDirectory)
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

runKingEnvStderr :: Bool -> RIO KingEnv a -> IO a
runKingEnvStderr verb inner = do
  logOptions <-
    logOptionsHandle stderr verb <&> setLogUseTime True <&> setLogUseLoc False

  withLogFunc logOptions $ \logFunc -> runKingEnv logFunc logFunc inner

runKingEnvLogFile :: Bool -> RIO KingEnv a -> IO a
runKingEnvLogFile verb inner = withLogFileHandle $ \h -> do
  logOptions <-
    logOptionsHandle h verb <&> setLogUseTime True <&> setLogUseLoc False
  stderrLogOptions <-
    logOptionsHandle stderr verb <&> setLogUseTime False <&> setLogUseLoc False

  withLogFunc stderrLogOptions $ \stderrLogFunc -> withLogFunc logOptions
    $ \logFunc -> runKingEnv logFunc stderrLogFunc inner

withLogFileHandle :: (Handle -> IO a) -> IO a
withLogFileHandle act = do
  home <- getHomeDirectory
  let logDir = home </> ".urbit"
  createDirectoryIfMissing True logDir
  withFile (logDir </> "king.log") AppendMode $ \handle -> do
    hSetBuffering handle LineBuffering
    act handle

runKingEnvNoLog :: RIO KingEnv a -> IO a
runKingEnvNoLog act = withFile "/dev/null" AppendMode $ \handle -> do
  logOptions <- logOptionsHandle handle True
  withLogFunc logOptions $ \logFunc -> runKingEnv logFunc logFunc act

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

-- RunningEnv ------------------------------------------------------------------

-- The running environment is everything in King, eyre configuration shared
-- across ships, and IP information shared across ships.
--
-- TODO: Implement that IP information for real.

class HasMultiEyreApi a where
  multiEyreApiL :: Lens' a MultiEyreApi

class (HasKingEnv a, HasMultiEyreApi a, HasPortControlApi a) =>
      HasRunningEnv a where
  runningEnvL :: Lens' a RunningEnv

data RunningEnv = RunningEnv
  { _runningEnvKingEnv        :: !KingEnv
  , _runningEnvMultiEyreApi   :: MultiEyreApi
  , _runningEnvPortControlApi :: PortControlApi
  }

makeLenses ''RunningEnv

instance HasKingEnv RunningEnv where
  kingEnvL = runningEnvKingEnv

instance HasLogFunc RunningEnv where
  logFuncL = kingEnvL . logFuncL

instance HasStderrLogFunc RunningEnv where
  stderrLogFuncL = kingEnvL . stderrLogFuncL

instance HasProcId RunningEnv where
  procIdL = kingEnvL . procIdL

instance HasKingId RunningEnv where
  kingIdL = kingEnvL . kingEnvKingId

instance HasMultiEyreApi RunningEnv where
  multiEyreApiL = runningEnvMultiEyreApi

instance HasPortControlApi RunningEnv where
  portControlApiL = runningEnvPortControlApi

-- Running Running Envs --------------------------------------------------------

runRunningEnv :: MultiEyreApi -> PortControlApi -> RIO RunningEnv ()
              -> RIO KingEnv ()
runRunningEnv multi ports action = do
    king <- ask

    let runningEnv = RunningEnv { _runningEnvKingEnv        = king
                                , _runningEnvMultiEyreApi   = multi
                                , _runningEnvPortControlApi = ports
                                }

    io (runRIO runningEnv action)

-- PierEnv ---------------------------------------------------------------------

class (HasKingEnv a, HasRunningEnv a, HasPierConfig a, HasNetworkConfig a) =>
      HasPierEnv a where
  pierEnvL :: Lens' a PierEnv

data PierEnv = PierEnv
  { _pierEnvRunningEnv    :: !RunningEnv
  , _pierEnvPierConfig    :: !PierConfig
  , _pierEnvNetworkConfig :: !NetworkConfig
  , _pierEnvKillSignal    :: !(TMVar ())
  }

makeLenses ''PierEnv

instance HasKingEnv PierEnv where
  kingEnvL = pierEnvRunningEnv . kingEnvL

instance HasRunningEnv PierEnv where
  runningEnvL = pierEnvRunningEnv

instance HasMultiEyreApi PierEnv where
  multiEyreApiL = pierEnvRunningEnv . multiEyreApiL

instance HasPortControlApi PierEnv where
  portControlApiL = pierEnvRunningEnv . portControlApiL

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
  :: PierConfig -> NetworkConfig -> TMVar () -> RIO PierEnv a -> RIO RunningEnv a
runPierEnv pierConfig networkConfig vKill action = do
  running <- ask

  let pierEnv = PierEnv { _pierEnvRunningEnv    = running
                        , _pierEnvPierConfig    = pierConfig
                        , _pierEnvNetworkConfig = networkConfig
                        , _pierEnvKillSignal    = vKill
                        }

  io (runRIO pierEnv action)
