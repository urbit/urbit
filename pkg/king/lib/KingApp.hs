module KingApp
    ( App
    , runApp
    , runAppLogFile
    , HasDaemon(..)
    ) where

import UrbitPrelude
import RIO.Directory

import Arvo (Belt)

import qualified Vere.NounServ as NounServ
import qualified Vere.Term.API as Term


-- TODO These don't really belong here. ----------------------------------------

type TermConn = NounServ.Conn Belt [Term.Ev]

data ShipCtl = ShipCtl
  { scTerm :: TermConn -> STM ()
  }

type FleetCtl = TVar (Map Ship ShipCtl)

--------------------------------------------------------------------------------

class HasDaemon env where
    daemonL :: Lens' env Daemon

data Daemon = Daemon
     { root  :: FilePath
     , fleet :: FleetCtl
     }
  deriving (Generic)

data App = App
    { _appLogFunc :: !LogFunc
    , _appDaemon  :: !Daemon
    }

makeLenses ''App

instance HasLogFunc App where
    logFuncL = appLogFunc

instance HasDaemon App where
    daemonL = appName

withLogFileHandle :: (Handle -> IO a) -> IO a
withLogFileHandle act = do
    home <- getHomeDirectory
    let logDir = home </> "log"
    createDirectoryIfMissing True logDir
    withTempFile logDir "king-" $ \_tmpFile handle -> do
        hSetBuffering handle LineBuffering
        act handle

runApp :: RIO App a -> IO a
runApp inner = do
    logOptions <- logOptionsHandle stdout True
        <&> setLogUseTime True
        <&> setLogUseLoc False

    rootDir <- (</> "urbit") <$> getHomeDirectory

    withLogFunc logOptions $ \logFunc ->

    withLogFunc logOptions $ \logFunc ->
        go $ App { _appLogFunc = logFunc
                 , _appDaemon  = Daemon rootDir
                 }
  where
    go app = runRIO app inner

runAppLogFile :: RIO App a -> IO a
runAppLogFile inner = do
    withLogFileHandle $ \logFile -> do
        logOptions <- logOptionsHandle logFile True
            <&> setLogUseTime True
            <&> setLogUseLoc False

        withLogFunc logOptions $ \logFunc ->
            go $ App { _appLogFunc = logFunc
                     , _appDaemon  = "Vere"
                     }
  where
    go app = runRIO app inner
