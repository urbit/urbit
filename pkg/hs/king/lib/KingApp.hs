module KingApp
    ( App
    , runApp
    , runPierApp
    , HasAppName(..)
    ) where

import Config
import RIO.Directory
import UrbitPrelude

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

withLogFileHandle :: (Handle -> IO a) -> IO a
withLogFileHandle act = do
    home <- getHomeDirectory
    let logDir = home <> "/log"
    createDirectoryIfMissing True logDir
    withTempFile logDir "king-" $ \_tmpFile handle -> do
        hSetBuffering handle LineBuffering
        act handle

runApp :: RIO App a -> IO a
runApp inner = do
    withLogFileHandle $ \logFile -> do
        logOptions <- logOptionsHandle logFile True
            <&> setLogUseTime True
            <&> setLogUseLoc False

        withLogFunc logOptions $ \logFunc ->
            go $ App { _appLogFunc = logFunc
                     , _appName    = "Vere"
                     }
  where
    go app = runRIO app inner


--------------------------------------------------------------------------------

-- A PierApp is like an App, except that it also provides a PierConfig
data PierApp = PierApp
    { _shipAppLogFunc       :: !LogFunc
    , _shipAppName          :: !Utf8Builder
    , _shipAppPierConfig    :: !PierConfig
    , _shipAppNetworkConfig :: !NetworkConfig
    }

makeLenses ''PierApp

instance HasLogFunc PierApp where
    logFuncL = shipAppLogFunc

instance HasAppName PierApp where
    appNameL = shipAppName

instance HasPierConfig PierApp where
    pierConfigL = shipAppPierConfig

instance HasNetworkConfig PierApp where
    networkConfigL = shipAppNetworkConfig

runPierApp :: PierConfig -> NetworkConfig -> RIO PierApp a -> IO a
runPierApp pierConfig networkConfig inner = do
    withLogFileHandle $ \logFile -> do
        logOptions <- logOptionsHandle logFile True
            <&> setLogUseTime True
            <&> setLogUseLoc False

        withLogFunc logOptions $ \logFunc ->
            go $ PierApp { _shipAppLogFunc = logFunc
                         , _shipAppName    = "Vere"
                         , _shipAppPierConfig = pierConfig
                         , _shipAppNetworkConfig = networkConfig
                         }
  where
    go app = runRIO app inner
