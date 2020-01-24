{-|
    Code for setting up the RIO environment.
-}
module Urbit.King.App
    ( App
    , runApp
    , runAppLogFile
    , runAppLogHandle
    , runAppNoLog
    , runPierApp
    , HasConfigDir(..)
    ) where

import Urbit.King.Config
import Urbit.Prelude

import System.Directory (createDirectoryIfMissing, getHomeDirectory)

--------------------------------------------------------------------------------

class HasConfigDir a where
    configDirL ∷ Lens' a FilePath

data App = App
    { _appLogFunc :: !LogFunc
    }

makeLenses ''App

instance HasLogFunc App where
    logFuncL = appLogFunc

runAppLogHandle :: Handle -> RIO App a -> IO a
runAppLogHandle logHandle inner = do
    logOptions <- logOptionsHandle logHandle True
        <&> setLogUseTime True
        <&> setLogUseLoc False

    withLogFunc logOptions $ \logFunc ->
        go (App logFunc)
  where
    go app = runRIO app inner

runApp :: RIO App a -> IO a
runApp = runAppLogHandle stdout

runAppLogFile :: RIO App a -> IO a
runAppLogFile inner = withLogFileHandle (\h -> runAppLogHandle h inner)

withLogFileHandle :: (Handle -> IO a) -> IO a
withLogFileHandle act = do
    home <- getHomeDirectory
    let logDir = home </> ".urbit"
    createDirectoryIfMissing True logDir
    withFile (logDir </> "king.log") AppendMode $ \handle -> do
        hSetBuffering handle LineBuffering
        act handle

runAppNoLog :: RIO App a -> IO a
runAppNoLog act =
    withFile "/dev/null" AppendMode $ \handle ->
        runAppLogHandle handle act

--------------------------------------------------------------------------------

-- | A PierApp is like an App, except that it also provides a PierConfig
data PierApp = PierApp
    { _pierAppLogFunc       :: !LogFunc
    , _pierAppPierConfig    :: !PierConfig
    , _pierAppNetworkConfig :: !NetworkConfig
    }

makeLenses ''PierApp

instance HasLogFunc PierApp where
    logFuncL = pierAppLogFunc

instance HasPierConfig PierApp where
    pierConfigL = pierAppPierConfig

instance HasNetworkConfig PierApp where
    networkConfigL = pierAppNetworkConfig

instance HasConfigDir PierApp where
    configDirL = pierAppPierConfig . pcPierPath

runPierApp :: PierConfig -> NetworkConfig -> Bool -> RIO PierApp a -> IO a
runPierApp pierConfig networkConfig daemon inner =
    if daemon
    then exec stderr
    else withLogFileHandle exec
  where
    exec logHandle = do
        logOptions <- logOptionsHandle logHandle True
            <&> setLogUseTime True
            <&> setLogUseLoc False

        withLogFunc logOptions $ \logFunc ->
            go $ PierApp { _pierAppLogFunc = logFunc
                         , _pierAppPierConfig = pierConfig
                         , _pierAppNetworkConfig = networkConfig
                         }
    go app = runRIO app inner
