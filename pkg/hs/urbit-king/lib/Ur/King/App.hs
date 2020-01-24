{-|
    Code for setting up the RIO environment.
-}
module Ur.King.App
    ( App
    , runApp
    , runAppLogFile
    , runAppLogHandle
    , runPierApp
    , HasConfigDir(..)
    ) where

import Ur.King.Config
import Ur.Prelude

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
  where
    withLogFileHandle :: (Handle -> IO a) -> IO a
    withLogFileHandle act = do
        home <- getHomeDirectory
        let logDir = home </> ".urbit"
        createDirectoryIfMissing True logDir
        withFile (logDir </> "king.log") AppendMode $ \handle -> do
            hSetBuffering handle LineBuffering
            act handle


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

runPierApp :: PierConfig -> NetworkConfig -> RIO PierApp a -> IO a
runPierApp pierConfig networkConfig inner = do
    logOptions <- logOptionsHandle stdout True
        <&> setLogUseTime True
        <&> setLogUseLoc False

    withLogFunc logOptions $ \logFunc ->
        go $ PierApp { _pierAppLogFunc = logFunc
                     , _pierAppPierConfig = pierConfig
                     , _pierAppNetworkConfig = networkConfig
                     }
  where
    go app = runRIO app inner
