{-|
    Code for setting up the RIO environment.
-}
module Urbit.King.App
    ( App
    , runAppStderr
    , runAppLogFile
    , runAppNoLog
    , runPierApp
    , HasConfigDir(..)
    , HasStderrLogFunc(..)
    ) where

import Urbit.King.Config
import Urbit.Prelude

import System.Directory (createDirectoryIfMissing, getHomeDirectory)

--------------------------------------------------------------------------------

class HasConfigDir a where
    configDirL ∷ Lens' a FilePath

class HasStderrLogFunc a where
    stderrLogFuncL :: Lens' a LogFunc

--------------------------------------------------------------------------------

data App = App
    { _appLogFunc       :: !LogFunc
    , _appStderrLogFunc :: !LogFunc
    }

makeLenses ''App

instance HasLogFunc App where
    logFuncL = appLogFunc

instance HasStderrLogFunc App where
    stderrLogFuncL = appStderrLogFunc

runAppStderr :: RIO App a -> IO a
runAppStderr inner = do
    logOptions <- logOptionsHandle stderr True
        <&> setLogUseTime True
        <&> setLogUseLoc False

    withLogFunc logOptions $ \logFunc ->
        runRIO (App logFunc logFunc) inner

runAppLogFile :: RIO App a -> IO a
runAppLogFile inner =
    withLogFileHandle $ \h -> do
      logOptions <- logOptionsHandle h True
          <&> setLogUseTime True
          <&> setLogUseLoc False
      stderrLogOptions <- logOptionsHandle stderr True
          <&> setLogUseTime False
          <&> setLogUseLoc False

      withLogFunc stderrLogOptions $ \stderrLogFunc ->
        withLogFunc logOptions $ \logFunc ->
          runRIO (App logFunc stderrLogFunc) inner

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
    withFile "/dev/null" AppendMode $ \handle -> do
      logOptions <- logOptionsHandle handle True
      withLogFunc logOptions $ \logFunc ->
        runRIO (App logFunc logFunc) act

--------------------------------------------------------------------------------

-- | A PierApp is like an App, except that it also provides a PierConfig
data PierApp = PierApp
    { _pierAppLogFunc       :: !LogFunc
    , _pierAppStderrLogFunc :: !LogFunc
    , _pierAppPierConfig    :: !PierConfig
    , _pierAppNetworkConfig :: !NetworkConfig
    }

makeLenses ''PierApp

instance HasStderrLogFunc PierApp where
    stderrLogFuncL = pierAppStderrLogFunc

instance HasLogFunc PierApp where
    logFuncL = pierAppLogFunc

instance HasPierConfig PierApp where
    pierConfigL = pierAppPierConfig

instance HasNetworkConfig PierApp where
    networkConfigL = pierAppNetworkConfig

instance HasConfigDir PierApp where
    configDirL = pierAppPierConfig . pcPierPath

runPierApp :: PierConfig -> NetworkConfig -> RIO PierApp a -> RIO App a
runPierApp pierConfig networkConfig action = do
  app <- ask

  let pierApp = PierApp { _pierAppLogFunc       = app ^. logFuncL
                        , _pierAppStderrLogFunc = app ^. stderrLogFuncL
                        , _pierAppPierConfig    = pierConfig
                        , _pierAppNetworkConfig = networkConfig
                        }

  io (runRIO pierApp action)
