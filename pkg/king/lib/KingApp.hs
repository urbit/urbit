module KingApp
    ( App
    , runApp
    , runAppLogFile
    , HasAppName(..)
    ) where

import UrbitPrelude
import RIO.Directory

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
    logOptions <- logOptionsHandle stdout True
        <&> setLogUseTime True
        <&> setLogUseLoc False

    withLogFunc logOptions $ \logFunc ->
        go $ App { _appLogFunc = logFunc
                 , _appName    = "Vere"
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
                     , _appName    = "Vere"
                     }
  where
    go app = runRIO app inner
