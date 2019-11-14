{-# LANGUAGE UndecidableInstances #-}

module KingApp
    ( Config(..)
    , App
    , PierEnv
    , runApp
    , inPierEnv
    , inPierEnvRAcquire
    , runAppNoConfig
    , HasConfig(..)
    , HasAmesPort(..)
    , HasShip(..)
    , HasIsFake(..)
    ) where

import UrbitPrelude
import RIO.Directory
import Arvo.Common
import Data.Default


-- Command Line Configuration --------------------------------------------------

data Config = Config
    { _configAmesPort :: !(Maybe Port)
    }

makeLenses ''Config


-- App Environment -------------------------------------------------------------

data App = App
    { _appLogFunc :: !LogFunc
    , _appConfig  :: !Config
    }

makeLenses ''App

instance HasLogFunc App where
    logFuncL = appLogFunc


-- Pier Environment ------------------------------------------------------------

data PierEnv = PierEnv
    { _pierEnvLogFunc :: !LogFunc
    , _pierEnvConfig  :: !Config
    , _pierEnvShip    :: !Ship
    , _pierEnvIsFake  :: !Bool
    }

makeLenses ''PierEnv

instance Default Config where
  def = Config Nothing

instance HasLogFunc PierEnv where
    logFuncL = pierEnvLogFunc


-- Ames Port -------------------------------------------------------------------

class HasAmesPort env where
    amesPortL :: Lens' env (Maybe Port)

instance HasAmesPort Config  where amesPortL = configAmesPort
instance HasAmesPort App     where amesPortL = appConfig . configAmesPort
instance HasAmesPort PierEnv where amesPortL = pierEnvConfig . configAmesPort




-- Which Ship are we? ----------------------------------------------------------

class HasShip env where
    shipL :: Lens' env Ship

instance HasShip PierEnv where
    shipL = pierEnvShip


-- Are we a fake ship? ---------------------------------------------------------

class HasIsFake env where
  isFakeL :: Lens' env Bool

instance HasIsFake PierEnv where isFakeL = pierEnvIsFake


-- HasConfig -------------------------------------------------------------------

class HasAmesPort env => HasConfig env where
    configL :: Lens' env Config

instance HasConfig Config where
    configL = id

instance HasConfig App where
    configL = appConfig

instance HasConfig PierEnv where
    configL = pierEnvConfig


--------------------------------------------------------------------------------

withLogFileHandle :: (Handle -> IO a) -> IO a
withLogFileHandle act = do
    home <- getHomeDirectory
    let logDir = home <> "/log"
    createDirectoryIfMissing True logDir
    withTempFile logDir "king-" $ \_tmpFile handle -> do
        hSetBuffering handle LineBuffering
        act handle

runAppNoConfig :: RIO App a -> IO a
runAppNoConfig = runApp def

inPierEnv :: ∀e a. (HasLogFunc e, HasConfig e)
          => Ship -> Bool -> RIO PierEnv a -> RIO e a
inPierEnv ship fake =
    withRIO $ \x -> PierEnv (x ^. logFuncL) (x ^. configL) ship fake

inPierEnvRAcquire :: ∀e a. (HasLogFunc e, HasConfig e)
                  => Ship -> Bool
                  -> (RAcquire PierEnv a -> RAcquire e a)
inPierEnvRAcquire ship fake =
    withRAcquire $ \x -> PierEnv (x ^. logFuncL) (x ^. configL) ship fake

runApp :: Config -> RIO App a -> IO a
runApp conf inner = do
    withLogFileHandle $ \logFile -> do
        logOptions <- logOptionsHandle logFile True
            <&> setLogUseTime True
            <&> setLogUseLoc False

        withLogFunc logOptions $ \logFunc ->
            go $ App { _appLogFunc = logFunc
                     , _appConfig  = conf
                     }
  where
    go app = runRIO app inner
