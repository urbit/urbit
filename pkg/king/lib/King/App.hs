module King.App
    ( App
    , runApp
    , runPierApp
    , HasConfigDir(..)
    ) where

import Config
import UrbitPrelude

--------------------------------------------------------------------------------

class HasConfigDir a where
    configDirL ∷ Lens' a FilePath

data App = App
    { _appLogFunc :: !LogFunc
    }

makeLenses ''App

instance HasLogFunc App where
    logFuncL = appLogFunc

runApp :: RIO App a -> IO a
runApp inner = do
    logOptions <- logOptionsHandle stderr True
        <&> setLogUseTime True
        <&> setLogUseLoc False

    withLogFunc logOptions $ \logFunc ->
        go (App logFunc)
  where
    go app = runRIO app inner


--------------------------------------------------------------------------------

-- A PierApp is like an App, except that it also provides a PierConfig
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
    logOptions <- logOptionsHandle stderr True
        <&> setLogUseTime True
        <&> setLogUseLoc False

    withLogFunc logOptions $ \logFunc ->
        go $ PierApp { _pierAppLogFunc = logFunc
                     , _pierAppPierConfig = pierConfig
                     , _pierAppNetworkConfig = networkConfig
                     }
  where
    go app = runRIO app inner
