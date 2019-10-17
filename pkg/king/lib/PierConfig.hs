module PierConfig where

import UrbitPrelude

-- State before lunch: I had threaded the pier path through everything. I tried
-- putting the kingid here, but realized it would be a bad idea since it has
-- non-monadic uses. I started the plan of NetworkType for -O vs -L vs nothing.


-- The
data NetworkingType = NetworkNormal | NetworkLocalhost


-- All the configuration data revolving around a ship and the current execution
-- options.
data PierConfig = PierConfig
  { pcPierPath   :: FilePath
  -- Configurable networking options
  , pcNetworking :: NetworkingType
  }

class HasPierConfig env where
    pierConfigL :: Lens' env PierConfig

getPierPath :: (MonadReader env m, HasPierConfig env) => m FilePath
getPierPath = do
  PierConfig{..} <- view pierConfigL
  pure pcPierPath

getNetworkingType :: (MonadReader env m, HasPierConfig env) => m NetworkingType
getNetworkingType = do
  PierConfig{..} <- view pierConfigL
  pure pcNetworking
