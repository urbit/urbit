module PierConfig where

import UrbitPrelude

data NetworkingType = NetworkNormal | NetworkLocalhost

-- All the configuration data revolving around a ship and the current execution
-- options.
data PierConfig = PierConfig
  { pcPierPath   :: FilePath
  -- Configurable networking options
  , pcNetworking :: NetworkingType
  , pcAmesPort   :: Maybe Word16
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

getAmesPort :: (MonadReader env m, HasPierConfig env) => m (Maybe Word16)
getAmesPort = do
  PierConfig{..} <- view pierConfigL
  pure pcAmesPort
