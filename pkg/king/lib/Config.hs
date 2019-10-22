module Config where

import UrbitPrelude

-- All the configuration data revolving around a ship and the current execution
-- options.
data PierConfig = PierConfig
  { pcPierPath :: FilePath
  , pcDryRun   :: Bool
  } deriving (Show)

class HasPierConfig env where
    pierConfigL :: Lens' env PierConfig

getPierPath :: (MonadReader env m, HasPierConfig env) => m FilePath
getPierPath = do
  PierConfig{..} <- view pierConfigL
  pure pcPierPath

getIsDryRun :: (MonadReader env m, HasPierConfig env) => m Bool
getIsDryRun = do
  PierConfig{..} <- view pierConfigL
  pure pcDryRun

-------------------------------------------------------------------------------

data NetworkingType
  = NetworkNone
  | NetworkNormal
  | NetworkLocalhost
  deriving (Show)

data NetworkConfig = NetworkConfig
  { ncNetworking :: NetworkingType
  , ncAmesPort   :: Maybe Word16
  } deriving (Show)

class HasNetworkConfig env where
    networkConfigL :: Lens' env NetworkConfig

getNetworkingType :: (MonadReader env m, HasNetworkConfig env)
                  => m NetworkingType
getNetworkingType = do
  NetworkConfig{..} <- view networkConfigL
  pure ncNetworking

getAmesPort :: (MonadReader env m, HasNetworkConfig env) => m (Maybe Word16)
getAmesPort = do
  NetworkConfig{..} <- view networkConfigL
  pure ncAmesPort
