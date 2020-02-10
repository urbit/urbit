{-|
    Pier Configuration
-}
module Urbit.King.Config where

import Urbit.Prelude

{-|
    All the configuration data revolving around a ship and the current
    execution options.
-}
data PierConfig = PierConfig
    { _pcPierPath :: FilePath
    , _pcDryRun   :: Bool
    } deriving (Show)

makeLenses ''PierConfig

class HasPierConfig env where
    pierConfigL :: Lens' env PierConfig

pierPathL ∷ HasPierConfig a => Lens' a FilePath
pierPathL = pierConfigL . pcPierPath

dryRunL :: HasPierConfig a => Lens' a Bool
dryRunL = pierConfigL . pcDryRun

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
