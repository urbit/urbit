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

class HasPierPath a where
  pierPathL :: Lens' a FilePath

class HasDryRun a where
  dryRunL :: Lens' a Bool

class (HasPierPath a, HasDryRun a) => HasPierConfig a where
  pierConfigL :: Lens' a PierConfig

instance HasPierPath PierConfig where
  pierPathL = pcPierPath

instance HasDryRun PierConfig where
  dryRunL = pcDryRun


-------------------------------------------------------------------------------

data NetMode
  = NMNone
  | NMLocalhost
  | NMNormal
 deriving (Eq, Ord, Show)

data NetworkConfig = NetworkConfig
  { _ncNetMode    :: NetMode
  , _ncAmesPort   :: Maybe Word16
  , _ncNoAmes     :: Bool
  , _ncNoHttp     :: Bool
  , _ncNoHttps    :: Bool
  , _ncHttpPort   :: Maybe Word16
  , _ncHttpsPort  :: Maybe Word16
  , _ncLocalPort  :: Maybe Word16
  } deriving (Show)

makeLenses ''NetworkConfig

class HasNetworkConfig env where
    networkConfigL :: Lens' env NetworkConfig
