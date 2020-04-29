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

data NetMode
  = NMNone
  | NMLocalhost
  | NMNormal
 deriving (Eq, Ord, Show)

data NetworkConfig = NetworkConfig
  { _ncNetMode    :: NetMode
  , _ncAmesPort   :: Maybe Word16
  , _ncHttpPort   :: Maybe Word16
  , _ncHttpsPort  :: Maybe Word16
  , _ncLocalPort  :: Maybe Word16
  , _ncAmesLatency :: Maybe Int
  } deriving (Show)

makeLenses ''NetworkConfig

class HasNetworkConfig env where
    networkConfigL :: Lens' env NetworkConfig
