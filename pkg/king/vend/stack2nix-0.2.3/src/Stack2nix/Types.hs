module Stack2nix.Types where

import           Data.Time                       (UTCTime)
import           Distribution.PackageDescription (FlagName)
import           Distribution.System             (Platform)

data Args = Args
  { argRev                 :: Maybe String
  , argOutFile             :: Maybe FilePath
  , argStackYaml           :: FilePath
  , argThreads             :: Int
  , argTest                :: Bool
  , argBench               :: Bool
  , argHaddock             :: Bool
  , argHackageSnapshot     :: Maybe UTCTime
  , argPlatform            :: Platform
  , argUri                 :: String
  , argIndent              :: Bool
  , argVerbose             :: Bool
  , argCabal2nixArgs       :: Maybe String
  , argEnsureExecutables   :: Bool
  }
  deriving (Show)

type Flags = [(FlagName, Bool)]
