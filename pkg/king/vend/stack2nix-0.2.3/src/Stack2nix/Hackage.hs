-- | This module is needed to replace P.parseDB which would otherwise filter out
--   deprecated packages, which may end up being in an LTS.
module Stack2nix.Hackage
  ( loadHackageDB
  ) where

import Control.Exception (SomeException, mapException)
import qualified Data.Map as Map
import Data.Time.Clock (UTCTime)
import Distribution.Hackage.DB.Path               (hackageTarball)
import qualified Distribution.Hackage.DB.Unparsed as U
import qualified Distribution.Hackage.DB.Parsed   as P
import Distribution.Package (PackageName)
import Distribution.Hackage.DB.Errors (HackageDBPackageName(..))
import qualified Distribution.Nixpkgs.Haskell.Hackage as H



loadHackageDB :: Maybe FilePath
              -- ^ The path to the Hackage database.
              -> Maybe UTCTime
              -- ^ If we have hackage-snapshot time.
              -> IO H.HackageDB
loadHackageDB optHackageDB optHackageSnapshot = do
  dbPath <- maybe hackageTarball return optHackageDB
  readTarball optHackageSnapshot dbPath


readTarball :: Maybe UTCTime -> FilePath -> IO H.HackageDB
readTarball ts p = do
  dbu <- U.readTarball ts p
  let dbp = parseDB dbu
  return (Map.mapWithKey (H.parsePackageData dbu) dbp)


parseDB :: U.HackageDB -> P.HackageDB
parseDB = Map.mapWithKey parsePackageData

parsePackageData :: PackageName -> U.PackageData -> P.PackageData
parsePackageData pn (U.PackageData _ vs') =
  mapException (\e -> HackageDBPackageName pn (e :: SomeException)) $
   Map.mapWithKey (P.parseVersionData pn) $ vs'
