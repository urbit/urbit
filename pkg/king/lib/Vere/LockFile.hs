module Vere.LockFile (lockFile) where

import UrbitPrelude

import Data.Default                (def)
import RIO.Directory               (createDirectoryIfMissing)
import System.IO.LockFile.Internal (LockingParameters(..), RetryStrategy(..),
                                    lock, unlock)

--------------------------------------------------------------------------------

lockFile :: HasLogFunc e => FilePath -> RAcquire e ()
lockFile pax = void $ mkRAcquire start stop
  where
    fil = pax <> "/.vere.lock"

    stop handle = do
        logInfo $ display @Text $ ("Releasing lock file: " <> pack fil)
        io $ unlock fil handle

    params = def { retryToAcquireLock = No }

    start = do
        createDirectoryIfMissing True pax
        logInfo $ display @Text $ ("Taking lock file: " <> pack fil)
        io (lock params fil)
