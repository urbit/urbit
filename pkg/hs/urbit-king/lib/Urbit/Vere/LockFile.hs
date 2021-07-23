{-|
    Acquire and release the vere lockfile.
-}

module Urbit.Vere.LockFile (lockFile) where

import Urbit.Prelude

import Urbit.King.App.Class

import Data.Default                (def)
import RIO.Directory               (createDirectoryIfMissing)
import System.IO.LockFile.Internal (LockingParameters(..), RetryStrategy(..),
                                    LockingException(..), lock, unlock)

--------------------------------------------------------------------------------

lockFile :: (HasLogFunc e, HasStderrLogFunc e) => FilePath -> RAcquire e ()
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
        handle failure $ io (lock params fil)

    failure (e :: LockingException) = do
      logStderr $ logError $ display @Text $
        "Cannot acquire lock file " <> pack fil <> "."
      logStderr $ logError $
        "Please make sure there are no other instances of this ship running, "
        <> "then try again."
      logStderr $ logError $
        "If you are sure, you can delete the file and try again."
      throwIO e

logStderr :: HasStderrLogFunc e => RIO LogFunc a -> RIO e a
logStderr action = do
  logFunc <- view stderrLogFuncL
  runRIO logFunc action
