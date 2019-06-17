module Vere.Pier where

import ClassyPrelude
import Vere.Pier.Types
import qualified Vere.Log as Log


-- This is ugly and wrong
newPier :: FilePath -> LogIdentity -> IO Pier
newPier top id = do
  let logPath = top <> "/log"

  computeQueue <- newTQueueIO
  persistQueue <- newTQueueIO
  releaseQueue <- newTQueueIO

  -- What we really want to do is write the log identity and then do normal
  -- startup, but writeLogIdentity requires a full log state including
  -- input/output queues.
  logState <- Log.init logPath persistQueue (writeTQueue releaseQueue)

  Log.writeLogIdentity logState id

  pure (Pier{..})


restartPier :: FilePath -> IO Pier
restartPier top = do
  let logPath = top <> "/log"

  computeQueue <- newTQueueIO
  persistQueue <- newTQueueIO
  releaseQueue <- newTQueueIO

  logState <- Log.init logPath persistQueue (writeTQueue releaseQueue)

  -- When we create a worker, we should take arguments indicating the identity.

  pure (Pier{..})

