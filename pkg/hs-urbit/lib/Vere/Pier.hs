module Vere.Pier where

import ClassyPrelude

import Data.Noun
import Data.Noun.Pill
import Vere
import Vere.Pier.Types

import qualified Vere.Log as Log
import qualified Vere.Worker as Worker

ioDrivers = [] :: [IODriver]

-- This is called to make a freshly booted pier. It assigns an identity to an
-- event log and takes a chill pill.
newPier :: Pill -> FilePath -> LogIdentity -> IO Pier
newPier pill top id = do
  let logPath = top <> "/log"

  computeQueue <- newTQueueIO
  persistQueue <- newTQueueIO
  releaseQueue <- newTQueueIO

  -- What we really want to do is write the log identity and then do normal
  -- startup, but writeLogIdentity requires a full log state including
  -- input/output queues.
  logState <- Log.init logPath persistQueue (writeTQueue releaseQueue)

  -- In first boot, we need to write this!
  Log.writeLogIdentity logState id

  let logLatestEventNumber = 0
  let getEvents = Log.readEvents logState

  workerState <- Worker.startWorkerProcess

  Worker.bootWorker workerState id pill

  performCommonPierStartup workerState computeQueue persistQueue releaseQueue logState


-- This reads in a pier
runPierFromDisk :: FilePath -> IO Pier
runPierFromDisk top = do
  let logPath = top <> "/log"

  computeQueue <- newTQueueIO
  persistQueue <- newTQueueIO
  releaseQueue <- newTQueueIO

  -- What we really want to do is write the log identity and then do normal
  -- startup, but writeLogIdentity requires a full log state including
  -- input/output queues.
  logState <- Log.init logPath persistQueue (writeTQueue releaseQueue)

  -- In first boot, we need to write this!
  id <- Log.readLogIdentity logState
  logLatestEventNumber <- Log.latestEventNumber logState

  let getEvents = Log.readEvents logState

  workerState <- Worker.startWorkerProcess
  Worker.resumeWorker workerState id logLatestEventNumber getEvents

  performCommonPierStartup workerState computeQueue persistQueue releaseQueue logState


performCommonPierStartup :: Worker.Worker
                         -> TQueue Noun
                         -> TQueue (Writ [Eff])
                         -> TQueue (Writ [Eff])
                         -> LogState
                         -> IO Pier
performCommonPierStartup workerState computeQueue persistQueue releaseQueue logState = do
  for ioDrivers $ \x -> do
    bootMessage <- bornEvent x
    atomically $ writeTQueue computeQueue bootMessage

  driverThreads <- for ioDrivers $ \x -> do
    startDriver x (writeTQueue computeQueue)

  -- TODO: Don't do a bunch of extra work; we send all events to all drivers
  portingThread <- async $ do
    forever $ do
      r <- atomically (readTQueue releaseQueue)
      for_ driverThreads $ \(_, k) ->
        for_ (payload r) $ \eff ->
          k eff

  Worker.workerThread workerState

  pure (Pier{..})
