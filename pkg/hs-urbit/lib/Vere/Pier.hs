module Vere.Pier where

import ClassyPrelude

import Data.Noun
import Data.Noun.Pill
import Vere
import Vere.Pier.Types

import qualified Vere.Log     as Log
import qualified Vere.Persist as Persist
import qualified Vere.Serf    as Serf

import Vere.Serf (Serf, EventId)


--------------------------------------------------------------------------------

ioDrivers = [] :: [IODriver]


--------------------------------------------------------------------------------

-- This is called to make a freshly booted pier. It assigns an identity to an
-- event log and takes a chill pill.
boot :: Pill -> FilePath -> LogIdentity -> IO (Serf, EventLog, EventId, Mug)
boot pill top id = do
  let logPath = top <> "/log"

  log <- Log.open logPath

  Log.writeIdent log id

  serf   <- Serf.startSerfProcess top
  (e, m) <- Serf.bootSerf serf id pill

  pure (serf, log, e, m)


{-
    What we really want to do is write the log identity and then do
    normal startup, but writeIdent requires a full log state
    including input/output queues.
-}
resume :: FilePath -> IO (Serf, EventLog, EventId, Mug)
resume top = do
  log    <- Log.open (top <> "/.urb/log")
  ident  <- Log.readIdent log
  lastEv <- Log.latestEventNumber log
  serf   <- Serf.startSerfProcess top
  (e, m) <- Serf.replay serf ident lastEv (Log.readEvents log)

  pure (serf, log, e, m)

{-
performCommonPierStartup :: Serf.Serf
                         -> TQueue Ovum
                         -> TQueue (Writ [Eff])
                         -> TQueue (Writ [Eff])
                         -> LogState
                         -> IO Pier
performCommonPierStartup serf computeQ persistQ releaseQ logState = do
  for ioDrivers $ \x -> do
    bootMessage <- bornEvent x
    atomically $ writeTQueue computeQ bootMessage

  driverThreads <- for ioDrivers $ \x -> do
    startDriver x (writeTQueue computeQ)

  -- TODO: Don't do a bunch of extra work; we send all events to all drivers
  portingThread <- async $ do
    forever $ do
      r <- atomically (readTQueue releaseQ)
      for_ driverThreads $ \(_, k) ->
        for_ (payload r) $ \eff ->
          k eff

  Serf.workerThread serf (readTQueue computeQ) undefined

  pure (Pier{..})
-}
