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


-- Run Pier --------------------------------------------------------------------

{-
/* _pier_work_save(): tell worker to save checkpoint.
*/
static void
_pier_work_save(u3_pier* pir_u)
{
  u3_controller* god_u = pir_u->god_u;
  u3_disk* log_u = pir_u->log_u;
  u3_save* sav_u = pir_u->sav_u;

  c3_assert( god_u->dun_d == sav_u->req_d );
  c3_assert( log_u->com_d >= god_u->dun_d );

  {
    u3_noun mat = u3ke_jam(u3nc(c3__save, u3i_chubs(1, &god_u->dun_d)));
    u3_newt_write(&god_u->inn_u, mat, 0);

    //  XX wait on some report of success before updating?
    //
    sav_u->dun_d = sav_u->req_d;
  }

  //  if we're gracefully shutting down, do so now
  //
  if ( u3_psat_done == pir_u->sat_e ) {
    _pier_exit_done(pir_u);
  }
}
-}



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
