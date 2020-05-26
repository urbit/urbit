{-# OPTIONS_GHC -Wwarn #-}

{-|
    Serf Interface

    TODO: `recvLen` is not big-endian safe.
-}

module Urbit.Vere.Serf
  ( module Urbit.Vere.Serf.IPC
  , withSerf
  , execReplay
  , execSnapshot
  , execShutdown
  )
where

import Urbit.Prelude

import Data.Conduit
import System.Process
import System.ProgressBar
import Urbit.Arvo
import Urbit.Vere.Pier.Types
import Urbit.Vere.Serf.IPC
import System.Posix.Signals

import Data.Bits              (setBit)
import Data.ByteString        (hGet)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Foreign.Marshal.Alloc  (alloca)
import Foreign.Ptr            (castPtr)
import Foreign.Storable       (peek, poke)
import System.Exit            (ExitCode)
import Urbit.King.App         (HasStderrLogFunc(..))

import qualified Data.ByteString.Unsafe   as BS
import qualified Data.Conduit.Combinators as CC
import qualified Data.Text                as T
import qualified System.IO                as IO
import qualified System.IO.Error          as IO
import qualified Urbit.Ob                 as Ob
import qualified Urbit.Time               as Time
import qualified Urbit.Vere.Log           as Log


--------------------------------------------------------------------------------

-- TODO XXX HACK FIXME
data MissingBootEventsInEventLog = MissingBootEventsInEventLog Word Word
 deriving (Show, Exception)


--------------------------------------------------------------------------------

bytesNouns :: MonadIO m => ConduitT ByteString Noun m ()
bytesNouns = await >>= \case
  Nothing -> pure ()
  Just bs -> do
    noun               <- cueBSExn bs
    (mug :: Noun, bod) <- fromNounExn noun
    yield bod
    bytesNouns

withSerf :: HasLogFunc e => Config -> RAcquire e (Serf, SerfInfo)
withSerf config = mkRAcquire (io $ start config) kill
 where
  kill (serf, _) = do
    void $ rio $ execShutdown serf

{-
  TODO This needs to be thought through carfully once the callsites
  have stabilized.
-}
execShutdown :: HasLogFunc e => Serf -> RIO e ()
execShutdown serf = do
  race_ (wait2sec >> forceKill) $ do
    logTrace "Getting current serf state (taking lock, might block if in use)."
    finalState <- takeMVar (serfLock serf)
    logTrace "Got serf state (and took lock). Requesting shutdown."
    io (shutdown serf 0)
    logTrace "Sent shutdown request. Waiting for process to die."
    io $ waitForProcess (serfProc serf)
    logTrace "RIP Serf process."
 where
  wait2sec = threadDelay 5_000_000 
  forceKill = do
    logTrace "Serf taking too long to go down, kill with fire (SIGTERM)."
    io (getPid $ serfProc serf) >>= \case
      Nothing  -> do
        logTrace "Serf process already dead."
      Just pid -> do
        io $ signalProcess sigKILL pid
        io $ waitForProcess (serfProc serf)
        logTrace "Finished killing serf process with fire."
    
execSnapshot :: forall e . HasLogFunc e => Serf -> RIO e ()
execSnapshot serf = do
  logTrace "execSnapshot: taking lock"
  serfState <- takeMVar (serfLock serf)
  io (sendSnapshotRequest serf (ssLast serfState))
  logTrace "execSnapshot: releasing lock"
  putMVar (serfLock serf) serfState

execReplay
  :: forall e
   . HasLogFunc e
  => Serf
  -> Log.EventLog
  -> Maybe Word64
  -> RIO e (Maybe PlayBail)
execReplay serf log last = do
  lastEventInSnap <- io (ssLast <$> serfCurrentStateBlocking serf)
  if lastEventInSnap == 0 then doBoot else doReplay
 where
  doBoot :: RIO e (Maybe PlayBail)
  doBoot = do
    let bootSeqLen = lifecycleLen (Log.identity log)

    evs <- runConduit $ Log.streamEvents log 1
                     .| CC.take (fromIntegral bootSeqLen)
                     .| bytesNouns
                     .| CC.sinkList

    let numEvs = fromIntegral (length evs)
    let bootLn = bootSeqLen

    when (numEvs /= bootLn) $ do
      throwIO (MissingBootEventsInEventLog numEvs bootLn)

    io (bootSeq serf evs) >>= \case
      Just err -> pure (Just err)
      Nothing  -> doReplay

  doReplay :: RIO e (Maybe PlayBail)
  doReplay = do
    logTrace "Beginning event log replay"

    lastEventInSnap <- io (ssLast <$> serfCurrentStateBlocking serf)

    last & \case
        Nothing -> pure ()
        Just lt -> logTrace $ display $
                     "User requested to replay up to event #" <> tshow lt

    logLastEv :: Word64 <- fromIntegral <$> Log.lastEv log

    logTrace $ display $ "Last event in event log is #" <> tshow logLastEv

    let replayUpTo = min (fromMaybe logLastEv last) logLastEv

    let numEvs :: Int = fromIntegral replayUpTo - fromIntegral lastEventInSnap

    when (numEvs < 0) $ do
      error "impossible"

    logTrace $ display $ "Replaying up to event #" <> tshow replayUpTo
    logTrace $ display $ "Will replay " <> tshow numEvs <> " in total."

    runConduit $ Log.streamEvents log (lastEventInSnap + 1)
              .| CC.take (fromIntegral numEvs)
              .| bytesNouns
              .| replay serf
