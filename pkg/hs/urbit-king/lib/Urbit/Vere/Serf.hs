{-# OPTIONS_GHC -Wwarn #-}

{-|
    Serf Interface

    TODO: `recvLen` is not big-endian safe.
-}

module Urbit.Vere.Serf
  ( module Urbit.Vere.Serf.IPC
  , withSerf
  , execReplay
  , shutdown
  , snapshot
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

withSerf :: HasLogFunc e => Config -> RAcquire e Serf
withSerf config = mkRAcquire (io $ fmap fst $ start config) kill
 where
  kill serf = do
    void $ rio $ shutdown serf

execReplay
  :: forall e
   . HasLogFunc e
  => Serf
  -> Log.EventLog
  -> Maybe Word64
  -> RIO e (Maybe PlayBail)
execReplay serf log last = do
  lastEventInSnap <- io (serfLastEventBlocking serf)
  if lastEventInSnap == 0 then doBoot else doReplay
 where
  doBoot :: RIO e (Maybe PlayBail)
  doBoot = do
    logTrace "Beginning boot sequence"

    let bootSeqLen = lifecycleLen (Log.identity log)

    evs <- runConduit $ Log.streamEvents log 1
                     .| CC.take (fromIntegral bootSeqLen)
                     .| bytesNouns
                     .| CC.sinkList

    let numEvs = fromIntegral (length evs)

    when (numEvs /= bootSeqLen) $ do
      throwIO (MissingBootEventsInEventLog numEvs bootSeqLen)

    io (bootSeq serf evs) >>= \case
      Just err -> pure (Just err)
      Nothing  -> doReplay

  doReplay :: RIO e (Maybe PlayBail)
  doReplay = do
    logTrace "Beginning event log replay"

    lastEventInSnap <- io (serfLastEventBlocking serf)

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
