module Urbit.Vere.AmesNockWriter where

import Urbit.Prelude

import Urbit.Arvo.Effect
import Urbit.Arvo.Event
import Urbit.King.App
import Network.Ames.Types
import Urbit.Vere.Pier.Types

import Control.Concurrent.STM.TVar (stateTVar)

import qualified Data.Map as M

-- The Nock Writer

-- This file defines the Urbit Nock 4K implementation of the Writer interface
-- for the King Ames system. A Writer deals with the details of durable message
-- persistence, and the NockWriter takes details of message sending and
-- receiving and puts them


data NockWriter = NockWriter
  { nwUnackedMsgs :: TVar (Map UD (Bool -> STM ()))
  , nwNextRecvNum :: TVar UD
  }

nockWriter'
  :: (HasPierEnv e, HasNetworkConfig e, HasKingId e)
  => AmesRouterWriterApi
  -> ShipLife
  -> RIO e ([Ev], RAcquire e (DriverApi KamsEf))
nockWriter' api who = do
  ventQ :: TQueue EvErr <- newTQueueIO
  env <- ask
  let (bornEvs, startDriver) = nockWriter env api who (writeTQueue ventQ)

  let runDriver = do
        diOnEffect <- startDriver
        let diEventSource = fmap RRWork <$> tryReadTQueue ventQ
        pure (DriverApi {..})

  pure (bornEvs, runDriver)


-- The Nock Writer is a per-ship IO driver which plugs into the per-King Router
-- infrastructure. It takes the message sending function from the Router and
-- handles the details of the
--
-- TODO: Probably need to pass in a pier instead so we can figure out the
-- ShipLife from the ship configuration instead of hard coding 1.
nockWriter :: forall e
            . (HasLogFunc e, HasNetworkConfig e, HasKingId e)
           => e
           -> AmesRouterWriterApi
           -> ShipLife
           -> (EvErr -> STM ())
           -> ([Ev], RAcquire e (KamsEf -> IO ()))
nockWriter env api who enqueueEv = (initialEvs, runWriter)
  where
    -- Tell the ames system to resend any unacknowledged messages.
    kingid = fromIntegral (env ^. kingIdL)
    -- initialEvs = [EvBlip $ BlipEvKams $ KamsEvBorn (fromIntegral kingid, ()) ()]
    initialEvs = []

    runWriter :: RAcquire e (KamsEf -> IO ())
    runWriter = handleEf <$> mkRAcquire joinRouter leaveRouter
      where
        joinRouter = do
          nw <- NockWriter <$> newTVarIO mempty <*> newTVarIO 0
          let w = Writer (recv nw) key
          io $ ((arwaJoinRouter api) w who)
          pure nw

        leaveRouter x = io $ ((arwaLeaveRouter api) who)

    handleEf :: NockWriter -> KamsEf -> IO ()
    handleEf writer = runRIO env . \case
      KamsEfSend (_, _) msgNum source dest msg -> do
        logInfo $ displayShow ("(nock writer) send: ", msgNum, source, dest)

        io $ (arwaSend api) source dest msg callback
        where
          callback response = enqueueEv (EvErr (ackEv response) ignoreResult)
          ackEv response = EvBlip $ BlipEvKams $ KamsEvAck () msgNum response
          ignoreResult _ = pure ()

      KamsEfAck (_, _) (thisKingid, msgNum) ack -> do
        logInfo $ displayShow ("(nock writer) acknowledging: ", msgNum, ack)
        when (kingid == (fromIntegral thisKingid)) $ do
          mybCallback <- atomically $ do
            stateTVar (nwUnackedMsgs writer)
              (M.updateLookupWithKey (\k v -> Nothing) msgNum)

          case mybCallback of
            Nothing -> logDebug $
              displayShow ("(nock writer) missing callback for ", msgNum)
            Just callback -> atomically $ callback ack

    -- Called by the router when this ship receives a message. We enqueue a
    -- message and write to the completeVar when we have an answer of whether
    -- this message completed or not.
    recv :: NockWriter
         -> MsgSource
         -> MsgDest
         -> Atom
         -> (Bool -> STM ())
         -> IO ()
    recv nw src dst msg completeVar = do
      recvNum <- atomically $ do
        cur <- readTVar (nwNextRecvNum nw)
        writeTVar (nwNextRecvNum nw) (cur + 1)
        pure cur

      atomically $ do
        enqueueEv (EvErr (recvEv recvNum) ignoreResult)
        modifyTVar (nwUnackedMsgs nw) (M.insert recvNum completeVar)
      where
        recvEv recvNum = EvBlip $ BlipEvKams $
          KamsEvHear () (kingid, recvNum) src msg
        ignoreResult _ = pure ()

    -- The first thing the Router does is ask the Writer what the private key
    -- it wants to use to encrypt messages which go out on Transports. (Note:
    -- messages are not encrypted in the local only case where both ship's
    -- Writers are connected to the same router.)
    key ship = mempty
