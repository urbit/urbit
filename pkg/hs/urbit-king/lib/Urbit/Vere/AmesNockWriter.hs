module Urbit.Vere.AmesNockWriter where

import Urbit.Prelude

import Urbit.Vere.KingAmes
--import Urbit.Vere.Pier.Types

-- The Nock Writer

-- This file defines the Urbit Nock 4K implementation of the Writer interface
-- for the King Ames system. A Writer deals with the details of durable message
-- persistence, and the NockWriter takes details of message sending and
-- receiving and puts them


-- Ames Events/Effects ---------------------------------------------------------

data AmesNockWriterEf
    = AmesNockWriterEfSend (Atom, ()) UD MsgSource MsgDest Atom
      -- ^ Sends a plaintext message to the destination as one of a set of
      -- azimuth ids. The message will be encrypted by the king using the
      -- ship's key scryed out of the ship, if appropriate.
  deriving (Eq, Ord, Show)

data AmesNockWriterEv
    = AmesNockWriterEvHear () UD MsgSource Atom
      -- ^ NockWriter sends a %hear event to Urbit when hearing an event on the
      -- network. The message will be decrypted by the king using the ship's
      -- key scryed out of the ship, if needed.

    | AmesNockWriterEvAck () UD (Maybe Atom)
      -- ^ NockWriter sends an %ack event to Urbit when hearing either a
      -- positive or negative acknowledgement relayed by the router.
  deriving (Eq, Ord, Show)


data NockWriter = NockWriter
  { nwUnackedMsgs :: TVar [(TMVar (Maybe Atom), UD)]
  }


-- The Nock Writer is a per-ship IO driver which plugs into the per-King Router
-- infrastructure. It takes the message sending function from the Router and
-- handles the details of the
--
-- TODO: Write the ' version of this.
nockWriter :: forall e
            . (HasLogFunc e)
           => e
           -> AmesRouterWriterApi
           -> Ship
           -> ([AmesNockWriterEv], RAcquire e (AmesNockWriterEf -> IO ()))
nockWriter env api who = (initialEvs, runWriter)
  where
    -- TODO: This isn't going to work for the restart case. We'll have to
    -- figure out how to make this take 
    initialEvs = []

    runWriter :: RAcquire e (AmesNockWriterEf -> IO ())
    runWriter = handleEf <$> mkRAcquire joinRouter leaveRouter
      where
        joinRouter = do
          let w = Writer recv key restart
          io $ ((arwaJoinRouter api) w who)
          NockWriter <$> newTVarIO []

        leaveRouter x = do
          io $ ((arwaLeaveRouter api) who)
          pure ()

    handleEf :: NockWriter -> AmesNockWriterEf -> IO ()
    handleEf writer = runRIO env . \case
      AmesNockWriterEfSend (_, _) msgNum source dest msg -> do
        logDebug $ displayShow ("(nock writer) send: ", msgNum, source, dest)

        -- Send the message to the router, receiving a signaling variable which
        -- will be filled when the message has been acknowledged remotely.
        responseVar <- io $ (arwaSend api) source dest msg

        -- Record the responseVar that will be signaled when the message is
        -- acknowledged.
        io $ atomically $ modifyTVar
          (nwUnackedMsgs writer)
          (\x -> snoc x (responseVar, msgNum))

    -- Called by the router when this ship receives a message
    recv a b c = do
      -- TODO: Unsure how to write this at first glance. We're receiving a
      -- message from another subsystem, which wants a synchronous response
      -- since

      -- TODO: This always is immediately acknowledging the message, even
      -- though that's not what should go on here?
      pure Nothing

    -- The first thing the Router does is ask the Writer what the private key
    -- it wants to use to encrypt messages which go out on Transports. (Note:
    -- messages are not encrypted in the local only case where both ship's
    -- Writers are connected to the same router.)
    key ship = Nothing

    -- The second thing the Router does on connection is to signal to the ship
    -- to resend all unacknowledged messages.
    restart = pure ()
