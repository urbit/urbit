module Network.Ames.Types where

import Urbit.Prelude

import Data.List.NonEmpty (NonEmpty) --((:|)))

import qualified Data.List.NonEmpty as NE

-- Let's just start making data structures.

-- The main operations in ames are %hear task (a new packet from unix) which
-- sends a %boon gift to a vane inside Urbit, and %plea (request to send a
-- message outbound) which emits a %send task to unix. The general shape of
-- this interface can be kept, modulo the details of %hear/%send, which instead
-- of having a `=lane` should have an urbit level identity instead of an udp ip
-- level identity, and =blob, which should be decrypted at this point.

-- My naive reading is that the things before a peer core should go in king
-- ames, while the peer-core specific things should stay inside urbit. ie, in
-- +on-hear-shut, we do a bunch of things to decrypt the packet and that should
-- go in the king, while the calling of +on-hear-shut-packet:peer-core should
-- stay inside of Urbit.

-- Basic Data Structures -------------------------------------------------------

-- The life number of a key.
data Life = Life UD
  deriving (Eq, Ord, Show)

-- An network key is a ship and a life, which corresponds to a unique
-- public/private keypair.
data ShipLife = ShipLife Ship Life
  deriving (Eq, Ord, Show)

instance ToNoun ShipLife where
  toNoun (ShipLife ship (Life life)) = toNoun (ship, life)

instance FromNoun ShipLife where
  parseNoun n = named "ShipLife" $ do
    (ship, life) <- parseNoun n
    pure (ShipLife ship (Life life))

-- A message comes from a MsgSource, which is one or more identified ships.
data MsgSource = MsgSource (NonEmpty ShipLife)
  deriving (Eq, Ord, Show)

instance ToNoun MsgSource where
  toNoun (MsgSource x) = toNoun $ toList x

instance FromNoun MsgSource where
  parseNoun n = named "MsgSource" $ do
    l :: [ShipLife] <- parseNoun n
    pure (MsgSource $ NE.fromList l)

-- A message can only be delivered to a single identified target: a ship/lyfe
-- pair.
data MsgDest = MsgDest ShipLife
  deriving (Eq, Ord, Show)

instance ToNoun MsgDest where
  toNoun (MsgDest sl) = toNoun sl

instance FromNoun MsgDest where
  parseNoun n = named "MsgDest" $ do
    sl <- parseNoun n
    pure (MsgDest sl)

-- Transport Layer Definitions -------------------------------------------------

-- A route is a destination address that a transport can send a message to.
-- rtAddr only has semantic meaning for the transport rtTransport.
data Route = Route
  { rtTransport :: String
  , rtAddr      :: String
  }
  deriving (Ord, Eq, Show)

instance ToNoun Route where
  toNoun (Route transport addr) = toNoun (transport, addr)

instance FromNoun Route where
  parseNoun n = named "Route" $ do
    (transport, addr) <- parseNoun n
    pure (Route transport addr)

-- Attempting to send a message will result in one of the following results.
data TransportSendResult
  = TSRDelivered Bool
    -- ^ The remote end acknowledged the message, either positively or
    -- negatively, and the end-to-end property is satisfied; we can remove it
    -- from our outgoing message store.
  | TSRFAIL
    -- ^ The transport failed to route the message, we should try another one,
    -- or wait a while and try again.

-- A transport is a module handle which sends/receives a message over some
-- specific protocol.
data Transport = Transport
  { tName :: String
    -- ^ Name of the transport

  , tSend :: Route -> Atom -> IO TransportSendResult
    -- ^ Function to send a message outbound to a route.

  , tReceive :: TQueue (Route, Atom)
    -- ^ The transport will place incoming messages into this queue, which
    -- will be drained by the Router.
  }

-- Writing ------------------------

-- A writer is an interface for a thing which wishes to send and receive
-- natural numbers on the network.
data Writer = Writer
  { wRecvMsg :: MsgSource -> MsgDest -> Atom -> (Bool -> STM ()) -> IO ()
    -- ^ The writer receives an inbound message from a MsgDest.

  , wPrivateKey :: ShipLife -> ByteString
    -- ^ Provide the rest of the system with the private key for the ship, so
    -- that the Router has the private key to encrypt the message if necessary.
  }

-- Router ------------------------

-- The router is the central component that mediates between the Transports and
-- the Writers.

data Router = Router


-- The Writer wishes to send a message as a MsgSource. Returns an stm function
-- which will be called when/if the remote end signals that it acknowledges the
-- message or sends a naxplenation.
type WriterSendMsg = MsgSource -> MsgDest -> Atom -> (Bool -> STM ())
                  -> IO ()

-- The Writer calls this to add itself into the router
type WriterJoinRouter = Writer -> ShipLife -> IO ()

type WriterLeaveRouter = ShipLife -> IO ()

-- The API given to the Writer to access Router functionality
data AmesRouterWriterApi = AmesRouterWriterApi
  { arwaSend :: WriterSendMsg
    -- ^ Function the Writer should call to send an outbound message.

  , arwaJoinRouter :: WriterJoinRouter
  , arwaLeaveRouter :: WriterLeaveRouter
    -- ^ Functions with a writer calls to join and leave the router.
  }

-- TODO: Everything about this.
data AmesTransportRouterApi = AmesTransportRouterApi

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-

The problem: we want to actually support both actor model passing
(confirmations of at-least-once delivery are handled by the runtime) and
current Ames behaviour (messags resolve in an ack/nack inside of ames).

The conceptual interface is `Atom -> Bool`, where the bool is whether the
request succeeded or failed. Naxplanations, as in current ames, are sent as
additional packets, which can only be acked. Notably, Ted was really right when
he was talking about naxplanations having to be separate messages from the
actual nack message. That was subtle and I didn't totally get that before.

Current Urbit on King Ames:

  [send effect] -> {local send msg} -> {remote queues message}
                                    -> [remote runs ok, emits ack effect]
                                    <- {remote uses ack effect to ack msg]
                -> {local takes ack effect and enqueues ack event in local}

  [send effect] -> {local send msg} -> {remote queues message}
                                    -> [remote crashes and replaces, emits nack
                                        effect, naxplanation effect]
                                    <- {remote uses ack effect to nack msg]
                                    <- {remote sends naxplanation message}
                -> {local takes nack effect and enqueues nack event in local}
                -> {local enqueues naxplanation msg}
                   -> {works}
                   -> {sends nackplanation message} -> {remote queues}
                                                    -> [remote works because has
                                                        to, emits ack]
                                                    <- {remote uses ack effect
                                                        to ack msg}
                -> {local takes ack effect and queues ack event in local}

`Network.Ames.SimpleLibrary`:

The simple library just has an lmdb database of unacknowledged messages and a
function supplied by the user.

  <receives a recv call> -> <runs an (Atom -> Either [Text] ()) function>
                         -> <returned (Right ())>
                         -> [sends positive ack back]

  <receives a recv call> -> <runs an (Atom -> Either [Text] ()) function>
                         -> <returned (Left errmsg)>
                         <- {sends negative ack back}
                         <- {sends naxplanation msg as msg}
                         -> {receives naxplanation ack, marks message committed}

  <sends a msg> -> {round trip}
                -> {library receives positive ack. marks as committed}

  <sends a msg> -> {round trip}
                -> {library receives negative ack. marks as committed}
                -> {library receives naxplanation. calls the naxplanation
                    function}

SKEW actor:

  [send effect] -> {local send msg} -> {remote recv msg}
                                    -> [remote runs ok]
                                    -> {remote sends ack}
                -> {local notes ack and marks message as handled}

  [send effect] -> {local send msg} -> {remote recv msg}
                                    -> [remote crash]
                                    -> {remote sends nack}
                                    -> {remote enqueues nackplanation message}
                -> {local receives nack and marks message as handled}
                -> {local queues up nackplanation message, must succeed}
                -> {local sends positve ack re: naxplanation to remote}
                                    -> {remote marks naxplanation as handled}

This looks general enough to be implementable everywhere under different
persistence regiemes.

-}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- TODO: The following is scratch thinking about messages between routers that
-- I'd need to handle

-- A list of messages sent between KingAmes environments. These do not
-- correlate to messages that are sent into Urbit itself.
data KingAmesMsg
  = KAMCometIntroduction
    -- ^ A comet introduces itself, equivalent to `+$open-packet` and
    -- `+on-hear-open`.

  | KAMRouteRequest ShipLife
    -- ^ One side of the pipe asks for known routes to ShipLife
  | KAMRouteResponse ShipLife [Route]
    -- ^ The other side replies, gossiping about routes

  -- TODO: Should we gossip about keys, or should we just always read Azimuth?

  | KAMSend ByteString

-- Things which get moved entirely into the king and which never touch your
-- Urbit:
--
--  - +on-hear-open / comet plaintext self-attestation
