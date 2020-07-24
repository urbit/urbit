module Network.Ames.Types where

import Urbit.Prelude

import Data.List.NonEmpty (NonEmpty) --((:|)))

import Control.Concurrent.STM.TMVar ()

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

-- A message comes from a MsgSource, which is one or more identified ships.
data MsgSource = MsgSource (NonEmpty ShipLife)
  deriving (Eq, Ord, Show)

-- A message can only be delivered to a single identified target: a ship/lyfe
-- pair.
data MsgDest = MsgDest ShipLife
  deriving (Eq, Ord, Show)

-- Transport Layer Definitions -------------------------------------------------

-- A route is a destination address that a transport can send a message to.
-- rtAddr only has semantic meaning for the transport rtTransport.
data Route = Route
  { rtTransport :: String
  , rtAddr      :: String
  }

-- TODO: Write this instance; routes must be noun serializable
--instance ToNoun Route where

-- Attempting to send a message will result in one of the following results.
data TransportSendResult
  = TSROK
    -- ^ The remote end acknowledged the message, and the end-to-end property
    -- is satisfied; we can remove it from our outgoing message store.
  | TSRNACK ByteString
    -- ^ The remote end failed with the above nacksplanation, which must be
    -- sent back to the sender.
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
  { wRecvMsg :: MsgSource -> MsgDest -> Atom -> IO (Maybe Atom)
    -- ^ The writer receives an inbound message from a MsgDest.

  , wPrivateKey :: ShipLife -> ByteString
    -- ^ Provide the rest of the system with the private key for the ship, so
    -- that the Router has the private key to encrypt the message if necessary.

  , wRestart :: IO ()
    -- ^ Called on startup to restore whatever unacknowledged messages were
    -- never acknowledged.
  }

-- Router ------------------------

-- The router is the central component that mediates between the Transports and
-- the Writers.

data Router = Router


-- The Writer wishes to send a message as a MsgSource. Returns a TMVar
-- which will be written to when/if the remote end signals that it
-- acknowledges the message or sends a naxplenation.
type WriterSendMsg = MsgSource -> MsgDest -> Atom -> IO (TMVar (Maybe Atom))

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


