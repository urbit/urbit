module Urbit.Ames where

import ClassyPrelude

import Arvo
import Data.Acquire
import Network.Socket            hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Noun

import Control.Concurrent (threadDelay)
import Control.Lens       ((&))

import qualified Urbit.Time as Time

--------------------------------------------------------------------------------

{-
  On startup (u3_ames_ef_bake):
    *_ef_bake means "send any initial events"
    Send event: [//newt/u3A->sen [%barn ~]]

  On driver init (u3_ames_io_init):
    Basically just allocation.
    Set %wake timer.
    Record that the UDP listener is not running.

  u3_ames_ef_turf: Called on turf effect.
    If we're not live then start the listener.
    For now, just use the first turf in the list.
    Turf is TLD-first domain name
      /org/urbit/dns -> dns.urbit.org

    TODO If we're not live, we should always drop packet sends.

  On u3_ames_io_talk?
    *_io_talk is called after everything is up.
    Does nothing.
    (Normally, this would be where you bring up the UDP listener)
    TODO If we're not live, we should always drop packet sends.

  On driver shutdown:
      Kill the timer (TODO what is the timer for?)
      uv_close(&sam_u->had_u, 0);
-}

-- TODO Move these to a common module ------------------------------------------

type QueueEv = Ev -> STM ()

type EffCb a = a -> IO ()

newtype KingInstance = KingInst Atom
  deriving newtype (Eq, Ord, Num, Real, Enum, Integral, FromNoun, ToNoun)

--------------------------------------------------------------------------------

data AmesDrv = AmesDrv
  { aIsLive    :: IORef Bool
  , aWakeTimer :: Async ()
  , aListener  :: Async ()
  }

--------------------------------------------------------------------------------

{-
    inst      -- Process instance number.
    who       -- Which ship are we?
    enqueueEv -- Queue-event action.
    mPort     -- Explicit port override from command line arguments.

    We ignore the %turf arguments for now. We only have fake ships,
    so we don't implement the DNS stuff yet.

    TODO Handle socket exceptions in waitPacket

    4096 is a reasonable number for recvFrom. Packets of that size are
    not possible on the internet.

    TODO log when `sendTo` sent fewer bytes than requested.

    TODO verify that the KingInstances match on effects.
-}
ames :: KingInstance -> Ship -> Maybe Port -> QueueEv
     -> ([Ev], Acquire (EffCb NewtEf))
ames inst who mPort enqueueEv =
    ([barnEv], callback . aIsLive <$> mkAcquire start stop)
  where
    start :: IO AmesDrv
    start = do
        vLiv <- newIORef False
        time <- async runTimer
        hear <- async waitPacket
        pure $ AmesDrv vLiv time hear

    stop :: AmesDrv -> IO ()
    stop (AmesDrv{..}) = do
        cancel aWakeTimer
        cancel aListener

    barnEv, wakeEv :: Ev
    barnEv = EvBlip $ BlipEvNewt $ NewtEvBarn (fromIntegral inst, ()) ()
    wakeEv = EvBlip $ BlipEvAmes $ AmesEvWake () ()

    hearEv :: Time.Wen -> PortNumber -> HostAddress -> ByteString -> Ev
    hearEv w p a bs = EvBlip $ BlipEvAmes $ AmesEvHear () lane (MkBytes bs)
      where lane = If w (fromIntegral p) a

    runTimer :: IO ()
    runTimer = forever $ do
        threadDelay (300 * 1000000) -- 300 seconds
        atomically (enqueueEv wakeEv)

    ourPort :: PortNumber
    ourPort = mPort & \case Nothing -> shipPort who
                            Just p  -> fromIntegral p

    waitPacket :: IO ()
    waitPacket = do
        s  <- socket AF_INET Datagram defaultProtocol
        () <- bind s (SockAddrInet ourPort localhost)
        forever $ do
            (bs, addr) <- recvFrom s 4096
            wen        <- Time.now
            case addr of
              SockAddrInet p a -> atomically $ enqueueEv $ hearEv wen p a bs
              _                -> pure ()

    callback :: IORef Bool -> NewtEf -> IO ()
    callback vLive = \case
      NewtEfTurf (_id, ()) turfs ->
          writeIORef vLive True

      NewtEfSend (_id, ()) lane (MkBytes bs) -> do
          live <- readIORef vLive
          when live $ do
              s  <- socket AF_INET Datagram defaultProtocol
              laneSockAddr lane & \case
                  Nothing -> pure ()
                  Just sa -> void (sendTo s bs sa)

localhost :: HostAddress
localhost = tupleToHostAddress (127,0,0,1)

laneSockAddr :: Lane -> Maybe SockAddr
laneSockAddr = \case
  If _ p     a -> pure (SockAddrInet (fromIntegral p) a)
  Ix _ p     a -> pure (SockAddrInet (fromIntegral p) a)
  Is _ mLane _ -> mLane >>= laneSockAddr

ipv4Addr :: SockAddr -> Maybe (PortNumber, HostAddress)
ipv4Addr = \case
    SockAddrInet p a -> Just (p, a)
    _                -> Nothing

shipPort :: Ship -> PortNumber
shipPort s | s < 256 = fromIntegral (31337 + s)
shipPort _           = 0

{-
data GalaxyInfo = GalaxyInfo { ip :: IPv4, age :: Time.Unix }

turf :: Ames -> [VA.Turf] -> IO Ames
turf ames []       = undefined
turf ames (turf:_) = do
  let t = (mconcat . intersperse "." . fmap unCord . VA.unTurf) turf
  pure (ames {globalDomain = Just t})

data NetworkMode
  = LocalOnlyNetworking
  | GlobalNetworking

computePort :: NetworkMode -> Atom -> Int
computePort LocalOnlyNetworking who = 31337 + (fromIntegral who)
computePort GlobalNetworking who    = 13337 + (fromIntegral who)
-}
