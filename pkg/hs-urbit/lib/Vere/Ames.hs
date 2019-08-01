module Vere.Ames (ames) where

import UrbitPrelude

import Arvo                      hiding (Fake)
import Network.Socket            hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Vere.Pier.Types

import Control.Concurrent (threadDelay)

import qualified Urbit.Time as Time


-- Types -----------------------------------------------------------------------

data AmesDrv = AmesDrv
  { aIsLive    :: IORef Bool
  , aSocket    :: Socket
  , aWakeTimer :: Async ()
  , aListener  :: Async ()
  }

data NetworkMode = Fake | Real
  deriving (Eq, Ord, Show)

{-
data GalaxyInfo = GalaxyInfo { ip :: Ipv4, age :: Time.Unix }
  deriving (Eq, Ord, Show)
-}


-- Utils -----------------------------------------------------------------------

galaxyPort :: NetworkMode -> Galaxy -> PortNumber
galaxyPort Fake (Galaxy g) = fromIntegral g + 31337
galaxyPort Real (Galaxy g) = fromIntegral g + 13337

listenPort :: NetworkMode -> Ship -> PortNumber
listenPort m s | s < 256 = galaxyPort m (fromIntegral s)
listenPort m _           = 0

localhost :: HostAddress
localhost = tupleToHostAddress (127,0,0,1)

okayFakeAddr :: AmesDest -> Bool
okayFakeAddr = \case
    ADGala _ _          -> True
    ADIpv4 _ p (Ipv4 a) -> a == localhost

destSockAddr :: NetworkMode -> AmesDest -> SockAddr
destSockAddr m = \case
    ADGala _ g   -> SockAddrInet (galaxyPort m g) localhost
    ADIpv4 _ p a -> SockAddrInet (fromIntegral p) (unIpv4 a)

barnEv :: KingInstance -> Ev
barnEv inst =
  EvBlip $ BlipEvNewt $ NewtEvBarn (fromIntegral inst, ()) ()

wakeEv :: Ev
wakeEv =
  EvBlip $ BlipEvAmes $ AmesEvWake () ()

hearEv :: Time.Wen -> PortNumber -> HostAddress -> ByteString -> Ev
hearEv w p a bs =
    EvBlip $ BlipEvAmes $ AmesEvHear () dest (MkBytes bs)
  where
    dest = ADIpv4 w (fromIntegral p) (Ipv4 a)

_turfText :: Turf -> Text
_turfText = intercalate "." . reverse . fmap unCord . unTurf


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
    (initialEvents, runAmes)
  where
    initialEvents :: [Ev]
    initialEvents = [barnEv inst]

    runAmes :: Acquire (EffCb NewtEf)
    runAmes = do
        drv <- mkAcquire start stop
        pure (handleEffect drv)

    start :: IO AmesDrv
    start = do
        vLiv <- newIORef False
        time <- async runTimer
        sock <- bindSock
        hear <- async (waitPacket sock)
        pure $ AmesDrv vLiv sock time hear

    netMode :: NetworkMode
    netMode = Fake

    stop :: AmesDrv -> IO ()
    stop (AmesDrv{..}) = do
        cancel aWakeTimer
        cancel aListener
        close' aSocket

    runTimer :: IO ()
    runTimer = forever $ do
        threadDelay (300 * 1000000) -- 300 seconds
        atomically (enqueueEv wakeEv)

    bindSock :: IO Socket
    bindSock = do
        let ourPort = maybe (listenPort netMode who) fromIntegral mPort
        s  <- socket AF_INET Datagram defaultProtocol
        () <- bind s (SockAddrInet ourPort localhost)
        pure s

    waitPacket :: Socket -> IO ()
    waitPacket s = forever $ do
        (bs, addr) <- recvFrom s 4096
        wen        <- Time.now
        case addr of
            SockAddrInet p a -> atomically (enqueueEv $ hearEv wen p a bs)
            _                -> pure ()

    handleEffect :: AmesDrv -> NewtEf -> IO ()
    handleEffect AmesDrv{..} = \case
      NewtEfTurf (_id, ()) turfs -> do
          writeIORef aIsLive True

      NewtEfSend (_id, ()) dest (MkBytes bs) -> do
          live <- readIORef aIsLive
          when live $ do
              when (netMode == Real || okayFakeAddr dest) $ do
                  void $ sendTo aSocket bs $ destSockAddr netMode dest
