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


-- Lane Destinations -----------------------------------------------------------


-- TODO Move these to a common module ------------------------------------------

type QueueEv = Ev -> STM ()

type EffCb a = a -> IO ()

newtype KingInstance = KingInst Atom
  deriving newtype (Eq, Ord, Num, Real, Enum, Integral, FromNoun, ToNoun)


-- Utils -----------------------------------------------------------------------

data AmesDrv = AmesDrv
  { aIsLive    :: IORef Bool
  , aSocket    :: Socket
  , aWakeTimer :: Async ()
  , aListener  :: Async ()
  }

galaxyPort :: Galaxy -> PortNumber
galaxyPort (Galaxy g) = fromIntegral g + 31337

listenPort :: Ship -> PortNumber
listenPort s | s < 256 = galaxyPort (fromIntegral s)
listenPort _           = 0

localhost :: HostAddress
localhost = tupleToHostAddress (127,0,0,1)

okayFakeAddr :: AmesDest -> Bool
okayFakeAddr = \case
    ADGala _ _          -> True
    ADIpv4 _ p (Ipv4 a) -> a == localhost

destSockAddr :: AmesDest -> SockAddr
destSockAddr = \case
    ADGala _ g   -> SockAddrInet (galaxyPort g) localhost
    ADIpv4 _ p a -> SockAddrInet (fromIntegral p) (unIpv4 a)

ipv4Addr :: SockAddr -> Maybe (PortNumber, HostAddress)
ipv4Addr = \case
    SockAddrInet p a -> Just (p, a)
    _                -> Nothing

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
    initialEvents = [barnEv]

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

    stop :: AmesDrv -> IO ()
    stop (AmesDrv{..}) = do
        cancel aWakeTimer
        cancel aListener
        close' aSocket

    barnEv :: Ev
    barnEv = EvBlip $ BlipEvNewt $ NewtEvBarn (fromIntegral inst, ()) ()

    wakeEv :: Ev
    wakeEv = EvBlip $ BlipEvAmes $ AmesEvWake () ()

    hearEv :: Time.Wen -> PortNumber -> HostAddress -> ByteString -> Ev
    hearEv w p a bs = EvBlip $ BlipEvAmes $ AmesEvHear () dest (MkBytes bs)
      where dest = ADIpv4 w (fromIntegral p) (Ipv4 a)

    runTimer :: IO ()
    runTimer = forever $ do
        threadDelay (300 * 1000000) -- 300 seconds
        atomically (enqueueEv wakeEv)

    ourPort :: PortNumber
    ourPort = mPort & \case Nothing -> listenPort who
                            Just p  -> fromIntegral p

    bindSock :: IO Socket
    bindSock = do
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
              when (okayFakeAddr dest) $ do
                  void $ sendTo aSocket bs $ destSockAddr dest

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
