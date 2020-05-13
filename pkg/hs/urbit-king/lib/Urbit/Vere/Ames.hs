{-|
  Ames IO Driver
-}

module Urbit.Vere.Ames (ames) where

import Urbit.Prelude

import Network.Socket        hiding (recvFrom, sendTo)
import Urbit.Arvo            hiding (Fake)
import Urbit.King.Config
import Urbit.Vere.Pier.Types

import Urbit.King.App      (HasKingId(..))
import Urbit.Vere.Ames.DNS (NetworkMode(..), ResolvServ(..))
import Urbit.Vere.Ames.DNS (galaxyPort, resolvServ)
import Urbit.Vere.Ames.UDP (UdpServ(..), fakeUdpServ, realUdpServ)


-- Types -----------------------------------------------------------------------

data AmesDrv = AmesDrv
  { aTurfs    :: TVar (Maybe [Turf])
  , aUdpServ  :: UdpServ
  , aResolvr  :: ResolvServ
  , aRecvTid  :: Async ()
  }


-- Utils -----------------------------------------------------------------------

listenPort :: NetworkMode -> Ship -> PortNumber
listenPort m s | s < 256 = galaxyPort m (fromIntegral s)
listenPort m _           = 0

localhost :: HostAddress
localhost = tupleToHostAddress (127, 0, 0, 1)

inaddrAny :: HostAddress
inaddrAny = tupleToHostAddress (0, 0, 0, 0)

modeAddress :: NetworkMode -> Maybe HostAddress
modeAddress = \case
  Fake      -> Just localhost
  Localhost -> Just localhost
  Real      -> Just inaddrAny
  NoNetwork -> Nothing

okFakeAddr :: AmesDest -> Bool
okFakeAddr = \case
  EachYes _                            -> True
  EachNo  (Jammed (AAIpv4 (Ipv4 a) _)) -> a == localhost
  EachNo  (Jammed (AAVoid v         )) -> absurd v

localAddr :: NetworkMode -> AmesDest -> SockAddr
localAddr mode = \case
  EachYes g                     -> SockAddrInet (galaxyPort mode g) localhost
  EachNo  (Jammed (AAIpv4 _ p)) -> SockAddrInet (fromIntegral p) localhost
  EachNo  (Jammed (AAVoid v  )) -> absurd v

bornEv :: KingId -> Ev
bornEv inst = EvBlip $ BlipEvNewt $ NewtEvBorn (fromIntegral inst, ()) ()

hearEv :: PortNumber -> HostAddress -> ByteString -> Ev
hearEv p a bs =
  EvBlip $ BlipEvAmes $ AmesEvHear () dest (MkBytes bs)
 where
  dest = EachNo $ Jammed $ AAIpv4 (Ipv4 a) (fromIntegral p)


--------------------------------------------------------------------------------

netMode :: HasNetworkConfig e => Bool -> RIO e NetworkMode
netMode isFake = do
  netMode <- view (networkConfigL . ncNetMode)
  noAmes  <- view (networkConfigL . ncNoAmes)
  pure $ case (noAmes, isFake, netMode) of
    (True, _   , _          ) -> NoNetwork
    (_   , _   , NMNone     ) -> NoNetwork
    (_   , True, _          ) -> Fake
    (_   , _   , NMNormal   ) -> Real
    (_   , _   , NMLocalhost) -> Localhost

udpPort :: Bool -> Ship -> HasNetworkConfig e => RIO e PortNumber
udpPort isFake who = do
  mode <- netMode isFake
  mPort <- view (networkConfigL . ncAmesPort)
  pure $ maybe (listenPort mode who) fromIntegral mPort

udpServ :: (HasLogFunc e, HasNetworkConfig e) => Bool -> Ship -> RIO e UdpServ
udpServ isFake who = do
  mode <- netMode isFake
  port <- udpPort isFake who
  case modeAddress mode of
    Nothing   -> fakeUdpServ
    Just host -> realUdpServ port host

{-|
    inst      -- Process instance number.
    who       -- Which ship are we?
    enqueueEv -- Queue-event action.
    mPort     -- Explicit port override from command line arguments.

    4096 is a reasonable number for recvFrom. Packets of that size are
    not possible on the internet.

    TODO verify that the KingIds match on effects.
-}
ames
  :: forall e
   . (HasLogFunc e, HasNetworkConfig e, HasKingId e)
  => e
  -> Ship
  -> Bool
  -> QueueEv
  -> (Text -> RIO e ())
  -> ([Ev], RAcquire e (EffCb e NewtEf))
ames env who isFake enqueueEv stderr = (initialEvents, runAmes)
 where
  king = fromIntegral (env ^. kingIdL)

  initialEvents :: [Ev]
  initialEvents = [bornEv king]

  runAmes :: RAcquire e (EffCb e NewtEf)
  runAmes = do
    mode <- rio (netMode isFake)
    drv  <- mkRAcquire start stop
    pure (handleEffect drv mode)

  start :: RIO e AmesDrv
  start = do
    aTurfs   <- newTVarIO Nothing
    aUdpServ <- udpServ isFake who
    aRecvTid <- queuePacketsThread aUdpServ
    aResolvr <- resolvServ aTurfs (usSend aUdpServ) stderr
    pure (AmesDrv { .. })

  queuePacketsThread :: UdpServ -> RIO e (Async ())
  queuePacketsThread UdpServ {..} = async $ forever $ atomically $ do
    (p, a, b) <- usRecv
    enqueueEv (hearEv p a b)

  stop :: AmesDrv -> RIO e ()
  stop AmesDrv {..} = io $ do
    usKill aUdpServ
    rsKill aResolvr
    cancel aRecvTid

  handleEffect :: AmesDrv -> NetworkMode -> NewtEf -> RIO e ()
  handleEffect drv@AmesDrv {..} mode = \case
    NewtEfTurf (_id, ()) turfs -> do
      atomically $ writeTVar aTurfs (Just turfs)

    NewtEfSend (_id, ()) dest (MkBytes bs) -> do
      atomically (readTVar aTurfs) >>= \case
        Nothing    -> pure ()
        Just turfs -> sendPacket drv mode dest bs

  sendPacket :: AmesDrv -> NetworkMode -> AmesDest -> ByteString -> RIO e ()
  sendPacket AmesDrv {..} mode dest byt = do
    let to adr = io (usSend aUdpServ adr byt)

    case (mode, dest) of
      (NoNetwork, _ ) -> pure ()
      (Fake     , _ ) -> when (okFakeAddr dest) $ to (localAddr Fake dest)
      (Localhost, _ ) -> to (localAddr Localhost dest)
      (Real     , ra) -> ra & \case
        EachYes gala -> io (rsSend aResolvr gala byt)
        EachNo  addr -> to (ipv4Addr addr)

  ipv4Addr (Jammed (AAVoid v  )) = absurd v
  ipv4Addr (Jammed (AAIpv4 a p)) = SockAddrInet (fromIntegral p) (unIpv4 a)
