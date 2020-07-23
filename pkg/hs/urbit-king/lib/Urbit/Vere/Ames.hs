{-|
  Ames IO Driver
-}

module Urbit.Vere.Ames (ames, ames', PacketOutcome(..)) where

import Urbit.Prelude

import Network.Socket              hiding (recvFrom, sendTo)
import Urbit.Arvo                  hiding (Fake)
import Urbit.King.Config
import Urbit.Vere.Pier.Types

import Urbit.King.App      (HasKingId(..), HasPierEnv(..))
import Urbit.Vere.Ames.DNS (NetworkMode(..), ResolvServ(..))
import Urbit.Vere.Ames.DNS (galaxyPort, resolvServ)
import Urbit.Vere.Ames.UDP (UdpServ(..), fakeUdpServ, realUdpServ)


-- Constants -------------------------------------------------------------------

-- | How many unprocessed ames packets to allow in the queue before we start
-- dropping incoming packets.
queueBound :: Word
queueBound = 1000

-- | How often, measured in number of packets dropped, we should announce packet
-- loss.
packetsDroppedPerComplaint :: Word
packetsDroppedPerComplaint = 1000


-- Types -----------------------------------------------------------------------

data AmesDrv = AmesDrv
  { aTurfs    :: TVar (Maybe [Turf])
  , aDropped  :: TVar Word
  , aUdpServ  :: UdpServ
  , aResolvr  :: ResolvServ
  , aRecvTid  :: Async ()
  }

data PacketOutcome
  = Intake
  | Ouster


-- Utils -----------------------------------------------------------------------

listenPort :: NetworkMode -> Ship -> PortNumber
listenPort m s | s < 256 = galaxyPort m (fromIntegral s)
listenPort m _           = 0 -- I don't care, just give me any port.

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

udpPort :: HasNetworkConfig e => Bool -> Ship -> RIO e PortNumber
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

_bornFailed :: e -> WorkError -> IO ()
_bornFailed env _ = runRIO env $ do
  pure () -- TODO What can we do?

ames'
  :: HasPierEnv e
  => Ship
  -> Bool
  -> (Text -> RIO e ())
  -> RIO e ([Ev], RAcquire e (DriverApi NewtEf))
ames' who isFake stderr = do
  -- Unfortunately, we cannot use TBQueue because the only behavior
  -- provided for when full is to block the writer. The implementation
  -- below uses materially the same data structures as TBQueue, however.
  ventQ :: TQueue EvErr <- newTQueueIO
  avail :: TVar Word <- newTVarIO queueBound
  let
    enqueuePacket p = do
      vail <- readTVar avail
      if vail > 0
        then do
          modifyTVar avail (subtract 1)
          writeTQueue ventQ p
          pure Intake
        else do
          _ <- readTQueue ventQ
          writeTQueue ventQ p
          pure Ouster
    dequeuePacket = do
      pM <- tryReadTQueue ventQ
      when (isJust pM) $ modifyTVar avail (+ 1)
      pure pM

  env <- ask
  let (bornEvs, startDriver) = ames env who isFake enqueuePacket stderr

  let runDriver = do
        diOnEffect <- startDriver
        let diEventSource = fmap RRWork <$> dequeuePacket
        pure (DriverApi {..})

  pure (bornEvs, runDriver)


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
  -> (EvErr -> STM PacketOutcome)
  -> (Text -> RIO e ())
  -> ([Ev], RAcquire e (NewtEf -> IO ()))
ames env who isFake enqueueEv stderr = (initialEvents, runAmes)
 where
  king = fromIntegral (env ^. kingIdL)

  initialEvents :: [Ev]
  initialEvents = [bornEv king]

  runAmes :: RAcquire e (NewtEf -> IO ())
  runAmes = do
    mode <- rio (netMode isFake)
    drv  <- mkRAcquire start stop
    pure (handleEffect drv mode)

  start :: HasLogFunc e => RIO e AmesDrv
  start = do
    aTurfs   <- newTVarIO Nothing
    aDropped <- newTVarIO 0
    aUdpServ <- udpServ isFake who
    aRecvTid <- queuePacketsThread aDropped aUdpServ
    aResolvr <- resolvServ aTurfs (usSend aUdpServ) stderr
    pure (AmesDrv { .. })

  hearFailed _ = pure ()

  queuePacketsThread :: HasLogFunc e => TVar Word -> UdpServ -> RIO e (Async ())
  queuePacketsThread dropCtr UdpServ {..} = async $ forever $ do
    outcome <- atomically $ do
      (p, a, b) <- usRecv
      enqueueEv (EvErr (hearEv p a b) hearFailed)
    case outcome of
      Intake -> pure ()
      Ouster -> do
        d <- atomically $ do
          d <- readTVar dropCtr
          writeTVar dropCtr (d + 1)
          pure d
        when (d `rem` packetsDroppedPerComplaint == 0) $
          logWarn "ames: queue full; dropping inbound packets"

  stop :: AmesDrv -> RIO e ()
  stop AmesDrv {..} = io $ do
    usKill aUdpServ
    rsKill aResolvr
    cancel aRecvTid

  handleEffect :: AmesDrv -> NetworkMode -> NewtEf -> IO ()
  handleEffect drv@AmesDrv {..} mode = runRIO env . \case
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
