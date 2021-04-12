-- This is required due to the use of 'Void' in a constructor slot in
-- combination with 'deriveNoun' which generates an unreachable pattern.
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

{-|
  Ames IO Driver
-}

module Urbit.Vere.Ames (ames, ames', PacketOutcome(..)) where

import Urbit.Prelude

import Network.Socket
import Urbit.Arvo                  hiding (Fake)
import Urbit.King.Config
import Urbit.King.Scry
import Urbit.Vere.Ames.LaneCache
import Urbit.Vere.Ames.Packet
import Urbit.Vere.Pier.Types
import Urbit.Vere.Ports

import Data.Serialize      (decode, encode)
import Urbit.King.App      (HasKingId(..), HasPierEnv(..))
import Urbit.Vere.Ames.DNS (NetworkMode(..), ResolvServ(..))
import Urbit.Vere.Ames.DNS (galaxyPort, resolvServ)
import Urbit.Vere.Ames.UDP (UdpServ(..), fakeUdpServ, realUdpServ)
import Urbit.Vere.Stat     (AmesStat(..), bump, bump')


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

type Version = Word8

data AmesDrv = AmesDrv
  { aTurfs    :: TVar (Maybe [Turf])
  , aVersion  :: TVar (Maybe Version)
  , aUdpServ  :: UdpServ
  , aResolvr  :: ResolvServ
  , aVersTid  :: Async ()
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
  EachYes _                   -> True
  EachNo  (AAIpv4 (Ipv4 a) _) -> a == localhost

localAddr :: NetworkMode -> AmesDest -> SockAddr
localAddr mode = \case
  EachYes g            -> SockAddrInet (galaxyPort mode g) localhost
  EachNo  (AAIpv4 _ p) -> SockAddrInet (fromIntegral p) localhost

bornEv :: KingId -> Ev
bornEv inst = EvBlip $ BlipEvNewt $ NewtEvBorn (fromIntegral inst, ()) ()

hearEv :: PortNumber -> HostAddress -> ByteString -> Ev
hearEv p a bs =
  EvBlip $ BlipEvAmes $ AmesEvHear () (ipDest p a) (MkBytes bs)

ipDest :: PortNumber -> HostAddress -> AmesDest
ipDest p a = EachNo $ AAIpv4 (Ipv4 a) (fromIntegral p)


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

udpServ :: (HasLogFunc e, HasNetworkConfig e, HasPortControlApi e)
        => Bool
        -> Ship
        -> AmesStat
        -> RIO e UdpServ
udpServ isFake who stat =  do
  mode <- netMode isFake
  port <- udpPort isFake who
  case modeAddress mode of
    Nothing   -> fakeUdpServ
    Just host -> realUdpServ port host stat

_bornFailed :: e -> WorkError -> IO ()
_bornFailed env _ = runRIO env $ do
  pure () -- TODO What can we do?

ames'
  :: HasPierEnv e
  => Ship
  -> Bool
  -> AmesStat
  -> ScryFunc
  -> (Text -> RIO e ())
  -> RIO e ([Ev], RAcquire e (DriverApi NewtEf))
ames' who isFake stat scry stderr = do
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
          modifyTVar' avail (subtract 1)
          writeTQueue ventQ p
          pure Intake
        else do
          _ <- readTQueue ventQ
          writeTQueue ventQ p
          pure Ouster
    dequeuePacket = do
      pM <- tryReadTQueue ventQ
      when (isJust pM) $ modifyTVar' avail (+ 1)
      pure pM

  env <- ask
  let (bornEvs, startDriver) = ames env who isFake stat scry enqueuePacket stderr

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
   . (HasLogFunc e, HasNetworkConfig e, HasPortControlApi e, HasKingId e)
  => e
  -> Ship
  -> Bool
  -> AmesStat
  -> ScryFunc
  -> (EvErr -> STM PacketOutcome)
  -> (Text -> RIO e ())
  -> ([Ev], RAcquire e (NewtEf -> IO ()))
ames env who isFake stat scry enqueueEv stderr = (initialEvents, runAmes)
 where
  king = fromIntegral (env ^. kingIdL)

  initialEvents :: [Ev]
  initialEvents = [bornEv king]

  runAmes :: RAcquire e (NewtEf -> IO ())
  runAmes = do
    mode <- rio (netMode isFake)
    drv  <- mkRAcquire start stop
    pure (handleEffect drv mode)

  start :: RIO e AmesDrv
  start = do
    mode <- rio (netMode isFake)
    cachedScryLane <- cache scryLane

    aTurfs   <- newTVarIO Nothing
    aVersion <- newTVarIO Nothing
    aVersTid <- trackVersionThread aVersion
    aUdpServ <- udpServ isFake who stat
    aResolvr <- resolvServ aTurfs (usSend aUdpServ) stderr
    aRecvTid <- queuePacketsThread
      aVersion
      cachedScryLane
      (send aUdpServ aResolvr mode)
      aUdpServ
      stat

    pure (AmesDrv { .. })

  hearFailed AmesStat {..} = runRIO env . \case
    RunSwap{} -> bump asSwp
    RunBail gs -> do
      for gs \(t, es) ->
        for es \e ->
          logWarn $ hark
            ["ames: goof: ", unTerm t, ": ", tankToText e]
      bump asBal 
    RunOkay{} -> bump asOky

  trackVersionThread :: HasLogFunc e => TVar (Maybe Version) -> RIO e (Async ())
  trackVersionThread versSlot = async $ forever do
    scryVersion >>= \case
      Just v -> do
        v0 <- readTVarIO versSlot
        atomically $ writeTVar versSlot (Just v)
        if (v0 == Just v)
          then logInfo $ displayShow ("ames: proto version unchanged at", v)
          else stderr ("ames: protocol version now " <> tshow v)

      Nothing -> logError "ames: could not scry for version"

    threadDelay (10 * 60 * 1_000_000)  -- 10m

  queuePacketsThread :: HasLogFunc e
                     => TVar (Maybe Version)
                     -> (Ship -> RIO e (Maybe [AmesDest]))
                     -> (AmesDest -> ByteString -> RIO e ())
                     -> UdpServ
                     -> AmesStat
                     -> RIO e (Async ())
  queuePacketsThread vers lan forward UdpServ{..} s@(AmesStat{..}) = async $ forever $ do
      -- port number, host address, bytestring
    (p, a, b) <- atomically (bump' asRcv >> usRecv)
    ver <- readTVarIO vers
    case decode b of
      Right (pkt@Packet {..}) | ver == Nothing || ver == Just pktVersion -> do
        logDebug $ displayShow ("ames: bon packet", pkt, showUD $ bytesAtom b)

        if pktRcvr == who
          then do
            bump asSup
            serfsUp p a b
          else lan pktRcvr >>= \case
            Just ls
              |  dest:_ <- filter notSelf ls
              -> do
                bump asFwd
                forward dest $ encode pkt
                  { pktOrigin = pktOrigin
                            <|> Just (AAIpv4 (Ipv4 a) (fromIntegral p)) }
              where
                notSelf (EachYes g) = who /= Ship (fromIntegral g)
                notSelf (EachNo  _) = True

            _ -> do
              bump asDrt
              logInfo $ displayShow ("ames: dropping unroutable", pkt)

      Right pkt -> do
        bump asDvr
        logInfo $ displayShow ("ames: dropping ill-versed", pkt, ver)

      -- XX better handle misversioned or illegible packets.
      -- Remarks from 67f06ce5, pkg/urbit/vere/io/ames.c, L1010:
      --
      -- [There are] two protocol-change scenarios [which we must think about]:
      --
      --  - packets using old protocol versions from our sponsees
      --    these must be let through, and this is a transitive condition;
      --    they must also be forwarded where appropriate
      --    they can be validated, as we know their semantics
      --
      --  - packets using newer protocol versions
      --    these should probably be let through, or at least
      --    trigger printfs suggesting upgrade.
      --    they cannot be filtered, as we do not know their semantics
      --
      Left e -> do
        bump asDml
        logInfo $ displayShow ("ames: dropping malformed", e)

    where
      serfsUp p a b =
        atomically (enqueueEv (EvErr (hearEv p a b) (hearFailed s))) >>= \case
          Intake -> bump asSrf
          Ouster -> do
            d <- atomically $ do
              bump' asQuf
              readTVar asQuf
            when (d `rem` packetsDroppedPerComplaint == 1) $
              logWarn "ames: queue full; dropping inbound packets"

  stop :: forall e. AmesDrv -> RIO e ()
  stop AmesDrv {..} = io $ do
    usKill aUdpServ
    rsKill aResolvr
    cancel aVersTid
    cancel aRecvTid

  handleEffect :: AmesDrv -> NetworkMode -> NewtEf -> IO ()
  handleEffect drv@AmesDrv {..} mode = runRIO env . \case
    NewtEfTurf (_id, ()) turfs -> do
      atomically $ writeTVar aTurfs (Just turfs)

    NewtEfSend (_id, ()) dest (MkBytes bs) -> do
      atomically (readTVar aTurfs) >>= \case
        Nothing    -> stderr "ames: send before turfs" >> pure ()
        Just turfs -> send aUdpServ aResolvr mode dest bs

  send :: UdpServ
       -> ResolvServ
       -> NetworkMode
       -> AmesDest
       -> ByteString
       -> RIO e ()
  send udpServ resolvr mode dest byt = do
    let to adr = io (usSend udpServ adr byt)

    case (mode, dest) of
      (NoNetwork, _ ) -> pure ()
      (Fake     , _ ) -> when (okFakeAddr dest) $ to (localAddr Fake dest)
      (Localhost, _ ) -> to (localAddr Localhost dest)
      (Real     , ra) -> ra & \case
        EachYes gala -> io (rsSend resolvr gala byt)
        EachNo  addr -> to (ipv4Addr addr)

  scryVersion :: HasLogFunc e => RIO e (Maybe Version)
  scryVersion = scryNow scry "ax" "" ["protocol", "version"]

  scryLane :: HasLogFunc e
           => Ship
           -> RIO e (Maybe [AmesDest])
  scryLane ship = scryNow scry "ax" "" ["peers", tshow ship, "forward-lane"]

  ipv4Addr (AAIpv4 a p) = SockAddrInet (fromIntegral p) (unIpv4 a)
