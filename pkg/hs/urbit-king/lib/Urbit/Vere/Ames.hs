{-|
  Ames IO Driver
-}

module Urbit.Vere.Ames (ames) where

import Urbit.Prelude

import Network.Socket        hiding (recvFrom, sendTo)
import Urbit.Arvo            hiding (Fake)
import Urbit.King.Config
import Urbit.Vere.Pier.Types

import Urbit.Vere.Ames.UDP (UdpServ(..), fakeUdpServ, realUdpServ)

import qualified Data.Map   as M
import qualified Urbit.Ob   as Ob
import qualified Urbit.Time as Time


-- Types -----------------------------------------------------------------------

data AmesDrv = AmesDrv
  { aTurfs    :: TVar (Maybe [Turf])
  , aGalaxies :: IORef (M.Map Galaxy (Async (), TQueue ByteString))
  , aUdpServ  :: UdpServ
  , aRecvTid  :: Async ()
  }

data NetworkMode = Fake | Localhost | Real | NoNetwork
  deriving (Eq, Ord, Show)


-- Utils -----------------------------------------------------------------------

galaxyPort :: NetworkMode -> Galaxy -> PortNumber
galaxyPort Fake      (Patp g) = fromIntegral g + 31337
galaxyPort Localhost (Patp g) = fromIntegral g + 13337
galaxyPort Real      (Patp g) = fromIntegral g + 13337
galaxyPort NoNetwork _        = fromIntegral 0

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

okayFakeAddr :: AmesDest -> Bool
okayFakeAddr = \case
  EachYes _                            -> True
  EachNo  (Jammed (AAIpv4 (Ipv4 a) _)) -> a == localhost
  EachNo  (Jammed (AAVoid v         )) -> absurd v

localhostSockAddr :: NetworkMode -> AmesDest -> SockAddr
localhostSockAddr mode = \case
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

turfText :: Turf -> Text
turfText = intercalate "." . reverse . fmap unCord . unTurf

renderGalaxy :: Galaxy -> Text
renderGalaxy = Ob.renderPatp . Ob.patp . fromIntegral . unPatp


--------------------------------------------------------------------------------

netMode :: HasNetworkConfig e => Bool -> RIO e NetworkMode
netMode True  = pure Fake
netMode False = view (networkConfigL . ncNetMode . to cvt)
 where
  cvt :: NetMode -> NetworkMode
  cvt = \case
    NMNormal    -> Real
    NMLocalhost -> Localhost
    NMNone      -> NoNetwork

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
ames :: forall e. (HasLogFunc e, HasNetworkConfig e)
     => KingId -> Ship -> Bool -> QueueEv
     -> (Text -> RIO e ())
     -> ([Ev], RAcquire e (EffCb e NewtEf))
ames inst who isFake enqueueEv stderr =
    (initialEvents, runAmes)
  where
    initialEvents :: [Ev]
    initialEvents = [bornEv inst]

    runAmes :: RAcquire e (EffCb e NewtEf)
    runAmes = do
      mode <- rio (netMode isFake)
      drv  <- mkRAcquire start stop
      pure (handleEffect drv mode)

    start :: RIO e AmesDrv
    start = do
      aTurfs    <- newTVarIO Nothing
      aGalaxies <- newIORef mempty
      aUdpServ  <- udpServ isFake who
      aRecvTid  <- queuePacketsThread aUdpServ
      pure (AmesDrv{..})

    queuePacketsThread :: UdpServ -> RIO e (Async ())
    queuePacketsThread UdpServ{..} = async $ forever $ atomically $ do
      (p, a, b) <- usRecv
      enqueueEv (hearEv p a b)

    stop :: AmesDrv -> RIO e ()
    stop AmesDrv{..} = do
      io (usKill aUdpServ)
      cancel aRecvTid
      readIORef aGalaxies >>= mapM_ (cancel . fst)

    handleEffect :: AmesDrv -> NetworkMode -> NewtEf -> RIO e ()
    handleEffect drv@AmesDrv{..} mode = \case
      NewtEfTurf (_id, ()) turfs -> do
        atomically $ writeTVar aTurfs (Just turfs)

      NewtEfSend (_id, ()) dest (MkBytes bs) -> do
        atomically (readTVar aTurfs) >>= \case
          Nothing    -> pure ()
          Just turfs -> sendPacket drv mode dest bs

    -- Asynchronous thread per galaxy which handles domain resolution, and can
    -- block its own queue of ByteStrings to send.
    --
    -- Maybe perform the resolution asynchronously, injecting into the resolver
    -- queue as a message.
    --
    -- TODO: Figure out how the real haskell time library works.
    galaxyResolver :: Galaxy -> TVar (Maybe [Turf]) -> TQueue ByteString
                   -> (SockAddr -> ByteString -> RIO e ())
                   -> RIO e ()
    galaxyResolver galaxy turfVar incoming queueSendToGalaxy =
      loop Nothing Time.unixEpoch
      where
        loop :: Maybe SockAddr -> Time.Wen -> RIO e ()
        loop lastGalaxyIP lastLookupTime = do
          packet <- atomically $ readTQueue incoming

          checkIP lastGalaxyIP lastLookupTime >>= \case
            (Nothing, t) -> do
              -- We've failed to lookup the IP. Drop the outbound packet
              -- because we have no IP for our galaxy, including possible
              -- previous IPs.
              logDebug $ displayShow
                ("(ames) Dropping packet; no ip for galaxy ", galaxy)
              loop Nothing t
            (Just ip, t) -> do
              queueSendToGalaxy ip packet
              loop (Just ip) t

        checkIP :: Maybe SockAddr -> Time.Wen
                -> RIO e (Maybe SockAddr, Time.Wen)
        checkIP lastIP lastLookupTime = do
          current <- io $ Time.now
          if (Time.gap current lastLookupTime ^. Time.secs) < 300
          then pure (lastIP, lastLookupTime)
          else do
            toCheck <- fromMaybe [] <$> atomically (readTVar turfVar)
            mybIp <- resolveFirstIP lastIP toCheck
            timeAfterResolution <- io $ Time.now
            pure (mybIp, timeAfterResolution)

        resolveFirstIP :: Maybe SockAddr -> [Turf] -> RIO e (Maybe SockAddr)
        resolveFirstIP prevIP [] = do
          stderr $ "ames: czar at " ++ renderGalaxy galaxy ++ ": not found"
          logDebug $ displayShow
              ("(ames) Failed to lookup IP for ", galaxy)
          pure prevIP

        resolveFirstIP prevIP (x:xs) = do
          hostname <- buildDNS galaxy x
          let portstr = show $ galaxyPort Real galaxy
          listIPs <- io $ getAddrInfo Nothing (Just hostname) (Just portstr)
          case listIPs of
            []     -> resolveFirstIP prevIP xs
            (y:ys) -> do
              let sockaddr = Just $ addrAddress y
              when (sockaddr /= prevIP) $
                stderr $ "ames: czar " ++ renderGalaxy galaxy ++ ": ip " ++
                         (tshow $ addrAddress y)
              logDebug $ displayShow
                ("(ames) Looked up ", hostname, portstr, y)
              pure sockaddr

        buildDNS :: Galaxy -> Turf -> RIO e String
        buildDNS (Patp g) turf = do
          let nameWithSig = Ob.renderPatp $ Ob.patp $ fromIntegral g
          name <- case stripPrefix "~" nameWithSig of
              Nothing -> error "Urbit.ob didn't produce string with ~"
              Just x  -> pure (unpack x)
          pure $ name ++ "." ++ (unpack $ turfText turf)

    sendPacket :: AmesDrv -> NetworkMode -> AmesDest -> ByteString -> RIO e ()
    sendPacket AmesDrv{..} mode dest bs = do
      let go adr byt = io (usSend aUdpServ adr byt)

      case (mode, dest) of
        (NoNetwork, _) -> do
          pure ()

        (Fake, _) | okayFakeAddr dest -> do
          go (localhostSockAddr Fake dest) bs

        (Fake, _) | otherwise -> do
          pure ()

        (Localhost, _) -> do
          go (localhostSockAddr Localhost dest) bs

        (Real, EachYes galaxy) -> do
          galaxies <- readIORef aGalaxies
          queue <- case M.lookup galaxy galaxies of
            Just (_, queue) -> pure queue
            Nothing -> do
              inQueue <- newTQueueIO
              thread <- async (galaxyResolver galaxy aTurfs inQueue go)
              modifyIORef (aGalaxies) (M.insert galaxy (thread, inQueue))
              pure inQueue
          atomically $ writeTQueue queue bs

        (Real, EachNo (Jammed (AAIpv4 a p))) -> do
          go (SockAddrInet (fromIntegral p) (unIpv4 a)) bs

        (Real, EachNo (Jammed (AAVoid v))) -> do
          absurd v
