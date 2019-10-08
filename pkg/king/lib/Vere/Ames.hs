module Vere.Ames (ames) where

import UrbitPrelude

import Arvo                      hiding (Fake)
import Network.Socket            hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Vere.Pier.Types

import qualified Data.ByteString as BS
import qualified Data.Map        as M
import qualified Urbit.Ob        as Ob
import qualified Urbit.Time      as Time

-- Types -----------------------------------------------------------------------

data AmesDrv = AmesDrv
  { aIsLive        :: IORef Bool
  , aTurfs         :: TVar [Turf]
  , aGalaxies      :: IORef (M.Map Galaxy (Async (), TQueue ByteString))
  , aSocket        :: Socket
  , aWakeTimer     :: Async ()
  , aListener      :: Async ()
  , aSendingQueue  :: TQueue (SockAddr, ByteString)
  , aSendingThread :: Async ()
  }

data NetworkMode = Fake | Real
  deriving (Eq, Ord, Show)

-- data GalaxyInfo = GalaxyInfo { ip :: Ipv4, age :: Time.Unix }
--   deriving (Eq, Ord, Show)


-- Utils -----------------------------------------------------------------------

galaxyPort :: NetworkMode -> Galaxy -> PortNumber
galaxyPort Fake (Galaxy g) = fromIntegral g + 31337
galaxyPort Real (Galaxy g) = fromIntegral g + 13337

listenPort :: NetworkMode -> Ship -> PortNumber
listenPort m s | s < 256 = galaxyPort m (fromIntegral s)
listenPort m _ = 0

localhost :: HostAddress
localhost = tupleToHostAddress (127,0,0,1)

inaddrAny :: HostAddress
inaddrAny = tupleToHostAddress (0,0,0,0)

okayFakeAddr :: AmesDest -> Bool
okayFakeAddr = \case
    ADGala _ _          -> True
    ADIpv4 _ p (Ipv4 a) -> a == localhost

destSockAddr :: NetworkMode -> AmesDest -> SockAddr
destSockAddr m = \case
    -- As mentioned previously, "localhost" is wrong.
    ADGala _ g   -> SockAddrInet (galaxyPort m g) localhost
    ADIpv4 _ p a -> SockAddrInet (fromIntegral p) (unIpv4 a)

barnEv :: KingId -> Ev
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

    TODO verify that the KingIds match on effects.
-}
ames :: forall e. HasLogFunc e
     => KingId -> Ship -> Bool -> Maybe Port -> QueueEv
     -> ([Ev], RAcquire e (EffCb e NewtEf))
ames inst who isFake mPort enqueueEv =
    (initialEvents, runAmes)
  where
    initialEvents :: [Ev]
    initialEvents = [barnEv inst]

    runAmes :: RAcquire e (EffCb e NewtEf)
    runAmes = do
        drv <- mkRAcquire start stop
        pure (handleEffect drv)

    start :: RIO e AmesDrv
    start = do
        aIsLive        <- newIORef False
        aTurfs         <- newTVarIO []
        aGalaxies      <- newIORef mempty
        aSocket        <- bindSock
        aWakeTimer     <- async runTimer
        aListener      <- async (waitPacket aSocket)
        aSendingQueue  <- newTQueueIO
        aSendingThread <- async (sendingThread aSendingQueue aSocket)
        pure            $ AmesDrv{..}

    netMode :: NetworkMode
    netMode = if isFake then Fake else Real

    stop :: AmesDrv -> RIO e ()
    stop AmesDrv{..} = do
        galaxies <- readIORef aGalaxies
        mapM_ (cancel . fst) galaxies

        cancel aSendingThread
        cancel aWakeTimer
        cancel aListener
        io $ close' aSocket

    runTimer :: RIO e ()
    runTimer = forever $ do
        threadDelay (300 * 1000000) -- 300 seconds
        atomically (enqueueEv wakeEv)

    bindSock :: RIO e Socket
    bindSock = do
        let ourPort = maybe (listenPort netMode who) fromIntegral mPort
        s  <- io $ socket AF_INET Datagram defaultProtocol
        logTrace $ displayShow ("(ames) Binding to port ", ourPort)
        -- localhost may be wrong here; ames.c switches between INADDR_ANY and
        -- INADDR_LOOPBACK based on networking flags.
        let addr = SockAddrInet ourPort $
              if isFake then localhost else inaddrAny

        () <- io $ bind s addr
        pure s

    waitPacket :: Socket -> RIO e ()
    waitPacket s = forever $ do
        (bs, addr) <- io $ recvFrom s 4096
        logTrace $ displayShow ("(ames) Received packet from ", addr)
        wen        <- io $ Time.now
        case addr of
            SockAddrInet p a -> atomically (enqueueEv $ hearEv wen p a bs)
            _                -> pure ()

    handleEffect :: AmesDrv -> NewtEf -> RIO e ()
    handleEffect drv@AmesDrv{..} = \case
      NewtEfTurf (_id, ()) turfs -> do
          writeIORef aIsLive True
          atomically $ writeTVar aTurfs turfs

      NewtEfSend (_id, ()) dest (MkBytes bs) -> do
          whenM (readIORef aIsLive) (sendPacket drv netMode dest bs)

    sendPacket :: AmesDrv -> NetworkMode -> AmesDest -> ByteString -> RIO e ()

    sendPacket AmesDrv{..} Fake dest bs = do
      logTrace $ displayShow
        ("(ames) sendPacket Fake ", dest, (destSockAddr Fake dest))
      when (okayFakeAddr dest) $ do
        atomically $ writeTQueue aSendingQueue ((destSockAddr Fake dest), bs)

    sendPacket AmesDrv{..} Real (ADGala wen galaxy) bs = do
      logTrace $ displayShow
        ("(ames) sendPacket Real Galaxy ", galaxy)
      galaxies <- readIORef aGalaxies
      queue <- case M.lookup galaxy galaxies of
        Just (_, queue) -> pure queue
        Nothing -> do
          inQueue <- newTQueueIO
          thread <- async $ galaxyResolver galaxy aTurfs inQueue aSendingQueue
          modifyIORef (aGalaxies) (M.insert galaxy (thread, inQueue))
          pure inQueue

      atomically $ writeTQueue queue bs

    sendPacket AmesDrv{..} Real (ADIpv4 _ p a) bs = do
      let addr = SockAddrInet (fromIntegral p) (unIpv4 a)
      logTrace $ displayShow ("(ames) sendPacket Real Other ", addr)
      atomically $ writeTQueue aSendingQueue (addr, bs)

    -- An outbound queue of messages. We can only write to a socket from one
    -- thread, so coalesce those writes here.
    sendingThread :: TQueue (SockAddr, ByteString) -> Socket -> RIO e ()
    sendingThread queue socket = forever $ do
      (dest, bs) <- atomically $ readTQueue queue

      logTrace $ displayShow ("(ames) Sending packet to ", socket, dest)
      -- This line blocks! WTF!
      bytesSent <- io $ sendTo socket bs dest
      logTrace $ displayShow ("(ames) Packet sent! ", bytesSent)
      let len = BS.length bs
      when (bytesSent /= len) $ do
        logDebug $ displayShow
          ("(ames) Only sent ", bytesSent, " of ", (length bs))

    -- Asynchronous thread per galaxy which handles domain resolution, and can
    -- block its own queue of ByteStrings to send.
    --
    -- Maybe perform the resolution asynchronously, injecting into the resolver
    -- queue as a message.
    --
    -- TODO: Figure out how the real haskell time library works.
    galaxyResolver :: Galaxy -> TVar [Turf] -> TQueue ByteString
                   -> TQueue (SockAddr, ByteString)
                   -> RIO e ()
    galaxyResolver galaxy turfVar incoming outgoing =
      loop Nothing Time.unixEpoch
      where
        loop :: Maybe SockAddr -> Time.Wen -> RIO e ()
        loop lastGalaxyIP lastLookupTime = do
          packet <- atomically $ readTQueue incoming

          i <- checkIP lastGalaxyIP lastLookupTime
          case i of
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
            toCheck <- atomically $ readTVar turfVar
            ip <- resolveFirstIP lastIP toCheck
            timeAfterResolution <- io $ Time.now
            pure (ip, timeAfterResolution)

        resolveFirstIP :: Maybe SockAddr -> [Turf] -> RIO e (Maybe SockAddr)
        resolveFirstIP prevIP [] = do
          -- print ("ames: czar at %s: not found (b)\n")
          logDebug $ displayShow
              ("(ames) Failed to lookup IP for ", galaxy)
          pure prevIP

        resolveFirstIP prevIP (x:xs) = do
          let hostname = buildDNS galaxy x
          let portstr = show $ galaxyPort Real galaxy
          listIPs <- io $ getAddrInfo Nothing (Just hostname) (Just portstr)
          case listIPs of
            []     -> resolveFirstIP prevIP xs
            (y:ys) -> do
              logDebug $ displayShow
                ("(ames) Looked up ", hostname, portstr, y)
              pure $ Just $ addrAddress y

        buildDNS :: Galaxy -> Turf -> String
        buildDNS (Galaxy g) turf = name ++ "." ++ (unpack $ _turfText turf)
          where
            nameWithSig = Ob.renderPatp $ Ob.patp $ fromIntegral g
            name = case stripPrefix "~" nameWithSig of
              Nothing -> error "Urbit.ob didn't produce string with ~"
              Just x  -> (unpack x)

        queueSendToGalaxy :: SockAddr -> ByteString -> RIO e ()
        queueSendToGalaxy inet packet = do
          logTrace $ displayShow ("(ames) Sending galaxy packet to ", inet)
          atomically $ writeTQueue outgoing (inet, packet)
