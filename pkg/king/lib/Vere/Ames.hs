module Vere.Ames (ames) where

import UrbitPrelude

import Arvo                      hiding (Fake)
import Network.Socket            hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Vere.Pier.Types

import qualified Data.Map   as M
import qualified Urbit.Ob   as Ob
import qualified Urbit.Time as Time

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

okayFakeAddr :: AmesDest -> Bool
okayFakeAddr = \case
    ADGala _ _          -> True
    ADIpv4 _ p (Ipv4 a) -> a == localhost

destSockAddr :: NetworkMode -> AmesDest -> SockAddr
destSockAddr m = \case
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
ames :: KingId -> Ship -> Maybe Port -> QueueEv
     -> ([Ev], Acquire (EffCb e NewtEf))
ames inst who mPort enqueueEv =
    (initialEvents, runAmes)
  where
    initialEvents :: [Ev]
    initialEvents = [barnEv inst]

    runAmes :: Acquire (EffCb e NewtEf)
    runAmes = do
        drv <- mkAcquire start stop
        pure (io . handleEffect drv)

    start :: IO AmesDrv
    start = do
        vLiv      <- newIORef False
        vTurf     <- newTVarIO []
        vGalaxies <- newIORef mempty
        time      <- async runTimer
        sock      <- bindSock
        hear      <- async (waitPacket sock)
        sendQueue <- newTQueueIO
        sending   <- async (sendingThread sendQueue sock)
        pure       $ AmesDrv vLiv vTurf vGalaxies sock time hear sendQueue sending

    netMode :: NetworkMode
    netMode = Fake

    stop :: AmesDrv -> IO ()
    stop (AmesDrv{..}) = do
        galaxies <- readIORef aGalaxies
        mapM_ (cancel . fst) galaxies

        cancel aSendingThread
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
    handleEffect drv@AmesDrv{..} = \case
      NewtEfTurf (_id, ()) turfs -> do
          writeIORef aIsLive True
          atomically $ writeTVar aTurfs turfs

      NewtEfSend (_id, ()) dest (MkBytes bs) -> do
          whenM (readIORef aIsLive) (sendPacket drv netMode dest bs)

    sendPacket :: AmesDrv -> NetworkMode -> AmesDest -> ByteString -> IO ()

    sendPacket AmesDrv{..} Fake dest bs =
      when (okayFakeAddr dest) $ do
        atomically $ writeTQueue aSendingQueue ((destSockAddr Fake dest), bs)

    sendPacket AmesDrv{..} Real (ADGala wen galaxy) bs = do
      galaxies <- readIORef aGalaxies
      queue <- case M.lookup galaxy galaxies of
        Just (_, queue) -> pure queue
        Nothing -> do
          inQueue <- newTQueueIO
          thread <- galaxyResolver galaxy aTurfs inQueue aSendingQueue
          modifyIORef (aGalaxies) (M.insert galaxy (thread, inQueue))
          pure inQueue

      atomically $ writeTQueue queue bs

    sendPacket AmesDrv{..} Real ip@(ADIpv4 _ _ _) bs =
      atomically $ writeTQueue aSendingQueue ((destSockAddr Real ip), bs)

    -- An outbound queue of messages. We can only write to a socket from one
    -- thread, so coalesce those writes here.
    sendingThread :: TQueue (SockAddr, ByteString) -> Socket -> IO ()
    sendingThread queue socket = forever $ do
      (dest, bs) <- atomically $ readTQueue queue
      void $ sendTo socket bs dest

    -- Asynchronous thread per galaxy which handles domain resolution, and can
    -- block its own queue of ByteStrings to send.
    --
    -- Maybe perform the resolution asynchronously, injecting into the resolver
    -- queue as a message.
    --
    -- TODO: Figure out how the real haskell time library works.
    galaxyResolver :: Galaxy -> TVar [Turf] -> TQueue ByteString
                   -> TQueue (SockAddr, ByteString)
                   -> IO (Async ())
    galaxyResolver galaxy turfVar incoming outgoing =
      async $ loop Nothing Time.unixEpoch
      where
        loop :: Maybe SockAddr -> Time.Wen -> IO ()
        loop lastGalaxyIP lastLookupTime = do
          packet <- atomically $ readTQueue incoming

          i <- checkIP lastGalaxyIP lastLookupTime
          case i of
            (Nothing, t) -> do
              -- We've failed to lookup the IP. Drop the outbound packet
              -- because we have no IP for our galaxy, including possible
              -- previous IPs.
              loop Nothing t
            (Just ip, t) -> do
              queueSendToGalaxy ip packet
              loop (Just ip) t

        checkIP :: Maybe SockAddr -> Time.Wen -> IO (Maybe SockAddr, Time.Wen)
        checkIP lastIP lastLookupTime = do
          current <- Time.now
          if (Time.gap current lastLookupTime ^. Time.secs) < 300
          then pure (lastIP, lastLookupTime)
          else do
            toCheck <- atomically $ readTVar turfVar
            ip <- resolveFirstIP lastIP toCheck
            timeAfterResolution <- Time.now
            pure (ip, timeAfterResolution)

        resolveFirstIP :: Maybe SockAddr -> [Turf] -> IO (Maybe SockAddr)
        resolveFirstIP prevIP [] = do
          -- print ("ames: czar at %s: not found (b)\n")
          pure prevIP

        resolveFirstIP prevIP (x:xs) = do
          let hostname = buildDNS galaxy x
          listIPs <- getAddrInfo Nothing (Just hostname) Nothing
          case listIPs of
            []     -> resolveFirstIP prevIP xs
            (y:ys) -> pure $ Just $ addrAddress y

        buildDNS :: Galaxy -> Turf -> String
        buildDNS (Galaxy g) turf = name ++ "." ++ (unpack $ _turfText turf)
          where
            nameWithSig = Ob.renderPatp $ Ob.patp $ fromIntegral g
            name = case stripPrefix "~" nameWithSig of
              Nothing -> error "Urbit.ob didn't produce string with ~"
              Just x  -> (unpack x)

        queueSendToGalaxy :: SockAddr -> ByteString -> IO ()
        queueSendToGalaxy inet packet =
          atomically $ writeTQueue outgoing (inet, packet)
