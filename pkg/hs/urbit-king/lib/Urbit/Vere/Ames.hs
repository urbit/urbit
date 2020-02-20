{-|
    Ames IO Driver -- UDP
-}

module Urbit.Vere.Ames (ames) where

import Urbit.Prelude

import Control.Monad.Extra       hiding (mapM_)
import Network.Socket            hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Urbit.Arvo                hiding (Fake)
import Urbit.King.Config
import Urbit.Vere.Pier.Types

import qualified Data.ByteString as BS
import qualified Data.Map        as M
import qualified Urbit.Ob        as Ob
import qualified Urbit.Time      as Time

-- Types -----------------------------------------------------------------------

data AmesDrv = AmesDrv
  { aTurfs         :: TVar (Maybe [Turf])
  , aGalaxies      :: IORef (M.Map Galaxy (Async (), TQueue ByteString))
  , aSocket        :: Maybe Socket
  , aListener      :: Async ()
  , aSendingQueue  :: TQueue (SockAddr, ByteString)
  , aSendingThread :: Async ()
  }

data NetworkMode = Fake | Localhost | Real | NoNetwork
  deriving (Eq, Ord, Show)


-- Utils -----------------------------------------------------------------------

galaxyPort :: NetworkMode -> Galaxy -> PortNumber
galaxyPort Fake (Patp g)      = fromIntegral g + 31337
galaxyPort Localhost (Patp g) = fromIntegral g + 13337
galaxyPort Real (Patp g)      = fromIntegral g + 13337
galaxyPort NoNetwork _        = fromIntegral 0

listenPort :: NetworkMode -> Ship -> PortNumber
listenPort m s | s < 256 = galaxyPort m (fromIntegral s)
listenPort m _ = 0

localhost :: HostAddress
localhost = tupleToHostAddress (127,0,0,1)

inaddrAny :: HostAddress
inaddrAny = tupleToHostAddress (0,0,0,0)

okayFakeAddr :: AmesDest -> Bool
okayFakeAddr = \case
    EachYes _          -> True
    EachNo (Jammed (AAIpv4 (Ipv4 a) _)) -> a == localhost
    EachNo (Jammed (AAVoid v)) -> absurd v

localhostSockAddr :: NetworkMode -> AmesDest -> SockAddr
localhostSockAddr mode = \case
    EachYes g   -> SockAddrInet (galaxyPort mode g) localhost
    EachNo (Jammed (AAIpv4 _ p)) -> SockAddrInet (fromIntegral p) localhost
    EachNo (Jammed (AAVoid v)) -> absurd v

bornEv :: KingId -> Ev
bornEv inst =
  EvBlip $ BlipEvNewt $ NewtEvBorn (fromIntegral inst, ()) ()

hearEv :: PortNumber -> HostAddress -> ByteString -> Ev
hearEv p a bs =
    EvBlip $ BlipEvAmes $ AmesEvHear () dest (MkBytes bs)
  where
    dest = EachNo $ Jammed $ AAIpv4 (Ipv4 a) (fromIntegral p)

_turfText :: Turf -> Text
_turfText = intercalate "." . reverse . fmap unCord . unTurf

renderGalaxy :: Galaxy -> Text
renderGalaxy = Ob.renderPatp . Ob.patp . fromIntegral . unPatp


--------------------------------------------------------------------------------

{-|
    inst      -- Process instance number.
    who       -- Which ship are we?
    enqueueEv -- Queue-event action.
    mPort     -- Explicit port override from command line arguments.

    TODO Handle socket exceptions in waitPacket

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
        drv <- mkRAcquire start stop
        pure (handleEffect drv)

    start :: RIO e AmesDrv
    start = do
        aTurfs         <- newTVarIO Nothing
        aGalaxies      <- newIORef mempty
        aSocket        <- bindSock
        aListener      <- async (waitPacket aSocket)
        aSendingQueue  <- newTQueueIO
        aSendingThread <- async (sendingThread aSendingQueue aSocket)
        pure            $ AmesDrv{..}

    netMode :: RIO e NetworkMode
    netMode = do
      if isFake
      then pure Fake
      else getNetworkingType >>= \case
        NetworkNormal -> pure Real
        NetworkLocalhost -> pure Localhost
        NetworkNone -> pure NoNetwork

    stop :: AmesDrv -> RIO e ()
    stop AmesDrv{..} = do
        readIORef aGalaxies >>= mapM_ (cancel . fst)

        cancel aSendingThread
        cancel aListener
        io $ maybeM (pure ()) (close') (pure aSocket)
        -- io $ close' aSocket

    bindSock :: RIO e (Maybe Socket)
    bindSock = getBindAddr >>= doBindSocket
        where
           getBindAddr = netMode >>= \case
              Fake      -> pure $ Just localhost
              Localhost -> pure $ Just localhost
              Real      -> pure $ Just inaddrAny
              NoNetwork -> pure Nothing

           doBindSocket :: Maybe HostAddress -> RIO e (Maybe Socket)
           doBindSocket Nothing = pure Nothing
           doBindSocket (Just bindAddr) = do
             mode <- netMode
             mPort <- getAmesPort
             let ourPort = maybe (listenPort mode who) fromIntegral mPort
             s  <- io $ socket AF_INET Datagram defaultProtocol

             logTrace $ displayShow ("(ames) Binding to port ", ourPort)
             let addr = SockAddrInet ourPort bindAddr
             () <- io $ bind s addr

             pure $ Just s

    waitPacket :: Maybe Socket -> RIO e ()
    waitPacket Nothing = pure ()
    waitPacket (Just s) = forever $ do
        (bs, addr) <- io $ recvFrom s 4096
        logTrace $ displayShow ("(ames) Received packet from ", addr)
        case addr of
            SockAddrInet p a -> atomically (enqueueEv $ hearEv p a bs)
            _                -> pure ()

    handleEffect :: AmesDrv -> NewtEf -> RIO e ()
    handleEffect drv@AmesDrv{..} = \case
      NewtEfTurf (_id, ()) turfs -> do
          atomically $ writeTVar aTurfs (Just turfs)

      NewtEfSend (_id, ()) dest (MkBytes bs) -> do
          atomically (readTVar aTurfs) >>= \case
            Nothing -> pure ()
            Just turfs -> do
              mode <- netMode
              (sendPacket drv mode dest bs)

    sendPacket :: AmesDrv -> NetworkMode -> AmesDest -> ByteString -> RIO e ()

    sendPacket AmesDrv{..} NoNetwork dest bs = pure ()

    sendPacket AmesDrv{..} Fake dest bs = do
      when (okayFakeAddr dest) $ atomically $
        writeTQueue aSendingQueue ((localhostSockAddr Fake dest), bs)

    -- In localhost only mode, regardless of the actual destination, send it to
    -- localhost.
    sendPacket AmesDrv{..} Localhost dest bs = atomically $
      writeTQueue aSendingQueue ((localhostSockAddr Localhost dest), bs)

    sendPacket AmesDrv{..} Real (EachYes galaxy) bs = do
      galaxies <- readIORef aGalaxies
      queue <- case M.lookup galaxy galaxies of
        Just (_, queue) -> pure queue
        Nothing -> do
          inQueue <- newTQueueIO
          thread <- async $ galaxyResolver galaxy aTurfs inQueue aSendingQueue
          modifyIORef (aGalaxies) (M.insert galaxy (thread, inQueue))
          pure inQueue

      atomically $ writeTQueue queue bs

    sendPacket AmesDrv{..} Real (EachNo (Jammed (AAIpv4 a p))) bs = do
      let addr = SockAddrInet (fromIntegral p) (unIpv4 a)
      atomically $ writeTQueue aSendingQueue (addr, bs)

    sendPacket AmesDrv{..} Real (EachNo (Jammed (AAVoid v))) bs = do
      pure (absurd v)

    -- An outbound queue of messages. We can only write to a socket from one
    -- thread, so coalesce those writes here.
    sendingThread :: TQueue (SockAddr, ByteString) -> Maybe Socket -> RIO e ()
    sendingThread queue Nothing = pure ()
    sendingThread queue (Just socket) = forever $
      do
        (dest, bs) <- atomically $ readTQueue queue
        logTrace $ displayShow ("(ames) Sending packet to ", socket, dest)
        sendAll bs dest
      where
        sendAll bs dest = do
          bytesSent <- io $ sendTo socket bs dest
          when (bytesSent /= BS.length bs) $ do
            sendAll (drop bytesSent bs) dest

    -- Asynchronous thread per galaxy which handles domain resolution, and can
    -- block its own queue of ByteStrings to send.
    --
    -- Maybe perform the resolution asynchronously, injecting into the resolver
    -- queue as a message.
    --
    -- TODO: Figure out how the real haskell time library works.
    galaxyResolver :: Galaxy -> TVar (Maybe [Turf]) -> TQueue ByteString
                   -> TQueue (SockAddr, ByteString)
                   -> RIO e ()
    galaxyResolver galaxy turfVar incoming outgoing =
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
          pure $ name ++ "." ++ (unpack $ _turfText turf)

        queueSendToGalaxy :: SockAddr -> ByteString -> RIO e ()
        queueSendToGalaxy inet packet = do
          atomically $ writeTQueue outgoing (inet, packet)
