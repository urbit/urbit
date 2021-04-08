{- |
  Raw UDP Server used by Ames driver.

  1. Opens a UDP socket and makes sure that it stays open.

    - If can't open the port, wait and try again repeatedly.
    - If there is an error reading or writting from the open socket,
      close it and open another, making sure, however, to reuse the
      same port

  2. Receives packets from the socket.

    - When packets come in from the socket, they go into a bounded queue.
    - If the queue is full, the packet is dropped.
    - If the socket is closed, wait and try again repeatedly.
    - `usRecv` gets the first packet from the queue.

  3. Sends packets to the socket.

    - Packets sent to `usSend` enter a bounded queue.
    - If that queue is full, the packet is dropped.
    - Packets are taken off the queue one at a time.
    - If the socket is closed (or broken), the packet is dropped.

  4. Runs until `usKill` is run, then all threads are killed and the
     socket is closed.
-}

module Urbit.Vere.Ames.UDP
  ( UdpServ(..)
  , fakeUdpServ
  , realUdpServ
  )
where

import Urbit.Prelude
import Urbit.Vere.Ports

import Network.Socket

import Control.Monad.STM         (retry)
import Network.Socket.ByteString (recvFrom, sendTo)
import Urbit.Vere.Stat           (AmesStat(..), bump)

-- Types -----------------------------------------------------------------------

data UdpServ = UdpServ
  { usSend :: SockAddr -> ByteString -> IO ()
  , usRecv :: STM (PortNumber, HostAddress, ByteString)
  , usKill :: IO ()
  }


-- Utils -----------------------------------------------------------------------

{- |
  Writes to queue and returns `True` unless the queue is full, then do
  nothing and return `False`.
-}
tryWriteTBQueue :: TBQueue x -> x -> STM Bool
tryWriteTBQueue q x = do
  isFullTBQueue q >>= \case
    True  -> pure False
    False -> writeTBQueue q x $> True

{- |
  Open a UDP socket and bind it to a port
-}
doBind :: PortNumber -> HostAddress -> IO (Either IOError Socket)
doBind por hos = tryIOError $ do
  sok <- io $ socket AF_INET Datagram defaultProtocol
  ()  <- io $ bind sok (SockAddrInet por hos)
  pure sok

{- |
  Open a UDP socket and bind it to a port.

  If this fails, wait 250ms and repeat forever.
-}
forceBind :: HasLogFunc e => PortNumber -> HostAddress -> RIO e Socket
forceBind por hos = go
 where
  go = do
    logInfo (display ("AMES: UDP: Opening socket on port " <> tshow por))
    io (doBind por hos) >>= \case
      Right sk -> do
        logInfo (display ("AMES: UDP: Opened socket on port " <> tshow por))
        pure sk
      Left err -> do
        logInfo (display ("AMES: UDP: " <> tshow err))
        logInfo ("AMES: UDP: Failed to open UDP socket. Waiting")
        threadDelay 250_000
        go

{- |
  Attempt to send a packet to a socket.

  If it fails, return `False`. Otherwise, return `True`.
-}
sendPacket :: HasLogFunc e => ByteString -> SockAddr -> Socket -> RIO e Bool
sendPacket fullBytes adr sok = do
  logDebug $ displayShow ("AMES", "UDP", "Sending packet.")
  res <- io $ tryIOError $ go fullBytes
  case res of
    Left err -> do
      logError $ displayShow ("AMES", "UDP", "Failed to send packet", err)
      pure False
    Right () -> do
      logDebug $ displayShow ("AMES", "UDP", "Packet sent.")
      pure True
 where
  go byt = do
    sent <- sendTo sok byt adr
    when (sent /= length byt) $ do
      go (drop sent byt)

{- |
  Attempt to receive a packet from a socket.

  - If an exception is throw, return `Left exn`.
  - If it wasn't an IPv4 packet, return `Right Nothing`.
  - Otherwise, return `Right (Just packet)`.
-}
recvPacket
  :: HasLogFunc e
  => Socket
  -> RIO e (Either IOError (Maybe (ByteString, PortNumber, HostAddress)))
recvPacket sok = do
  io (tryIOError $ recvFrom sok 4096) <&> \case
    Left  exn                   -> Left exn
    Right (b, SockAddrInet p a) -> Right (Just (b, p, a))
    Right (_, _               ) -> Right Nothing


-- Fake Server for No-Networking Mode ------------------------------------------

{- |
  Fake UDP API for no-networking configurations.
-}
fakeUdpServ :: HasLogFunc e => RIO e UdpServ
fakeUdpServ = do
  logInfo $ displayShow ("AMES", "UDP", "\"Starting\" fake UDP server.")
  pure UdpServ { .. }
 where
  usSend = \_ _ -> pure ()
  usRecv = retry
  usKill = pure ()


-- Real Server -----------------------------------------------------------------

{- |
  Real UDP server.  See module-level docs.
-}
realUdpServ
  :: forall e
   . (HasLogFunc e, HasPortControlApi e)
  => PortNumber
  -> HostAddress
  -> AmesStat
  -> RIO e UdpServ
realUdpServ startPort hos sat = do
  logInfo $ displayShow ("AMES", "UDP", "Starting real UDP server.")

  env <- ask

  vSock <- newTVarIO Nothing
  vFail <- newEmptyTMVarIO
  qSend <- newTBQueueIO 100 -- TODO Tuning
  qRecv <- newTBQueueIO 100 -- TODO Tuning

  {-
    If reading or writing to a socket fails, unbind it and tell the
    socket-open thread to close it and open another.

    This is careful about edge-cases. In any of these cases, do nothing.

      - If vSock isn't set to the socket we used, do nothing.
      - If vFail is already set (another thread signaled failure already).
  -}
  let signalBrokenSocket :: Socket -> RIO e ()
      signalBrokenSocket sock = do
        logInfo $ displayShow ("AMES", "UDP"
                               , "Socket broken. Requesting new socket"
                               )
        atomically $ do
          mSock <- readTVar vSock
          mFail <- tryReadTMVar vFail
          when (mSock == Just sock && mFail == Nothing) $ do
            putTMVar vFail sock
            writeTVar vSock Nothing

      enqueueRecvPacket :: PortNumber -> HostAddress -> ByteString -> RIO e ()
      enqueueRecvPacket p a b = do
        did <- atomically (tryWriteTBQueue qRecv (p, a, b))
        when (did == False) $ do
          bump (asUqf sat)
          logWarn $ displayShow $ ("AMES", "UDP",)
            "Dropping inbound packet because queue is full."

      enqueueSendPacket :: SockAddr -> ByteString -> RIO e ()
      enqueueSendPacket a b = do
        did <- atomically (tryWriteTBQueue qSend (a, b))
        when (did == False) $ do
          logWarn "AMES: UDP: Dropping outbound packet because queue is full."
  let opener por = do
        logInfo $ displayShow $ ("AMES", "UDP", "Trying to open socket, port",)
          por
        sk <- forceBind por hos
        sn <- io $ getSocketName sk
        sp <- io $ socketPort sk
        logInfo $ displayShow $ ("AMES", "UDP", "Got socket", sn, sp)

        let waitForRelease = do
              atomically (writeTVar vSock (Just sk))
              broken <- atomically (takeTMVar vFail)
              logWarn "AMES: UDP: Closing broken socket."
              io (close broken)

        case sn of
          (SockAddrInet boundPort _) ->
           -- When we're on IPv4, maybe port forward at the NAT.
           rwith (requestPortAccess $ fromIntegral boundPort) $
               \() -> waitForRelease
          _ -> waitForRelease

        opener sp

  tOpen <- async $ opener startPort

  tSend <- async $ forever $ join $ atomically $ do
    (adr, byt) <- readTBQueue qSend
    readTVar vSock <&> \case
      Nothing -> pure ()
      Just sk -> do
        okay <- sendPacket byt adr sk
        unless okay (signalBrokenSocket sk)

  tRecv <- async $ forever $ do
    atomically (readTVar vSock) >>= \case
      Nothing -> threadDelay 100_000
      Just sk -> do
        recvPacket sk >>= \case
          Left exn -> do
            bump (asUdf sat)
            logError "AMES: UDP: Failed to receive packet"
            signalBrokenSocket sk
          Right Nothing -> do
            bump (asUi6 sat)
            logError "AMES: UDP: Dropping non-ipv4 packet"
            pure ()
          Right (Just (b, p, a)) -> do
            logDebug "AMES: UDP: Received packet."
            bump (asUdp sat)
            enqueueRecvPacket p a b

  let shutdown = do
        logInfo "AMES: UDP: Shutting down. (killing threads)"
        cancel tOpen
        cancel tSend
        cancel tRecv
        logInfo "AMES: UDP: Shutting down. (closing socket)"
        io $ join $ atomically $ do
          res <- readTVar vSock <&> maybe (pure ()) close
          writeTVar vSock Nothing
          pure res

  pure $ UdpServ { usSend = \a b -> runRIO env (enqueueSendPacket a b)
                 , usRecv = readTBQueue qRecv
                 , usKill = runRIO env shutdown
                 }
