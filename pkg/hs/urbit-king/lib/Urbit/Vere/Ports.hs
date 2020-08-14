module Urbit.Vere.Ports (HasPortControlApi(..),
                         PortControlApi,
                         buildInactivePorts,
                         buildNatPorts,
                         requestPortAccess) where

import Control.Monad.STM (check)
import Urbit.Prelude
import Network.NatPmp
import Data.Time.Clock.POSIX
import Network.Socket

import qualified Data.Heap as DH

-- This module deals with ports and port requests. When a component wants to
-- ensure that it is externally reachable, possibly from outside a NAT, it
-- makes a request to this module to hole-punch.

class HasPortControlApi a where
  portControlApiL :: Lens' a PortControlApi

data PortControlApi = PortControlApi
  { pAddPortRequest :: Word16 -> IO ()
  , pRemovePortRequest :: Word16 -> IO ()
  }

-- | Builds a PortControlApi struct which does nothing when called.
buildInactivePorts :: PortControlApi
buildInactivePorts = PortControlApi noop noop
 where
  noop x = pure ()

-- | Builds a PortControlApi struct which tries to hole-punch by talking to the
-- NAT gateway over NAT-PMP.
buildNatPorts :: (HasLogFunc e)
              => (Text -> RIO e ())
              -> RIO e PortControlApi
buildNatPorts stderr = do
  q <- newTQueueIO
  async $ portThread q stderr

  let addRequest port = do
        resp <- newEmptyTMVarIO
        atomically $
          writeTQueue q (PTMOpen port (putTMVar resp True))
        atomically $ takeTMVar resp
        pure ()

  let removeRequest port = atomically $ writeTQueue q (PTMClose port)

  pure $ PortControlApi addRequest removeRequest

portLeaseLifetime :: Word32
portLeaseLifetime = 15 * 60

-- Be paranoid and renew leases a full minute before they would naturally expire.
portRenewalTime :: Word32
portRenewalTime = portLeaseLifetime - 60

-- Messages sent from the main thread to the port mapping communication thread.
data PortThreadMsg
  = PTMOpen Word16 (STM ())
    -- ^ Does the open request, and then calls the passed in stm action to
    -- singal completion to the main thread. We want to block on the initial
    -- setting opening because we want the forwarding set up before we actually
    -- start using the port.

  | PTMClose Word16
    -- ^ Close command. No synchronization because there's nothing we can do if
    -- it fails.

-- We get requests to acquire a port as an RAII condition, but the actual APIs
-- are timeout based, so we have to maintain a heap of the next timer to
-- rerequest port access.
data RenewAction = RenewAction Word16

-- The port thread is an async which reads commands from an STM queue and then
-- executes them. This thread is here to bind the semantics that we want to how
-- NAT-PMP sees the world. We want for an RAcquire to be able to start a
-- request for port forwarding and then to release it when it goes out of
-- scope. OTOH, NAT-PMP is all timeout based, and we want that timeout to be
-- fairly short, such as 15 minutes, so the portThread needs to keep track of
-- the time of the next port request.
portThread :: forall e. (HasLogFunc e)
           => TQueue PortThreadMsg
           -> (Text -> RIO e ())
           -> RIO e ()
portThread q stderr = do
  initNatPmp >>= \case
    Left err -> do
      likelyIPAddress >>= \case
        Just ip@(192, 168, c, d) -> warnBehindRouterAndErr ip err
        Just ip@(10, _, _, _)    -> warnBehindRouterAndErr ip err
        _                        -> assumeOnPublicInternet
    Right pmp -> foundRouter pmp
 where
  warnBehindRouterAndErr (a, b, c, d) err = do
    stderr $ "port: you appear to be behind a router since your ip " ++
             "is " ++ (tshow a) ++ "." ++ (tshow b) ++ "." ++ (tshow c) ++
             "." ++ (tshow d) ++ ", but " ++
             "we could not request port forwarding (NAT-PMP error: " ++
             (tshow err) ++ ")"
    stderr $ "port: urbit performance will be degregaded unless you " ++
             "manually forward your ames port."
    loopErr q

  assumeOnPublicInternet = do
    stderr $ "port: couldn't find router; assuming on public internet"
    loopErr q

  foundRouter :: NatPmpHandle -> RIO e ()
  foundRouter pmp = do
    getPublicAddress pmp >>= \case
      Left _ -> pure ()
      Right addr -> do
        let (a, b, c, d) = hostAddressToTuple addr
        stderr $ "port: router reports that our public IP is " ++ (tshow a) ++
                 "." ++ (tshow b) ++ "." ++ (tshow c) ++ "." ++ (tshow d)
    loop pmp mempty

  loop :: NatPmpHandle -> DH.MinPrioHeap POSIXTime RenewAction -> RIO e ()
  loop pmp nextRenew = do
    now <- io $ getPOSIXTime
    delay <- case DH.viewHead nextRenew of
          Nothing -> newTVarIO False
          Just (fireTime, _) -> do
            let timeTo = fireTime - now
            let ms = round $ timeTo * 1000000
            registerDelay ms
    command <- atomically $
      (Left <$> fini delay) <|> (Right <$> readTQueue q)
    case command of
      Left ()   -> handleRenew pmp nextRenew
      Right msg -> handlePTM pmp msg nextRenew

  handlePTM :: NatPmpHandle
            -> PortThreadMsg
            -> DH.MinPrioHeap POSIXTime RenewAction
            -> RIO e ()
  handlePTM pmp msg nextRenew = case msg of
    PTMOpen p notifyComplete -> do
      logInfo $
        displayShow ("port: sending initial request to NAT-PMP for port ", p)
      setPortMapping pmp PTUdp p p portLeaseLifetime >>= \case
        Left err -> do
          logError $
            displayShow ("port: failed to request NAT-PMP for port ", p,
                         ":", err, ", disabling NAT-PMP")
          loopErr q
        Right _ -> do
          -- Filter any existing references to this port on the heap to ensure
          -- we don't double up on tasks.
          let filteredHeap = filterPort p nextRenew
          now <- io $ getPOSIXTime
          let withRenew =
                DH.insert (now + fromIntegral portRenewalTime, RenewAction p)
                          filteredHeap
          atomically notifyComplete
          loop pmp withRenew

    PTMClose p -> do
      logInfo $
        displayShow ("port: releasing lease for ", p)
      setPortMapping pmp PTUdp p p 0
      let removed = filterPort p nextRenew
      loop pmp removed

  handleRenew :: NatPmpHandle
              -> DH.MinPrioHeap POSIXTime RenewAction
              -> RIO e ()
  handleRenew pmp nextRenew = do
    case (DH.view nextRenew) of
      Nothing -> error "Internal heap managing error."
      Just ((_, RenewAction p), rest) -> do
        logInfo $
          displayShow ("port: sending renewing request to NAT-PMP for port ",
                       p)
        setPortMapping pmp PTUdp p p portLeaseLifetime >>= \case
          Left err -> do
            logError $
              displayShow ("port: failed to request NAT-PMP for port ", p,
                           ":", err, ", disabling NAT-PMP")
            loopErr q
          Right _ -> do
            -- We don't need to filter the port because we just did.
            now <- io $ getPOSIXTime
            let withRenew =
                  DH.insert (now + fromIntegral portRenewalTime, RenewAction p)
                            rest
            loop pmp withRenew

  filterPort :: Word16
             -> DH.MinPrioHeap POSIXTime RenewAction
             -> DH.MinPrioHeap POSIXTime RenewAction
  filterPort p = DH.filter okPort
   where
    okPort (_, RenewAction x) = p /= x

  -- block (retry) until the delay TVar is set to True
  fini :: TVar Bool -> STM ()
  fini = check <=< readTVar

  -- The NAT system is considered "off" but we still need to signal back to
  -- the main thread that blocking actions are complete.
  loopErr q = forever $ do
    (atomically $ readTQueue q) >>= \case
      PTMOpen _ onComplete -> atomically onComplete
      PTMClose _ -> pure ()

-- When we were unable to connect to a router, get the ip address on the
-- default ipv4 interface to check if we look like we're on an internal network
-- or not.
likelyIPAddress :: MonadIO m => m (Maybe (Word8, Word8, Word8, Word8))
likelyIPAddress = liftIO do
  -- Try opening a socket to 1.1.1.1 to get our own IP address. Since UDP is
  -- stateless and we aren't sending anything, we aren't actually contacting
  -- them in any way.
  sock <- socket AF_INET Datagram 0
  connect sock (SockAddrInet 53 (tupleToHostAddress (1, 1, 1, 1)))
  sockAddr <- getSocketName sock
  case sockAddr of
    SockAddrInet _ addr -> pure $ Just $ hostAddressToTuple addr
    _                   -> pure $ Nothing

-- Acquire a port for the duration of the RAcquire.
requestPortAccess :: forall e. (HasPortControlApi e) => Word16 -> RAcquire e ()
requestPortAccess port = do
  mkRAcquire request release
 where
  request :: RIO e ()
  request = do
    api <- view portControlApiL
    io $ pAddPortRequest api port

  release :: () -> RIO e ()
  release _ = do
    api <- view portControlApiL
    io $ pRemovePortRequest api port

