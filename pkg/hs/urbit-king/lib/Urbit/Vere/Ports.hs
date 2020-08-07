module Urbit.Vere.Ports (HasPortControlApi(..),
                         PortControlApi(..),
                         buildInactivePorts,
                         buildNATPorts,
                         requestPortAccess) where

import Control.Monad.STM (check)
import Urbit.Prelude
import Network.NATPMP
import Data.Time.Clock.POSIX
import Data.Heap

-- This module deals with ports and port requests. When a component wants to
-- ensure that it is externally reachable, possibly from outside a NAT, it
-- makes a request to this module to hole-punch.

class HasPortControlApi a where
  portControlApiL :: Lens' a PortControlApi

data PortControlApi = PortControlApi
  { pAddPortRequest :: Word16 -> IO ()
  , pRemovePortRequest :: Word16 -> IO ()
  }

-- Builds a Ports struct which does nothing when called.
buildInactivePorts :: RIO e PortControlApi
buildInactivePorts = pure $ PortControlApi noop noop
  where
    noop x = pure ()

-- Builds a Ports struct which tries to hole-punch by talking to the NAT
-- gateway over NAT-PMP.
buildNATPorts :: (HasLogFunc e) => RIO e PortControlApi
buildNATPorts = do
  q <- newTQueueIO
  async $ portThread q
  pure $ PortControlApi (addRequest q) (removeRequest q)
  where
    addRequest :: TQueue PortThreadMsg -> Word16 -> IO ()
    addRequest q port = do
      resp <- newEmptyTMVarIO
      atomically $
        writeTQueue q (PTMInitialRequestOpen port (putTMVar resp True))
      atomically $ takeTMVar resp
      pure ()

    removeRequest :: TQueue PortThreadMsg -> Word16 -> IO ()
    removeRequest q port = atomically $ writeTQueue q (PTMRequestClose port)

portLeaseLifetime :: Word32
portLeaseLifetime = 15 * 60

-- Be paranoid and renew leases a full minute before they would naturally expire.
portRenewalTime :: Word32
portRenewalTime = portLeaseLifetime - 60

-- Messages sent from the main thread to the port mapping communication thread.
data PortThreadMsg
  = PTMInitialRequestOpen Word16 (STM ())
    -- ^ Does the open request, and then calls the passed in stm action to
    -- singal completion to the main thread. We want to block on the initial
    -- setting opening because we want the forwarding set up before we actually
    -- start using the port.

  | PTMRequestOpen Word16
    -- ^ Repeating open command we send to ourselves.

  | PTMRequestClose Word16
    -- ^ Close command. No synchronization because there's nothing we can do if
    -- it fails.

-- The port thread is an async which reads commands from an STM queue and then
-- executes them. This thread is here to bind the semantics that we want to how
-- NAT-PMP sees the world. We want for an RAcquire to be able to start a
-- request for port forwarding and then to release it when it goes out of
-- scope. OTOH, NAT-PMP is all timeout based, and we want that timeout to be
-- fairly short, such as 15 minutes, so the portThread needs to keep track of
-- the time of the next port request.
portThread :: forall e. (HasLogFunc e)
           => TQueue PortThreadMsg
           -> RIO e ()
portThread q = do
  pmp <- io $ initNatPmp
  case pmp of
    Left err -> do
      logError "ports: error initializing NAT-PMP. Falling back to null."
      loopErr q
    Right pmp -> loop pmp mempty
  where
    loop :: NatPmpHandle -> MinPrioHeap POSIXTime PortThreadMsg -> RIO e ()
    loop pmp nextRenew = forever $ do
      now <- io $ getPOSIXTime
      delay <- case viewHead nextRenew of
            Nothing -> newTVarIO False
            Just (fireTime, _) -> do
              let timeTo = fireTime - now
              let ms = round $ timeTo * 1000000
              registerDelay ms
      command <- atomically $
        (Left <$> fini delay) <|> (Right <$> readTQueue q)
      case command of
        Left () -> do
          -- the timeout has fired, meaning the top of the heap should be
          -- popped and rerun.
          case (Data.Heap.view nextRenew) of
            Nothing -> error "Internal heap managing error."
            Just ((_, msg), rest) -> handlePTM pmp msg rest
        Right msg -> handlePTM pmp msg nextRenew

    handlePTM :: NatPmpHandle
              -> PortThreadMsg
              -> MinPrioHeap POSIXTime PortThreadMsg
              -> RIO e ()
    handlePTM pmp msg nextRenew = case msg of
      PTMInitialRequestOpen p notifyComplete -> do
        logInfo $
          displayShow ("ports: sending initial request to NAT-PMP for port ", p)
        ret <- io $ setPortMapping pmp PTUDP p p portLeaseLifetime
        case ret of
          Left err -> do
            logError $
              displayShow ("ports: failed to request NAT-PMP for port ", p,
                           ":", err, ", disabling NAT-PMP")
            loopErr q
          Right _ -> do
            let filteredPort = filterPort p nextRenew
            now <- io $ getPOSIXTime
            let repeatMsg = PTMRequestOpen p
            let withRenew =
                  insert (now + (fromIntegral portRenewalTime), repeatMsg)
                  filteredPort
            atomically notifyComplete
            loop pmp withRenew

      PTMRequestOpen p -> do
        logInfo $
          displayShow ("ports: sending renewing request to NAT-PMP for port ",
                       p)
        ret <- io $ setPortMapping pmp PTUDP p p portLeaseLifetime
        case ret of
          Left err -> do
            logError $
              displayShow ("ports: failed to request NAT-PMP for port ", p,
                           ":", err, ", disabling NAT-PMP")
            loopErr q
          Right _ -> do
            let filteredPort = filterPort p nextRenew
            now <- io $ getPOSIXTime
            let withRenew =
                  insert (now + (fromIntegral portRenewalTime), msg) filteredPort
            loop pmp withRenew

      PTMRequestClose p -> do
        logInfo $
          displayShow ("ports: releasing lease for ", p)
        io $ setPortMapping pmp PTUDP p p 0
        let removed = filterPort p nextRenew
        loop pmp removed

    filterPort :: Word16
               -> MinPrioHeap POSIXTime PortThreadMsg
               -> MinPrioHeap POSIXTime PortThreadMsg
    filterPort p = Data.Heap.filter okPort
      where
        -- initial requests should never be in the heap
        okPort (_, PTMInitialRequestOpen _ _) = False
        okPort (_, PTMRequestOpen x) = p /= x
        okPort (_, PTMRequestClose x) = p /= x

    -- block (retry) until the delay TVar is set to True
    fini :: TVar Bool -> STM ()
    fini = check <=< readTVar

    -- The NAT system is considered "off" but we still need to signal back to
    -- the main thread that blocking actions are copmlete
    loopErr q = forever $ do
      (atomically $ readTQueue q) >>= \case
        PTMInitialRequestOpen _ onComplete -> atomically onComplete
        PTMRequestOpen _ -> pure ()
        PTMRequestClose _ -> pure ()

-- Acquire a port for the duration of the RAcquire.
requestPortAccess :: forall e. (HasPortControlApi e) => Word16 -> RAcquire e ()
requestPortAccess port = do
  mkRAcquire request release
  where
    request :: RIO e ()
    request = do
      env <- ask
      let api = env ^. portControlApiL
      io $ (pAddPortRequest api) port

    release :: () -> RIO e ()
    release _ = do
      env <- ask
      let api = env ^. portControlApiL
      io $ (pRemovePortRequest api) port

