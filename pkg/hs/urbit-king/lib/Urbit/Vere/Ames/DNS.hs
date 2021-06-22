{-|
  Handles sending packets to galaxies. We need to get their IP addresses
  from DNS, which is more complicated.

-- Asynchronous thread per galaxy which handles domain resolution, and can
-- block its own queue of ByteStrings to send.
--
-- Maybe perform the resolution asynchronously, injecting into the resolver
-- queue as a message.
--
-- TODO: Figure out how the real haskell time library works.

-- We've failed to lookup the IP. Drop the outbound packet
-- because we have no IP for our galaxy, including possible
-- previous IPs.

{-
- Sending Packets to Galaxies.
  - Each galaxy has it's own DNS resolution thread.
  - Initially, no threads are started.
  - To send a message to a galaxy,
    - Check to see if it already has a resolution thread.
    - If it does, pass the packet to that thread.
    - If it doesn't, start a new thread and give it the packet.
- Galaxy resolution threads work as follows:
  - First, they are given:
    - They know which galaxy they are responsible for.
    - They have access to the turfs TVar (shared state with Ames driver).
    - They can be given packets (to be send to their galaxy).
    - They must be given a way to send UDP packets.
  - Next, we loop forever
    - In the loop we track:
      - the last-known IP address.
      - the time when we last looked up the IP address.
    - We wait to be given a packet.
    - We get the IP address.
      - If we looked up the IP address in the last 5 minute, use the
        cached IP address.
        - Just use the one from last time.
      - Otherwise,
        - Do a DNS lookup.
        - Go through the turf list one item at a time.
          - Try each one.
            - If it resolves to one-or-more IP addresses,
              - Use the first one.
            - If it resolves to zero IP addresses, move on to the next turf.
          - If none of the turfs can be used to resolve the IP address,
            then we don't know where the galaxy is.
            - Drop the packet.
-}
-}

module Urbit.Vere.Ames.DNS
  ( NetworkMode(..)
  , ResolvServ(..)
  , resolvServ
  , galaxyPort
  , renderGalaxy
  )
where

import Urbit.Prelude

import Network.Socket
import Urbit.Arvo            hiding (Fake)

import qualified Data.Map.Strict as M
import qualified Urbit.Noun.Time as Time
import qualified Urbit.Ob        as Ob


-- Types -----------------------------------------------------------------------

data NetworkMode = Fake | Localhost | Real | NoNetwork
  deriving (Eq, Ord, Show)

data ResolvServ = ResolvServ
  { rsSend :: Galaxy -> ByteString -> IO ()
  , rsKill :: IO ()
  }


-- Utils -----------------------------------------------------------------------

galaxyPort :: NetworkMode -> Galaxy -> PortNumber
galaxyPort Fake      (Patp g) = fromIntegral g + 31337
galaxyPort Localhost (Patp g) = fromIntegral g + 13337
galaxyPort Real      (Patp g) = fromIntegral g + 13337
galaxyPort NoNetwork _        = fromIntegral 0

turfText :: Turf -> Text
turfText = intercalate "." . reverse . fmap unCord . unTurf

renderGalaxy :: Galaxy -> Text
renderGalaxy = fromT . Ob.renderPatp . Ob.patp . fromIntegral . unPatp

galaxyHostname :: Galaxy -> Turf -> Text
galaxyHostname g t = galaName g ++ "." ++ turfText t
 where
  stripSig :: Text -> Text
  stripSig inp = fromMaybe inp (stripPrefix "~" inp)

  galaName :: Galaxy -> Text
  galaName = stripSig . renderGalaxy

resolv :: Galaxy -> [Turf] -> IO (Maybe (Turf, Text, PortNumber, SockAddr))
resolv gal = go
 where
  go = \case
    []           -> pure Nothing
    turf : turfs -> do
      let host = galaxyHostname gal turf
          port = galaxyPort Real gal
      getAddrInfo Nothing (Just (unpack host)) (Just (show port)) >>= \case
        []     -> go turfs
        ip : _ -> pure $ Just (turf, host, port, addrAddress ip)

doResolv
  :: HasLogFunc e
  => Galaxy
  -> (Time.Wen, Maybe SockAddr)
  -> [Turf]
  -> (Text -> RIO e ())
  -> RIO e (Maybe SockAddr, Time.Wen)
doResolv gal (prevWen, prevIP) turfs stderr = do
  current <- io $ Time.now
  if (Time.gap current prevWen ^. Time.secs) < 300
    then pure (prevIP, prevWen)
    else do
      tim <- io (Time.now)
      io (resolv gal turfs) >>= \case
        Nothing -> do
          stderr $ "ames: czar at " ++ galStr ++ ": not found"
          logInfo $ displayShow ("(ames) Failed to lookup IP for ", gal)
          pure (prevIP, tim)
        Just (turf, host, port, addr) -> do
          when (Just addr /= prevIP) (printCzar addr)
          logInfo $ displayShow ("(ames) Looked up ", host, port, turf, addr)
          pure (Just addr, tim)
 where
  galStr = renderGalaxy gal
  printCzar addr = stderr $ "ames: czar " ++ galStr ++ ": ip " ++ show addr


resolvWorker
  :: forall e
   . HasLogFunc e
  => Galaxy
  -> TVar (Maybe [Turf])
  -> TVar (Time.Wen, Maybe SockAddr)
  -> STM ByteString
  -> (SockAddr -> ByteString -> IO  ())
  -> (Text -> RIO e ())
  -> RIO e (Async ())
resolvWorker gal vTurfs vLast waitMsg send stderr = async (forever go)
 where
  logDrop =
    logInfo $ displayShow ("(ames) Dropping packet; no ip for galaxy ", gal)

  go :: RIO e ()
  go = do
    (packt, turfs, (lastTime, lastAddr)) <- atomically
      ((,,) <$> waitMsg <*> readTVar vTurfs <*> readTVar vLast)

    (newAddr, newTime) <- doResolv gal
                                   (lastTime, lastAddr)
                                   (fromMaybe [] turfs)
                                   stderr

    maybe logDrop (\ip -> io (send ip packt)) newAddr

    atomically $ writeTVar vLast (newTime, newAddr)


resolvServ
  :: HasLogFunc e
  => TVar (Maybe [Turf])
  -> (SockAddr -> ByteString -> IO ())
  -> (Text -> RIO e ())
  -> RIO e ResolvServ
resolvServ vTurfs send stderr = do
  vGala <- newTVarIO (mempty :: Map Galaxy (Async (), TQueue ByteString))
  vDead <- newTVarIO False
  envir <- ask

  let spawnWorker :: Galaxy -> IO (Async (), TQueue ByteString)
      spawnWorker gal = runRIO envir $ do
        que <- newTQueueIO
        las <- newTVarIO (Time.unixEpoch, Nothing)
        tid <- resolvWorker gal vTurfs las (readTQueue que) send stderr
        pure (tid, que)

  let getWorker :: Galaxy -> IO (Async (), TQueue ByteString)
      getWorker gal = do
        (fmap (lookup gal) $ atomically $ readTVar vGala) >>= \case
          Just (tid, que) -> do
            pure (tid, que)
          Nothing -> do
            (tid, que) <- spawnWorker gal
            atomically $ modifyTVar' vGala (M.insert gal (tid, que))
            pure (tid, que)

  let doSend :: Galaxy -> ByteString -> IO ()
      doSend gal byt = do
        dead <- atomically (readTVar vDead)
        unless dead $ do
          (_, que) <- getWorker gal
          atomically (writeTQueue que byt)

  let doKill :: IO ()
      doKill = do
        galas <- atomically $ do
          writeTVar vDead True
          readTVar vGala
        for_ galas (cancel . fst)

  pure (ResolvServ doSend doKill)
