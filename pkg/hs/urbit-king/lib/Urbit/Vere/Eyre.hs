{-|
    Http Server Driver

    TODO Make sure that HTTP sockets get closed on shutdown.

    TODO What is this about?

        //  if we don't explicitly set this field, h2o will send with
        //  transfer-encoding: chunked
        //
        if ( 1 == has_len_i ) {
          rec_u->res.content_length = ( 0 == gen_u->bod_u ) ?
                                      0 : gen_u->bod_u->len_w;
        }

    TODO Does this matter, is is using WAI's default behavior ok?

        rec_u->res.reason = (status < 200) ? "weird" :
                            (status < 300) ? "ok" :
                            (status < 400) ? "moved" :
                            (status < 500) ? "missing" :
                            "hosed";
-}

module Urbit.Vere.Eyre
  ( eyre
  , multiServ
  , ShipAPI(..)
  )
where

import Urbit.Prelude         hiding (Builder)

import Urbit.Arvo            hiding (ServerId, reqUrl, secure)
import Urbit.King.Config
import Urbit.Vere.Eyre.Wai   hiding (ReqId)
import Urbit.Vere.Pier.Types

import Data.PEM         (pemParseBS, pemWriteBS)
import Network.Socket   (SockAddr(..))
import RIO.Prelude      (decodeUtf8Lenient)
import System.Directory (doesFileExist, removeFile)
import System.Random    (randomIO)
import Urbit.Vere.Http  (convertHeaders, unconvertHeaders)

import qualified Network.HTTP.Types          as H
import qualified Network.Socket              as Net
import qualified Network.Wai                 as W
import qualified Network.Wai.Handler.Warp    as W
import qualified Network.Wai.Handler.WarpTLS as W


-- Internal Types --------------------------------------------------------------

type HasShipEnv e = (HasLogFunc e, HasNetworkConfig e, HasPierConfig e)

type ReqId = UD

data Ports = Ports
    { pHttps :: Maybe Port
    , pHttp  :: Port
    , pLoop  :: Port
    }
  deriving (Eq, Ord, Show)

newtype Drv = Drv { _unDrv :: MVar (Maybe Serv) }

data Serv = Serv
    { sServId    :: ServId
    , sConfig    :: HttpServerConf
    , sLoopTid   :: Async ()
    , sHttpTid   :: Async ()
    , sHttpsTid  :: Maybe (Async ())
    , sLoopSock  :: Net.Socket
    , sHttpSock  :: Net.Socket
    , sHttpsSock :: Net.Socket
    , sPorts     :: Ports
    , sPortsFile :: FilePath
    , sLiveReqs  :: TVar LiveReqs
    }


-- Generic Service Stop/Restart -- Using an MVar for Atomicity -----------------

{-|
    Restart a running service.

    This can probably be made simpler, but it

    - Sets the MVar to Nothing if there was an exception whil starting
      or stopping the service.

    - Keeps the MVar lock until the restart process finishes.
-}
restartService :: ∀e s. HasLogFunc e
               => MVar (Maybe s)
               -> RIO e s
               -> (s -> RIO e ())
               -> RIO e (Either SomeException s)
restartService vServ sstart kkill = do
    logDebug "restartService"
    modifyMVar vServ $ \case
        Nothing -> doStart
        Just sv -> doRestart sv
  where
    doRestart :: s -> RIO e (Maybe s, Either SomeException s)
    doRestart serv = do
        logDebug "doStart"
        try (kkill serv) >>= \case
            Left exn -> pure (Nothing, Left exn)
            Right () -> doStart

    doStart :: RIO e (Maybe s, Either SomeException s)
    doStart = do
        logDebug "doStart"
        try sstart <&> \case
            Right s  -> (Just s,  Right s)
            Left exn -> (Nothing, Left exn)

stopService :: HasLogFunc e
            => MVar (Maybe s)
            -> (s -> RIO e ())
            -> RIO e (Either SomeException ())
stopService vServ kkill = do
    logDebug "stopService"
    modifyMVar vServ $ \case
        Nothing -> pure (Nothing, Right ())
        Just sv -> do res <- try (kkill sv)
                      pure (Nothing, res)


-- Ports File ------------------------------------------------------------------

removePortsFile :: FilePath -> RIO e ()
removePortsFile pax =
    io (doesFileExist pax) >>= \case
        True  -> io $ removeFile pax
        False -> pure ()

portsFileText :: Ports -> Text
portsFileText Ports{..} =
  unlines $ catMaybes
    [ pHttps <&> \p -> (tshow p <> " secure public")
    , Just (tshow (unPort pHttp) <> " insecure public")
    , Just (tshow (unPort pLoop) <> " insecure loopback")
    ]

writePortsFile :: FilePath -> Ports -> RIO e ()
writePortsFile f = writeFile f . encodeUtf8 . portsFileText


-- Utilities for Constructing Events -------------------------------------------

data WhichServer = Secure | Insecure | Loopback
  deriving (Eq)

servEv :: HttpServerEv -> Ev
servEv = EvBlip . BlipEvHttpServer

bornEv :: KingId -> Ev
bornEv king =
    servEv $ HttpServerEvBorn (king, ()) ()

liveEv :: ServId -> Ports -> Ev
liveEv sId Ports{..} =
    servEv $ HttpServerEvLive (sId, ()) pHttp pHttps

cancelEv :: ServId -> ReqId -> Ev
cancelEv sId reqId =
    servEv $ HttpServerEvCancelRequest (sId, reqId, 1, ()) ()

reqEv :: ServId -> ReqId -> WhichServer -> Address -> HttpRequest -> Ev
reqEv sId reqId which addr req =
    case which of
        Loopback ->
            servEv $ HttpServerEvRequestLocal (sId, reqId, 1, ())
                   $ HttpServerReq False addr req
        _        ->
            servEv $ HttpServerEvRequest (sId, reqId, 1, ())
                   $ HttpServerReq (which == Secure) addr req


-- Top-Level Driver Interface --------------------------------------------------

data CantOpenPort = CantOpenPort W.Port
  deriving (Eq, Ord, Show, Exception)

data WhichPort
  = WPSpecific W.Port
  | WPChoices [W.Port]

data SockOpts = SockOpts
  { soLocalhost :: Bool
  , soWhich     :: WhichPort
  }

data PortsToTry = PortsToTry
  { pttSec :: SockOpts
  , pttIns :: SockOpts
  , pttLop :: SockOpts
  }

{-|
    Opens a socket on some port, accepting connections from `127.0.0.1`
    if fake and `0.0.0.0` if real.

    It will attempt to open a socket on each of the supplied ports in
    order. If they all fail, it will ask the operating system to give
    us an open socket on *any* open port. If that fails, it will throw
    an exception.
-}
openPort :: forall e . HasLogFunc e => SockOpts -> RIO e (W.Port, Net.Socket)
openPort SockOpts {..} = case soWhich of
  WPSpecific x  -> insist (fromIntegral x)
  WPChoices  xs -> loop (fromIntegral <$> xs)

 where
  loop :: [W.Port] -> RIO e (W.Port, Net.Socket)
  loop = \case
    [] -> do
      logTrace "Fallback: asking the OS to give us some free port."
      ps <- io W.openFreePort
      logTrace (display ("Opened port " <> tshow (fst ps)))
      pure ps
    x : xs -> do
      logTrace (display ("Trying to open port " <> tshow x))
      io (tryOpen x) >>= \case
        Left (err :: IOError) -> do
          logWarn (display ("Failed to open port " <> tshow x))
          logWarn (display (tshow err))
          loop xs
        Right ps -> do
          logTrace (display ("Opened port " <> tshow (fst ps)))
          pure ps

  insist :: W.Port -> RIO e (W.Port, Net.Socket)
  insist p = do
    logTrace (display ("Opening configured port " <> tshow p))
    io (tryOpen p) >>= \case
      Left (err :: IOError) -> do
        logWarn (display ("Failed to open port " <> tshow p))
        logWarn (display (tshow err))
        throwIO (CantOpenPort p)
      Right ps -> do
        logTrace (display ("Opened port " <> tshow (fst ps)))
        pure ps

  bindTo = if soLocalhost then "127.0.0.1" else "0.0.0.0"

  getBindAddr :: W.Port -> IO SockAddr
  getBindAddr por =
    Net.getAddrInfo Nothing (Just bindTo) (Just (show por)) >>= \case
      []    -> error "this should never happen."
      x : _ -> pure (Net.addrAddress x)

  bindListenPort :: W.Port -> Net.Socket -> IO Net.PortNumber
  bindListenPort por sok = do
    Net.bind sok =<< getBindAddr por
    Net.listen sok 1
    Net.socketPort sok

  --  `inet_addr`, `bind`, and `listen` all throw `IOError` if they fail.
  tryOpen :: W.Port -> IO (Either IOError (W.Port, Net.Socket))
  tryOpen por = do
    sok <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
    try (bindListenPort por sok) >>= \case
      Left  exn -> Net.close sok $> Left exn
      Right por -> pure (Right (fromIntegral por, sok))

httpServerPorts :: HasShipEnv e => Bool -> RIO e PortsToTry
httpServerPorts fak = do
  ins       <- view (networkConfigL . ncHttpPort . to (fmap fromIntegral))
  sec       <- view (networkConfigL . ncHttpsPort . to (fmap fromIntegral))
  lop       <- view (networkConfigL . ncLocalPort . to (fmap fromIntegral))
  localMode <- view (networkConfigL . ncNetMode . to (== NMLocalhost))

  let local = localMode || fak

  let pttSec = case (sec, fak) of
        (Just p , _    ) -> SockOpts local (WPSpecific p)
        (Nothing, False) -> SockOpts local (WPChoices (443 : [8443 .. 8448]))
        (Nothing, True ) -> SockOpts local (WPChoices ([8443 .. 8448]))

  let pttIns = case (ins, fak) of
        (Just p , _    ) -> SockOpts local (WPSpecific p)
        (Nothing, False) -> SockOpts local (WPChoices (80 : [8080 .. 8085]))
        (Nothing, True ) -> SockOpts local (WPChoices [8080 .. 8085])

  let pttLop = case (lop, fak) of
        (Just p , _) -> SockOpts local (WPSpecific p)
        (Nothing, _) -> SockOpts local (WPChoices [12321 .. 12326])

  pure (PortsToTry { .. })

eyreApp
  :: HasLogFunc e
  => e
  -> ServId
  -> TVar LiveReqs
  -> (Ev -> STM ())
  -> WhichServer
  -> W.Application
eyreApp env sId vLive plan which =
  app env vLive onReq onCancel
 where
  bodFile "" = Nothing
  bodFile bs = Just $ File $ Octs bs

  onReq :: Word64 -> ReqInfo -> STM ()
  onReq reqId ReqInfo{..} = do
    let evBod = bodFile riBod
        evHdr = convertHeaders riHdr
        evUrl = Cord (decodeUtf8Lenient riUrl)
        evReq = HttpRequest riMet evUrl evHdr evBod
        reqUd = fromIntegral reqId
        event = reqEv sId reqUd which riAdr evReq

    plan event

  onCancel :: Word64 -> STM ()
  onCancel reqId = plan (cancelEv sId (fromIntegral reqId))

parseCerts :: ByteString -> Maybe (ByteString, [ByteString])
parseCerts bs = do
  pems <- pemParseBS bs & either (const Nothing) Just
  case pems of
    [] -> Nothing
    p:ps -> pure (pemWriteBS p, pemWriteBS <$> ps)

fByt :: File -> ByteString
fByt = unOcts . unFile

reorgHttpEvent :: HttpEvent -> [RespAct]
reorgHttpEvent = \case
  Start h b True  -> [RAFull (hSta h) (hHdr h) (fByt $ fromMaybe "" b)]
  Start h b False -> [RAHead (hSta h) (hHdr h) (fByt $ fromMaybe "" b)]
  Cancel ()       -> [RADone]
  Continue b done -> toList (RABloc . fByt <$> b)
                  <> if done then [RADone] else []
 where
  hHdr :: ResponseHeader -> [H.Header]
  hHdr = unconvertHeaders . headers

  hSta :: ResponseHeader -> H.Status
  hSta = toEnum . fromIntegral . statusCode


respond :: HasLogFunc e
        => Drv -> Word64 -> HttpEvent -> RIO e ()
respond (Drv v) reqId ev = do
    readMVar v >>= \case
        Nothing -> logError "Got a response to a request that does not exist."
        Just sv -> do logDebug $ displayShow ev
                      for_ (reorgHttpEvent ev) $
                        atomically . routeRespAct (sLiveReqs sv) reqId

wainBytes :: Wain -> ByteString
wainBytes = encodeUtf8 . unWain

startServ :: (HasPierConfig e, HasLogFunc e, HasNetworkConfig e)
          => Bool -> HttpServerConf -> (Ev -> STM ())
          -> RIO e Serv
startServ isFake conf plan = do
  logDebug "startServ"

  let tls = do (PEM key, PEM certs) <- hscSecure conf
               (cert, chain)        <- parseCerts (wainBytes certs)
               pure $ W.tlsSettingsChainMemory cert chain $ wainBytes key

  sId <- io $ ServId . UV . fromIntegral <$> (randomIO :: IO Word32)
  liv <- newTVarIO emptyLiveReqs

  ptt <- httpServerPorts isFake

  (httpPortInt,  httpSock)  <- openPort (pttIns ptt)
  (httpsPortInt, httpsSock) <- openPort (pttSec ptt)
  (loopPortInt,  loopSock)  <- openPort (pttLop ptt)

  let httpPort  = Port (fromIntegral httpPortInt)
      httpsPort = Port (fromIntegral httpsPortInt)
      loopPort  = Port (fromIntegral loopPortInt)

  let loopOpts  = W.defaultSettings & W.setPort (fromIntegral loopPort)
                                    & W.setHost "127.0.0.1"
                                    & W.setTimeout (5 * 60)
      httpOpts  = W.defaultSettings & W.setHost "*"
                                    & W.setPort (fromIntegral httpPort)
      httpsOpts = W.defaultSettings & W.setHost "*"
                                    & W.setPort (fromIntegral httpsPort)

  env <- ask

  logDebug "Starting loopback server"
  loopTid  <- async $ io
                    $ W.runSettingsSocket loopOpts loopSock
                    $ eyreApp env sId liv plan Loopback

  logDebug "Starting HTTP server"
  httpTid  <- async $ io
                    $ W.runSettingsSocket httpOpts httpSock
                    $ eyreApp env sId liv plan Insecure

  logDebug "Starting HTTPS server"
  httpsTid <- for tls $ \tlsOpts ->
                async $ io
                      $ W.runTLSSocket tlsOpts httpsOpts httpsSock
                      $ eyreApp env sId liv plan Secure

  pierPath <- view pierPathL
  let por = Ports (tls <&> const httpsPort) httpPort loopPort
      fil = pierPath <> "/.http.ports"

  logDebug $ displayShow (sId, por, fil)

  logDebug "Finished started HTTP Servers"

  pure $ Serv sId conf
              loopTid httpTid httpsTid
              httpSock httpsSock loopSock
              por fil liv

killServ :: HasLogFunc e => Serv -> RIO e ()
killServ Serv{..} = do
    cancel sLoopTid
    cancel sHttpTid
    traverse_ cancel sHttpsTid
    io $ Net.close sHttpSock
    io $ Net.close sHttpsSock
    io $ Net.close sLoopSock
    removePortsFile sPortsFile
    (void . waitCatch) sLoopTid
    (void . waitCatch) sHttpTid
    traverse_ (void . waitCatch) sHttpsTid

kill :: HasLogFunc e => Drv -> RIO e ()
kill (Drv v) = stopService v killServ >>= fromEither

eyre :: ∀e. HasShipEnv e
     => KingId -> QueueEv -> Bool
     -> ([Ev], RAcquire e (EffCb e HttpServerEf))
eyre king plan isFake =
    (initialEvents, runHttpServer)
  where
    initialEvents :: [Ev]
    initialEvents = [bornEv king]

    runHttpServer :: RAcquire e (EffCb e HttpServerEf)
    runHttpServer = handleEf <$> mkRAcquire (Drv <$> newMVar Nothing) kill

    restart :: Drv -> HttpServerConf -> RIO e Serv
    restart (Drv var) conf = do
        logDebug "Restarting http server"
        res <- fromEither =<<
                 restartService var (startServ isFake conf plan) killServ
        logDebug "Done restating http server"
        pure res

    handleEf :: Drv -> HttpServerEf -> RIO e ()
    handleEf drv = \case
        HSESetConfig (i, ()) conf -> do
            -- print (i, king)
            -- when (i == fromIntegral king) $ do
                logDebug "restarting"
                Serv{..} <- restart drv conf
                logDebug "Enqueue %live"
                atomically $ plan (liveEv sServId sPorts)
                logDebug "Write ports file"
                writePortsFile sPortsFile sPorts
        HSEResponse (i, req, _seq, ()) ev -> do
            -- print (i, king)
            -- when (i == fromIntegral king) $ do
                logDebug "respond"
                respond drv (fromIntegral req) ev


-- Multi-Tenet HTTP ------------------------------------------------------------

{-
  # Very First Phase: Shared HTTP, no SSL.

  - Global configuration flag for shared HTTP port.

  - Shared server starts before ships.

  - Shared server is informed when ships go up and come down.

  - Shared server delivers requests to existing HTTP driver.

  - Existing HTTP driver can send responses to shared HTTP server.
-}

type ShareRequ = (ServId, ReqId, WhichServer, Address, HttpRequest)
type ShareResp = (ServId, UD, UD, HttpEvent)

data ShipAPI = ShipAPI
  { sapiReq :: ShareRequ -> STM ()
  , sapiRes :: STM ShareResp
  }

data MultiServ = MultiServ
  { msPort :: Maybe Word16
  , msShip :: TVar (Map Ship ShipAPI)
  , msBoot :: TMVar (Ship, ShipAPI)
  , msDead :: TMVar Ship
  , msKill :: STM ()
  }

data Hap = Deþ Ship
         | Lif (Ship, ShipAPI)
         | Res ShareResp
         | Kil ()

multiServ :: HasLogFunc e => MultiServ -> RIO e ()
multiServ ms = do
  case msPort ms of
    Nothing -> doNothing ms
    Just po -> doSomething ms po

{-
  If the port is set, we do things for real. We run an HTTP server,
  sends requests to the appropriate ship, respond to requests when
  responses are given, and shuts down when the king shuts down.
-}
doSomething :: HasLogFunc e => MultiServ -> Word16 -> RIO e ()
doSomething MultiServ{..} httpPort = do
  logDebug "Starting HTTP server"

  let httpOpts  = W.defaultSettings & W.setHost "*"
                                    & W.setPort (fromIntegral httpPort)

  sId <- io $ ServId . UV . fromIntegral <$> (randomIO :: IO Word32)

  vShips :: TVar (Map Ship ShipAPI) <- newTVarIO mempty
  liv <- newTVarIO emptyLiveReqs

  env <- ask

  plan <- error "TODO"

  httpTid  <- async $ io
                    $ W.runSettings httpOpts
                    $ eyreApp env sId liv plan Insecure

  let onHapn :: STM Hap
      onHapn = asum [ Lif <$> takeTMVar msBoot
                    , Deþ <$> takeTMVar msDead
                    , Res <$> (readTVar vShips >>= asum . fmap sapiRes . toList)
                    , Kil <$> msKill
                    ]

  let loop = join $ atomically $ onHapn >>= \case
        Deþ s       -> modifyTVar' vShips (deleteMap s) >> pure loop
        Lif (s,api) -> modifyTVar' vShips (insertMap s api) >> pure loop
        Res _       -> error "TODO"
        Kil _       -> pure (cancel httpTid)

  loop

{-
  If the port is not set, we still run a thread for the shared server. It
  doesn't run an HTTP server, it ignores all responses, and it shuts
  down when the king shuts down.
-}
doNothing :: MultiServ -> RIO e ()
doNothing MultiServ{..} = do
  vShips :: TVar (Map Ship ShipAPI) <- newTVarIO mempty

  let onHapn :: STM Hap
      onHapn = asum [ Lif <$> takeTMVar msBoot
                    , Deþ <$> takeTMVar msDead
                    , Res <$> (readTVar vShips >>= asum . fmap sapiRes . toList)
                    , Kil <$> msKill
                    ]

  let loop = join $ atomically $ onHapn >>= \case
        Deþ s       -> modifyTVar' vShips (deleteMap s) >> pure loop
        Lif (s,api) -> modifyTVar' vShips (insertMap s api) >> pure loop
        Res _       -> pure loop
        Kil _       -> pure (pure ())

  loop
