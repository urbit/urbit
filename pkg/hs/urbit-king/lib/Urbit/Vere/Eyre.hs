{-|
  Eyre: Http Server Driver
-}

module Urbit.Vere.Eyre
  ( eyre
  )
where

import Urbit.Prelude         hiding (Builder)

import Urbit.Arvo            hiding (ServerId, reqUrl, secure)
import Urbit.King.Config
import Urbit.Vere.Eyre.Wai   hiding (ReqId)
import Urbit.Vere.Eyre.Serv
import Urbit.Vere.Pier.Types

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.PEM           (pemParseBS, pemWriteBS)
import RIO.Prelude        (decodeUtf8Lenient)
import System.Directory   (doesFileExist, removeFile)
import System.Random      (randomIO)
import Urbit.Vere.Http    (convertHeaders, unconvertHeaders)

import qualified Network.HTTP.Types as H


-- Internal Types --------------------------------------------------------------

type HasShipEnv e = (HasLogFunc e, HasNetworkConfig e, HasPierConfig e)

type ReqId = UD

data Ports = Ports
  { pHttps :: Maybe Port
  , pHttp  :: Port
  , pLoop  :: Port
  }
 deriving (Eq, Ord, Show)

newtype Drv = Drv (MVar (Maybe Serv))

data Serv = Serv
  { sServId    :: ServId
  , sConfig    :: HttpServerConf
  , sLop       :: ServApi
  , sIns       :: ServApi
  , sSec       :: Maybe ServApi
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

data SockOpts = SockOpts
  { soLocalhost :: Bool
  , soWhich     :: ServPort
  }

data PortsToTry = PortsToTry
  { pttSec :: SockOpts
  , pttIns :: SockOpts
  , pttLop :: SockOpts
  }

httpServerPorts :: HasShipEnv e => Bool -> RIO e PortsToTry
httpServerPorts fak = do
  ins       <- view (networkConfigL . ncHttpPort . to (fmap fromIntegral))
  sec       <- view (networkConfigL . ncHttpsPort . to (fmap fromIntegral))
  lop       <- view (networkConfigL . ncLocalPort . to (fmap fromIntegral))
  localMode <- view (networkConfigL . ncNetMode . to (== NMLocalhost))

  let local = localMode || fak

  let pttSec = case (sec, fak) of
        (Just p , _    ) -> SockOpts local (SPChoices $ singleton p)
        (Nothing, False) -> SockOpts local (SPChoices (443 :| [8443 .. 8453]))
        (Nothing, True ) -> SockOpts local (SPChoices (8443 :| [8444 .. 8453]))

  let pttIns = case (ins, fak) of
        (Just p , _    ) -> SockOpts local (SPChoices $ singleton p)
        (Nothing, False) -> SockOpts local (SPChoices (80 :| [8080 .. 8090]))
        (Nothing, True ) -> SockOpts local (SPChoices (8080 :| [8081 .. 8090]))

  let pttLop = case (lop, fak) of
        (Just p , _) -> SockOpts local (SPChoices $ singleton p)
        (Nothing, _) -> SockOpts local SPAnyPort

  pure (PortsToTry { .. })

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
        Just sv -> do logTrace $ displayShow ev
                      for_ (reorgHttpEvent ev) $
                        atomically . routeRespAct (sLiveReqs sv) reqId

wainBytes :: Wain -> ByteString
wainBytes = encodeUtf8 . unWain

startServ :: (HasPierConfig e, HasLogFunc e, HasNetworkConfig e)
          => Bool -> HttpServerConf -> (Ev -> STM ())
          -> RIO e Serv
startServ isFake conf plan = do
  logTrace "startServ"

  srvId <- io $ ServId . UV . fromIntegral <$> (randomIO :: IO Word32)

  let mTls = do
        (PEM key, PEM certs) <- hscSecure conf
        (cert, chain)        <- parseCerts (wainBytes certs)
        pure $ TlsConfig (wainBytes key) cert chain

  ptt <- httpServerPorts isFake

  let secRedi = Nothing -- TODO

  let soHost :: SockOpts -> ServHost
      soHost so = if soLocalhost so then SHLocalhost else SHAnyHostOk

  vLive <- newTVarIO emptyLiveReqs

  let bodFile "" = Nothing
      bodFile bs = Just $ File $ Octs bs

  let onReq :: WhichServer -> Word64 -> ReqInfo -> STM ()
      onReq which reqId ReqInfo{..} = do
        let evBod = bodFile riBod
        let evHdr = convertHeaders riHdr
        let evUrl = Cord (decodeUtf8Lenient riUrl)
        let evReq = HttpRequest riMet evUrl evHdr evBod
        let reqUd = fromIntegral reqId
        let event = reqEv srvId reqUd which riAdr evReq
        plan event

  let onKilReq = plan . cancelEv srvId . fromIntegral

  logTrace "Starting loopback server"
  lop <- serv vLive $ ServConf
    { scHost = soHost (pttLop ptt)
    , scPort = soWhich (pttLop ptt)
    , scRedi = Nothing
    , scType = STHttp $ ReqApi
        { rcReq = \() -> onReq Loopback
        , rcKil = onKilReq
        }
    }

  logTrace "Starting insecure server"
  ins <- serv vLive $ ServConf
    { scHost = soHost (pttIns ptt)
    , scPort = soWhich (pttIns ptt)
    , scRedi = secRedi
    , scType = STHttp $ ReqApi
        { rcReq = \() -> onReq Insecure
        , rcKil = onKilReq
        }
    }

  mSec <- for mTls $ \tls -> do
    logTrace "Starting secure server"
    serv vLive $ ServConf
      { scHost = soHost (pttSec ptt)
      , scPort = soWhich (pttSec ptt)
      , scRedi = Nothing
      , scType = STHttps tls $ ReqApi
          { rcReq = \() -> onReq Secure
          , rcKil = onKilReq
          }
      }

  pierPath <- view pierPathL

  lopPor <- atomically (fmap fromIntegral $ saPor lop)
  insPor <- atomically (fmap fromIntegral $ saPor ins)
  secPor <- for mSec (fmap fromIntegral . atomically . saPor)

  let por = Ports secPor insPor lopPor
      fil = pierPath <> "/.http.ports"

  logTrace $ displayShow ("EYRE", "All Servers Started.", srvId, por, fil)

  pure $ Serv srvId conf lop ins mSec por fil vLive

killServ :: HasLogFunc e => Serv -> RIO e ()
killServ Serv{..} = do
  atomically (saKil sLop)
  atomically (saKil sIns)
  for_ sSec (\sec -> atomically (saKil sec))
  removePortsFile sPortsFile

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
