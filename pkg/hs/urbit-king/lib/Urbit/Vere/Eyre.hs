{-|
  Eyre: Http Server Driver
-}

module Urbit.Vere.Eyre
  ( eyre
  , eyre'
  )
where

import Urbit.Prelude hiding (Builder)

import Urbit.Arvo                hiding (ServerId, reqUrl)
import Urbit.King.App            ( killKingActionL
                                 , HasKingId(..)
                                 , HasMultiEyreApi(..)
                                 , HasPierEnv(..)
                                 )
import Urbit.King.Config
import Urbit.Vere.Eyre.Multi
import Urbit.Vere.Eyre.PortsFile
import Urbit.Vere.Eyre.Serv
import Urbit.Vere.Eyre.Service
import Urbit.Vere.Eyre.Wai
import Urbit.Vere.Pier.Types

import Data.List.NonEmpty          (NonEmpty((:|)))
import Data.PEM                    (pemParseBS, pemWriteBS)
import RIO.Prelude                 (decodeUtf8Lenient)
import System.Random               (randomIO)
import Urbit.Vere.Http             (convertHeaders, unconvertHeaders)
import Urbit.Vere.Eyre.KingSubsite (KingSubsite)

import qualified Network.HTTP.Types as H


-- Types -----------------------------------------------------------------------

type HasShipEnv e = (HasLogFunc e, HasNetworkConfig e, HasPierConfig e)

type ReqId = UD

newtype Drv = Drv (MVar (Maybe Serv))

data SockOpts = SockOpts
  { soLocalhost :: Bool
  , soWhich     :: ServPort
  }

data PortsToTry = PortsToTry
  { pttSec :: SockOpts
  , pttIns :: SockOpts
  , pttLop :: SockOpts
  }

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


-- Utilities for Constructing Events -------------------------------------------

servEv :: HttpServerEv -> Ev
servEv = EvBlip . BlipEvHttpServer

bornEv :: KingId -> Ev
bornEv king = servEv $ HttpServerEvBorn (king, ()) ()

liveEv :: ServId -> Ports -> Ev
liveEv sId Ports {..} = servEv $ HttpServerEvLive (sId, ()) pHttp pHttps

cancelEv :: ServId -> ReqId -> EvErr
cancelEv sId reqId =
  EvErr (servEv (HttpServerEvCancelRequest (sId, reqId, 1, ()) ())) cancelFailed

cancelFailed :: WorkError -> IO ()
cancelFailed _ = pure ()

reqEv :: ServId -> ReqId -> WhichServer -> Address -> HttpRequest -> Ev
reqEv sId reqId which addr req = case which of
  Loopback -> servEv $ HttpServerEvRequestLocal (sId, reqId, 1, ())
                     $ HttpServerReq False addr req
  _        -> servEv $ HttpServerEvRequest (sId, reqId, 1, ())
                     $ HttpServerReq (which == Secure) addr req


-- Based on Pier+Config, which ports should each server run? -------------------

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


-- Convert Between Urbit and WAI types. ----------------------------------------

parseTlsConfig :: (Key, Cert) -> Maybe TlsConfig
parseTlsConfig (PEM key, PEM certs) = do
  let (cerByt, keyByt) = (wainBytes certs, wainBytes key)
  pems <- pemParseBS (toBS cerByt) & either (const Nothing) Just
  (cert, chain) <- case pems of
    []     -> Nothing
    p : ps -> pure (pemWriteBS p, pemWriteBS <$> ps)
  pure $ TlsConfig (keyByt) (fromBS cert) (fromBS <$> chain)
 where
  wainBytes = encodeUtf8 . unWain

parseHttpEvent :: HttpEvent -> [RespAct]
parseHttpEvent = \case
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

  fByt :: File -> ByteString
  fByt = unOcts . unFile

requestEvent :: ServId -> WhichServer -> Word64 -> ReqInfo -> Ev
requestEvent srvId which reqId ReqInfo{..} = reqEv srvId reqUd which riAdr evReq
 where
  evBod = bodFile riBod
  evHdr = convertHeaders riHdr
  evUrl = Cord (fromT $ decodeUtf8Lenient $ toBS riUrl)
  evReq = HttpRequest riMet evUrl evHdr evBod
  reqUd = fromIntegral reqId

  bodFile :: ByteString -> Maybe File
  bodFile "" = Nothing
  bodFile bs = Just $ File $ Octs bs


-- Running Servers -------------------------------------------------------------

execRespActs :: HasLogFunc e => Drv -> Ship -> Word64 -> HttpEvent -> RIO e ()
execRespActs (Drv v) who reqId ev = readMVar v >>= \case
  Nothing -> logError "Got a response to a request that does not exist."
  Just sv -> do
    logDebug $ displayShow ev
    for_ (parseHttpEvent ev) $ \act -> do
      atomically (routeRespAct who (sLiveReqs sv) reqId act)

startServ
  :: (HasPierConfig e, HasLogFunc e, HasMultiEyreApi e, HasNetworkConfig e)
  => Ship
  -> Bool
  -> HttpServerConf
  -> (EvErr -> STM ())
  -> (Text -> RIO e ())
  -> IO ()
  -> KingSubsite
  -> RIO e Serv
startServ who isFake conf plan stderr onFatal sub = do
  logInfo (displayShow ("EYRE", "startServ"))

  multi <- view multiEyreApiL

  let vLive = meaLive multi

  srvId <- io $ ServId . UV . fromIntegral <$> (randomIO :: IO Word32)

  let mTls = hscSecure conf >>= parseTlsConfig

  mCre <- mTls & \case
   Nothing -> pure Nothing
   Just tc -> configCreds tc & \case
     Right rs -> pure (Just (tc, rs))
     Left err -> do
       logError "Couldn't Load TLS Credentials."
       pure Nothing

  ptt <- httpServerPorts isFake

  {-
    TODO If configuration requests a redirect, get the HTTPS port (if
    configuration specifies a specific port, use that. Otherwise, wait
    for the HTTPS server to start and then use the port that it chose).
    and run an HTTP server that simply redirects to the HTTPS server.
  -}
  let secRedi = Nothing

  let soHost :: SockOpts -> ServHost
      soHost so = if soLocalhost so then SHLocalhost else SHAnyHostOk

  noHttp  <- view (networkConfigL . ncNoHttp)
  noHttps <- view (networkConfigL . ncNoHttps)

  let reqEvFailed _ = pure ()

  let onReq :: WhichServer -> Ship -> Word64 -> ReqInfo -> STM ()
      onReq which _ship reqId reqInfo =
        plan $ EvErr (requestEvent srvId which reqId reqInfo) reqEvFailed

  let onKilReq :: Ship -> Word64 -> STM ()
      onKilReq _ship = plan . cancelEv srvId . fromIntegral

  logInfo (displayShow ("EYRE", "joinMultiEyre", who, mTls, mCre))

  atomically (joinMultiEyre multi who mCre onReq onKilReq sub)

  logInfo $ displayShow ("EYRE", "Starting loopback server")
  lop <- serv vLive onFatal $ ServConf
    { scHost = soHost (pttLop ptt)
    , scPort = soWhich (pttLop ptt)
    , scRedi = Nothing
    , scFake = False
    , scType = STHttp who sub $ ReqApi
        { rcReq = onReq Loopback
        , rcKil = onKilReq
        }
    }

  logInfo $ displayShow ("EYRE", "Starting insecure server")
  ins <- serv vLive onFatal $ ServConf
    { scHost = soHost (pttIns ptt)
    , scPort = soWhich (pttIns ptt)
    , scRedi = secRedi
    , scFake = noHttp
    , scType = STHttp who sub $ ReqApi
        { rcReq = onReq Insecure
        , rcKil = onKilReq
        }
    }

  mSec <- for mTls $ \tls -> do
    logInfo "Starting secure server"
    serv vLive onFatal $ ServConf
      { scHost = soHost (pttSec ptt)
      , scPort = soWhich (pttSec ptt)
      , scRedi = Nothing
      , scFake = noHttps
      , scType = STHttps who tls sub $ ReqApi
          { rcReq = onReq Secure
          , rcKil = onKilReq
          }
      }

  pierPath <- view pierPathL

  lopPor <- atomically (fmap fromIntegral $ saPor lop)
  insPor <- atomically (fmap fromIntegral $ saPor ins)
  secPor <- for mSec (fmap fromIntegral . atomically . saPor)

  let por = Ports secPor insPor lopPor
      fil = pierPath <> "/.http.ports"

  logInfo $ displayShow ("EYRE", "All Servers Started.", srvId, por, fil)
  for secPor $ \p ->
    stderr ("http: secure web interface live on https://localhost:" <> show p)
  stderr ("http: web interface live on http://localhost:" <> show insPor)
  stderr ("http: loopback live on http://localhost:" <> show lopPor)

  pure (Serv srvId conf lop ins mSec por fil vLive)


-- Eyre Driver -----------------------------------------------------------------

_bornFailed :: e -> WorkError -> IO ()
_bornFailed env _ = runRIO env $ do
  pure () -- TODO What should this do?

eyre'
  :: (HasPierEnv e, HasMultiEyreApi e)
  => Ship
  -> Bool
  -> (Text -> RIO e ())
  -> KingSubsite
  -> RIO e ([Ev], RAcquire e (DriverApi HttpServerEf))

eyre' who isFake stderr sub = do
  ventQ :: TQueue EvErr <- newTQueueIO
  env <- ask

  let (bornEvs, startDriver) =
        eyre env who (writeTQueue ventQ) isFake stderr sub

  let runDriver = do
        diOnEffect <- startDriver
        let diEventSource = fmap RRWork <$> tryReadTQueue ventQ
        pure (DriverApi {..})

  pure (bornEvs, runDriver)

{-|
  Eyre -- HTTP Server Driver

  Inject born events.
  Until born events succeeds, ignore effects.
  Wait until born event callbacks invoked.
    If success, signal success.
    If failure, try again several times.
      If still failure, bring down ship.
   Once born event succeeds:
     - Begin normal operation (start accepting requests)
-}
eyre
  :: forall e
   . (HasPierEnv e)
  => e
  -> Ship
  -> (EvErr -> STM ())
  -> Bool
  -> (Text -> RIO e ())
  -> KingSubsite
  -> ([Ev], RAcquire e (HttpServerEf -> IO ()))
eyre env who plan isFake stderr sub = (initialEvents, runHttpServer)
 where
  king = fromIntegral (env ^. kingIdL)
  multi = env ^. multiEyreApiL

  initialEvents :: [Ev]
  initialEvents = [bornEv king]

  runHttpServer :: RAcquire e (HttpServerEf -> IO ())
  runHttpServer = handleEf <$> mkRAcquire
    (Drv <$> newMVar Nothing)
    (\(Drv v) -> stopService v kill >>= fromEither)

  kill :: HasLogFunc e => Serv -> RIO e ()
  kill Serv{..} = do
    atomically (leaveMultiEyre multi who)
    io (saKil sLop)
    io (saKil sIns)
    io $ for_ sSec (\sec -> (saKil sec))
    io (removePortsFile sPortsFile)

  restart :: Drv -> HttpServerConf -> RIO e Serv
  restart (Drv var) conf = do
    logInfo "Restarting http server"
    let onFatal = runRIO env $ do
          -- XX instead maybe restart following logic under HSESetConfig below
          stderr "A web server problem has occurred. Please restart your ship."
          view killKingActionL >>= atomically
    let startAct = startServ who isFake conf plan stderr onFatal sub
    res <- fromEither =<< restartService var startAct kill
    logInfo "Done restating http server"
    pure res

  liveFailed _ = pure ()

  handleEf :: Drv -> HttpServerEf -> IO ()
  handleEf drv = runRIO env . \case
    HSESetConfig (i, ()) conf -> do
      logInfo (displayShow ("EYRE", "%set-config"))
      Serv {..} <- restart drv conf
      logInfo (displayShow ("EYRE", "%set-config", "Sending %live"))
      atomically $ plan (EvErr (liveEv sServId sPorts) liveFailed)
      logInfo "Write ports file"
      io (writePortsFile sPortsFile sPorts)
    HSEResponse (i, req, _seq, ()) ev -> do
      logDebug (displayShow ("EYRE", "%response"))
      execRespActs drv who (fromIntegral req) ev
