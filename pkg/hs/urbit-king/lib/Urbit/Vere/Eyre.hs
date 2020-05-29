{-|
  Eyre: Http Server Driver
-}

module Urbit.Vere.Eyre
  ( eyre
  )
where

import Urbit.Prelude hiding (Builder)

import Urbit.Arvo                hiding (ServerId, reqUrl, secure)
import Urbit.King.App            (HasKingId(..))
import Urbit.King.Config
import Urbit.Vere.Eyre.Multi
import Urbit.Vere.Eyre.PortsFile
import Urbit.Vere.Eyre.Serv
import Urbit.Vere.Eyre.Service
import Urbit.Vere.Eyre.Wai
import Urbit.Vere.Pier.Types

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.PEM           (pemParseBS, pemWriteBS)
import RIO.Prelude        (decodeUtf8Lenient)
import System.Random      (randomIO)
import Urbit.Vere.Http    (convertHeaders, unconvertHeaders)

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

cancelEv :: ServId -> ReqId -> Ev
cancelEv sId reqId = servEv $ HttpServerEvCancelRequest (sId, reqId, 1, ()) ()

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
  pems <- pemParseBS cerByt & either (const Nothing) Just
  (cert, chain) <- case pems of
    []     -> Nothing
    p : ps -> pure (pemWriteBS p, pemWriteBS <$> ps)
  pure $ TlsConfig keyByt cert chain
 where
  wainBytes :: Wain -> ByteString
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
  evUrl = Cord (decodeUtf8Lenient riUrl)
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
    logTrace $ displayShow ev
    for_ (parseHttpEvent ev) $ \act -> do
      atomically (routeRespAct who (sLiveReqs sv) reqId act)

startServ
  :: (HasPierConfig e, HasLogFunc e, HasNetworkConfig e)
  => MultiEyreApi
  -> Ship
  -> Bool
  -> HttpServerConf
  -> (Ev -> STM ())
  -> RIO e Serv
startServ multi who isFake conf plan = do
  logTrace (displayShow ("EYRE", "startServ"))

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

  let onReq :: WhichServer -> Ship -> Word64 -> ReqInfo -> STM ()
      onReq which _ship reqId reqInfo =
        plan (requestEvent srvId which reqId reqInfo)

  let onKilReq :: Ship -> Word64 -> STM ()
      onKilReq _ship = plan . cancelEv srvId . fromIntegral

  logTrace (displayShow ("EYRE", "joinMultiEyre", who, mTls, mCre))

  atomically (joinMultiEyre multi who mCre onReq onKilReq)

  logTrace $ displayShow ("EYRE", "Starting loopback server")
  lop <- serv vLive $ ServConf
    { scHost = soHost (pttLop ptt)
    , scPort = soWhich (pttLop ptt)
    , scRedi = Nothing
    , scFake = False
    , scType = STHttp who $ ReqApi
        { rcReq = onReq Loopback
        , rcKil = onKilReq
        }
    }

  logTrace $ displayShow ("EYRE", "Starting insecure server")
  ins <- serv vLive $ ServConf
    { scHost = soHost (pttIns ptt)
    , scPort = soWhich (pttIns ptt)
    , scRedi = secRedi
    , scFake = noHttp
    , scType = STHttp who $ ReqApi
        { rcReq = onReq Insecure
        , rcKil = onKilReq
        }
    }

  mSec <- for mTls $ \tls -> do
    logTrace "Starting secure server"
    serv vLive $ ServConf
      { scHost = soHost (pttSec ptt)
      , scPort = soWhich (pttSec ptt)
      , scRedi = Nothing
      , scFake = noHttps
      , scType = STHttps who tls $ ReqApi
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

  logTrace $ displayShow ("EYRE", "All Servers Started.", srvId, por, fil)

  pure (Serv srvId conf lop ins mSec por fil vLive)


-- Eyre Driver -----------------------------------------------------------------

eyre
  :: forall e
   . (HasShipEnv e, HasKingId e)
  => e
  -> MultiEyreApi
  -> Ship
  -> QueueEv
  -> Bool
  -> ([Ev], RAcquire e (EffCb e HttpServerEf))
eyre env multi who plan isFake = (initialEvents, runHttpServer)
 where
  king = fromIntegral (env ^. kingIdL)

  initialEvents :: [Ev]
  initialEvents = [bornEv king]

  runHttpServer :: RAcquire e (EffCb e HttpServerEf)
  runHttpServer = handleEf <$> mkRAcquire
    (Drv <$> newMVar Nothing)
    (\(Drv v) -> stopService v kill >>= fromEither)

  kill :: HasLogFunc e => Serv -> RIO e ()
  kill Serv{..} = do
    atomically (leaveMultiEyre multi who)
    atomically (saKil sLop)
    atomically (saKil sIns)
    for_ sSec (\sec -> atomically (saKil sec))
    io (removePortsFile sPortsFile)

  restart :: Drv -> HttpServerConf -> RIO e Serv
  restart (Drv var) conf = do
    logDebug "Restarting http server"
    let startAct = startServ multi who isFake conf plan
    res <- fromEither =<< restartService var startAct kill
    logDebug "Done restating http server"
    pure res

  handleEf :: Drv -> HttpServerEf -> RIO e ()
  handleEf drv = \case
    HSESetConfig (i, ()) conf -> do
      logDebug (displayShow ("EYRE", "%set-config"))
      Serv {..} <- restart drv conf
      logDebug (displayShow ("EYRE", "%set-config", "Sending %live"))
      atomically $ plan (liveEv sServId sPorts)
      logDebug "Write ports file"
      io (writePortsFile sPortsFile sPorts)
    HSEResponse (i, req, _seq, ()) ev -> do
      logDebug (displayShow ("EYRE", "%response"))
      execRespActs drv who (fromIntegral req) ev
