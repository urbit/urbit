{-# OPTIONS_GHC -Wwarn #-}

module Vere.Http.Server where

import Arvo            hiding (ServerId, secure, reqUrl)
import Noun
import UrbitPrelude
import Vere.Http       hiding (Method)
import Vere.Pier.Types

import Control.Concurrent (ThreadId, forkIO, killThread)

import qualified Network.HTTP.Types          as H
import qualified Network.Wai                 as W
import qualified Network.Wai.Handler.Warp    as W
import qualified Network.Wai.Handler.WarpTLS as W

import qualified Data.CaseInsensitive      as CI
import qualified Network.HTTP.Types        as HT
import qualified Network.HTTP.Types.Method as H


-- Live Requests ---------------------------------------------------------------

type ReqId = Word
type SeqId = Word

data LiveReqs = LiveReqs
    { nextReqId  :: ReqId
    , activeReqs :: Map ReqId (TMVar (SeqId, HttpEvent))
    }

emptyLiveReqs :: LiveReqs
emptyLiveReqs = LiveReqs 1 mempty

respondToLiveReq :: TVar LiveReqs -> ReqId -> SeqId -> HttpEvent -> STM ()
respondToLiveReq var req seq ev = do
    mVar <- lookup req . activeReqs <$> readTVar var
    case mVar of
        Nothing -> pure ()
        Just tv -> putTMVar tv (seq, ev)

newLiveReq :: TVar LiveReqs -> STM (ReqId, TMVar (SeqId, HttpEvent))
newLiveReq var = do
    liv <- readTVar var
    tmv <- newEmptyTMVar

    let (nex, act) = (nextReqId liv, activeReqs liv)

    writeTVar var (LiveReqs (nex+1) (insertMap nex tmv act))

    pure (nex, tmv)


-- Servers ---------------------------------------------------------------------


newtype Drv = Drv { unDrv :: MVar (Maybe Serv) }

data Serv = Serv
    { sConfig   :: HttpServerConf
    , sThread   :: Async ()
    , sLiveReqs :: TVar LiveReqs
    }


-- Generic Service Restart and Stop Logic --------------------------------------

{-
    Restart a running service.

    This can probably be made simpler, but it

    - Sets the MVar to Nothing if there was an exception whil starting
      or stopping the service.

    - Keeps the MVar lock until the restart process finishes.
-}
restartService :: forall s r
                . MVar (Maybe s)
               -> IO (s, r)
               -> (s -> IO ())
               -> IO (Either SomeException r)
restartService vServ sstart kkill = do
    modifyMVar vServ $ \case
        Nothing -> doStart
        Just sv -> doRestart sv
  where
    doRestart :: s -> IO (Maybe s, Either SomeException r)
    doRestart serv =
        try (kkill serv) >>= \case
            Left exn -> pure (Nothing, Left exn)
            Right () -> doStart

    doStart :: IO (Maybe s, Either SomeException r)
    doStart =
        try sstart <&> \case
            Right (s,r) -> (Just s,  Right r)
            Left exn    -> (Nothing, Left exn)


stopService :: forall s
             . MVar (Maybe s)
            -> (s -> IO ())
            -> IO (Either SomeException ())
stopService vServ kkill = do
    modifyMVar vServ $ \case
        Nothing -> pure (Nothing, Right ())
        Just sv -> do res <- try (kkill sv)
                      pure (Nothing, res)


-- Random Helpers --------------------------------------------------------------

cordBytes :: Cord -> ByteString
cordBytes = encodeUtf8 . unCord


-- Utilities for Constructing Events -------------------------------------------

servEv :: HttpServerEv -> Ev
servEv = EvBlip . BlipEvHttpServer

bornEv :: KingId -> Ev
bornEv king =
    servEv $ HttpServerEvBorn (fromIntegral king, ()) ()

liveEv :: KingId -> Port -> Maybe Port -> Ev
liveEv king non sec =
    servEv $ HttpServerEvLive (king, ()) non sec

reqEv :: KingId -> ReqId -> SeqId -> Bool -> Address -> HttpRequest -> Ev
reqEv king reqId seqId secure addr req =
    servEv $ HttpServerEvRequest (king, reqId, seqId, ())
           $ HttpServerReq secure addr req


--------------------------------------------------------------------------------

startServ :: HttpServerConf -> IO (Serv, (Port, Maybe Port))
startServ conf = do
    (insecurePort, securePort) <- undefined
    serv <- Serv conf <$> async undefined <*> newTVarIO emptyLiveReqs
    pure (insecurePort, securePort)

killServ :: Serv -> IO ()
killServ Serv{sThread} = cancel sThread >> wait sThread

restart :: Drv -> HttpServerConf -> IO (Port, Maybe Port)
restart (Drv var) conf = do
    fromEither =<< restartService var (startServ conf) killServ

kill :: Drv -> IO ()
kill (Drv v) = stopService v killServ >>= fromEither

respond :: Drv -> ReqId -> SeqId -> HttpEvent -> IO ()
respond (Drv v) req seq ev = do
    readMVar v >>= \case
        Nothing -> pure ()
        Just sv -> atomically (respondToLiveReq (sLiveReqs sv) req seq ev)


-- Top-Level Driver Interface --------------------------------------------------

serv :: KingId
     -> QueueEv
     -> ([Ev], Acquire (EffCb HttpServerEf))
serv king plan =
    (initialEvents, runHttpServer)
  where
    initialEvents :: [Ev]
    initialEvents = [bornEv king]

    runHttpServer :: Acquire (EffCb HttpServerEf)
    runHttpServer = handleEf <$> mkAcquire (Drv <$> newMVar Nothing) kill

    handleEf :: Drv -> HttpServerEf -> IO ()
    handleEf drv = \case
        HSESetConfig (i, ()) conf ->
            when (i == fromIntegral king) $ do
                (i, s) <- restart drv conf
                atomically (plan (liveEv king i s))
        HSEResponse (i, req, sec, ()) ev ->
            when (i == fromIntegral king) $
                respond drv (fromIntegral req) (fromIntegral sec) ev


--------------------------------------------------------------------------------

{-
    TODO Need to find an open port.
-}
startServer :: KingId -> HttpServerConf -> (Ev -> STM ())
            -> IO (Serv, (Port, Maybe Port))
startServer king conf plan = do
  tls <- case (hscSecure conf) of
      Nothing -> error "HACK: Implement support for missing PEMs"
      Just (PEM key, PEM cert) ->
        pure (W.tlsSettingsMemory (cordBytes cert) (cordBytes key))

  liv <- newTVarIO emptyLiveReqs
  tid <- async $ W.runTLS tls W.defaultSettings (app king liv plan True)

  pure (Serv conf tid liv, (Port 80, Just $ Port 443))

app :: KingId -> TVar LiveReqs -> (Ev -> STM ()) -> Bool -> W.Application
app king liv plan secure =

    \req respond ->

        bracket_ onStart onStop $ do
            bodyLbs <- W.strictRequestBody req

            let body = if length bodyLbs == 0
                           then Nothing
                           else Just $ File $ Octs (toStrict bodyLbs)

            let seqId     = 1
                addr      = reqAddr req
                Just meth = cookMeth req
                hdrs      = convertHeaders $ W.requestHeaders req
                evReq     = HttpRequest meth (reqUrl req) hdrs body

            respVar <- atomically $ do
                (reqId, var) <- newLiveReq liv
                sendReqEvent reqId seqId addr evReq
                pure var

            respond (W.responseLBS H.status200 [] "Hello World")

  where

    sendReqEvent :: ReqId -> SeqId -> Address -> HttpRequest -> STM ()
    sendReqEvent reqId seqId x y =
        plan (reqEv king reqId seqId secure x y)

    onStart = pure ()
    onStop  = pure ()

cookMeth :: W.Request -> Maybe Method
cookMeth re =
  case H.parseMethod (W.requestMethod re) of
    Left _  -> Nothing
    Right m -> Just m

reqIdCord :: ReqId -> Cord
reqIdCord = Cord . tshow

reqAddr :: W.Request -> Address
reqAddr = undefined . W.remoteHost

reqUrl :: W.Request -> Cord
reqUrl = Cord . decodeUtf8 . W.rawPathInfo

{-
data ClientResponse
    = Progress ResponseHeader Int (Maybe Int) (Maybe ByteString)
    | Finished ResponseHeader (Maybe MimeData)
    | Cancel ()

data MimeData = MimeData Text ByteString

readEvents :: W.Request -> IO Request
readEvents req = do
  let Just meth = cookMeth req
      url       = Cord $ decodeUtf8 $ W.rawPathInfo req
      headers   = convertHeaders (W.requestHeaders req)
  bodyLbs <- W.strictRequestBody req
  let body = if length bodyLbs == 0 then Nothing
        else Just $ Octs (toStrict bodyLbs)

  pure (Request meth url headers body)
-}
