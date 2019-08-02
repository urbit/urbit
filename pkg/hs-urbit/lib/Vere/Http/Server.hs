{-# OPTIONS_GHC -Wwarn #-}

module Vere.Http.Server where

import Arvo            hiding (ServerId, secure)
import Noun
import UrbitPrelude
import Vere.Http       hiding (Method)
import Vere.Pier.Types

import Control.Concurrent (ThreadId, forkIO, killThread)

import qualified Network.HTTP.Types          as H
import qualified Network.Wai                 as W
import qualified Network.Wai.Handler.Warp    as W
import qualified Network.Wai.Handler.WarpTLS as W


-- Types -----------------------------------------------------------------------

type ReqId = Word
type SeqId = Word

newtype Drv = Drv { unDrv :: MVar (Maybe Serv) }

data Serv = Serv
    { sConfig   :: HttpServerConf
    , sThread   :: Async ()
    , sLiveReqs :: TVar (Map ReqId (TMVar (SeqId, HttpEvent)))
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


-- Utilities -------------------------------------------------------------------

servEv :: HttpServerEv -> Ev
servEv = EvBlip . BlipEvHttpServer

bornEv :: KingId -> Ev
bornEv inst = servEv $ HttpServerEvBorn (fromIntegral inst, ()) ()

liveEv :: KingId -> Port -> Maybe Port -> Ev
liveEv inst non sec = servEv $ HttpServerEvLive (inst, ()) non sec


--------------------------------------------------------------------------------

startServ :: HttpServerConf -> IO (Serv, (Port, Maybe Port))
startServ conf = do
    (insecurePort, securePort) <- undefined
    serv <- Serv conf <$> async undefined <*> newTVarIO mempty
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
        Just sv -> atomically $ do
            liveReqs <- readTVar (sLiveReqs sv)
            lookup req liveReqs & \case
                Nothing -> pure ()
                Just tm -> putTMVar tm (seq, ev)


-- Top-Level Driver Interface --------------------------------------------------

serv :: KingId
     -> QueueEv
     -> ([Ev], Acquire (EffCb HttpServerEf))
serv inst plan =
    (initialEvents, runHttpServer)
  where
    initialEvents :: [Ev]
    initialEvents = [ bornEv inst ]

    runHttpServer :: Acquire (EffCb HttpServerEf)
    runHttpServer = handleEf <$> mkAcquire (Drv <$> newMVar Nothing) kill

    handleEf :: Drv -> HttpServerEf -> IO ()
    handleEf drv = \case
        HSESetConfig (i, ()) conf ->
            when (i == fromIntegral inst) $ do
                (i, s) <- restart drv conf
                atomically (plan (liveEv inst i s))
        HSEResponse (i, req, sec, ()) ev ->
            when (i == fromIntegral inst) $
                respond drv (fromIntegral req) (fromIntegral sec) ev


--------------------------------------------------------------------------------

{-
data ClientResponse
    = Progress ResponseHeader Int (Maybe Int) (Maybe ByteString)
    | Finished ResponseHeader (Maybe MimeData)
    | Cancel ()

data MimeData = MimeData Text ByteString
-}

{-
  Alright, so the flow here is:

    · Once we receive a request, send a %request or %request-local event.
    · The request thread should stick an MVar into a map, and wait on
      it for a response.
-}

{-
data HttpServerEv
    = HttpServerEvRequest       (KingId, Word, Word, ())  HttpServerReq
    | HttpServerEvRequestLocal  Path                      HttpServerReq
    | HttpServerEvLive          (KingId, ())              Port (Maybe Port)
-}

{-
cordBytes :: Cord -> ByteString
cordBytes = encodeUtf8 . unCord

startServer :: ServDrv -> Config -> IO ()
startServer s c = do
  tls <- case (secure c) of
    Nothing -> error "no wai"
    Just (PEM key, PEM cert) ->
      pure (W.tlsSettingsMemory (cordBytes cert) (cordBytes key))

  -- we need to do the dance where we do the socket checking dance. or shove a
  -- socket into it.
  tid <- forkIO $ W.runTLS tls W.defaultSettings (app s)
  putMVar (sdThread s) (Just (c, tid))

app :: ServDrv -> W.Application
app s req respond = bracket_
    (pure ())
    (pure ())
    (respond $ W.responseLBS H.status200 [] "Hello World")

cookMeth :: W.Request -> Maybe Method
cookMeth re =
  case H.parseMethod (W.requestMethod re) of
    Left _  -> Nothing
    Right m -> Just m

readEvents :: W.Request -> IO Request
readEvents req = do
  let Just meth = cookMeth req
      url       = Cord $ decodeUtf8 $ W.rawPathInfo req
      headers   = convertHeaders (W.requestHeaders req)
  bodyLbs <- W.strictRequestBody req
  let body = if length bodyLbs == 0 then Nothing
        else Just $ Octs (toStrict bodyLbs)

  -- TODO: Check if wai just deletes the 'host': header like h2o does?

  pure (Request meth url headers body)
-}
