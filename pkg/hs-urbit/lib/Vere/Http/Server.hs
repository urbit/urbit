{-# OPTIONS_GHC -Wwarn #-}

module Vere.Http.Server where

import Arvo         hiding (ServerId, reqBody, reqUrl, secure)
import Noun
import UrbitPrelude hiding (Builder)

import Vere.Http hiding (Cancel, Continue, Method, ResponseHeader(..), Start)

import Vere.Pier.Types

import Data.Binary.Builder (Builder, fromByteString)
import Data.Bits           (shiftL, (.|.))
import Network.Socket      (SockAddr(..))
import System.Random       (randomIO)

import qualified Network.HTTP.Types          as H
import qualified Network.Wai                 as W
import qualified Network.Wai.Handler.Warp    as W
import qualified Network.Wai.Handler.WarpTLS as W


-- Live Requests ---------------------------------------------------------------

type ReqId = Word
type SeqId = Word -- TODO Unused. Why is this a thing?

data LiveReqs = LiveReqs
    { nextReqId  :: ReqId
    , activeReqs :: Map ReqId (TMVar HttpEvent)
    }

emptyLiveReqs :: LiveReqs
emptyLiveReqs = LiveReqs 1 mempty

respondToLiveReq :: TVar LiveReqs -> ReqId -> HttpEvent -> STM ()
respondToLiveReq var req ev = do
    mVar <- lookup req . activeReqs <$> readTVar var
    case mVar of
        Nothing -> pure ()
        Just tv -> putTMVar tv ev

newLiveReq :: TVar LiveReqs -> STM (ReqId, TMVar HttpEvent)
newLiveReq var = do
    liv <- readTVar var
    tmv <- newEmptyTMVar

    let (nex, act) = (nextReqId liv, activeReqs liv)

    writeTVar var (LiveReqs (nex+1) (insertMap nex tmv act))

    pure (nex, tmv)


-- Servers ---------------------------------------------------------------------

newtype Drv = Drv { unDrv :: MVar (Maybe Serv) }

data Serv = Serv
    { sServId   :: ServId
    , sConfig   :: HttpServerConf
    , sHttpTid  :: Async ()
    , sHttpsTid :: Async ()
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

liveEv :: ServId -> Port -> Maybe Port -> Ev
liveEv sId non sec =
    servEv $ HttpServerEvLive (sId, ()) non sec

reqEv :: ServId -> ReqId -> Bool -> Address -> HttpRequest -> Ev
reqEv sId reqId secure addr req =
    servEv $ HttpServerEvRequest (sId, reqId, 1, ())
           $ HttpServerReq secure addr req


--------------------------------------------------------------------------------

killServ :: Serv -> IO ()
killServ Serv{sHttpsTid, sHttpTid} = do
    cancel sHttpTid
    cancel sHttpsTid
    wait sHttpTid
    wait sHttpsTid

kill :: Drv -> IO ()
kill (Drv v) = stopService v killServ >>= fromEither

respond :: Drv -> ReqId -> HttpEvent -> IO ()
respond (Drv v) req ev = do
    readMVar v >>= \case
        Nothing -> pure ()
        Just sv -> atomically (respondToLiveReq (sLiveReqs sv) req ev)


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

    restart :: Drv -> HttpServerConf -> IO (ServId, Port, Maybe Port)
    restart (Drv var) conf = do
        fromEither =<< restartService var (startServ conf plan) killServ

    handleEf :: Drv -> HttpServerEf -> IO ()
    handleEf drv = \case
        HSESetConfig (i, ()) conf ->
            when (i == fromIntegral king) $ do
                (sId, insecurePort, securePort) <- restart drv conf
                atomically (plan (liveEv sId insecurePort securePort))
        HSEResponse (i, req, _seq, ()) ev ->
            when (i == fromIntegral king) $
                respond drv (fromIntegral req) ev


--------------------------------------------------------------------------------

{-
    TODO Need to find an open port.
-}
startServ :: HttpServerConf -> (Ev -> STM ())
            -> IO (Serv, (ServId, Port, Maybe Port))
startServ conf plan = do
  tls <- case (hscSecure conf) of
      Nothing -> error "HACK: Implement support for missing PEMs"
      Just (PEM key, PEM cert) ->
        pure (W.tlsSettingsMemory (cordBytes cert) (cordBytes key))

  sId <- ServId <$> randomIO
  liv <- newTVarIO emptyLiveReqs

  httpsTid <- async $ W.runTLS tls W.defaultSettings (app sId liv plan True)

  httpTid <- async $ W.run 80 (app sId liv plan False)

  let res = (sId, Port 80, Just $ Port 443)

  pure (Serv sId conf httpTid httpsTid liv, res)

respondLoop :: (W.Response -> IO W.ResponseReceived)
            -> TMVar HttpEvent
            -> IO W.ResponseReceived
respondLoop respond tmv = start
  where
    start :: IO W.ResponseReceived
    start = do
        atomically (readTMVar tmv) >>= \case
            Cancel () ->
                fullCancel
            Continue _ _ -> do
                putStrLn "%continue before %start"
                start
            Start hdr init isDone -> do
                startStreaming hdr $ \s d -> do
                    whenJust init (sendBlock s)
                    stream isDone s d

    stream :: Bool -> (Builder -> IO ()) -> IO ()
             -> IO ()
    stream isDone send done =
        case isDone of
            True  -> closeStream done
            False -> do
                atomically (readTMVar tmv) >>= \case
                    Start _ _ _ -> do
                        putStrLn "%start after %continue"
                        stream isDone send done
                    Cancel () -> do
                        streamingCancel done
                    Continue blk doneNow -> do
                        whenJust blk (sendBlock send)
                        stream doneNow send done

    startStreaming :: ResponseHeader
                   -> ((Builder -> IO ()) -> IO () -> IO ())
                   -> IO W.ResponseReceived
    startStreaming hdr cb = do
        let status  = hdrStatus hdr
            headers = hdrHeaders hdr
        respond $ W.responseStream status headers cb

    closeStream :: IO () -> IO ()
    closeStream killReq = killReq

    streamingCancel :: IO () -> IO ()
    streamingCancel killReq = killReq

    sendBlock :: (Builder -> IO ()) -> File -> IO ()
    sendBlock sendBlk = sendBlk . fromByteString . unOcts . unFile

    fullCancel :: IO W.ResponseReceived
    fullCancel = respond $ W.responseLBS H.status500 [] "request canceled"

hdrHeaders :: ResponseHeader -> [H.Header]
hdrHeaders = unconvertHeaders . headers

hdrStatus :: ResponseHeader -> H.Status
hdrStatus = toEnum . fromIntegral . statusCode

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing  act = pure ()
whenJust (Just a) act = act a

{-
data HttpEvent
    = Start ResponseHeader (Maybe File) Bool
    | Continue (Maybe File) Bool
    | Cancel ()
  deriving (Eq, Ord, Show)
-}

app :: ServId -> TVar LiveReqs -> (Ev -> STM ()) -> Bool -> W.Application
app sId liv plan secure req respond = do
    body <- reqBody req
    meth <- maybe (error "bad method") pure (cookMeth req)

    let addr  = reqAddr req
        hdrs  = convertHeaders $ W.requestHeaders req
        evReq = HttpRequest meth (reqUrl req) hdrs body

    respVar <- atomically $ do (reqId, var) <- newLiveReq liv
                               sendReqEvent reqId addr evReq
                               pure var

    respondLoop respond respVar

  where

    sendReqEvent :: ReqId -> Address -> HttpRequest -> STM ()
    sendReqEvent reqId x y =
        plan (reqEv sId reqId secure x y)

cookMeth :: W.Request -> Maybe Method
cookMeth re =
  case H.parseMethod (W.requestMethod re) of
    Left _  -> Nothing
    Right m -> Just m

reqIdCord :: ReqId -> Cord
reqIdCord = Cord . tshow

reqBody :: W.Request -> IO (Maybe File)
reqBody req = do
    bodyLbs <- W.strictRequestBody req
    if length bodyLbs == 0
      then pure $ Nothing
      else pure $ Just $ File $ Octs (toStrict bodyLbs)

reqAddr :: W.Request -> Address
reqAddr = W.remoteHost >>> \case
    SockAddrInet  _ a     -> AIpv4 (Ipv4 a)
    SockAddrInet6 _ _ a _ -> AIpv6 (mkIpv6 a)
    _                     -> error "invalid sock addr"

mkIpv6 :: (Word32, Word32, Word32, Word32) -> Ipv6
mkIpv6 (p, q, r, s) = Ipv6 (pBits .|. qBits .|. rBits .|. sBits)
  where
    pBits = shiftL (fromIntegral p) 0
    qBits = shiftL (fromIntegral q) 32
    rBits = shiftL (fromIntegral r) 64
    sBits = shiftL (fromIntegral s) 96

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
