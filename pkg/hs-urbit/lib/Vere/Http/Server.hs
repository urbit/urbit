{-
    TODO Implement ports file.

    TODO What is this abount?

        //  if we don't explicitly set this field, h2o will send with
        //  transfer-encoding: chunked
        //
        if ( 1 == has_len_i ) {
          rec_u->res.content_length = ( 0 == gen_u->bod_u ) ?
                                      0 : gen_u->bod_u->len_w;
        }

    TODO Does this matter, is is using WAI's default behavior ok.

        rec_u->res.reason = (status < 200) ? "weird" :
                            (status < 300) ? "ok" :
                            (status < 400) ? "moved" :
                            (status < 500) ? "missing" :
                            "hosed";
-}

module Vere.Http.Server where

import Arvo            hiding (ServerId, reqBody, reqUrl, secure)
import Data.Conduit
import Noun
import UrbitPrelude    hiding (Builder)
import Vere.Pier.Types

import Data.Binary.Builder (Builder, fromByteString)
import Data.Bits           (shiftL, (.|.))
import Network.Socket      (SockAddr(..))
import System.Random       (randomIO)
import Vere.Http           (convertHeaders, unconvertHeaders)

import qualified Network.HTTP.Types          as H
import qualified Network.Wai                 as W
import qualified Network.Wai.Conduit         as W
import qualified Network.Wai.Handler.Warp    as W
import qualified Network.Wai.Handler.WarpTLS as W


-- RespAction -- Reorganized HttpEvent for Cleaner Processing ------------------

{-
    The sequence of actions on a given request *should* be:

        [%head .] [%bloc .]* %done

    But we will actually accept anything, and mostly do the right
    thing. There are two situations where we ignore ignore the data from
    some actions.

    - If you send something *after* a %done action, it will be ignored.
    - If you send a %done before a %head, we will produce "444 No
      Response" with an empty response body.
-}
data RespAction
    = RAHead ResponseHeader
    | RABloc File
    | RADone

reorgHttpEvent :: HttpEvent -> [RespAction]
reorgHttpEvent = \case
    Start head mBlk isDone -> [RAHead head]
                           <> toList (RABloc <$> mBlk)
                           <> if isDone then [RADone] else []
    Cancel ()              -> [RADone]
    Continue mBlk isDone   -> toList (RABloc <$> mBlk)
                           <> if isDone then [RADone] else []


-- Generic Service Stop/Restart -- Using an MVar for Atomicity -----------------

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


-- Live Requests Table -- All Requests Still Waiting for Responses -------------

type ReqId = Decimal
type SeqId = Decimal -- TODO Unused. Why is this a thing?

data LiveReqs = LiveReqs
    { nextReqId  :: ReqId
    , activeReqs :: Map ReqId (TMVar RespAction)
    }

emptyLiveReqs :: LiveReqs
emptyLiveReqs = LiveReqs 1 mempty

respondToLiveReq :: TVar LiveReqs -> ReqId -> RespAction -> STM ()
respondToLiveReq var req ev = do
    mVar <- lookup req . activeReqs <$> readTVar var
    case mVar of
        Nothing -> pure ()
        Just tv -> putTMVar tv ev

rmLiveReq :: TVar LiveReqs -> ReqId -> STM ()
rmLiveReq var reqId = do
    liv <- readTVar var
    writeTVar var (liv { activeReqs = deleteMap reqId (activeReqs liv) })

newLiveReq :: TVar LiveReqs -> STM (ReqId, TMVar RespAction)
newLiveReq var = do
    liv <- readTVar var
    tmv <- newEmptyTMVar

    let (nex, act) = (nextReqId liv, activeReqs liv)

    writeTVar var (LiveReqs (nex+1) (insertMap nex tmv act))

    pure (nex, tmv)


-- Random Helpers --------------------------------------------------------------

cordBytes :: Cord -> ByteString
cordBytes = encodeUtf8 . unCord

pass :: Monad m => m ()
pass = pure ()

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing  act = pure ()
whenJust (Just a) act = act a

cookMeth :: W.Request -> Maybe Method
cookMeth = H.parseMethod . W.requestMethod >>> \case
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


-- Utilities for Constructing Events -------------------------------------------

data WhichServer = Secure | Insecure | Loopback
  deriving (Eq)

servEv :: HttpServerEv -> Ev
servEv = EvBlip . BlipEvHttpServer

bornEv :: KingId -> Ev
bornEv king =
    servEv $ HttpServerEvBorn (king, ()) ()

liveEv :: ServId -> Port -> Maybe Port -> Ev
liveEv sId non sec =
    servEv $ HttpServerEvLive (sId, ()) non sec

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



-- Http Server Flows -----------------------------------------------------------

{-
    This accepts all action orderings so that there are no edge-cases
    to be handled:

    - If %bloc before %head, collect it and wait for %head.
    - If %done before %head, ignore all chunks and produce Nothing.
-}
getHead :: TMVar RespAction -> IO (Maybe (ResponseHeader, [File]))
getHead tmv = go []
  where
    go çunks = atomically (readTMVar tmv) >>= \case
                 RAHead head -> pure $ Just (head, reverse çunks)
                 RABloc çunk -> go (çunk : çunks)
                 RADone      -> pure Nothing

{-
    - Immediatly yield all of the initial chunks
    - Yield the data from %bloc action.
    - Close the stream when we hit a %done action.
-}
streamBlocks :: [File] -> TMVar RespAction -> ConduitT () (Flush Builder) IO ()
streamBlocks init tmv =
    for_ init yieldÇunk >> go
  where
    yieldFlush = \x -> yield (Chunk x) >> yield Flush
    yieldÇunk  = yieldFlush . fromByteString . unOcts . unFile
    logDupHead = putStrLn "Multiple %head actions on one request"

    go = atomically (readTMVar tmv) >>= \case
           RAHead head -> logDupHead >> go
           RABloc çunk -> yieldÇunk çunk
           RADone      -> pure ()

sendResponse :: (W.Response -> IO W.ResponseReceived)
            -> TMVar RespAction
            -> IO W.ResponseReceived
sendResponse cb tmv = do
    getHead tmv >>= \case
      Nothing    -> do cb $ W.responseLBS (H.mkStatus 444 "No Response") [] ""
      Just (h,i) -> do let çunks = streamBlocks i tmv
                       cb $ W.responseSource (hdrStatus h) (hdrHeaders h) çunks
  where
    hdrHeaders :: ResponseHeader -> [H.Header]
    hdrHeaders = unconvertHeaders . headers

    hdrStatus :: ResponseHeader -> H.Status
    hdrStatus = toEnum . fromIntegral . statusCode

liveReq :: TVar LiveReqs -> Acquire (ReqId, TMVar RespAction)
liveReq vLiv = mkAcquire ins del
  where
    ins = atomically (newLiveReq vLiv)
    del = atomically . rmLiveReq vLiv . fst

app :: ServId -> TVar LiveReqs -> (Ev -> STM ()) -> WhichServer -> W.Application
app sId liv plan which req respond = do
   with (liveReq liv) $ \(reqId, respVar) -> do
        body <- reqBody req
        meth <- maybe (error "bad method") pure (cookMeth req)

        let addr  = reqAddr req
            hdrs  = convertHeaders $ W.requestHeaders req
            evReq = HttpRequest meth (reqUrl req) hdrs body

        atomically $ plan (reqEv sId reqId which addr evReq)

        try (sendResponse respond respVar) >>= \case
          Right rr -> pure rr
          Left exn -> do atomically $ plan (cancelEv sId reqId)
                         throwIO (exn :: SomeException)


-- Top-Level Driver Interface --------------------------------------------------

newtype Drv = Drv { unDrv :: MVar (Maybe Serv) }

data Serv = Serv
    { sServId   :: ServId
    , sConfig   :: HttpServerConf
    , sLoopTid  :: Async ()
    , sHttpTid  :: Async ()
    , sHttpsTid :: Async ()
    , sLiveReqs :: TVar LiveReqs
    }

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

  sId <- ServId . UV . fromIntegral <$> (randomIO :: IO Word32)
  liv <- newTVarIO emptyLiveReqs

  let httpPort  = 8080  -- 80 if real ship
      httpsPort = 8443  -- 443 if real ship
      loopPort  = 12321 -- 443 if real ship

      loopOpts  = W.defaultSettings & W.setPort (fromIntegral loopPort)
                                    & W.setHost "127.0.0.1"
                                    & W.setTimeout (5 * 60)
      httpOpts  = W.defaultSettings & W.setPort (fromIntegral httpPort)
      httpsOpts = W.defaultSettings & W.setPort (fromIntegral httpsPort)

  loopTid  <- async $ W.runSettings loopOpts $ app sId liv plan Loopback
  httpTid  <- async $ W.runSettings httpOpts $ app sId liv plan Insecure
  httpsTid <- async $ W.runTLS tls httpsOpts $ app sId liv plan Secure

  let res = (sId, Port httpPort, Just $ Port httpsPort)

  pure (Serv sId conf loopTid httpTid httpsTid liv, res)


killServ :: Serv -> IO ()
killServ Serv{sLoopTid, sHttpTid, sHttpsTid} = do
    cancel sLoopTid
    cancel sHttpTid
    cancel sHttpsTid
    wait sLoopTid
    wait sHttpTid
    wait sHttpsTid

kill :: Drv -> IO ()
kill (Drv v) = stopService v killServ >>= fromEither

respond :: Drv -> ReqId -> HttpEvent -> IO ()
respond (Drv v) reqId ev = do
    readMVar v >>= \case
        Nothing -> pure ()
        Just sv -> atomically $ for_ (reorgHttpEvent ev) $
                                    respondToLiveReq (sLiveReqs sv) reqId

serv :: KingId -> QueueEv -> ([Ev], Acquire (EffCb HttpServerEf))
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
                atomically $ plan (liveEv sId insecurePort securePort)
        HSEResponse (i, req, _seq, ()) ev ->
            when (i == fromIntegral king) $
                respond drv (fromIntegral req) ev
