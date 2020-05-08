{-|
  WAI Application for `eyre` driver.

  # Request Lifecycles

  - Requests come in, are given an identifier and are passed to a callback.

  - When requests timeout, the identifier is passed to anothing callback.

  - The server pulls response actions, and passes them to the associated
    request.
-}

module Urbit.Vere.Eyre.Wai
  ( ReqId
  , RespAct(..)
  , RespApi(..)
  , LiveReqs(..)
  , ReqInfo(..)
  , emptyLiveReqs
  , routeRespAct
  , rmLiveReq
  , newLiveReq
  , app
  )
where

import Urbit.Prelude hiding (Builder)

import Data.Binary.Builder (Builder, fromByteString)
import Data.Bits           (shiftL, (.|.))
import Data.Conduit        (ConduitT, Flush(Chunk, Flush), yield)
import Network.Socket      (SockAddr(..))
import Urbit.Arvo          (Address(..), Ipv4(..), Ipv6(..), Method)

import qualified Network.HTTP.Types  as H
import qualified Network.Wai         as W
import qualified Network.Wai.Conduit as W


-- Types -----------------------------------------------------------------------

type ReqId = Word64

data RespAct
  = RAFull H.Status [H.Header] ByteString
  | RAHead H.Status [H.Header] ByteString
  | RABloc ByteString
  | RADone
 deriving (Eq, Ord, Show)

data RespApi = RespApi
  { raAct :: RespAct -> STM Bool
  , raKil :: STM ()
  }

data LiveReqs = LiveReqs
  { nextReqId  :: ReqId
  , activeReqs :: Map ReqId RespApi
  }

data ReqInfo = ReqInfo
  { riAdr :: Address
  , riMet :: H.StdMethod
  , riUrl :: ByteString
  , riHdr :: [H.Header]
  , riBod :: ByteString
  }


-- Live Requests Table -- All Requests Still Waiting for Responses -------------

emptyLiveReqs :: LiveReqs
emptyLiveReqs = LiveReqs 1 mempty

routeRespAct :: TVar LiveReqs -> ReqId -> RespAct -> STM Bool
routeRespAct vLiv reqId act =
  (lookup reqId . activeReqs <$> readTVar vLiv) >>= \case
    Nothing -> pure False
    Just tv -> raAct tv act

rmLiveReq :: TVar LiveReqs -> ReqId -> STM ()
rmLiveReq var reqId = modifyTVar' var
  $ \liv -> liv { activeReqs = deleteMap reqId (activeReqs liv) }

newLiveReq :: TVar LiveReqs -> STM (ReqId, STM RespAct)
newLiveReq var = do
  liv <- readTVar var
  tmv <- newTQueue
  kil <- newEmptyTMVar

  let waitAct    = (<|>) (readTMVar kil $> RADone) (readTQueue tmv)
      (nex, act) = (nextReqId liv, activeReqs liv)
      respApi    = RespApi
        { raKil = putTMVar kil ()
        , raAct = \act -> tryReadTMVar kil >>= \case
                    Nothing -> writeTQueue tmv act $> True
                    Just () -> pure False
        }


  writeTVar var (LiveReqs (nex + 1) (insertMap nex respApi act))

  pure (nex, waitAct)


-- Random Helpers --------------------------------------------------------------

cookMeth :: W.Request -> Maybe Method
cookMeth = H.parseMethod . W.requestMethod >>> \case
  Left  _ -> Nothing
  Right m -> Just m

reqAddr :: W.Request -> Address
reqAddr = W.remoteHost >>> \case
  SockAddrInet _ a      -> AIpv4 (Ipv4 a)
  SockAddrInet6 _ _ a _ -> AIpv6 (mkIpv6 a)
  _                     -> error "invalid sock addr"

mkIpv6 :: (Word32, Word32, Word32, Word32) -> Ipv6
mkIpv6 (p, q, r, s) = Ipv6 (pBits .|. qBits .|. rBits .|. sBits)
 where
  pBits = shiftL (fromIntegral p) 0
  qBits = shiftL (fromIntegral q) 32
  rBits = shiftL (fromIntegral r) 64
  sBits = shiftL (fromIntegral s) 96

reqUrl :: W.Request -> ByteString
reqUrl r = W.rawPathInfo r <> W.rawQueryString r


-- Responses -------------------------------------------------------------------

noHeader :: HasLogFunc e => RIO e a
noHeader = do
  logError "Response block with no response header."
  error "Bad HttpEvent: Response block with no response header."

emptyChunk :: HasLogFunc e => RIO e a
emptyChunk = do
  logError "Bad response action: empty chunk"
  error "Bad response action: empty chunk"

dupHead :: HasLogFunc e => RIO e a
dupHead = do
  logError "Multiple %head actions on one request"
  error "Bad HttpEvent: Multiple header actions per on one request."

{-|
  - Immediately yield all of the initial chunks
  - Yield the data from %bloc action.
  - Close the stream when we hit a %done action.
-}
streamBlocks
  :: HasLogFunc e
  => e
  -> ByteString
  -> STM RespAct
  -> ConduitT () (Flush Builder) IO ()
streamBlocks env init getAct = send init >> loop
 where
  loop = atomically getAct >>= \case
    RAHead _ _ _ -> runRIO env dupHead
    RAFull _ _ _ -> runRIO env dupHead
    RADone       -> pure ()
    RABloc c     -> send c >> loop

  send "" = runRIO env emptyChunk
  send c  = do
    runRIO env (logTrace (display ("sending chunk " <> tshow c)))
    yield $ Chunk $ fromByteString c
    yield Flush

sendResponse
  :: HasLogFunc e
  => (W.Response -> IO W.ResponseReceived)
  -> STM RespAct
  -> RIO e W.ResponseReceived
sendResponse cb waitAct = do
  env <- ask
  atomically waitAct >>= \case
    RADone       -> io $ cb $ W.responseLBS (H.mkStatus 444 "No Response") [] ""
    RAFull s h b -> io $ cb $ W.responseLBS s h $ fromStrict b
    RAHead s h b -> io $ cb $ W.responseSource s h $ streamBlocks env b waitAct
    RABloc _     -> noHeader

liveReq :: TVar LiveReqs -> RAcquire e (ReqId, STM RespAct)
liveReq vLiv = mkRAcquire ins del
 where
  ins = atomically (newLiveReq vLiv)
  del = atomically . rmLiveReq vLiv . fst

app
  :: HasLogFunc e
  => e
  -> TVar LiveReqs
  -> (ReqId -> ReqInfo -> STM ())
  -> (ReqId -> STM ())
  -> W.Application
app env liv inform cancel req respond =
  runRIO env $ rwith (liveReq liv) $ \(reqId, respApi) -> do
    bod <- io (toStrict <$> W.strictRequestBody req)
    met <- maybe (error "bad method") pure (cookMeth req)

    let adr = reqAddr req
        hdr = W.requestHeaders req
        url = reqUrl req

    atomically $ inform reqId $ ReqInfo adr met url hdr bod

    try (sendResponse respond respApi) >>= \case
      Right rr  -> pure rr
      Left  exn -> do
        atomically (cancel reqId)
        logError $ display ("Exception during request" <> tshow exn)
        throwIO (exn :: SomeException)
