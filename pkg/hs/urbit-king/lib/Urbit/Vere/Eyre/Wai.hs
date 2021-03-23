{-# LANGUAGE LambdaCase #-}

-- |
--  WAI Application for `eyre` driver.
--
--  # Request Lifecycles
--
--  - Requests come in, are given an identifier and are passed to a callback.
--
--  - When requests timeout, the identifier is passed to anothing callback.
--
--  - The server pulls response actions, and passes them to the associated
--    request.
module Urbit.Vere.Eyre.Wai
  ( RespAct (..),
    RespApi (..),
    LiveReqs (..),
    ReqInfo (..),
    emptyLiveReqs,
    routeRespAct,
    rmLiveReq,
    newLiveReq,
    app,
    EyreSite,
    scrySite,
    fourOhFourSite,
  )
where

import Data.Binary.Builder (fromByteString)
import Data.Bits (shiftL, (.|.))
import Data.ByteString.Builder (Builder, charUtf8, toLazyByteString)
import Data.Conduit (ConduitT, Flush (Chunk, Flush), yield)
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr (..))
import qualified Network.Wai as W
import qualified Network.Wai.Conduit as W
import System.Random (newStdGen, randoms)
import Urbit.Arvo (Address (..), Ipv4 (..), Ipv6 (..), Method)
import Urbit.King.Scry
import Urbit.Prelude hiding (Builder)
import Urbit.Vere.Eyre.KingSubsite (KingSubsite, runKingSubsite)

-- Types -----------------------------------------------------------------------
newtype EyreSite = ES {runEyreSite :: W.Application}

data RespAct
  = RAFull H.Status [H.Header] ByteString
  | RAHead H.Status [H.Header] ByteString
  | RABloc ByteString
  | RADone
  deriving (Eq, Ord, Show)

data RespApi = RespApi
  { raAct :: RespAct -> STM Bool,
    raKil :: STM ()
  }

data LiveReqs = LiveReqs
  { reqIdSuply :: [Word64],
    activeReqs :: Map Word64 (Ship, RespApi)
  }

data ReqInfo = ReqInfo
  { riAdr :: Address,
    riMet :: H.StdMethod,
    riUrl :: ByteString,
    riHdr :: [H.Header],
    riBod :: ByteString
  }

data ScryExn = BadScry deriving (Show)

instance Exception ScryExn

-- Live Requests Table -- All Requests Still Waiting for Responses -------------

emptyLiveReqs :: IO LiveReqs
emptyLiveReqs = io $ do
  gen <- newStdGen
  pure (LiveReqs (randoms gen) mempty)

routeRespAct :: Ship -> TVar LiveReqs -> Word64 -> RespAct -> STM Bool
routeRespAct who vLiv reqId act =
  (lookup reqId . activeReqs <$> readTVar vLiv) >>= \case
    Nothing -> pure False
    Just (own, tv) -> do
      if (who == own)
        then raAct tv act
        else pure False

rmLiveReq :: TVar LiveReqs -> Word64 -> STM ()
rmLiveReq var reqId = modifyTVar' var $
  \liv -> liv {activeReqs = deleteMap reqId (activeReqs liv)}

allocateReqId :: TVar LiveReqs -> STM Word64
allocateReqId var = do
  LiveReqs supply tbl <- readTVar var

  let loop :: [Word64] -> (Word64, [Word64])
      loop [] = error "impossible"
      loop (x : xs) | member x tbl = loop xs
      loop (x : xs) = (x, xs)

  let (fresh, supply') = loop supply
  writeTVar var (LiveReqs supply' tbl)
  pure fresh

newLiveReq :: Ship -> TVar LiveReqs -> STM (Word64, STM RespAct)
newLiveReq who var = do
  tmv <- newTQueue
  kil <- newEmptyTMVar
  nex <- allocateReqId var

  LiveReqs sup tbl <- readTVar var

  let waitAct = (<|>) (readTMVar kil $> RADone) (readTQueue tmv)
      respApi =
        RespApi
          { raKil = putTMVar kil (),
            raAct = \act ->
              tryReadTMVar kil >>= \case
                Nothing -> writeTQueue tmv act $> True
                Just () -> pure False
          }

  writeTVar var (LiveReqs sup (insertMap nex (who, respApi) tbl))

  pure (nex, waitAct)

-- Random Helpers --------------------------------------------------------------

cookMeth :: W.Request -> Maybe Method
cookMeth =
  H.parseMethod . W.requestMethod >>> \case
    Left _ -> Nothing
    Right m -> Just m

reqAddr :: W.Request -> Address
reqAddr =
  W.remoteHost >>> \case
    SockAddrInet _ a -> AIpv4 (Ipv4 a)
    SockAddrInet6 _ _ a _ -> AIpv6 (mkIpv6 a)
    _ -> error "invalid sock addr"

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

dupHead :: HasLogFunc e => RIO e a
dupHead = do
  logError "Multiple %head actions on one request"
  error "Bad HttpEvent: Multiple header actions per on one request."

-- |
--  - Immediately yield all of the initial chunks
--  - Yield the data from %bloc action.
--  - Close the stream when we hit a %done action.
streamBlocks ::
  HasLogFunc e =>
  e ->
  ByteString ->
  STM RespAct ->
  ConduitT () (Flush Builder) IO ()
streamBlocks env init getAct = send init >> loop
  where
    loop =
      atomically getAct >>= \case
        RAHead {} -> runRIO env dupHead
        RAFull {} -> runRIO env dupHead
        RADone -> pure ()
        RABloc c -> send c >> loop

    send "" = pure ()
    send c = do
      runRIO env (logDebug (display ("sending chunk " <> tshow c)))
      yield $ Chunk $ fromByteString c
      yield Flush

sendResponse ::
  HasLogFunc e =>
  (W.Response -> IO W.ResponseReceived) ->
  STM RespAct ->
  RIO e W.ResponseReceived
sendResponse cb waitAct = do
  env <- ask
  atomically waitAct >>= \case
    RADone -> io $ cb $ W.responseLBS (H.mkStatus 444 "No Response") [] ""
    RAFull s h b -> io $ cb $ W.responseLBS s h $ fromStrict b
    RAHead s h b -> io $ cb $ W.responseSource s h $ streamBlocks env b waitAct
    RABloc _ -> noHeader

liveReq :: Ship -> TVar LiveReqs -> RAcquire e (Word64, STM RespAct)
liveReq who vLiv = mkRAcquire ins del
  where
    ins = atomically (newLiveReq who vLiv)
    del = atomically . rmLiveReq vLiv . fst

app ::
  HasLogFunc e =>
  e ->
  Ship ->
  TVar LiveReqs ->
  (Word64 -> ReqInfo -> STM ()) ->
  (Word64 -> STM ()) ->
  EyreSite ->
  KingSubsite ->
  W.Application
app env who liv inform cancel site sub req respond = do
  met <- maybe (error "bad method") pure (cookMeth req)
  let getp = met == H.GET
      scryFlow =
        onException
          (runEyreSite site req respond)
          normalFlow
      normalFlow =
        runEyreSite
          (normalSite env who liv inform cancel)
          req
          respond
  case W.pathInfo req of
    ("~_~" : _) -> runKingSubsite sub req respond
    _ -> if getp then scryFlow else normalFlow

normalSite ::
  HasLogFunc e =>
  e ->
  Ship ->
  TVar LiveReqs ->
  (Word64 -> ReqInfo -> STM ()) ->
  (Word64 -> STM ()) ->
  EyreSite
normalSite env who liv inform cancel = ES $ \req respond ->
  runRIO env $
    rwith (liveReq who liv) $ \(reqId, respApi) -> do
      bod <- io (toStrict <$> W.strictRequestBody req)
      met <- maybe (error "bad method") pure (cookMeth req)

      let adr = reqAddr req
          hdr = W.requestHeaders req
          url = reqUrl req

      atomically $ inform reqId $ ReqInfo adr met url hdr bod

      try (sendResponse respond respApi) >>= \case
        Right rr -> pure rr
        Left exn -> do
          atomically (cancel reqId)
          logError $ display ("Exception during request" <> tshow exn)
          throwIO (exn :: SomeException)

scrySite :: HasLogFunc e => Ship -> ScryFunc -> RAcquire e EyreSite
scrySite who scry = do
  env <- ask
  pure $
    ES $ \req respond -> do

      let url =  filter (/= '"') $ tshow $ reqUrl req
          host = filter (/= '"') $ maybe "" tshow (W.requestHeaderHost req)
      res <- runRIO env $ scryPath $ ["scry", host, url]
      respond $ scryResp res
  where
    scryPath path = do
      logDebug $ display ("scrying for " <> tshow path)
      s <- scryOcts path
      case s of
        Just octs -> do
          logDebug $ display ("scry result: " <> tshow octs)
          pure $ unOcts octs
        Nothing -> do 
          logError $ display ("exception during scry on path: " <> tshow path)
          throwIO BadScry

    scryOcts :: HasLogFunc e => [Text] -> RIO e (Maybe Octs)
    scryOcts path = scryNow scry "ex" "" path

    scryResp msg =
      W.responseBuilder (H.mkStatus 200 "OK") scryHeads $ fromByteString msg

    scryHeads =
      [ ("Content-Type", "text/plain"),
        ("Cache-Control", "no-cache")
      ]

fourOhFourSite :: Ship -> EyreSite
fourOhFourSite who = ES $ \req respond ->
  respond $ W.responseLBS (H.mkStatus 404 "Not Found") [] body
  where
    body = toLazyByteString $ foldMap charUtf8 $ msg
    msg = "Ship " <> show who <> " not docked."
