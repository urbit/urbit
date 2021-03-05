{-|
    Http Client Driver

    TODO When making a request, handle the case where the request id is
    already in use.
-}

module Urbit.Vere.Http.Client where

import Urbit.Prelude hiding (Builder, finally)

import Urbit.Vere.Http
import Urbit.Vere.Pier.Types
import Urbit.King.App

import Urbit.Arvo (BlipEv(..), Ev(..), HttpClientEf(..), HttpClientEv(..),
                   HttpClientReq(..), HttpEvent(..), KingId, ResponseHeader(..))

import RIO.Orphans ()
import Control.Monad.Catch (finally)

import qualified Data.Map.Strict         as M
import qualified Network.HTTP.Client     as H
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types      as HT


-- Types -----------------------------------------------------------------------

type ReqId = Word

data HttpClientDrv = HttpClientDrv
  { hcdManager :: H.Manager
  , hcdLive    :: TVar (Map ReqId (Async ()))
  }

--------------------------------------------------------------------------------

cvtReq :: HttpClientReq -> Maybe H.Request
cvtReq r =
  H.parseRequest (unpack (unCord $ url r)) <&> \init -> init
    { H.method = encodeUtf8 $ tshow (method r)
    , H.requestHeaders = unconvertHeaders (headerList r)
    , H.requestBody =
        H.RequestBodyBS $ case body r of
                            Nothing        -> ""
                            Just (Octs bs) -> bs
    }

cvtRespHeaders :: H.Response a -> ResponseHeader
cvtRespHeaders resp =
    ResponseHeader (fromIntegral $ HT.statusCode (H.responseStatus resp)) heads
  where
    heads = convertHeaders (H.responseHeaders resp)

bornEv :: KingId -> Ev
bornEv king =
    EvBlip $ BlipEvHttpClient $ HttpClientEvBorn (king, ()) ()

--------------------------------------------------------------------------------

_bornFailed :: e -> WorkError -> IO ()
_bornFailed env _ = runRIO env $ do
  pure () -- TODO What to do in this case?

client'
  :: HasPierEnv e
  => RIO e ([Ev], RAcquire e (DriverApi HttpClientEf))
client' = do
  ventQ :: TQueue EvErr <- newTQueueIO
  env <- ask

  let (bornEvs, startDriver) = client env (writeTQueue ventQ)

  let runDriver = do
        diOnEffect <- startDriver
        let diEventSource = fmap RRWork <$> tryReadTQueue ventQ
        pure (DriverApi {..})

  pure (bornEvs, runDriver)


{-|
  Iris -- HTTP Client Driver

  Until born events succeeds, ignore effects.
  Wait until born event callbacks invoked.
    If success, signal success.
    If failure, try again several times.
      If still failure, bring down ship.
   Once born event succeeds, hold on to effects.
   Once all other drivers have booted:
     - Execute stashed effects.
     - Begin normal operation (start accepting requests)
-}
client
  :: forall e
   . (HasLogFunc e, HasKingId e)
  => e
  -> (EvErr -> STM ())
  -> ([Ev], RAcquire e (HttpClientEf -> IO ()))
client env plan = (initialEvents, runHttpClient)
  where
    kingId = view (kingIdL . to fromIntegral) env

    initialEvents :: [Ev]
    initialEvents = [bornEv kingId]

    runHttpClient :: RAcquire e (HttpClientEf -> IO ())
    runHttpClient = handleEffect <$> mkRAcquire start stop

    start :: RIO e (HttpClientDrv)
    start = HttpClientDrv <$>
      (io $ H.newManager TLS.tlsManagerSettings) <*>
      newTVarIO M.empty

    stop :: HttpClientDrv -> RIO e ()
    stop HttpClientDrv{..} = do
      -- Cancel all the outstanding asyncs, ignoring any exceptions.
      liveThreads <- atomically $ readTVar hcdLive
      mapM_ cancel liveThreads

    handleEffect :: HttpClientDrv -> HttpClientEf -> IO ()
    handleEffect drv = \case
      HCERequest _ id req -> runRIO env (newReq drv id req)
      HCECancelRequest _ id -> runRIO env (cancelReq drv id)

    newReq :: HttpClientDrv -> ReqId -> HttpClientReq -> RIO e ()
    newReq drv id req = do
      async <- runReq drv id req
      -- If the async has somehow already completed, don't put it in the map,
      -- because then it might never get out.
      atomically $ pollSTM async >>= \case
        Nothing -> modifyTVar' (hcdLive drv) (insertMap id async)
        Just _  -> pure ()

    -- The problem with the original http client code was that it was written
    -- to the idea of what the events "should have" been instead of what they
    -- actually were. This means that this driver doesn't run like the vere
    -- http client driver. The vere driver was written assuming that parts of
    -- events could be compressed together: a Start might contain the only
    -- chunk of data and immediately complete, where here the Start event, the
    -- Continue (with File) event, and the Continue (completed) event are three
    -- separate things.
    runReq :: HttpClientDrv -> ReqId -> HttpClientReq -> RIO e (Async ())
    runReq HttpClientDrv{..} id req = async $ flip finally aftr $
      case cvtReq req of
        Nothing -> do
          logInfo $ displayShow ("(malformed http client request)", id, req)
          planEvent id (Cancel ())
        Just r -> do
          logDebug $ displayShow ("(http client request)", id, req)
          withRunInIO $ \run ->
            H.withResponse r hcdManager $ \x -> run (exec x)
      where
        -- Make sure to remove our entry from hcdLive after we're done so the
        -- map doesn't grow without bound.
        aftr :: RIO e ()
        aftr = atomically $ modifyTVar' hcdLive (deleteMap id)

        recv :: H.BodyReader -> RIO e (Maybe ByteString)
        recv read = io $ read <&> \case chunk | null chunk -> Nothing
                                              | otherwise  -> Just chunk

        exec :: H.Response H.BodyReader -> RIO e ()
        exec resp = do
          let headers  = cvtRespHeaders resp
              getChunk = recv (H.responseBody resp)
              loop     = getChunk >>= \case
                           Nothing -> planEvent id (Continue Nothing True)
                           Just bs -> do
                             planEvent id $
                               Continue (Just $ File $ Octs bs) False
                             loop
          planEvent id (Start headers Nothing False)
          loop

    planEvent :: ReqId -> HttpEvent -> RIO e ()
    planEvent id ev = do
      logDebug $ displayShow ("(http client response)", id, (describe ev))

      let recvEv = EvBlip
                 $ BlipEvHttpClient
                 $ HttpClientEvReceive (kingId, ()) (fromIntegral id) ev

      let recvFailed _ = pure ()

      atomically $ plan (EvErr recvEv recvFailed)

    -- show an HttpEvent with byte count instead of raw data
    describe :: HttpEvent -> String
    describe (Start header Nothing final) =
      "(Start " ++ (show header) ++ " ~ " ++ (show final)
    describe (Start header (Just (File (Octs bs))) final) =
      "(Start " ++ (show header) ++ " (" ++ (show $ length bs) ++ " bytes) " ++
      (show final)
    describe (Continue Nothing final) =
      "(Continue ~ " ++ (show final)
    describe (Continue (Just (File (Octs bs))) final) =
      "(Continue (" ++ (show $ length bs) ++ " bytes) " ++ (show final)
    describe (Cancel ()) = "(Cancel ())"

    waitCancel :: Async a -> RIO e (Either SomeException a)
    waitCancel async = cancel async >> waitCatch async

    cancelThread :: ReqId -> Async a -> RIO e ()
    cancelThread id =
      waitCancel >=> \case Left _  -> planEvent id $ Cancel ()
                           Right _ -> pure ()

    cancelReq :: HttpClientDrv -> ReqId -> RIO e ()
    cancelReq drv id =
      join $ atomically $ do
        tbl <- readTVar (hcdLive drv)
        case lookup id tbl of
          Nothing    -> pure (pure ())
          Just async -> do writeTVar (hcdLive drv) (deleteMap id tbl)
                           pure (cancelThread id async)
