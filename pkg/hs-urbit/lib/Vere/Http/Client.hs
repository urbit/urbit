{-
  - TODO When making a request, handle the case where the request id is
         already in use.
-}

module Vere.Http.Client where

import ClassyPrelude

import Noun
import Vere.Http
import Arvo (Header(..))

import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Client  as H
import qualified Network.HTTP.Types   as HT


-- Types -----------------------------------------------------------------------

type ReqId = Word

data Ev = Receive ReqId Event -- [%receive @ todo]

data Eff
    = NewReq ReqId Request -- [%request @ todo]
    | CancelReq ReqId      -- [%cancel-request @]
  deriving (Eq, Ord, Show)

data State = State
  { sManager :: H.Manager
  , sLive    :: TVar (Map ReqId (Async ()))
  , sChan    :: MVar Ev
  }


--------------------------------------------------------------------------------

cvtReq :: Request -> Maybe H.Request
cvtReq r =
  H.parseRequest (unpack (unCord $ url r)) <&> \init -> init
    { H.method = encodeUtf8 $ tshow (method r),
      H.requestHeaders =
        headerList r <&> \(Header k v) -> (CI.mk (unBytes k), unBytes v),
      H.requestBody =
        H.RequestBodyBS $ case body r of
                            Nothing        -> ""
                            Just (Octs bs) -> bs
    }

cvtRespHeaders :: H.Response a -> ResponseHeader
cvtRespHeaders resp =
    ResponseHeader (fromIntegral $ HT.statusCode (H.responseStatus resp)) heads
  where
    heads = convertHeaders (H.responseHeaders resp)


--------------------------------------------------------------------------------

initState :: IO State
initState = State <$> H.newManager H.defaultManagerSettings
                  <*> newTVarIO mempty
                  <*> newEmptyMVar

emit :: State -> Ev -> IO ()
emit st event = putMVar (sChan st) event

runEff :: State -> Eff -> IO ()
runEff st = \case NewReq id req -> newReq st id req
                  CancelReq id  -> cancelReq st id

newReq :: State -> ReqId -> Request -> IO ()
newReq st id req = do async <- runReq st id req
                      atomically $ modifyTVar (sLive st) (insertMap id async)

waitCancel :: Async a -> IO (Either SomeException a)
waitCancel async = cancel async >> waitCatch async

cancelThread :: State -> ReqId -> Async a -> IO ()
cancelThread st id =
  waitCancel >=> \case Left _  -> emit st (Receive id Canceled)
                       Right _ -> pure ()

cancelReq :: State -> ReqId -> IO ()
cancelReq st id =
  join $ atomically $ do
    tbl <- readTVar (sLive st)
    case lookup id tbl of
      Nothing    -> pure (pure ())
      Just async -> do writeTVar (sLive st) (deleteMap id tbl)
                       pure (cancelThread st id async)

runReq :: State -> ReqId -> Request -> IO (Async ())
runReq st id req = async $
  case cvtReq req of
    Nothing -> emit st (Receive id (Failed "bad-request-e"))
    Just r  -> H.withResponse r (sManager st) exec
  where
    recv :: H.BodyReader -> IO (Maybe ByteString)
    recv read = read <&> \case chunk | null chunk -> Nothing
                                     | otherwise  -> Just chunk

    exec :: H.Response H.BodyReader -> IO ()
    exec resp = do
      let headers  = cvtRespHeaders resp
          getChunk = recv (H.responseBody resp)
          loop     = getChunk >>= \case
                       Nothing -> emit st (Receive id Done)
                       Just bs -> do emit st (Receive id $ Received $ Octs bs)
                                     loop
      emit st (Receive id $ Started headers)
      loop
