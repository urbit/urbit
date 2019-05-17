{-
  - TODO When making a request, handle the case where the request id is
         already in use.
  - TODO When canceling a request, don't send Http.Canceled if the
         request already finished.
-}

module Vere.Http.Client where

import ClassyPrelude
import Data.Void
import Vere.Http as Http
import Control.Concurrent hiding (newEmptyMVar, putMVar)

import qualified Network.HTTP.Client as H

--------------------------------------------------------------------------------


type ReqId = Word

data Ev = Receive ReqId Http.Event -- %receive

data Eff
  = NewReq ReqId Request -- %request
  | CancelReq ReqId      -- %cancel-request

data State = State
  { sManager :: H.Manager
  , sLive    :: TVar (Map ReqId ThreadId)
  , sChan    :: MVar Ev
  }


--------------------------------------------------------------------------------

cvtReq :: Request -> H.Request
cvtReq = undefined

cvtRespHeaders :: H.Response a -> ResponseHeader
cvtRespHeaders resp = undefined


--------------------------------------------------------------------------------

initState :: IO State
initState = State <$> H.newManager H.defaultManagerSettings
                  <*> newTVarIO mempty
                  <*> newEmptyMVar

emit :: State -> Ev -> IO ()
emit (State _ _ chan) event = putMVar chan event

runEff :: State -> Eff -> IO ()
runEff st@(State _ s _) = \case CancelReq id  -> cancelReq st id
                                NewReq id req -> newReq st id req

newReq :: State -> ReqId -> Request -> IO ()
newReq st id req = do tid <- runReq st id req
                      atomically $ modifyTVar (sLive st) (insertMap id tid)

cancelReq :: State -> ReqId -> IO ()
cancelReq st id =
  join $ atomically $ do
    tbl <- readTVar (sLive st)
    case lookup id tbl of
      Nothing  -> pure (pure ())
      Just tid -> do
        writeTVar (sLive st) (deleteMap id tbl)
        pure $ do killThread tid
                  emit st (Receive id Canceled)

runReq :: State -> ReqId -> Request -> IO ThreadId
runReq st id request =
    forkIO $ H.withResponse (cvtReq request) (sManager st) $ \resp -> do
      let headers  = cvtRespHeaders resp
      let getChunk = recv (H.responseBody resp)
      let loop = getChunk >>= \case
                   Just bs -> emit st (Receive id $ Received bs) >> loop
                   Nothing -> emit st (Receive id Done)
      emit st (Receive id $ Started headers)
      loop
  where
    recv :: H.BodyReader -> IO (Maybe ByteString)
    recv read = read <&> \case chunk | null chunk -> Nothing
                                     | otherwise  -> Just chunk
