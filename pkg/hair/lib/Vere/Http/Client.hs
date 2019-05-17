-- +http-client ----------------------------------------------------------------

module Vere.Http.Client where

import ClassyPrelude
import Data.Void
import qualified Vere.Http as Http
import Control.Concurrent hiding (newEmptyMVar, putMVar)

import Network.HTTP.Client as H

-- | An http client effect is either requesting outbound, or canceling an old
-- outbound connection.
data Eff
  = Request Word Http.Request
  | CancelRequest Word

data Ev
  = Receive Word Http.Event

-- | All live requests
data State = State H.Manager (TVar (Map Word ThreadId)) (MVar Ev)

initState :: IO State
initState = do
  manager <- H.newManager defaultManagerSettings
  liveReqs <- newTVarIO mempty
  channels <- newEmptyMVar
  pure (State manager liveReqs channels)


emitEvent :: State -> Ev -> IO ()
emitEvent (State _ _ chan) event =
  putMVar chan event

run :: State -> Eff -> IO ()
run st@(State manager s _) (Request id request) = do
  -- TODO: Handle case where id is already live
  x <- startHTTP st id request
  atomically $ modifyTVar s (insertMap id x)

run st@(State manager s _) (CancelRequest id) =
  join $ atomically $ do
    m <- readTVar s
    case lookup id m of
      Nothing -> pure (pure ())
      Just r -> do
        m <- writeTVar s (deleteMap id m)
        pure (cancelHTTP st id r)


startHTTP :: State -> Word -> Http.Request -> IO ThreadId
startHTTP st@(State manager _ chan) id request = forkIO $ do
    withResponse (convertRequest request) manager $ \response -> do
      let headers = convertResponseHeaders response
      emitEvent st (Receive id (Http.Start headers Nothing False))
      readChunks (H.responseBody response)
  where
    readChunks :: H.BodyReader -> IO ()
    readChunks reader = do
      chunk <- H.brRead reader
      if null chunk
        then emitEvent st (Receive id (Http.Continue Nothing True))
        else do
          emitEvent st (Receive id (Http.Continue (Just chunk) False))
          readChunks reader


convertRequest :: Http.Request -> H.Request
convertRequest = undefined

convertResponseHeaders :: H.Response a -> Http.ResponseHeader
convertResponseHeaders response = undefined

cancelHTTP :: State -> Word -> ThreadId -> IO ()
cancelHTTP st requestId threadId = do
  -- There's a race condition here because threadId could have already
  -- finished.
  killThread threadId
  emitEvent st (Receive requestId (Http.Cancel))
