module Urbit.Vere.Eyre.Site (app) where

import Urbit.Prelude hiding (Builder)

import Data.ByteString.Builder

import Data.Conduit       (ConduitT, Flush(..), yield)
import Data.Text.Encoding (encodeUtf8Builder)
import Urbit.Noun.Tank    (wash)

import qualified Network.HTTP.Types  as H
import qualified Network.Wai         as W
import qualified Network.Wai.Conduit as W

data SlogAction
  = KeepAlive
  | Slog (Atom, Tank)

-- veify that if you have multiple open uwu slogs, you multiplex
-- thread TVar func and this server through from pier (loopback only)
-- LATER check cookies & scry, support on all servers

conduit :: SlogAction -> ConduitT () (Flush Builder) IO ()
conduit a = do
  case a of
    KeepAlive -> pure ()
    Slog (_, t) -> for_ (wash (WashCfg 0 80) (tankTree t)) $ \l -> do
      yield $ Chunk "data:"
      yield $ Chunk $ encodeUtf8Builder $ unTape l
      yield $ Chunk "\n"
  yield $ Chunk "\n"
  yield $ Flush

app :: HasLogFunc e
    => TVar ((Atom, Tank) -> IO ())
    -> RAcquire e W.Application
app func = do
  slogQ :: TQueue (Atom, Tank) <- newTQueueIO
  baton :: TMVar ()            <- newEmptyTMVarIO
  atomically $ writeTVar func (\s -> atomically $ writeTQueue slogQ s)
  acquireWorker "Runtime subsite keep-alive" $ forever $ do
    atomically $ putTMVar baton ()
    threadDelay 30_000_000

  let action = (KeepAlive <$ takeTMVar baton)  -- every 30s
           <|> (Slog <$> readTQueue slogQ)
  
  -- TODO write more compactly
  let loop = forever (atomically action >>= conduit)

  pure $ \req respond -> respond $ case W.pathInfo req of
    ("~_~":"slog":_) -> W.responseSource (H.mkStatus 200 "OK")        [] loop
    _                -> W.responseLBS    (H.mkStatus 404 "Not Found") [] ""
