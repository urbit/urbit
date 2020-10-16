{-|
  KingSubsite: runtime-exclusive HTTP request handling, for /~_~
-}

module Urbit.Vere.Eyre.KingSubsite
  ( KingSubsite
  , kingSubsite
  , runKingSubsite
  , fourOhFourSubsite
  ) where

import Urbit.Prelude hiding (Builder)

import Data.ByteString.Builder

import Data.Conduit       (ConduitT, Flush(..), yield)
import Data.Text.Encoding (encodeUtf8Builder)
import Urbit.Noun.Tank    (wash)

import qualified Network.HTTP.Types  as H
import qualified Network.Wai         as W
import qualified Network.Wai.Conduit as W

newtype KingSubsite = KS { runKingSubsite :: W.Application }

data SlogAction
  = KeepAlive
  | Slog (Atom, Tank)

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

kingSubsite :: HasLogFunc e
            => TVar ((Atom, Tank) -> IO ())
            -> RAcquire e KingSubsite
kingSubsite func = do
  slogQ :: TQueue (Atom, Tank) <- newTQueueIO
  baton :: TMVar ()            <- newEmptyTMVarIO
  atomically $ writeTVar func (\s -> atomically $ writeTQueue slogQ s)
  acquireWorker "Runtime subsite keep-alive" $ forever $ do
    atomically $ putTMVar baton ()
    threadDelay 20_000_000

  let action = (KeepAlive <$ takeTMVar baton)  -- every 20s
           <|> (Slog <$> readTQueue slogQ)
           --TODO  queue builds even without listeners connected.
           --      and with listeners connected, only one pops from queue!
           --      need queue per connection, not global?

  let loop = yield Flush >> forever (atomically action >>= conduit)

  --TODO  scry to verify cookie authentication
  pure $ KS $ \req respond -> respond $ case W.pathInfo req of
    ("~_~":"slog":_) -> W.responseSource (H.mkStatus 200 "OK")        heads loop
    _                -> W.responseLBS    (H.mkStatus 404 "Not Found") []    ""
    where
      heads = [ ("Content-Type" , "text/event-stream")
              , ("Cache-Control", "no-cache")
              , ("Connection"   , "keep-alive")
              ]

fourOhFourSubsite :: Ship -> KingSubsite
fourOhFourSubsite who = KS $ \req respond ->
  respond $ W.responseLBS (H.mkStatus 404 "Not Found") [] body
  where
    body = toLazyByteString $ foldMap charUtf8 $ msg
    msg  = "Ship " <> (show who) <> " not docked."
