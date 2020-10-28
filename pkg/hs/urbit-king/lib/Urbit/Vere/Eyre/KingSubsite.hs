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
import Urbit.Vere.Serf.Types

import Data.Conduit       (ConduitT, Flush(..), yield)
import Data.Text.Encoding (encodeUtf8Builder)
import Urbit.Noun.Tank    (wash)

import qualified Data.Text.Encoding  as E
import qualified Network.HTTP.Types  as H
import qualified Network.Wai         as W
import qualified Network.Wai.Conduit as W
import qualified Urbit.Noun.Time     as Time

newtype KingSubsite = KS { runKingSubsite :: W.Application }

data SlogAction
  = KeepAlive
  | Slog (Atom, Tank)

streamSlog :: Monad m => SlogAction -> ConduitT () (Flush Builder) m ()
streamSlog a = do
  case a of
    KeepAlive -> pure ()
    Slog (_, t) -> for_ (wash (WashCfg 0 80) (tankTree t)) $ \l -> do
      yield $ Chunk "data:"
      yield $ Chunk $ encodeUtf8Builder $ unTape l
      yield $ Chunk "\n"
  yield $ Chunk "\n"
  yield $ Flush

kingSubsite :: HasLogFunc e
            => Ship
            -> (Time.Wen -> Gang -> Path -> IO (Maybe (Term, Noun)))
            -> TVar ((Atom, Tank) -> IO ())
            -> RAcquire e KingSubsite
kingSubsite who scry func = do  --TODO  unify who into scry
  clients <- newTVarIO (mempty :: Map Word (SlogAction -> IO ()))
  nextId  <- newTVarIO (0 :: Word)
  baton   <- newTMVarIO ()
  env     <- ask

  atomically $ writeTVar func $ \s -> readTVarIO clients >>= traverse_ ($ Slog s)

  acquireWorker "Runtime subsite keep-alive" $ forever $ do
    threadDelay 20_000_000
    io $ readTVarIO clients >>= traverse_ ($ KeepAlive)

  --TODO  scry to verify cookie authentication
  pure $ KS $ \req respond -> case W.pathInfo req of
    ("~_~":"slog":_) -> bracket
      (do
        id <- atomically $ do
          id <- readTVar nextId
          modifyTVar' nextId (+ 1)
          pure id
        slogQ <- newTQueueIO
        atomically $
          modifyTVar' clients (insertMap id (atomically . writeTQueue slogQ))
        pure (id, slogQ))
      (\(id, _) -> atomically $ modifyTVar' clients (deleteMap id))
      (\(_, q) -> do
        authed <- authenticated env req
        if not authed then
          respond $ W.responseLBS (H.mkStatus 403 "Permission Denied") [] ""
        else
          let loop = yield Flush >> forever (atomically (readTQueue q) >>= streamSlog)
          in  respond $ W.responseSource (H.mkStatus 200 "OK") heads loop)

    _ -> respond $ W.responseLBS (H.mkStatus 404 "Not Found") [] ""

  where
    heads = [ ("Content-Type" , "text/event-stream")
            , ("Cache-Control", "no-cache")
            , ("Connection"   , "keep-alive")
            ]

    authenticated env req = runRIO env
                          $ (scryAuth $ getCookie req)
                          >>= pure . fromMaybe False

    getCookie req = intercalate "; "
                  $ fmap (E.decodeUtf8 . snd)
                  $ filter ((== "cookie") . fst)
                  (W.requestHeaders req)

    scryAuth :: HasLogFunc e
              => Text
              -> RIO e (Maybe Bool)
    scryAuth cookie =
      scry' $ fmap MkKnot ["authenticated", "cookie", textAsTa cookie]

    --TODO  refactor into scry lib, as:
    -- (forall n. FromNoun n => Text -> Text -> [Text] -> IO (Maybe n))
    --                      vanecare -> desk -> restofpath
    scry' :: forall e n
           . (HasLogFunc e, FromNoun n)
          => [Knot]
          -> RIO e (Maybe n)
    scry' p = do
      env <- ask
      wen <- io Time.now
      let nkt = MkKnot $ tshow $ Time.MkDate wen
      let pax = Path $ "ex" : MkKnot (tshow who) : "" : nkt : p
      putStrLn (tshow pax)
      io (scry wen Nothing pax) >>= \case
        Just (_, fromNoun @n -> Just v) -> pure $ Just v
        Just (_, n) -> do
          logError $ displayShow ("eyre: uncanny scry result", pax, n)
          pure Nothing
        Nothing -> pure Nothing

fourOhFourSubsite :: Ship -> KingSubsite
fourOhFourSubsite who = KS $ \req respond ->
  respond $ W.responseLBS (H.mkStatus 404 "Not Found") [] body
  where
    body = toLazyByteString $ foldMap charUtf8 $ msg
    msg  = "Ship " <> (show who) <> " not docked."
