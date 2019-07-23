-- +http-server ----------------------------------------------------------------

module Vere.Http.Server where

import ClassyPrelude

import Noun
import Vere.Http

import Control.Concurrent (ThreadId, forkIO, killThread)

import qualified Network.HTTP.Types          as H
import qualified Network.Wai                 as W
import qualified Network.Wai.Handler.Warp    as W
import qualified Network.Wai.Handler.WarpTLS as W

-- Types -----------------------------------------------------------------------

type ServerId     = Word
type ConnectionId = Word
type RequestId    = Word

-- Note: We need to parse PEM-encoded RSA private keys and cert or cert chain
-- from Wain
type Key = PEM
type Cert = PEM
newtype Wain = Wain [Cord]
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

newtype PEM = PEM Cord
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)


--------------------------------------------------------------------------------

data Eff = Eff ServerId ConnectionId RequestId ServerRequest
  deriving (Eq, Ord, Show)

-- | An http server effect is configuration, or it sends an outbound response
data ServerRequest
  = SetConfig Config
  | Response Event
  deriving (Eq, Ord, Show)

data Config = Config
    { secure   :: Maybe (Key, Cert)
    , proxy    :: Bool
    , log      :: Bool
    , redirect :: Bool
    }
  deriving (Eq, Ord, Show)

deriveNoun ''Config

data ClientResponse
  = Progress ResponseHeader Int (Maybe Int) (Maybe ByteString)
  | Finished ResponseHeader (Maybe MimeData)
  | Cancel ()

data MimeData = MimeData Text ByteString

data Ev

data State = State
  { thread :: MVar (Maybe (Config, ThreadId))
  , sChan  :: MVar Ev
  }

--------------------------------------------------------------------------------

init :: IO State
init =
  -- When we initialize things, we send an event into arvo
  -- When we receive the set-config event, then we start stuff up

  --  This works for now, but we need to actually do stuff per above.
  State <$> newMVar Nothing
        <*> newEmptyMVar

onSetConfig :: State -> Config -> IO ()
onSetConfig s c = do
  v <- takeMVar (thread s)

  maybe (pure ()) (killThread . snd) v

  putMVar (thread s) Nothing
  startServer s c

cordBytes :: Cord -> ByteString
cordBytes = encodeUtf8 . unCord

startServer :: State -> Config -> IO ()
startServer s c = do
  tls <- case (secure c) of
    Nothing -> error "no wai"
    Just (PEM key, PEM cert) ->
      pure (W.tlsSettingsMemory (cordBytes cert) (cordBytes key))

  -- we need to do the dance where we do the socket checking dance. or shove a
  -- socket into it.
  tid <- forkIO $ W.runTLS tls W.defaultSettings (app s)
  putMVar (thread s) (Just (c, tid))

app :: State -> W.Application
app s req respond = bracket_
    (pure ())
    (pure ())
    (respond $ W.responseLBS H.status200 [] "Hello World")

cookMeth :: W.Request -> Maybe Method
cookMeth re =
  case H.parseMethod (W.requestMethod re) of
    Left _  -> Nothing
    Right m -> Just m

readEvents :: W.Request -> IO Request
readEvents req = do
  let Just meth = cookMeth req
      url       = Cord $ decodeUtf8 $ W.rawPathInfo req
      headers   = convertHeaders (W.requestHeaders req)
  bodyLbs <- W.strictRequestBody req
  let body = if length bodyLbs == 0 then Nothing
        else Just $ Octs (toStrict bodyLbs)

  -- TODO: Check if wai just deletes the 'host': header like h2o does?

  pure (Request meth url headers body)
