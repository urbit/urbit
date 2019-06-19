-- +http-server ----------------------------------------------------------------

module Vere.Http.Server where

import ClassyPrelude
import Vere.Http
import Data.Noun.Atom
import Control.Lens

import Control.Concurrent (ThreadId, killThread, forkIO)
import Data.Noun.Pill     (pill, pillBS, Pill(..))

import qualified Data.ByteString             as BS
import qualified Network.HTTP.Types          as H
import qualified Network.Wai                 as W
import qualified Network.Wai.Handler.Warp    as W
import qualified Network.Wai.Handler.WarpTLS as W

type ServerId = Word
type ConnectionId = Word
type RequestId = Word

data Eff = Eff ServerId ConnectionId RequestId ServerRequest
  deriving (Eq, Ord, Show)

-- | An http server effect is configuration, or it sends an outbound response
data ServerRequest
  = SetConfig Config
  | Response Event
  deriving (Eq, Ord, Show)

data Config = Config
  { secure :: Maybe (Key, Cert)
  , proxy :: Bool
  , log :: Bool
  , redirect :: Bool
  }
  deriving (Eq, Ord, Show)


-- Note: We need to parse PEM-encoded RSA private keys and cert or cert chain
-- from Wain
type Key = PEM
type Cert = PEM
data Wain = Wain [Text]

newtype PEM = PEM ByteString
  deriving newtype (Eq, Ord, Show)

data ClientResponse
  = Progress ResponseHeader Int (Maybe Int) (Maybe ByteString)
  | Finished ResponseHeader (Maybe MimeData)
  | Cancel

data MimeData = MimeData Text ByteString

--

data Ev

data State = State
  { thread :: MVar (Maybe (Config, ThreadId))
  , sChan  :: MVar Ev
  }

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

startServer :: State -> Config -> IO ()
startServer s c = do
  tls <- case (secure c) of
    Nothing -> error "no wai"
    Just (PEM key, PEM cert) ->
      pure (W.tlsSettingsMemory cert key)

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
    Left _ -> Nothing
    Right m -> Just m

data Octs = Octs Atom Atom

bsOcts :: Iso' ByteString Octs
bsOcts = iso toOcts fromOcts
  where
    toOcts :: ByteString -> Octs
    toOcts bs =
      Octs (fromIntegral (length bs)) (bs ^. from (pill . pillBS))

    fromOcts :: Octs -> ByteString
    fromOcts (Octs (fromIntegral -> len) atm) = bs <> pad
      where
        bs  = atm ^. pill . pillBS
        pad = BS.replicate (max 0 (len - length bs)) 0

readEvents :: W.Request -> IO Request
readEvents request = do
  let Just method = cookMeth request
      url = decodeUtf8 (W.rawPathInfo request)
      headers = convertHeaders (W.requestHeaders request)
  bodyLbs <- W.strictRequestBody request
  let body = if length bodyLbs == 0 then Nothing
        else Just (toStrict bodyLbs)

  -- TODO: Check if wai just deletes the 'host': header like h2o does?

  pure (Request method url headers body)
