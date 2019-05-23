-- +http-server ----------------------------------------------------------------

module Vere.Http.Server where

import ClassyPrelude
import Vere.Http

import Data.Noun.Atom
import Data.Noun.Pill (packAtom)
import qualified Network.HTTP.Types.Method as H
import qualified Network.Wai as H

type ServerId = Word
type ConnectionId = Word
type RequestId = Word

data Eff = Eff ServerId ConnectionId RequestId ServerRequest

-- | An http server effect is configuration, or it sends an outbound response
data ServerRequest
  = SetConfig Config
  | Response Event

data Config = Config
  { secure :: Maybe (Key, Cert)
  , proxy :: Bool
  , log :: Bool
  , redirect :: Bool
  }

-- Note: We need to parse PEM-encoded RSA private keys and cert or cert chain
-- from Wain
newtype Key = Key PEM
newtype Cert = Cert PEM
data Wain = Wain [Text]

data PEM

data ClientResponse
  = Progress ResponseHeader Int (Maybe Int) (Maybe ByteString)
  | Finished ResponseHeader (Maybe MimeData)
  | Cancel

data MimeData = MimeData Text ByteString

--

cookMeth :: H.Request -> Maybe Method
cookMeth re =
  case H.parseMethod (H.requestMethod re) of
    Left _ -> Nothing
    Right m -> Just m

data Octs = Octs Atom Atom

bsToOcts :: ByteString -> Octs
bsToOcts bs = Octs (fromIntegral (length bs)) (packAtom bs)

readEvents :: H.Request -> IO Request
readEvents request = do
  let Just method = cookMeth request
      url = decodeUtf8 (H.rawPathInfo request)
      headers = convertHeaders (H.requestHeaders request)
  bodyLbs <- H.strictRequestBody request
  let body = if length bodyLbs == 0 then Nothing
        else Just (toStrict bodyLbs)

  -- TODO: Check if wai just deletes the 'host': header like h2o does?

  pure (Request method url headers body)
