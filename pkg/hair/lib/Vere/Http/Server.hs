-- +http-server ----------------------------------------------------------------

module Vere.Http.Server where

import ClassyPrelude
import Vere.Http

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
