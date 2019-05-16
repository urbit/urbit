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

newtype Key = Key Wain
newtype Cert = Cert Wain
data Wain = Wain [Text]

data ClientResponse
  = Progress ResponseHeader Int (Maybe Int) (Maybe ByteString)
  | Finished ResponseHeader (Maybe MimeData)
  | Cancel

data MimeData = MimeData Text ByteString
