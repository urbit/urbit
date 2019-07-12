-- zuse: +http -----------------------------------------------------------------

module Vere.Http where

import ClassyPrelude
import Noun
import Noun.TH

import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.Method as H

--------------------------------------------------------------------------------

data Header = Header Text Text
  deriving (Eq, Ord, Show)

type Method = H.StdMethod

data Request = Request
    { method     :: Method
    , url        :: Text
    , headerList :: [Header]
    , body       :: Maybe ByteString
    }
  deriving (Eq, Ord, Show)

data ResponseHeader = ResponseHeader
     { statusCode :: Word
     , headers :: [Header]
     }
  deriving (Eq, Ord, Show)


data Event
    = Started ResponseHeader -- [%start hdr (unit octs) ?]
    | Received ByteString    -- [%continue [~ octs] %.n]
    | Done                   -- [%continue ~ %.y]
    | Canceled               -- %cancel
    | Failed Text            -- %cancel
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

instance ToNoun H.StdMethod where
  toNoun = toNoun . Cord . H.renderStdMethod

instance FromNoun H.StdMethod where
  parseNoun n = do
    Cord m <- parseNoun n
    case H.parseMethod m of
      Left bs -> fail ("Unexpected method: " <> unpack (decodeUtf8 bs))
      Right m -> pure m

deriveNoun ''Header
deriveNoun ''ResponseHeader
deriveNoun ''Event
deriveNoun ''Request

--------------------------------------------------------------------------------

convertHeaders :: [HT.Header] -> [Header]
convertHeaders = fmap f
  where
    f (k, v) = Header (decodeUtf8 (CI.original k)) (decodeUtf8 v)
