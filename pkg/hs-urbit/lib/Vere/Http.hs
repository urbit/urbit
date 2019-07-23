-- zuse: +http -----------------------------------------------------------------

module Vere.Http where

import ClassyPrelude
import Noun

import qualified Data.CaseInsensitive      as CI
import qualified Network.HTTP.Types        as HT
import qualified Network.HTTP.Types.Method as H

--------------------------------------------------------------------------------

data Header = Header Cord Bytes
  deriving (Eq, Ord, Show)

type Method = H.StdMethod

data Request = Request
    { method     :: Method
    , url        :: Cord
    , headerList :: [Header]
    , body       :: Maybe Octs
    }
  deriving (Eq, Ord, Show)

data ResponseHeader = ResponseHeader
     { statusCode :: Word
     , headers    :: [Header]
     }
  deriving (Eq, Ord, Show)

data RawEvent
    = Start ResponseHeader (Maybe Octs) Bool
    | Continue (Maybe Octs) Bool
    | Cancel
  deriving (Eq, Ord, Show)

deriveNoun ''Request
deriveNoun ''Header
deriveNoun ''ResponseHeader
deriveNoun ''RawEvent


--------------------------------------------------------------------------------

data Event
    = Started ResponseHeader -- [%start hdr (unit octs) ?]
    | Received Octs          -- [%continue [~ octs] %.n]
    | Done                   -- [%continue ~ %.y]
    | Canceled               -- %cancel
    | Failed Cord            -- %cancel
  deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------

instance ToNoun H.StdMethod where
  toNoun = toNoun . Cord . decodeUtf8 . H.renderStdMethod

instance FromNoun H.StdMethod where
  parseNoun n = named "StdMethod" $ do
    Cord m <- parseNoun n
    case H.parseMethod (encodeUtf8 m) of
      Left bs -> fail ("Unexpected method: " <> unpack (decodeUtf8 bs))
      Right m -> pure m


--------------------------------------------------------------------------------

convertHeaders :: [HT.Header] -> [Header]
convertHeaders = fmap f
  where
    f (k, v) = Header (Cord $ decodeUtf8 $ CI.original k)
                      (MkBytes v)
