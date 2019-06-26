-- zuse: +http -----------------------------------------------------------------

module Vere.Http where

import ClassyPrelude
import Data.Noun
import Data.Noun.Atom
import Data.Noun.Poet

import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.Method as H

data Header = Header Text Text
  deriving (Eq, Ord, Show, Generic, ToNoun)

type Method = H.StdMethod

instance ToNoun H.StdMethod where
  toNoun = toNoun . Cord . encodeUtf8 . tshow

data Request = Request
    { method     :: Method
    , url        :: Text
    , headerList :: [Header]
    , body       :: Maybe ByteString
    }
  deriving (Eq, Ord, Show, Generic, ToNoun)

data ResponseHeader = ResponseHeader
     { statusCode :: Word
     , headers :: [Header]
     }
  deriving (Eq, Ord, Show, Generic, ToNoun)


data Event
    = Started ResponseHeader -- [%start hdr (unit octs) ?]
    | Received ByteString    -- [%continue [~ octs] %.n]
    | Done                   -- [%continue ~ %.y]
    | Canceled               -- %cancel
    | Failed Text            -- %cancel
  deriving (Eq, Ord, Show, Generic, ToNoun)


convertHeaders :: [HT.Header] -> [Header]
convertHeaders = fmap f
  where
    f (k, v) = Header (decodeUtf8 (CI.original k)) (decodeUtf8 v)
