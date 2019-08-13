-- zuse: +http -----------------------------------------------------------------

module Vere.Http where

import ClassyPrelude
import Noun
import Arvo

import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types   as HT

--------------------------------------------------------------------------------

data Request = Request
    { method     :: Method
    , url        :: Cord
    , headerList :: [Header]
    , body       :: Maybe Octs
    }
  deriving (Eq, Ord, Show)

data RawEvent
    = Start ResponseHeader (Maybe Octs) Bool
    | Continue (Maybe Octs) Bool
    | Cancel
  deriving (Eq, Ord, Show)

deriveNoun ''Request
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

convertHeaders :: [HT.Header] -> [Header]
convertHeaders = fmap f
  where
    f (k, v) = Header (Cord $ decodeUtf8 $ CI.foldedCase k)
                      (Cord $ decodeUtf8 v)

unconvertHeaders :: [Header] -> [HT.Header]
unconvertHeaders = fmap f
  where
    f (Header (Cord k) (Cord v)) = (CI.mk (encodeUtf8 k), (encodeUtf8 v))
