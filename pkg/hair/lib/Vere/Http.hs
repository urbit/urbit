-- zuse: +http -----------------------------------------------------------------

module Vere.Http where

import ClassyPrelude
import Data.Noun

import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.Method as H

data Header = Header Text Text

type Method = H.StdMethod

data Request = Request
  { method :: Method
  , url :: Text
  , headerList :: [Header]
  , body :: Maybe ByteString
  }

data ResponseHeader = ResponseHeader
  { statusCode :: Int
  , headers :: [Header]
  }

data Event = Started ResponseHeader -- [%start hdr (unit octs) ?]
           | Received ByteString    -- [%continue [~ octs] %.n]
           | Done                   -- [%continue ~ %.y]
           | Canceled               -- %cancel
           | Failed Text            -- %cancel

convertHeaders :: [HT.Header] -> [Header]
convertHeaders = fmap f
  where
    f (k, v) = Header (decodeUtf8 (CI.original k)) (decodeUtf8 v)
