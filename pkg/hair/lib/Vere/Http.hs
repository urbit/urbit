-- zuse: +http -----------------------------------------------------------------

module Vere.Http where

import ClassyPrelude
import Data.Noun

data Header = Header Text Text

data Method = CONNECT | DELETE | GET | HEAD | OPTIONS | POST | PUT | TRACE
  deriving (Eq,Ord,Show)

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
