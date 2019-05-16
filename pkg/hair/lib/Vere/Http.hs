-- zuse: +http -----------------------------------------------------------------

module Vere.Http where

import ClassyPrelude
import Data.Noun

data Header = Header Text Text

data Method = CONNECT
            | DELETE
            | GET
            | HEAD
            | OPTIONS
            | POST
            | PUT
            | TRACE
            deriving (Eq,Ord,Show)

data Request = Request
  { method :: Method
  , url :: Text
  , headerList :: [Header]
  , body :: Maybe ByteString
  }

data ResponseHeader = ResponseHeader
  { statusCode :: Integer
  , headers :: [Header]
  }

data Event = Start ResponseHeader (Maybe ByteString) Bool
           | Continue (Maybe ByteString) Bool
           | Cancel

--instance FromNoun Event where
--  fromNoun = undefined
