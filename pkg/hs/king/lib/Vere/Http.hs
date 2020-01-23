-- zuse: +http -----------------------------------------------------------------

module Vere.Http where

import ClassyPrelude
import Ur.Noun
import Arvo

import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types   as HT

--------------------------------------------------------------------------------

convertHeaders :: [HT.Header] -> [Header]
convertHeaders = fmap f
  where
    f (k, v) = Header (Cord $ decodeUtf8 $ CI.foldedCase k)
                      (MkBytes v)

unconvertHeaders :: [Header] -> [HT.Header]
unconvertHeaders = fmap f
  where
    f (Header (Cord k) (MkBytes v)) = (CI.mk (encodeUtf8 k), v)
