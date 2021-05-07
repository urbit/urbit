{-|
    HTTP Driver
-}

module Urbit.Vere.Http where

import Urbit.Prelude
import Urbit.Arvo

import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types   as HT

--------------------------------------------------------------------------------

convertHeaders :: [HT.Header] -> [Header]
convertHeaders = fmap f
  where
    f (k, v) = Header (Cord $ decodeUtf8 $ CI.foldedCase k)
                      (MkBytes $ fromBS v)

unconvertHeaders :: [Header] -> [HT.Header]
unconvertHeaders = fmap f
  where
    f (Header (Cord k) (MkBytes (toBS -> v))) = (CI.mk (encodeUtf8 k), v)
