module Urbit.Noun.ByteString where

import ClassyPrelude hiding (ByteString)
import qualified ClassyPrelude as CP
import Data.Char (ord)
import qualified Data.Vector as P
import GHC.Base (unsafeChr)

type ByteString = P.Vector Word8

toBS :: ByteString -> CP.ByteString
toBS = pack . P.toList

fromBS :: CP.ByteString -> ByteString
fromBS = P.fromList . unpack

w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral
{-# INLINE w2c #-}

c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

instance IsString ByteString where
  fromString = fromBS . fromString