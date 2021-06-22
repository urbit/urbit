module Urbit.Noun.ByteString where

import ClassyPrelude hiding (ByteString, Text)
import RIO (Display(..))
import qualified ClassyPrelude as CP
import Data.Char (ord)
import qualified Data.Vector as P
import GHC.Base (unsafeChr)
import qualified Data.Text.Encoding as E

type ByteString = P.Vector Word8

type Text = [Char]

toBS :: ByteString -> CP.ByteString
toBS = pack . P.toList

fromBS :: CP.ByteString -> ByteString
fromBS = P.fromList . unpack

toT :: Text -> CP.Text
toT = pack

fromT :: CP.Text -> Text
fromT = unpack

w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral
{-# INLINE w2c #-}

c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

instance IsString ByteString where
  fromString = fromBS . fromString

instance Display Text where
  display = display . toT

encodeUtf8 :: Text -> ByteString
encodeUtf8 = fromBS . E.encodeUtf8 . toT

decodeUtf8 :: ByteString -> Text
decodeUtf8 = fromT . E.decodeUtf8 . toBS