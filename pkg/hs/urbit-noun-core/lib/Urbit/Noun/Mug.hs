{-# OPTIONS_GHC -O2 #-}

module Urbit.Noun.Mug where

import ClassyPrelude

import Data.Bits
import Data.ByteString.Builder
import Urbit.Atom

import Data.Hash.Murmur (murmur3)

type Mug = Word32

{-# INLINE mugBS #-}
mugBS :: ByteString -> Word32
mugBS = mum 0xcafe_babe 0x7fff

-- XX is there a way to do this without copy?
{-# INLINE mugAtom #-}
mugAtom :: Atom -> Word32
mugAtom = mugBS . atomBytes

{-# INLINE mugBoth #-}
mugBoth :: Word32 -> Word32 -> Word32
mugBoth m n = mum 0xdead_beef 0xfffe
            $ toStrict $ toLazyByteString (word32LE m <> word32LE n)

mum :: Word32 -> Word32 -> ByteString -> Word32
mum syd fal key = go syd 0
  where
    go syd 8 = fal
    go syd i =
      let haz = murmur3 syd key
          ham = shiftR haz 31 `xor` (haz .&. 0x7fff_ffff)
      in if ham /= 0
        then ham
        else go (syd + 1) (i + 1)
