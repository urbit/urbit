{-# OPTIONS_GHC -O2 #-}

module Urbit.Noun.Mug where

import ClassyPrelude

import Data.Bits
import Urbit.Atom

import Data.Hash.Murmur (murmur3)

type Mug = Word32

{-# INLINE mugBS #-}
mugBS :: ByteString -> Word32
mugBS = go 0xcafebabe
  where
    go seed buf =
      let haz = murmur3 seed buf
          ham = shiftR haz 31 `xor` (haz .&. 0x7fff_ffff)
      in if ham == 0
        then go (seed + 1) buf
        else ham

-- XX is there a way to do this without copy?
{-# INLINE mugAtom #-}
mugAtom :: Atom -> Word32
mugAtom = mugBS . atomBytes

{-# INLINE mugBoth #-}
mugBoth :: Word32 -> Word32 -> Word32
mugBoth m n = mugAtom $ fromIntegral $ m `xor` 0x7fff_ffff `xor` n
