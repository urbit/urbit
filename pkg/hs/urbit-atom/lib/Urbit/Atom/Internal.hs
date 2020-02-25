{-|
    Atom implementation with fast conversions between bytestrings
    and atoms.

    TODO Support 32-bit archetectures.
    TODO Support Big Endian.
-}

module Urbit.Atom.Internal where

import Prelude

import Data.Bits                 (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString           (ByteString)
import Data.Vector.Primitive     (Vector(..))
import Data.Word                 (Word8)
import GHC.Exts                  (Ptr(Ptr), sizeofByteArray#)
import GHC.Int                   (Int(..))
import GHC.Integer.GMP.Internals (BigNat(..), bigNatToWord, sizeofBigNat#)
import GHC.Integer.GMP.Internals (indexBigNat#)
import GHC.Integer.GMP.Internals (byteArrayToBigNat#, wordToBigNat, zeroBigNat)
import GHC.Natural               (Natural(..))
import GHC.Prim                  (clz#, minusWord#, plusWord#)
import GHC.Prim                  (Word#, int2Word#, subIntC#, timesWord#)
import GHC.Word                  (Word(..))
import System.IO.Unsafe          (unsafePerformIO)

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Unsafe    as BS
import qualified Data.ByteString.Internal  as BS
import qualified Data.Primitive.ByteArray  as Prim
import qualified Data.Primitive.Types      as Prim
import qualified Data.Vector.Primitive     as VP
import qualified Foreign.ForeignPtr.Unsafe as Ptr


--------------------------------------------------------------------------------

wordBitWidth# :: Word# -> Word#
wordBitWidth# w = minusWord# 64## (clz# w)

wordBitWidth :: Word -> Word
wordBitWidth (W# w) = W# (wordBitWidth# w)

bigNatBitWidth# :: BigNat -> Word#
bigNatBitWidth# nat =
    lswBits `plusWord#` ((int2Word# lastIdx) `timesWord#` 64##)
  where
    (# lastIdx, _ #) = subIntC# (sizeofBigNat# nat) 1#
    lswBits          = wordBitWidth# (indexBigNat# nat lastIdx)

atomBitWidth# :: Natural -> Word#
atomBitWidth# (NatS# gl) = wordBitWidth# gl
atomBitWidth# (NatJ# bn) = bigNatBitWidth# bn

bitWidth :: Num a => Natural -> a
bitWidth a = fromIntegral (W# (atomBitWidth# a))

--------------------------------------------------------------------------------

{-# INLINE takeBitsWord #-}
takeBitsWord :: Int -> Word -> Word
takeBitsWord wid wor = wor .&. (shiftL 1 wid - 1)


--------------------------------------------------------------------------------

{-
    A `Pill` is a bytestring without trailing zeros.
-}
newtype Pill = Pill { unPill :: ByteString }

instance Eq Pill where
  (==) x y = pillBytes x == pillBytes y

instance Show Pill where
  show = show . pillBytes

--------------------------------------------------------------------------------

strip :: ByteString → ByteString
strip buf = BS.take (len - go 0 (len - 1)) buf
  where
    len = BS.length buf
    go n i | i < 0                  = n
           | 0 == BS.unsafeIndex buf i = go (n+1) (i-1)
           | otherwise              = n

pillBytes :: Pill -> ByteString
pillBytes = strip . unPill

bytesPill :: ByteString -> Pill
bytesPill = Pill . strip


--------------------------------------------------------------------------------

{-
    Cast a BigNat to a vector without a copy.
-}
bigNatWords ∷ BigNat → Vector Word
bigNatWords (BN# bArr) =
    Vector 0 (I# (sizeofByteArray# bArr) `div` 8)
             (Prim.ByteArray bArr)

{-|
    Cast a vector to a BigNat. This will not copy.

    TODO Don't crash if given a slice.
-}
wordsBigNat ∷ Vector Word → BigNat
wordsBigNat v@(Vector off (I# len) (Prim.ByteArray buf)) =
    case VP.length v of
        0 -> zeroBigNat
        1 -> case VP.unsafeIndex v 0 of W# w -> wordToBigNat w
        n -> if off /= 0 then error "words2Nat: bad-vec" else
             byteArrayToBigNat# buf len

{-|
    More careful version of `wordsBigNat`, but not yet tested.

    Cast a vector to a BigNat. This will not copy unless input is a slice.

    Note that the length of the vector is in words, and the length passed
    to `byteArrayToBigNat#` is also in words.
-}
wordsBigNat' ∷ Vector Word → BigNat
wordsBigNat' v =
    case VP.length v of
        0 -> zeroBigNat
        1 -> wordToBigNat w where W# w = VP.unsafeIndex v 0
        n -> if offset v == 0 then extract v else extract (VP.force v)
  where
    offset (Vector off _ _) = off

    extract (Vector _ (I# len) (Prim.ByteArray buf)) =
        byteArrayToBigNat# buf len


--------------------------------------------------------------------------------

-- | Cast a nat to a vector (no copy)
natWords :: Natural → Vector Word
natWords = bigNatWords . natBigNat

-- | Cast a vector to a nat (no copy)
wordsNat ∷ Vector Word → Natural
wordsNat = bigNatNat . wordsBigNat

-- | Cast a Nat to a BigNat (no copy).
natBigNat ∷ Natural → BigNat
natBigNat (NatS# w)  = wordToBigNat w
natBigNat (NatJ# bn) = bn

-- | Cast a BigNat to a Nat (no copy).
bigNatNat ∷ BigNat → Natural
bigNatNat bn =
    case sizeofBigNat# bn of
        0# -> 0
        1# -> NatS# (bigNatToWord bn)
        _  -> NatJ# bn

--------------------------------------------------------------------------------

-- | TODO This assumes 64-bit words
wordBytes ∷ Word → ByteString
wordBytes wor =
    BS.reverse $ BS.pack $ go 0 []
  where
    go i acc | i >= 8    = acc
    go i acc | otherwise = go (i+1) (fromIntegral (shiftR wor (i*8)) : acc)

-- | TODO This assumes 64-bit words
bytesFirstWord ∷ ByteString → Word
bytesFirstWord buf = go 0 0
  where
    top        = min 8 (BS.length buf)
    i idx off  = shiftL (fromIntegral $ BS.index buf idx) off
    go acc idx = if idx >= top then acc else
                 go (acc .|. i idx (8*idx)) (idx+1)

--------------------------------------------------------------------------------

pillWords ∷ Pill → Vector Word
pillWords = bsToWords . pillBytes

wordsPill ∷ Vector Word → Pill
wordsPill = bytesPill . vecBytes . wordsToBytes

--------------------------------------------------------------------------------

wordsToBytes :: Vector Word -> Vector Word8
wordsToBytes (Vector off sz buf) =
    Vector (off*8) (sz*8) buf

bsToWords :: ByteString -> Vector Word
bsToWords bs =
    VP.generate (1 + BS.length bs `div` 8) $ \i ->
        bytesFirstWord (BS.drop (i*8) bs)

--------------------------------------------------------------------------------

vecBytes :: Vector Word8 -> ByteString
vecBytes (Vector off sz buf) =
    unsafePerformIO $ do
        fp <- BS.mallocByteString sz
        let Ptr a = Ptr.unsafeForeignPtrToPtr fp -- Safe b/c returning fp
        Prim.copyByteArrayToAddr (Prim.Addr a) buf 0 sz
        pure (BS.PS fp off sz)

bytesVec ∷ ByteString → Vector Word8
bytesVec bs = VP.generate (BS.length bs) (BS.index bs)

--------------------------------------------------------------------------------

natPill ∷ Natural → Pill
natPill = wordsPill . natWords

pillNat ∷ Pill → Natural
pillNat = wordsNat . bsToWords . pillBytes

--------------------------------------------------------------------------------

