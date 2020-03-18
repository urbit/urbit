{-# LANGUAGE CPP, UnliftedFFITypes #-}

{-|
    Atom implementation with fast conversions between bytestrings
    and atoms.

    TODO Support Big Endian.
-}

module Urbit.Atom.Fast
  ( wordsAtom
  , bytesAtom
  , atomBytes
  , atomWords
  , exportBytes
  , importBytes
  , wordBitWidth#
  , wordBitWidth
  , atomBitWidth#
  , atomBitWidth
  , takeBitsWord
  , bigNatWords
  , wordsBigNat
  , bit
  , byt
  )
where

import Prelude

import Control.Monad.Primitive   (primitive_)
import Data.Bits                 (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString           (ByteString)
import Data.Vector.Primitive     (Vector(..))
import Data.Word                 (Word8)
import GHC.Exts                  (Ptr(Ptr), sizeofByteArray#)
import GHC.Exts                  (Int(..))
import GHC.Integer.GMP.Internals (BigNat(..))
import GHC.Natural               (Natural(..))
import GHC.Prim                  (Int#, clz#, minusWord#, plusWord#)
import GHC.Prim                  (Word#, Addr#, int2Word#, timesWord#)
import GHC.Prim                  (copyByteArrayToAddr#)
import GHC.Word                  (Word(..))
import System.IO.Unsafe          (unsafePerformIO)

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Internal  as BS
import qualified Data.ByteString.Unsafe    as BS
import qualified Data.Primitive.ByteArray  as Prim
import qualified Data.Vector.Primitive     as VP
import qualified Foreign.ForeignPtr        as Ptr
import qualified Foreign.ForeignPtr.Unsafe as Ptr
import qualified GHC.Integer.GMP.Internals as G


-- Setup BIT and BYT macros. ---------------------------------------------------

#include <MachDeps.h>

#if WORD_SIZE_IN_BITS == 64
#define BIT 64
#define BYT 8
#elif WORD_SIZE_IN_BITS == 32
#define BIT 32
#define BYT 4
#else
#error WORD_SIZE_IN_BITS must be either 32 or 64
#endif

bit :: Word
bit = BIT

byt :: Word
byt = BYT


--------------------------------------------------------------------------------

wordBitWidth# :: Word# -> Word#
wordBitWidth# w = minusWord# BIT## (clz# w)

wordBitWidth :: Word -> Word
wordBitWidth (W# w) = W# (wordBitWidth# w)

bigNatBitWidth# :: BigNat -> Word#
bigNatBitWidth# nat =
  lswBits `plusWord#` ((int2Word# lastIdx) `timesWord#` BIT##)
 where
  I# lastIdx = (I# (G.sizeofBigNat# nat)) - 1
  lswBits    = wordBitWidth# (G.indexBigNat# nat lastIdx)

atomBitWidth# :: Natural -> Word#
atomBitWidth# (NatS# gl) = wordBitWidth# gl
atomBitWidth# (NatJ# bn) = bigNatBitWidth# bn

atomBitWidth :: Num a => Natural -> a
atomBitWidth a = fromIntegral (W# (atomBitWidth# a))

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

strip :: ByteString -> ByteString
strip buf = BS.take (len - go 0 (len - 1)) buf
 where
  len = BS.length buf
  go n i | i < 0                     = n
         | 0 == BS.unsafeIndex buf i = go (n + 1) (i - 1)
         | otherwise                 = n

pillBytes :: Pill -> ByteString
pillBytes = strip . unPill

bytesPill :: ByteString -> Pill
bytesPill = Pill . strip


--------------------------------------------------------------------------------

{-
    Cast a BigNat to a vector without a copy.
-}
bigNatWords :: BigNat -> Vector Word
bigNatWords bn | G.isZeroBigNat bn = mempty
bigNatWords bn@(BN# bArr) =
  Vector 0 (I# (sizeofByteArray# bArr) `div` BYT) (Prim.ByteArray bArr)

{-|
    Cast a vector to a BigNat. This will not copy.

    TODO Don't crash if given a slice.
-}
wordsBigNat :: Vector Word -> BigNat
wordsBigNat v@(Vector off (I# len) (Prim.ByteArray buf)) = case VP.length v of
  0 -> G.zeroBigNat
  1 -> case VP.unsafeIndex v 0 of
    W# w -> G.wordToBigNat w
  n -> if off /= 0
    then error "words2Nat: bad-vec"
    else G.byteArrayToBigNat# buf len

{-|
    More careful version of `wordsBigNat`, but not yet tested.

    Cast a vector to a BigNat. This will not copy unless input is a slice.

    Note that the length of the vector is in words, and the length passed
    to `byteArrayToBigNat#` is also in words.
-}
_wordsBigNat :: Vector Word -> BigNat
_wordsBigNat v = case VP.length v of
  0 -> G.zeroBigNat
  1 -> G.wordToBigNat w where W# w = VP.unsafeIndex v 0
  n -> if offset v == 0 then extract v else extract (VP.force v)
 where
  offset (Vector off _ _) = off

  extract (Vector _ (I# len) (Prim.ByteArray buf)) =
    G.byteArrayToBigNat# buf len


--------------------------------------------------------------------------------

-- | Cast a nat to a vector (no copy)
atomWords :: Natural -> Vector Word
atomWords = bigNatWords . natBigNat

-- | Cast a vector to a nat (no copy)
wordsAtom :: Vector Word -> Natural
wordsAtom = bigNatNat . wordsBigNat

-- | Cast a Nat to a BigNat (no copy).
natBigNat :: Natural -> BigNat
natBigNat (NatS# w ) = G.wordToBigNat w
natBigNat (NatJ# bn) = bn

-- | Cast a BigNat to a Nat (no copy).
bigNatNat :: BigNat -> Natural
bigNatNat bn = case G.sizeofBigNat# bn of
  0# -> 0
  1# -> NatS# (G.bigNatToWord bn)
  _  -> NatJ# bn


--------------------------------------------------------------------------------

_wordBytes :: Word -> ByteString
_wordBytes wor = BS.reverse $ BS.pack $ go 0 []
 where
  go i acc | i >= BYT  = acc
  go i acc | otherwise = go (i + 1) (fromIntegral (shiftR wor (i * BYT)) : acc)

bytesFirstWord :: ByteString -> Word
bytesFirstWord buf = go 0 0
 where
  top = min BYT (BS.length buf)
  i idx off = shiftL (fromIntegral $ BS.index buf idx) off
  go acc idx =
    if idx >= top then acc else go (acc .|. i idx (BYT * idx)) (idx + 1)


--------------------------------------------------------------------------------

_pillWords :: Pill -> Vector Word
_pillWords = bsToWords . pillBytes

wordsPill :: Vector Word -> Pill
wordsPill = bytesPill . vecBytes . wordsToBytes


--------------------------------------------------------------------------------

wordsToBytes :: Vector Word -> Vector Word8
wordsToBytes (Vector off sz buf) = Vector (off * BYT) (sz * BYT) buf

bsToWords :: ByteString -> Vector Word
bsToWords bs = VP.generate (1 + BS.length bs `div` BYT)
  $ \i -> bytesFirstWord (BS.drop (i * BYT) bs)


--------------------------------------------------------------------------------

vecBytes :: Vector Word8 -> ByteString
vecBytes (Vector off sz buf) = unsafePerformIO $ do
  fp <- BS.mallocByteString sz
  let Ptr a = Ptr.unsafeForeignPtrToPtr fp -- Safe b/c returning fp
  copyByteArrayToAddr a buf 0 sz
  pure (BS.PS fp off sz)
 where
  unI# :: Int -> Int#
  unI# (I# n#) = n#

  --  Hack to get GHCJS build working, since it has an old version of the
  --  `primitive` library.
  copyByteArrayToAddr dst# (Prim.ByteArray src#) soff sz =
    primitive_ (copyByteArrayToAddr# src# (unI# soff) dst# (unI# sz))

_bytesVec :: ByteString -> Vector Word8
_bytesVec bs = VP.generate (BS.length bs) (BS.index bs)


--------------------------------------------------------------------------------

natPill :: Natural -> Pill
natPill = wordsPill . atomWords

pillAtom :: Pill -> Natural
pillAtom = wordsAtom . bsToWords . pillBytes

-- | Dump an atom to a bytestring.
atomBytes :: Natural -> ByteString
atomBytes = pillBytes . natPill

-- | Load a bytestring into an atom.
bytesAtom :: ByteString -> Natural
bytesAtom = pillAtom . bytesPill

-- Try using GMPs `input/export` feature. --------------------------------------

-- sizeInBaseInteger i 256#
-- sz = sizeInBaseNatural wor 256#
-- exportBigNatToAddr :: BigNat -> Addr# -> Int# -> IO Word Source#
-- exportWordToAddr :: Word -> Addr# -> Int# -> IO Word

-- Use "sizeInBaseInteger i 256#" to compute the exact number
-- of bytes written in advance for i /= 0. In case of i == 0,
-- exportIntegerToMutableByteArray will write and report zero bytes written,
-- whereas sizeInBaseInteger report one byte.

sizeInBaseNatural :: Natural -> Int# -> Word#
{-# INLINE sizeInBaseNatural #-}
sizeInBaseNatural (NatS# w) base = G.sizeInBaseWord# w base
sizeInBaseNatural (NatJ# n) base = G.sizeInBaseBigNat n base

exportNaturalToAddr :: Natural -> Addr# -> Int# -> IO Word
exportNaturalToAddr (NatS# w) = G.exportWordToAddr (W# w)
exportNaturalToAddr (NatJ# n) = G.exportBigNatToAddr n

exportNaturalToByteString :: Natural -> Int -> ByteString
exportNaturalToByteString nat (I# i#) = unsafePerformIO $ do
  let sz# = sizeInBaseNatural nat 256#
  let szi = fromIntegral (W# sz#)
  fp <- BS.mallocByteString szi
  let Ptr a = Ptr.unsafeForeignPtrToPtr fp
  exportNaturalToAddr nat a i#
  pure (BS.PS fp 0 szi)

exportBytes :: Natural -> ByteString
exportBytes 0 = mempty
exportBytes n = exportNaturalToByteString n 0

bigNatNatural :: BigNat -> Natural
bigNatNatural big =
  case G.sizeofBigNat# big of
    0# -> 0
    1# -> NatS# (G.bigNatToWord big)
    _  -> NatJ# big

stripBytes :: ByteString -> ByteString
stripBytes buf = BS.take (len - go 0 (len - 1)) buf
 where
  len = BS.length buf
  go n i | i < 0                     = n
         | 0 == BS.unsafeIndex buf i = go (n + 1) (i - 1)
         | otherwise                 = n


importBytes :: ByteString -> Natural
importBytes = go . stripBytes
 where
  go (BS.PS fp 0 sz) = unsafePerformIO $ do
    let Ptr a  = Ptr.unsafeForeignPtrToPtr fp -- TODO Not safe!
    let W# sz# = fromIntegral sz
    res <- bigNatNatural <$> G.importBigNatFromAddr a sz# 0#
    Ptr.touchForeignPtr fp
    pure res

  -- TODO Avoid this extra copy when given a slice. Should be able to
  -- just offset the raw pointer.
  go bs = importBytes (BS.copy bs)
