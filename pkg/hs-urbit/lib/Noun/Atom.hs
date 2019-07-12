{-
  TODO Support 32-bit archetectures.
  TODO Support Big Endian.
-}

{-# OPTIONS_GHC -Werror #-}

module Noun.Atom
    ( Atom(..)
    , atomBitWidth#, wordBitWidth#, wordBitWidth
    , takeBitsWord, bitWidth
    , atomBytes, bigNatWords, atomWords
    ) where

import ClassyPrelude
import Control.Lens hiding (Index)

import Data.Bits                 (shiftL, shiftR, (.&.), (.|.))
import GHC.Exts                  (sizeofByteArray#)
import GHC.Integer.GMP.Internals (BigNat(..), bigNatToWord, sizeofBigNat#)
import GHC.Integer.GMP.Internals (indexBigNat#)
import GHC.Integer.GMP.Internals (wordToBigNat, byteArrayToBigNat#, zeroBigNat)
import GHC.Int                   (Int(..))
import GHC.Natural               (Natural(..))
import GHC.Prim                  (plusWord#, clz#, minusWord#)
import GHC.Prim                  (Word#, subIntC#, timesWord#, int2Word#)
import GHC.Word                  (Word(..))
import System.IO.Unsafe          (unsafePerformIO)

import qualified Data.Primitive.Types     as Prim
import qualified Data.Primitive.ByteArray as Prim
import qualified Data.Vector.Primitive    as VP
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Unsafe   as BU


-- Types -----------------------------------------------------------------------

type Atom = Natural


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

atomBitWidth# :: Atom -> Word#
atomBitWidth# (NatS# gl) = wordBitWidth# gl
atomBitWidth# (NatJ# bn) = bigNatBitWidth# bn

bitWidth :: Num a => Atom -> a
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
  (==) x y = (x ^. pillBS) == (y ^. pillBS)

instance Show Pill where
  show = show . view pillBS

--------------------------------------------------------------------------------

strip :: (IsSequence seq, Int ~ Index seq, Eq (Element seq), Num (Element seq))
      => seq -> seq
strip buf = take (len - go 0 (len - 1)) buf
  where
    len = length buf
    go n i | i < 0                  = n
           | 0 == unsafeIndex buf i = go (n+1) (i-1)
           | otherwise              = n

pillBS :: Iso' Pill ByteString
pillBS = iso to from
  where
    to :: Pill -> ByteString
    to = strip . unPill

    from :: ByteString -> Pill
    from = Pill . strip


--------------------------------------------------------------------------------

bigNatWords :: Iso' BigNat (VP.Vector Word)
bigNatWords = iso to from
  where
    to (BN# bArr) = VP.Vector 0 (I# (sizeofByteArray# bArr) `div` 8)
                                (Prim.ByteArray bArr)

    from v@(VP.Vector off (I# len) (Prim.ByteArray buf)) =
      case VP.length v of
        0 -> zeroBigNat
        1 -> wordToBigNat (case VP.unsafeIndex v 0 of W# w -> w)
        n -> if off /= 0 then error "words2Nat: bad-vec" else
             byteArrayToBigNat# buf len

--------------------------------------------------------------------------------

natWords :: Iso' Natural (VP.Vector Word)
natWords = naturalBigNat . bigNatWords

naturalBigNat :: Iso' Natural BigNat
naturalBigNat = iso to from
  where
    to = \case NatS# w  -> wordToBigNat w
               NatJ# bn -> bn

    from bn = case sizeofBigNat# bn of 0# -> 0
                                       1# -> NatS# (bigNatToWord bn)
                                       _  -> NatJ# bn

--------------------------------------------------------------------------------

-- TODO This assumes 64-bit words
packedWord :: Iso' ByteString Word
packedWord = iso to from
 where
    from wor = reverse $ fromList $ go 0 []
      where
        go i acc | i >= 8    = acc
        go i acc | otherwise = go (i+1) (fromIntegral (shiftR wor (i*8)) : acc)

    to buf = go 0 0
      where
        top        = min 8 (length buf)
        i idx off  = shiftL (fromIntegral $ BS.index buf idx) off
        go acc idx = if idx >= top then acc else
                     go (acc .|. i idx (8*idx)) (idx+1)

--------------------------------------------------------------------------------

wordsToBytes :: VP.Vector Word -> VP.Vector Word8
wordsToBytes (VP.Vector off sz buf) =
  VP.Vector (off*8) (sz*8) buf

bsToWords :: ByteString -> VP.Vector Word
bsToWords bs =
  VP.generate (1 + length bs `div` 8) $ \i ->
    view packedWord (BS.drop (i*8) bs)

{-
    TODO Support Big-Endian
    TODO This still has a (small) risk of segfaulting. The right thing to
         do is to manually copy the data to the C heap, setup the
         finalizers, and then manually construct a bytestring from
         that pointer.  -- finalizers, and make a bytestring from that.
-}
bytesBS :: Iso' (VP.Vector Word8) ByteString
bytesBS = iso to from
  where
    to :: VP.Vector Word8 -> ByteString
    to (VP.Vector off sz buf) =
        unsafePerformIO $ do
          Prim.Addr ptr <- evaluate $ Prim.byteArrayContents buf
          bs <- BU.unsafePackAddressLen sz ptr
          evaluate $ force $ BS.copy $ BS.drop off bs

    from :: ByteString -> VP.Vector Word8
    from bs = VP.generate (length bs) (BS.index bs)

pillWords :: Iso' Pill (VP.Vector Word)
pillWords = iso toVec fromVec
  where
    toVec   = view (pillBS . to bsToWords)
    fromVec = view (to wordsToBytes . bytesBS . from pillBS)

--------------------------------------------------------------------------------

atomWords :: Iso' Atom (VP.Vector Word)
atomWords = natWords

pill :: Iso' Atom Pill
pill = iso toAtom fromPill
  where
    toAtom   = view (natWords . from pillWords)
    fromPill = view (pillBS . to bsToWords . from natWords)

atomBytes :: Iso' Atom ByteString
atomBytes = pill . pillBS
