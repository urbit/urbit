{-# LANGUAGE MagicHash, GeneralizedNewtypeDeriving, UnboxedTuples #-}

module Data.Noun.Atom where

import ClassyPrelude
import Control.Lens
-- import Prelude ((^))
import GHC.Integer.GMP.Internals
import GHC.Natural
import GHC.Prim
import GHC.Word
import GHC.Int
import Data.Bits
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Text.Printf
import Data.Flat

import Data.Hashable (Hashable)

--------------------------------------------------------------------------------

newtype Atom = MkAtom { unAtom :: Natural }
  deriving newtype (Eq, Ord, Num, Bits, Enum, Real, Integral, Flat, Hashable, NFData)

instance Show Atom where
  show (MkAtom a) = show a

{-
  An Atom with a bit-offset.
-}
data Cursor = Cursor
    { _cOffset :: {-# UNPACK #-} !Int
    , _cBuffer :: !Atom
    }
  deriving (Eq, Ord, Show)

data Slice = Slice
    { _sOffset :: {-# UNPACK #-} !Int
    , _sWidth  :: {-# UNPACK #-} !Int
    , _sBuffer :: !Atom
    }
  deriving (Eq, Ord, Show)

makeLenses ''Cursor
makeLenses ''Slice


-- Instances -------------------------------------------------------------------

instance Arbitrary Natural where
  arbitrary = fromInteger . abs <$> arbitrary

instance Arbitrary Atom where
  arbitrary = do
    arbitrary >>= \case
      False -> MkAtom <$> arbitrary
      True  -> arbitrary <&> ((`mod` 16) . MkAtom)

-- Conversion ------------------------------------------------------------------

class IsAtom a where
  toAtom   :: a -> Atom
  fromAtom :: Atom -> a

instance IsAtom Atom where
  toAtom   = id
  fromAtom = id

instance IsAtom Natural where
  toAtom              = MkAtom
  fromAtom (MkAtom a) = a

instance IsAtom Word8 where
  toAtom   = fromIntegral
  fromAtom = fromIntegral

instance IsAtom Word16 where
  toAtom   = fromIntegral
  fromAtom = fromIntegral

instance IsAtom Word32 where
  toAtom   = fromIntegral
  fromAtom = fromIntegral

instance IsAtom Word64 where
  toAtom   = fromIntegral
  fromAtom = fromIntegral

instance IsAtom Word where
  toAtom   = fromIntegral
  fromAtom = fromIntegral

instance IsAtom Int where
  toAtom   = fromIntegral
  fromAtom = fromIntegral

instance IsAtom Integer where
  toAtom   = fromIntegral
  fromAtom = fromIntegral


--------------------------------------------------------------------------------

{-
  TODO Support 32-bit archetectures.
-}

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
atomBitWidth# (MkAtom (NatS# gl)) = wordBitWidth# gl
atomBitWidth# (MkAtom (NatJ# bn)) = bigNatBitWidth# bn

bitWidth :: Num a => Atom -> a
bitWidth a = fromIntegral (W# (atomBitWidth# a))

--------------------------------------------------------------------------------

cursor :: Atom -> Atom -> Cursor
cursor offset buf = Cursor (fromIntegral offset) buf

fromCursor :: Cursor -> Atom
fromCursor (Cursor off buf) = shiftR buf off

bumpCursor :: Word -> Cursor -> Cursor
bumpCursor off = over cOffset (+ fromIntegral off)

instance IsAtom Cursor where
  toAtom (Cursor off bits) = shiftR bits off
  fromAtom = Cursor 0


--------------------------------------------------------------------------------

{-# INLINE slice #-}
slice :: (Atom, Atom) -> Atom -> Atom
slice (offset, size) buf =
  fromSlice (Slice (fromAtom offset) (fromAtom size) buf)

{-# INLINE fromSlice #-}
fromSlice :: Slice -> Atom
fromSlice (Slice off wid buf) = takeBits wid (shiftR buf off)

--------------------------------------------------------------------------------

{-# INLINE takeBits #-}
takeBits :: Int -> Atom -> Atom
takeBits wid buf = buf .&. (shiftL (MkAtom 1) wid - 1)

{-# INLINE takeBitsWord #-}
takeBitsWord :: Int -> Word -> Word
takeBitsWord wid wor = wor .&. (shiftL 1 wid - 1)

{-# INLINE bitIdx #-}
bitIdx :: Int -> Atom -> Bool
bitIdx idx buf = testBit buf idx

{-# INLINE bitConcat #-}
bitConcat :: Atom -> Atom -> Atom
bitConcat x y = x .|. shiftL y (bitWidth x)


-- Bit Buffers -----------------------------------------------------------------

data Buf = Buf !Int !Atom

instance Show Buf where
  show (Buf sz bits) = "0b"
                    <> replicate (sz - bitWidth bits) '0'
                    <> printf "%b (%d bits)" (toInteger bits) sz

instance Semigroup Buf where
  Buf xSz xBuf <> Buf ySz yBuf = Buf (xSz+ySz) (xBuf .|. shiftL yBuf xSz)

instance Monoid Buf where
  mempty = Buf 0 0

instance IsAtom Buf where
  toAtom (Buf _ bits) = bits
  fromAtom bits = Buf (bitWidth bits) bits
