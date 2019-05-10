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

--------------------------------------------------------------------------------

newtype Atom = Atom Natural
  deriving (Eq, Ord, Num, Bits, Enum, Real, Integral)

instance Show Atom where
  show (Atom a) = show a

data Cursor = Cursor
    { _cOffset :: {-# UNPACK #-} !Int
    , _cBuffer :: {-# UNPACK #-} !Atom
    }
  deriving (Eq, Ord, Show)

data Slice = Slice
    { _sOffset :: {-# UNPACK #-} !Int
    , _sWidth  :: {-# UNPACK #-} !Int
    , _sBuffer :: {-# UNPACK #-} !Atom
    }
  deriving (Eq, Ord, Show)

makeLenses ''Cursor
makeLenses ''Slice

--------------------------------------------------------------------------------

wordBitWidth :: Word# -> Word#
wordBitWidth w = minusWord# 64## (clz# w)

bigNatBitWidth :: BigNat -> Word#
bigNatBitWidth nat =
    lswBits `plusWord#` ((int2Word# lastIdx) `timesWord#` 64##)
  where
    (# lastIdx, _ #) = subIntC# (sizeofBigNat# nat) 1#
    lswBits          = wordBitWidth (indexBigNat# nat lastIdx)

bitWidth :: Atom -> Int
bitWidth (Atom (NatS# gl)) = I# (word2Int# (wordBitWidth gl))
bitWidth (Atom (NatJ# bn)) = I# (word2Int# (bigNatBitWidth bn))


--------------------------------------------------------------------------------

cursor :: Atom -> Atom -> Cursor
cursor offset buf = Cursor (fromIntegral offset) buf

fromCursor :: Cursor -> Atom
fromCursor (Cursor off buf) = shiftR buf off

bumpCursor :: Word -> Cursor -> Cursor
bumpCursor off = over cOffset (+ fromIntegral off)


--------------------------------------------------------------------------------

slice :: Atom -> Atom -> Atom -> Slice
slice offset size buf = Slice (fromIntegral offset) (fromIntegral size) buf

fromSlice :: Slice -> Atom
fromSlice (Slice off wid buf) = mask .&. (shiftR buf off)
  where mask = shiftL (Atom 1) wid - 1

--------------------------------------------------------------------------------

takeBits :: Atom -> Atom -> Atom
takeBits wid buf = mask .&. buf
  where mask = shiftL (Atom 1) (fromIntegral wid) - 1

bitIdx :: Int -> Atom -> Bool
bitIdx idx buf = testBit buf (fromIntegral idx)
