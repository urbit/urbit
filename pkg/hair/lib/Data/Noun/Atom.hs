{-# LANGUAGE MagicHash, GeneralizedNewtypeDeriving, UnboxedTuples #-}

module Data.Noun.Atom where

import ClassyPrelude
import Prelude ((^))
import GHC.Integer.GMP.Internals
import GHC.Natural
import GHC.Prim
import GHC.Word

newtype Atom = Atom Natural
  deriving (Eq, Ord, Show, Num)

wordBitWidth :: Word# -> Word#
wordBitWidth w = minusWord# 64## (clz# w)

bigNatBitWidth :: BigNat -> Word#
bigNatBitWidth nat =
    lswBits `plusWord#` ((int2Word# lastIdx) `timesWord#` 64##)
  where
    (# lastIdx, _ #) = subIntC# (sizeofBigNat# nat) 1#
    lswBits          = wordBitWidth (indexBigNat# nat lastIdx)

bitWidth :: Atom -> Atom
bitWidth (Atom (NatS# gl)) = Atom (NatS# (wordBitWidth gl))
bitWidth (Atom (NatJ# bn)) = Atom (NatS# (bigNatBitWidth bn))
