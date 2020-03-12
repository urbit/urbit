{-# LANGUAGE CPP, MagicHash, UnboxedTuples, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

#include "MachDeps.h"

-- (Hopefully) Fast integer logarithms to base 2.
-- integerLog2# and wordLog2# are of general usefulness,
-- the others are only needed for a fast implementation of
-- fromRational.
-- Since they are needed in Urbit.Float, we must expose this
-- module, but it should not show up in the docs.

module Urbit.Integer.Logarithms.Internals
    ( integerLog2#
    , integerLog2IsPowerOf2#
    , wordLog2#
    , roundingMode#
    ) where

import GHC.Prim
import Urbit.Integer.Type
import GHC.Types

default ()

-- When larger word sizes become common, add support for those,
-- it's not hard, just tedious.
#if (WORD_SIZE_IN_BITS != 32) && (WORD_SIZE_IN_BITS != 64)

-- We don't know whether the word has 30 bits or 128 or even more,
-- so we can't start from the top, although that would be much more
-- efficient.
wordLog2# :: Word# -> Int#
wordLog2# w = go 8# w
  where
    go acc u = case u `uncheckedShiftRL#` 8# of
                0## -> case leadingZeros of
                        BA ba -> acc -# indexInt8Array# ba (word2Int# u)
                v   -> go (acc +# 8#) v

#else

-- This one at least can also be done efficiently.
-- wordLog2# 0## = -1#
{-# INLINE wordLog2# #-}
wordLog2# :: Word# -> Int#
wordLog2# w =
  case leadingZeros of
   BA lz ->
    let zeros u = indexInt8Array# lz (word2Int# u) in
#if WORD_SIZE_IN_BITS == 64
    case uncheckedShiftRL# w 56# of
     a ->
      if isTrue# (a `neWord#` 0##)
       then 64# -# zeros a
       else
        case uncheckedShiftRL# w 48# of
         b ->
          if isTrue# (b `neWord#` 0##)
           then 56# -# zeros b
           else
            case uncheckedShiftRL# w 40# of
             c ->
              if isTrue# (c `neWord#` 0##)
               then 48# -# zeros c
               else
                case uncheckedShiftRL# w 32# of
                 d ->
                  if isTrue# (d `neWord#` 0##)
                   then 40# -# zeros d
                   else
#endif
                    case uncheckedShiftRL# w 24# of
                     e ->
                      if isTrue# (e `neWord#` 0##)
                       then 32# -# zeros e
                       else
                        case uncheckedShiftRL# w 16# of
                         f ->
                          if isTrue# (f `neWord#` 0##)
                           then 24# -# zeros f
                           else
                            case uncheckedShiftRL# w 8# of
                             g ->
                              if isTrue# (g `neWord#` 0##)
                               then 16# -# zeros g
                               else  8# -# zeros w

#endif

-- Assumption: Integer is strictly positive,
-- otherwise return -1# arbitrarily
-- Going up in word-sized steps should not be too bad.
integerLog2# :: Integer -> Int#
integerLog2# (Positive digits) = step 0# digits
  where
    step acc (Some dig None) = acc +# wordLog2# dig
    step acc (Some _ digs)   =
        step (acc +# WORD_SIZE_IN_BITS#) digs
    step acc None = acc     -- should be impossible, throw error?
integerLog2# _ = negateInt# 1#

-- Again, integer should be strictly positive
integerLog2IsPowerOf2# :: Integer -> (# Int#, Int# #)
integerLog2IsPowerOf2# (Positive digits) = couldBe 0# digits
  where
    couldBe acc (Some dig None) =
        (# acc +# wordLog2# dig, word2Int# (and# dig (minusWord# dig 1##)) #)
    couldBe acc (Some dig digs) =
        if isTrue# (eqWord# dig 0##)
           then couldBe (acc +# WORD_SIZE_IN_BITS#) digs
           else noPower (acc +# WORD_SIZE_IN_BITS#) digs
    couldBe acc None = (# acc, 1# #) -- should be impossible, error?
    noPower acc (Some dig None) =
        (# acc +# wordLog2# dig, 1# #)
    noPower acc (Some _ digs)   =
        noPower (acc +# WORD_SIZE_IN_BITS#) digs
    noPower acc None = (# acc, 1# #) -- should be impossible, error?
integerLog2IsPowerOf2# _ = (# negateInt# 1#, 1# #)

-- Assumption: Integer and Int# are strictly positive, Int# is less
-- than logBase 2 of Integer, otherwise havoc ensues.
-- Used only for the numerator in fromRational when the denominator
-- is a power of 2.
-- The Int# argument is log2 n minus the number of bits in the mantissa
-- of the target type, i.e. the index of the first non-integral bit in
-- the quotient.
--
-- 0# means round down (towards zero)
-- 1# means we have a half-integer, round to even
-- 2# means round up (away from zero)
-- This function should probably be improved.
roundingMode# :: Integer -> Int# -> Int#
roundingMode# m h =
    case oneInteger `shiftLInteger` h of
      c -> case m `andInteger`
                ((c `plusInteger` c) `minusInteger` oneInteger) of
             r ->
               if c `ltInteger` r
                 then 2#
                 else if c `gtInteger` r
                        then 0#
                        else 1#

-- Lookup table
data BA = BA ByteArray#

leadingZeros :: BA
leadingZeros =
    let mkArr s =
          case newByteArray# 256# s of
            (# s1, mba #) ->
              case writeInt8Array# mba 0# 9# s1 of
                s2 ->
                  let fillA lim val idx st =
                        if isTrue# (idx ==# 256#)
                          then st
                          else if isTrue# (idx <# lim)
                                then case writeInt8Array# mba idx val st of
                                        nx -> fillA lim val (idx +# 1#) nx
                                else fillA (2# *# lim) (val -# 1#) idx st
                  in case fillA 2# 8# 1# s2 of
                      s3 -> case unsafeFreezeByteArray# mba s3 of
                              (# _, ba #) -> ba
    in case mkArr realWorld# of
        b -> BA b
