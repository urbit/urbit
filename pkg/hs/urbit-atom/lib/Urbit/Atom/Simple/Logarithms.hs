{-# LANGUAGE MagicHash, UnboxedTuples, NoImplicitPrelude #-}
module Urbit.Atom.Simple.Logarithms
    ( integerLogBase#
    , integerLog2#
    , wordLog2#
    ) where

import GHC.Prim
import Urbit.Atom.Simple
import qualified Urbit.Atom.Simple.Logarithms.Internals as I

-- | Calculate the integer logarithm for an arbitrary base.
--   The base must be greater than 1, the second argument, the number
--   whose logarithm is sought, should be positive, otherwise the
--   result is meaningless.
--
-- > base ^ integerLogBase# base m <= m < base ^ (integerLogBase# base m + 1)
--
-- for @base > 1@ and @m > 0@.
integerLogBase# :: Atom -> Atom -> Int#
integerLogBase# b m = case step b of
                        (# _, e #) -> e
  where
    step pw =
      if m `ltAtom` pw
        then (# m, 0# #)
        else case step (pw `timesAtom` pw) of
               (# q, e #) ->
                 if q `ltAtom` pw
                   then (# q, 2# *# e #)
                   else (# q `quotAtom` pw, 2# *# e +# 1# #)

-- | Calculate the integer base 2 logarithm of an 'Atom'.
--   The calculation is more efficient than for the general case,
--   on platforms with 32- or 64-bit words much more efficient.
--
--  The argument must be strictly positive, that condition is /not/ checked.
integerLog2# :: Atom -> Int#
integerLog2# = I.integerLog2#

-- | This function calculates the integer base 2 logarithm of a 'Word#'.
wordLog2# :: Word# -> Int#
wordLog2# = I.wordLog2#
