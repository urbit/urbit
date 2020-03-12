{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Urbit.Atom.Simple
-- Copyright   :  (c) Ian Lynagh 2007-2012
-- License     :  BSD3
--
-- Maintainer  :  igloo@earth.li
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- An simple definition of the 'Atom' type.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module Urbit.Atom.Simple (
    Atom, mkAtom,
    smallAtom, wordToAtom, integerToWord, integerToInt,
#if WORD_SIZE_IN_BITS < 64
    integerToWord64, word64ToAtom,
    integerToInt64, int64ToAtom,
#endif
    plusAtom, minusAtom, timesAtom, negateAtom,
    eqAtom, neqAtom, absAtom, signumAtom,
    leAtom, gtAtom, ltAtom, geAtom, compareAtom,
    eqAtom#, neqAtom#,
    leAtom#, gtAtom#, ltAtom#, geAtom#,
    divAtom, modAtom,
    divModAtom, quotRemAtom, quotAtom, remAtom,
    encodeFloatAtom, decodeFloatAtom, floatFromAtom,
    encodeDoubleAtom, decodeDoubleAtom, doubleFromAtom,
    -- gcdAtom, lcmAtom, -- XXX
    andAtom, orAtom, xorAtom, complementAtom,
    shiftLAtom, shiftRAtom, testBitAtom,
    hashAtom,
 ) where

import Urbit.Atom.Simple.Type

