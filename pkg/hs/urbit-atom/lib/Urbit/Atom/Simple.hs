{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Urbit.Integer
-- Copyright   :  (c) Ian Lynagh 2007-2012
-- License     :  BSD3
--
-- Maintainer  :  igloo@earth.li
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- An simple definition of the 'Integer' type.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module Urbit.Atom.Simple (
    Integer, mkInteger,
    smallInteger, wordToInteger, integerToWord, integerToInt,
#if WORD_SIZE_IN_BITS < 64
    integerToWord64, word64ToInteger,
    integerToInt64, int64ToInteger,
#endif
    plusInteger, minusInteger, timesInteger, negateInteger,
    eqInteger, neqInteger, absInteger, signumInteger,
    leInteger, gtInteger, ltInteger, geInteger, compareInteger,
    eqInteger#, neqInteger#,
    leInteger#, gtInteger#, ltInteger#, geInteger#,
    divInteger, modInteger,
    divModInteger, quotRemInteger, quotInteger, remInteger,
    encodeFloatInteger, decodeFloatInteger, floatFromInteger,
    encodeDoubleInteger, decodeDoubleInteger, doubleFromInteger,
    -- gcdInteger, lcmInteger, -- XXX
    andInteger, orInteger, xorInteger, complementInteger,
    shiftLInteger, shiftRInteger, testBitInteger,
    hashInteger,
 ) where

import Urbit.Atom.Simple.Type

