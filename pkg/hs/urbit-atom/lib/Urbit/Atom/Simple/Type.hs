
{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude, BangPatterns, UnboxedTuples,
             UnliftedFFITypes #-}

-- Commentary of Integer library is located on the wiki:
-- http://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/Integer
--
-- It gives an in-depth description of implementation details and
-- decisions.

-----------------------------------------------------------------------------
-- |
-- Module      :  Urbit.Integer.Type
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

module Urbit.Atom.Simple.Type where

import GHC.Prim
import GHC.Classes
import GHC.Types
import GHC.Tuple ()
#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

data Atom = Positive !Positive | Negative !Positive | Naught

-------------------------------------------------------------------
-- The hard work is done on positive numbers

-- Least significant bit is first

-- Positive's have the property that they contain at least one Bit,
-- and their last Bit is One.
type Positive = Digits
type Positives = List Positive

data Digits = Some !Digit !Digits
            | None
type Digit = Word#

-- XXX Could move [] above us
data List a = Nil | Cons a (List a)

mkAtom :: Bool   -- non-negative?
          -> [Int]  -- absolute value in 31 bit chunks, least significant first
                    -- ideally these would be Words rather than Ints, but
                    -- we don't have Word available at the moment.
          -> Atom
mkAtom nonNegative is = let abs = f is
                           in if nonNegative then abs else negateAtom abs
    where f [] = Naught
          f (I# i : is') = smallAtom i `orAtom` shiftLAtom (f is') 31#

errorAtom :: Atom
errorAtom = Positive errorPositive

errorPositive :: Positive
errorPositive = Some 47## None -- Random number

{-# NOINLINE smallAtom #-}
smallAtom :: Int# -> Atom
smallAtom i = if isTrue# (i >=# 0#) then wordToAtom (int2Word# i)
                 else -- XXX is this right for -minBound?
                      negateAtom (wordToAtom (int2Word# (negateInt# i)))

{-# NOINLINE wordToAtom #-}
wordToAtom :: Word# -> Atom
wordToAtom w = if isTrue# (w `eqWord#` 0##)
                  then Naught
                  else Positive (Some w None)

{-# NOINLINE integerToWord #-}
integerToWord :: Atom -> Word#
integerToWord (Positive (Some w _)) = w
integerToWord (Negative (Some w _)) = 0## `minusWord#` w
-- Must be Naught by the invariant:
integerToWord _ = 0##

{-# NOINLINE integerToInt #-}
integerToInt :: Atom -> Int#
integerToInt i = word2Int# (integerToWord i)

#if WORD_SIZE_IN_BITS == 64
-- Nothing
#elif WORD_SIZE_IN_BITS == 32
{-# NOINLINE integerToWord64 #-}
integerToWord64 :: Atom -> Word64#
integerToWord64 i = int64ToWord64# (integerToInt64 i)

{-# NOINLINE word64ToAtom #-}
word64ToAtom:: Word64# -> Atom
word64ToAtom w = if isTrue# (w `eqWord64#` wordToWord64# 0##)
                    then Naught
                    else Positive (word64ToPositive w)

{-# NOINLINE integerToInt64 #-}
integerToInt64 :: Atom -> Int64#
integerToInt64 Naught = intToInt64# 0#
integerToInt64 (Positive p) = word64ToInt64# (positiveToWord64 p)
integerToInt64 (Negative p)
    = negateInt64# (word64ToInt64# (positiveToWord64 p))

{-# NOINLINE int64ToAtom #-}
int64ToAtom :: Int64# -> Atom
int64ToAtom i
 = if isTrue# (i `eqInt64#` intToInt64# 0#)
   then Naught
   else if isTrue# (i `gtInt64#` intToInt64# 0#)
   then Positive (word64ToPositive (int64ToWord64# i))
   else Negative (word64ToPositive (int64ToWord64# (negateInt64# i)))
#else
#error WORD_SIZE_IN_BITS not supported
#endif

oneAtom :: Atom
oneAtom = Positive onePositive

negativeOneAtom :: Atom
negativeOneAtom = Negative onePositive

twoToTheThirtytwoAtom :: Atom
twoToTheThirtytwoAtom = Positive twoToTheThirtytwoPositive

{-# NOINLINE encodeDoubleAtom #-}
encodeDoubleAtom :: Atom -> Int# -> Double#
encodeDoubleAtom (Positive ds0) e0 = f 0.0## ds0 e0
    where f !acc None        (!_) = acc
          f !acc (Some d ds) !e   = f (acc +## encodeDouble# d e)
                                      ds
                                      -- XXX We assume that this adding to e
                                      -- isn't going to overflow
                                      (e +# WORD_SIZE_IN_BITS#)
encodeDoubleAtom (Negative ds) e
    = negateDouble# (encodeDoubleAtom (Positive ds) e)
encodeDoubleAtom Naught _ = 0.0##

foreign import ccall unsafe "__word_encodeDouble"
        encodeDouble# :: Word# -> Int# -> Double#

{-# NOINLINE encodeFloatAtom #-}
encodeFloatAtom :: Atom -> Int# -> Float#
encodeFloatAtom (Positive ds0) e0 = f 0.0# ds0 e0
    where f !acc None        (!_) = acc
          f !acc (Some d ds) !e   = f (acc `plusFloat#` encodeFloat# d e)
                                      ds
                                      -- XXX We assume that this adding to e
                                      -- isn't going to overflow
                                      (e +# WORD_SIZE_IN_BITS#)
encodeFloatAtom (Negative ds) e
    = negateFloat# (encodeFloatAtom (Positive ds) e)
encodeFloatAtom Naught _ = 0.0#

foreign import ccall unsafe "__word_encodeFloat"
    encodeFloat# :: Word# -> Int# -> Float#

{-# NOINLINE decodeFloatAtom #-}
decodeFloatAtom :: Float# -> (# Atom, Int# #)
decodeFloatAtom f = case decodeFloat_Int# f of
                       (# mant, exp #) -> (# smallAtom mant, exp #)

-- XXX This could be optimised better, by either (word-size dependent)
-- using single 64bit value for the mantissa, or doing the multiplication
-- by just building the Digits directly
{-# NOINLINE decodeDoubleAtom #-}
decodeDoubleAtom :: Double# -> (# Atom, Int# #)
decodeDoubleAtom d
 = case decodeDouble_2Int# d of
   (# mantSign, mantHigh, mantLow, exp #) ->
       (# (smallAtom mantSign) `timesAtom`
          (  (wordToAtom mantHigh `timesAtom` twoToTheThirtytwoAtom)
             `plusAtom` wordToAtom mantLow),
          exp #)

{-# NOINLINE doubleFromAtom #-}
doubleFromAtom :: Atom -> Double#
doubleFromAtom Naught = 0.0##
doubleFromAtom (Positive p) = doubleFromPositive p
doubleFromAtom (Negative p) = negateDouble# (doubleFromPositive p)

{-# NOINLINE floatFromAtom #-}
floatFromAtom :: Atom -> Float#
floatFromAtom Naught = 0.0#
floatFromAtom (Positive p) = floatFromPositive p
floatFromAtom (Negative p) = negateFloat# (floatFromPositive p)

{-# NOINLINE andAtom #-}
andAtom :: Atom -> Atom -> Atom
Naught     `andAtom` (!_)       = Naught
(!_)       `andAtom` Naught     = Naught
Positive x `andAtom` Positive y = digitsToAtom (x `andDigits` y)
{-
To calculate x & -y we need to calculate
    x & twosComplement y
The (imaginary) sign bits are 0 and 1, so &ing them give 0, i.e. positive.
Note that
    twosComplement y
has infinitely many 1s, but x has a finite number of digits, so andDigits
will return a finite result.
-}
Positive x `andAtom` Negative y = let y' = twosComplementPositive y
                                      z = y' `andDigitsOnes` x
                                  in digitsToAtom z
Negative x `andAtom` Positive y = Positive y `andAtom` Negative x
{-
To calculate -x & -y, naively we need to calculate
    twosComplement (twosComplement x & twosComplement y)
but
    twosComplement x & twosComplement y
has infinitely many 1s, so this won't work. Thus we use de Morgan's law
to get
    -x & -y = !(!(-x) | !(-y))
            = !(!(twosComplement x) | !(twosComplement y))
            = !(!(!x + 1) | (!y + 1))
            = !((x - 1) | (y - 1))
but the result is negative, so we need to take the two's complement of
this in order to get the magnitude of the result.
    twosComplement !((x - 1) | (y - 1))
            = !(!((x - 1) | (y - 1))) + 1
            = ((x - 1) | (y - 1)) + 1
-}
-- We don't know that x and y are /strictly/ greater than 1, but
-- minusPositive gives us the required answer anyway.
Negative x `andAtom` Negative y = let x' = x `minusPositive` onePositive
                                      y' = y `minusPositive` onePositive
                                      z = x' `orDigits` y'
                                      -- XXX Cheating the precondition:
                                      z' = succPositive z
                                  in digitsToNegativeAtom z'

{-# NOINLINE orAtom #-}
orAtom :: Atom -> Atom -> Atom
Naught     `orAtom` (!i)       = i
(!i)       `orAtom` Naught     = i
Positive x `orAtom` Positive y = Positive (x `orDigits` y)
{-
x | -y = - (twosComplement (x | twosComplement y))
       = - (twosComplement !(!x & !(twosComplement y)))
       = - (twosComplement !(!x & !(!y + 1)))
       = - (twosComplement !(!x & (y - 1)))
       = - ((!x & (y - 1)) + 1)
-}
Positive x `orAtom` Negative y = let x' = flipBits x
                                     y' = y `minusPositive` onePositive
                                     z = x' `andDigitsOnes` y'
                                     z' = succPositive z
                                 in digitsToNegativeAtom z'
Negative x `orAtom` Positive y = Positive y `orAtom` Negative x
{-
-x | -y = - (twosComplement (twosComplement x | twosComplement y))
        = - (twosComplement !(!(twosComplement x) & !(twosComplement y)))
        = - (twosComplement !(!(!x + 1) & !(!y + 1)))
        = - (twosComplement !((x - 1) & (y - 1)))
        = - (((x - 1) & (y - 1)) + 1)
-}
Negative x `orAtom` Negative y = let x' = x `minusPositive` onePositive
                                     y' = y `minusPositive` onePositive
                                     z = x' `andDigits` y'
                                     z' = succPositive z
                                 in digitsToNegativeAtom z'

{-# NOINLINE xorAtom #-}
xorAtom :: Atom -> Atom -> Atom
Naught     `xorAtom` (!i)       = i
(!i)       `xorAtom` Naught     = i
Positive x `xorAtom` Positive y = digitsToAtom (x `xorDigits` y)
{-
x ^ -y = - (twosComplement (x ^ twosComplement y))
       = - (twosComplement !(x ^ !(twosComplement y)))
       = - (twosComplement !(x ^ !(!y + 1)))
       = - (twosComplement !(x ^ (y - 1)))
       = - ((x ^ (y - 1)) + 1)
-}
Positive x `xorAtom` Negative y = let y' = y `minusPositive` onePositive
                                      z = x `xorDigits` y'
                                      z' = succPositive z
                                  in digitsToNegativeAtom z'
Negative x `xorAtom` Positive y = Positive y `xorAtom` Negative x
{-
-x ^ -y = twosComplement x ^ twosComplement y
        = (!x + 1) ^ (!y + 1)
        = (!x + 1) ^ (!y + 1)
        = !(!x + 1) ^ !(!y + 1)
        = (x - 1) ^ (y - 1)
-}
Negative x `xorAtom` Negative y = let x' = x `minusPositive` onePositive
                                      y' = y `minusPositive` onePositive
                                      z = x' `xorDigits` y'
                                  in digitsToAtom z

{-# NOINLINE complementAtom #-}
complementAtom :: Atom -> Atom
complementAtom x = negativeOneAtom `minusAtom` x

{-# NOINLINE shiftLAtom #-}
shiftLAtom :: Atom -> Int# -> Atom
shiftLAtom (Positive p) i = Positive (shiftLPositive p i)
shiftLAtom (Negative n) i = Negative (shiftLPositive n i)
shiftLAtom Naught       _ = Naught

{-# NOINLINE shiftRAtom #-}
shiftRAtom :: Atom -> Int# -> Atom
shiftRAtom (Positive p)   i = shiftRPositive p i
shiftRAtom j@(Negative _) i
    = complementAtom (shiftRAtom (complementAtom j) i)
shiftRAtom Naught         _ = Naught

-- XXX this could be a lot more efficient, but this is a quick
-- reimplementation of the default Data.Bits instance, so that we can
-- implement the Atom interface
testBitAtom :: Atom -> Int# -> Bool
testBitAtom x i = (x `andAtom` (oneAtom `shiftLAtom` i))
        `neqAtom` Naught

twosComplementPositive :: Positive -> DigitsOnes
twosComplementPositive p = flipBits (p `minusPositive` onePositive)

flipBits :: Digits -> DigitsOnes
flipBits ds = DigitsOnes (flipBitsDigits ds)

flipBitsDigits :: Digits -> Digits
flipBitsDigits None = None
flipBitsDigits (Some w ws) = Some (not# w) (flipBitsDigits ws)

{-# NOINLINE negateAtom #-}
negateAtom :: Atom -> Atom
negateAtom (Positive p) = Negative p
negateAtom (Negative p) = Positive p
negateAtom Naught       = Naught

-- Note [Avoid patError]
{-# NOINLINE plusAtom #-}
plusAtom :: Atom -> Atom -> Atom
Positive p1    `plusAtom` Positive p2 = Positive (p1 `plusPositive` p2)
Negative p1    `plusAtom` Negative p2 = Negative (p1 `plusPositive` p2)
Positive p1    `plusAtom` Negative p2
    = case p1 `comparePositive` p2 of
      GT -> Positive (p1 `minusPositive` p2)
      EQ -> Naught
      LT -> Negative (p2 `minusPositive` p1)
Negative p1    `plusAtom` Positive p2
    = Positive p2 `plusAtom` Negative p1
Naught         `plusAtom` Naught         = Naught
Naught         `plusAtom` i@(Positive _) = i
Naught         `plusAtom` i@(Negative _) = i
i@(Positive _) `plusAtom` Naught         = i
i@(Negative _) `plusAtom` Naught         = i

{-# NOINLINE minusAtom #-}
minusAtom :: Atom -> Atom -> Atom
i1 `minusAtom` i2 = i1 `plusAtom` negateAtom i2

{-# NOINLINE timesAtom #-}
timesAtom :: Atom -> Atom -> Atom
Positive p1 `timesAtom` Positive p2 = Positive (p1 `timesPositive` p2)
Negative p1 `timesAtom` Negative p2 = Positive (p1 `timesPositive` p2)
Positive p1 `timesAtom` Negative p2 = Negative (p1 `timesPositive` p2)
Negative p1 `timesAtom` Positive p2 = Negative (p1 `timesPositive` p2)
(!_)        `timesAtom` (!_)        = Naught

{-# NOINLINE divModAtom #-}
divModAtom :: Atom -> Atom -> (# Atom, Atom #)
n `divModAtom` d =
    case n `quotRemAtom` d of
        (# q, r #) ->
            if signumAtom r `eqAtom`
               negateAtom (signumAtom d)
            then (# q `minusAtom` oneAtom, r `plusAtom` d #)
            else (# q, r #)

{-# NOINLINE divAtom #-}
divAtom :: Atom -> Atom -> Atom
n `divAtom` d = quotient
    where (# quotient, _ #) = n `divModAtom` d

{-# NOINLINE modAtom #-}
modAtom :: Atom -> Atom -> Atom
n `modAtom` d = modulus
    where (# _, modulus #) = n `divModAtom` d

{-# NOINLINE quotRemAtom #-}
quotRemAtom :: Atom -> Atom -> (# Atom, Atom #)
Naught      `quotRemAtom` (!_)        = (# Naught, Naught #)
(!_)        `quotRemAtom` Naught
    = (# errorAtom, errorAtom #) -- XXX Can't happen
-- XXX _            `quotRemAtom` Naught     = error "Division by zero"
Positive p1 `quotRemAtom` Positive p2 = p1 `quotRemPositive` p2
Negative p1 `quotRemAtom` Positive p2 = case p1 `quotRemPositive` p2 of
                                           (# q, r #) ->
                                               (# negateAtom q,
                                                  negateAtom r #)
Positive p1 `quotRemAtom` Negative p2 = case p1 `quotRemPositive` p2 of
                                           (# q, r #) ->
                                               (# negateAtom q, r #)
Negative p1 `quotRemAtom` Negative p2 = case p1 `quotRemPositive` p2 of
                                           (# q, r #) ->
                                               (# q, negateAtom r #)

{-# NOINLINE quotAtom #-}
quotAtom :: Atom -> Atom -> Atom
x `quotAtom` y = case x `quotRemAtom` y of
                    (# q, _ #) -> q

{-# NOINLINE remAtom #-}
remAtom :: Atom -> Atom -> Atom
x `remAtom` y = case x `quotRemAtom` y of
                   (# _, r #) -> r

{-# NOINLINE compareAtom #-}
compareAtom :: Atom -> Atom -> Ordering
Positive x `compareAtom` Positive y = x `comparePositive` y
Positive _ `compareAtom` (!_)       = GT
Naught     `compareAtom` Naught     = EQ
Naught     `compareAtom` Negative _ = GT
Negative x `compareAtom` Negative y = y `comparePositive` x
(!_)       `compareAtom` (!_)       = LT

{-# NOINLINE eqAtom# #-}
eqAtom# :: Atom -> Atom -> Int#
x `eqAtom#` y = case x `compareAtom` y of
                        EQ -> 1#
                        _  -> 0#

{-# NOINLINE neqAtom# #-}
neqAtom# :: Atom -> Atom -> Int#
x `neqAtom#` y = case x `compareAtom` y of
                         EQ -> 0#
                         _  -> 1#

{-# INLINE eqAtom  #-}
{-# INLINE neqAtom #-}
eqAtom, neqAtom :: Atom -> Atom -> Bool
eqAtom  a b = isTrue# (a `eqAtom#`  b)
neqAtom a b = isTrue# (a `neqAtom#` b)

instance  Eq Atom  where
    (==) = eqAtom
    (/=) = neqAtom

{-# NOINLINE ltAtom# #-}
ltAtom# :: Atom -> Atom -> Int#
x `ltAtom#` y = case x `compareAtom` y of
                        LT -> 1#
                        _  -> 0#

{-# NOINLINE gtAtom# #-}
gtAtom# :: Atom -> Atom -> Int#
x `gtAtom#` y = case x `compareAtom` y of
                        GT -> 1#
                        _  -> 0#

{-# NOINLINE leAtom# #-}
leAtom# :: Atom -> Atom -> Int#
x `leAtom#` y = case x `compareAtom` y of
                        GT -> 0#
                        _  -> 1#

{-# NOINLINE geAtom# #-}
geAtom# :: Atom -> Atom -> Int#
x `geAtom#` y = case x `compareAtom` y of
                        LT -> 0#
                        _  -> 1#

{-# INLINE leAtom #-}
{-# INLINE ltAtom #-}
{-# INLINE geAtom #-}
{-# INLINE gtAtom #-}
leAtom, gtAtom, ltAtom, geAtom :: Atom -> Atom -> Bool
leAtom a b = isTrue# (a `leAtom#` b)
gtAtom a b = isTrue# (a `gtAtom#` b)
ltAtom a b = isTrue# (a `ltAtom#` b)
geAtom a b = isTrue# (a `geAtom#` b)

instance Ord Atom where
    (<=) = leAtom
    (>)  = gtAtom
    (<)  = ltAtom
    (>=) = geAtom
    compare = compareAtom

{-# NOINLINE absAtom #-}
absAtom :: Atom -> Atom
absAtom (Negative x) = Positive x
absAtom x = x

{-# NOINLINE signumAtom #-}
signumAtom :: Atom -> Atom
signumAtom (Negative _) = negativeOneAtom
signumAtom Naught       = Naught
signumAtom (Positive _) = oneAtom

{-# NOINLINE hashAtom #-}
hashAtom :: Atom -> Int#
hashAtom = integerToInt

-------------------------------------------------------------------
-- The hard work is done on positive numbers

onePositive :: Positive
onePositive = Some 1## None

halfBoundUp, fullBound :: () -> Digit
lowHalfMask :: () -> Digit
highHalfShift :: () -> Int#
twoToTheThirtytwoPositive :: Positive
#if WORD_SIZE_IN_BITS == 64
halfBoundUp   () = 0x8000000000000000##
fullBound     () = 0xFFFFFFFFFFFFFFFF##
lowHalfMask   () = 0xFFFFFFFF##
highHalfShift () = 32#
twoToTheThirtytwoPositive = Some 0x100000000## None
#elif WORD_SIZE_IN_BITS == 32
halfBoundUp   () = 0x80000000##
fullBound     () = 0xFFFFFFFF##
lowHalfMask   () = 0xFFFF##
highHalfShift () = 16#
twoToTheThirtytwoPositive = Some 0## (Some 1## None)
#else
#error Unhandled WORD_SIZE_IN_BITS
#endif

digitsMaybeZeroToAtom :: Digits -> Atom
digitsMaybeZeroToAtom None = Naught
digitsMaybeZeroToAtom ds = Positive ds

digitsToAtom :: Digits -> Atom
digitsToAtom ds = case removeZeroTails ds of
                     None -> Naught
                     ds' -> Positive ds'

digitsToNegativeAtom :: Digits -> Atom
digitsToNegativeAtom ds = case removeZeroTails ds of
                             None -> Naught
                             ds' -> Negative ds'

removeZeroTails :: Digits -> Digits
removeZeroTails (Some w ds) = if isTrue# (w `eqWord#` 0##)
                              then case removeZeroTails ds of
                                   None -> None
                                   ds' -> Some w ds'
                              else Some w (removeZeroTails ds)
removeZeroTails None = None

#if WORD_SIZE_IN_BITS < 64
word64ToPositive :: Word64# -> Positive
word64ToPositive w
 = if isTrue# (w `eqWord64#` wordToWord64# 0##)
   then None
   else Some (word64ToWord# w) (word64ToPositive (w `uncheckedShiftRL64#` 32#))

positiveToWord64 :: Positive -> Word64#
positiveToWord64 None = wordToWord64# 0## -- XXX Can't happen
positiveToWord64 (Some w None) = wordToWord64# w
positiveToWord64 (Some low (Some high _))
    = wordToWord64# low `or64#` (wordToWord64# high `uncheckedShiftL64#` 32#)
#endif

-- Note [Avoid patError]
comparePositive :: Positive -> Positive -> Ordering
Some x xs `comparePositive` Some y ys = case xs `comparePositive` ys of
                                        EQ ->      if isTrue# (x `ltWord#` y) then LT
                                              else if isTrue# (x `gtWord#` y) then GT
                                              else                                 EQ
                                        res -> res
None      `comparePositive` None      = EQ
(Some {}) `comparePositive` None      = GT
None      `comparePositive` (Some {}) = LT

plusPositive :: Positive -> Positive -> Positive
plusPositive x0 y0 = addWithCarry 0## x0 y0
 where -- digit `elem` [0, 1]
       -- Note [Avoid patError]
       addWithCarry :: Digit -> Positive -> Positive -> Positive
       addWithCarry c None            None            = addOnCarry c None
       addWithCarry c xs@(Some {})    None            = addOnCarry c xs
       addWithCarry c None            ys@(Some {})    = addOnCarry c ys
       addWithCarry c xs@(Some x xs') ys@(Some y ys')
        = if isTrue# (x `ltWord#` y) then addWithCarry c ys xs
          -- Now x >= y
          else if isTrue# (y `geWord#` halfBoundUp ())
               -- So they are both at least halfBoundUp, so we subtract
               -- halfBoundUp from each and thus carry 1
               then case x `minusWord#` halfBoundUp () of
                    x' ->
                     case y `minusWord#` halfBoundUp () of
                     y' ->
                      case x' `plusWord#` y' `plusWord#` c of
                      this ->
                       Some this withCarry
          else if isTrue# (x `geWord#` halfBoundUp ())
               then case x `minusWord#` halfBoundUp () of
                    x' ->
                     case x' `plusWord#` y `plusWord#` c of
                     z ->
                      -- We've taken off halfBoundUp, so now we need to
                      -- add it back on
                      if isTrue# (z `ltWord#` halfBoundUp ())
                       then Some (z `plusWord#`  halfBoundUp ()) withoutCarry
                       else Some (z `minusWord#` halfBoundUp ()) withCarry
          else Some (x `plusWord#` y `plusWord#` c) withoutCarry
           where withCarry    = addWithCarry 1## xs' ys'
                 withoutCarry = addWithCarry 0## xs' ys'

       -- digit `elem` [0, 1]
       addOnCarry :: Digit -> Positive -> Positive
       addOnCarry (!c) (!ws) = if isTrue# (c `eqWord#` 0##)
                               then ws
                               else succPositive ws

-- digit `elem` [0, 1]
succPositive :: Positive -> Positive
succPositive None = Some 1## None
succPositive (Some w ws) = if isTrue# (w `eqWord#` fullBound ())
                           then Some 0## (succPositive ws)
                           else Some (w `plusWord#` 1##) ws

-- Requires x > y
-- In recursive calls, x >= y and x == y => result is None
-- Note [Avoid patError]
minusPositive :: Positive -> Positive -> Positive
Some x xs `minusPositive` Some y ys
 = if isTrue# (x `eqWord#` y)
   then case xs `minusPositive` ys of
        None -> None
        s -> Some 0## s
   else if isTrue# (x `gtWord#` y) then
        Some (x `minusWord#` y) (xs `minusPositive` ys)
   else case (fullBound () `minusWord#` y) `plusWord#` 1## of
        z -> -- z = 2^n - y, calculated without overflow
         case z `plusWord#` x of
         z' -> -- z = 2^n + (x - y), calculated without overflow
          Some z' ((xs `minusPositive` ys) `minusPositive` onePositive)
xs@(Some {}) `minusPositive` None      = xs
None         `minusPositive` None      = None
None         `minusPositive` (Some {}) = errorPositive -- XXX Can't happen
-- XXX None `minusPositive` _ = error "minusPositive: Requirement x > y not met"

-- Note [Avoid patError]
timesPositive :: Positive -> Positive -> Positive
-- XXX None's can't happen here:
None            `timesPositive` None        = errorPositive
None            `timesPositive` (Some {})   = errorPositive
(Some {})       `timesPositive` None        = errorPositive
-- x and y are the last digits in Positive numbers, so are not 0:
xs@(Some x xs') `timesPositive` ys@(Some y ys')
 = case xs' of
   None ->
       case ys' of
           None ->
               x `timesDigit` y
           Some {} ->
               ys `timesPositive` xs
   Some {} ->
       case ys' of
       None ->
           -- y is the last digit in a Positive number, so is not 0.
           let zs = Some 0## (xs' `timesPositive` ys)
           in -- We could actually skip this test, and everything would
              -- turn out OK. We already play tricks like that in timesPositive.
              if isTrue# (x `eqWord#` 0##)
              then zs
              else (x `timesDigit` y) `plusPositive` zs
       Some {} ->
           (Some x None `timesPositive` ys) `plusPositive`
           Some 0## (xs' `timesPositive` ys)

{-
-- Requires arguments /= 0
Suppose we have 2n bits in a Word. Then
    x = 2^n xh + xl
    y = 2^n yh + yl
    x * y = (2^n xh + xl) * (2^n yh + yl)
          = 2^(2n) (xh yh)
          + 2^n    (xh yl)
          + 2^n    (xl yh)
          +        (xl yl)
                   ~~~~~~~ - all fit in 2n bits
-}
timesDigit :: Digit -> Digit -> Positive
timesDigit (!x) (!y)
 = case splitHalves x of
   (# xh, xl #) ->
    case splitHalves y of
    (# yh, yl #) ->
     case xh `timesWord#` yh of
     xhyh ->
      case splitHalves (xh `timesWord#` yl) of
      (# xhylh, xhyll #) ->
       case xhyll `uncheckedShiftL#` highHalfShift () of
       xhyll' ->
        case splitHalves (xl `timesWord#` yh) of
        (# xlyhh, xlyhl #) ->
         case xlyhl `uncheckedShiftL#` highHalfShift () of
         xlyhl' ->
          case xl `timesWord#` yl of
          xlyl ->
           -- Add up all the high word results. As the result fits in
           -- 4n bits this can't overflow.
           case xhyh `plusWord#` xhylh `plusWord#` xlyhh of
           high ->
           -- low: xhyll<<n + xlyhl<<n + xlyl
            -- From this point we might make (Some 0 None), but we know
            -- that the final result will be positive and the addition
            -- will work out OK, so everything will work out in the end.
            -- One thing we do need to be careful of is avoiding returning
            -- Some 0 (Some 0 None) + Some n None, as this will result in
            -- Some n (Some 0 None) instead of Some n None.
            let low = Some xhyll' None `plusPositive`
                      Some xlyhl' None `plusPositive`
                      Some xlyl   None
            in if isTrue# (high `eqWord#` 0##)
               then low
               else Some 0## (Some high None) `plusPositive` low

splitHalves :: Digit -> (# {- High -} Digit, {- Low -} Digit #)
splitHalves (!x) = (# x `uncheckedShiftRL#` highHalfShift (),
                      x `and#` lowHalfMask () #)

-- Assumes 0 <= i
shiftLPositive :: Positive -> Int# -> Positive
shiftLPositive p i
    = if isTrue# (i >=# WORD_SIZE_IN_BITS#)
      then shiftLPositive (Some 0## p) (i -# WORD_SIZE_IN_BITS#)
      else smallShiftLPositive p i

-- Assumes 0 <= i < WORD_SIZE_IN_BITS#
smallShiftLPositive :: Positive -> Int# -> Positive
smallShiftLPositive (!p) 0# = p
smallShiftLPositive (!p) (!i) =
    case WORD_SIZE_IN_BITS# -# i of
    j -> let f carry None = if isTrue# (carry `eqWord#` 0##)
                            then None
                            else Some carry None
             f carry (Some w ws) = case w `uncheckedShiftRL#` j of
                                   carry' ->
                                    case w `uncheckedShiftL#` i of
                                    me ->
                                     Some (me `or#` carry) (f carry' ws)
         in f 0## p

-- Assumes 0 <= i
shiftRPositive :: Positive -> Int# -> Atom
shiftRPositive None _ = Naught
shiftRPositive p@(Some _ q) i
    = if isTrue# (i >=# WORD_SIZE_IN_BITS#)
      then shiftRPositive q (i -# WORD_SIZE_IN_BITS#)
      else smallShiftRPositive p i

-- Assumes 0 <= i < WORD_SIZE_IN_BITS#
smallShiftRPositive :: Positive -> Int# -> Atom
smallShiftRPositive (!p) (!i) =
    if isTrue# (i ==# 0#)
    then Positive p
    else case smallShiftLPositive p (WORD_SIZE_IN_BITS# -# i) of
         Some _ p'@(Some _ _) -> Positive p'
         _                    -> Naught

-- Long division
quotRemPositive :: Positive -> Positive -> (# Atom, Atom #)
(!xs) `quotRemPositive` (!ys)
    = case f xs of
      (# d, m #) -> (# digitsMaybeZeroToAtom d,
                       digitsMaybeZeroToAtom m #)
    where
          subtractors :: Positives
          subtractors = mkSubtractors (WORD_SIZE_IN_BITS# -# 1#)

          mkSubtractors (!n) = if isTrue# (n ==# 0#)
                               then Cons ys Nil
                               else Cons (ys `smallShiftLPositive` n)
                                         (mkSubtractors (n -# 1#))

          -- The main function. Go the the end of xs, then walk
          -- back trying to divide the number we accumulate by ys.
          f :: Positive -> (# Digits, Digits #)
          f None = (# None, None #)
          f (Some z zs)
              = case f zs of
                (# ds, m #) ->
                    let -- We need to avoid making (Some Zero None) here
                        m' = some z m
                    in case g 0## subtractors m' of
                       (# d, m'' #) ->
                        (# some d ds, m'' #)

          g :: Digit -> Positives -> Digits -> (# Digit, Digits #)
          g (!d) Nil             (!m) = (# d, m #)
          g (!d) (Cons sub subs) (!m)
              = case d `uncheckedShiftL#` 1# of
                d' ->
                 case m `comparePositive` sub of
                 LT -> g d' subs m
                 _  -> g (d' `plusWord#` 1##)
                         subs
                         (m `minusPositive` sub)

some :: Digit -> Digits -> Digits
some (!w) None  = if isTrue# (w `eqWord#` 0##) then None else Some w None
some (!w) (!ws) = Some w ws

-- Note [Avoid patError]
andDigits :: Digits -> Digits -> Digits
andDigits None          None          = None
andDigits (Some {})     None          = None
andDigits None          (Some {})     = None
andDigits (Some w1 ws1) (Some w2 ws2) = Some (w1 `and#` w2) (andDigits ws1 ws2)

-- DigitsOnes is just like Digits, only None is really 0xFFFFFFF...,
-- i.e. ones off to infinity. This makes sense when we want to "and"
-- a DigitOnes with a Digits, as the latter will bound the size of the
-- result.
newtype DigitsOnes = DigitsOnes Digits

-- Note [Avoid patError]
andDigitsOnes :: DigitsOnes -> Digits -> Digits
andDigitsOnes (DigitsOnes None)          None          = None
andDigitsOnes (DigitsOnes None)          ws2@(Some {}) = ws2
andDigitsOnes (DigitsOnes (Some {}))     None          = None
andDigitsOnes (DigitsOnes (Some w1 ws1)) (Some w2 ws2)
    = Some (w1 `and#` w2) (andDigitsOnes (DigitsOnes ws1) ws2)

-- Note [Avoid patError]
orDigits :: Digits -> Digits -> Digits
orDigits None          None          = None
orDigits None          ds@(Some {})  = ds
orDigits ds@(Some {})  None          = ds
orDigits (Some w1 ds1) (Some w2 ds2) = Some (w1 `or#` w2) (orDigits ds1 ds2)

-- Note [Avoid patError]
xorDigits :: Digits -> Digits -> Digits
xorDigits None          None          = None
xorDigits None          ds@(Some {})  = ds
xorDigits ds@(Some {})  None          = ds
xorDigits (Some w1 ds1) (Some w2 ds2) = Some (w1 `xor#` w2) (xorDigits ds1 ds2)

-- XXX We'd really like word2Double# for this
doubleFromPositive :: Positive -> Double#
doubleFromPositive None = 0.0##
doubleFromPositive (Some w ds)
    = case splitHalves w of
      (# h, l #) ->
       (doubleFromPositive ds *## (2.0## **## WORD_SIZE_IN_BITS_FLOAT##))
       +## (int2Double# (word2Int# h) *##
              (2.0## **## int2Double# (highHalfShift ())))
       +## int2Double# (word2Int# l)

-- XXX We'd really like word2Float# for this
floatFromPositive :: Positive -> Float#
floatFromPositive None = 0.0#
floatFromPositive (Some w ds)
    = case splitHalves w of
      (# h, l #) ->
       (floatFromPositive ds `timesFloat#` (2.0# `powerFloat#` WORD_SIZE_IN_BITS_FLOAT#))
       `plusFloat#` (int2Float# (word2Int# h) `timesFloat#`
             (2.0# `powerFloat#` int2Float# (highHalfShift ())))
       `plusFloat#` int2Float# (word2Int# l)

{-
Note [Avoid patError]

If we use the natural set of definitions for functions, e.g.:

    orDigits None          ds            = ds
    orDigits ds            None          = ds
    orDigits (Some w1 ds1) (Some w2 ds2) = Some ... ...

then GHC may not be smart enough (especially when compiling with -O0)
to see that all the cases are handled, and will thus insert calls to
base:Control.Exception.Base.patError. But we are below base in the
package hierarchy, so this causes build failure!

We therefore help GHC out, by being more explicit about what all the
cases are:

    orDigits None          None          = None
    orDigits None          ds@(Some {})  = ds
    orDigits ds@(Some {})  None          = ds
    orDigits (Some w1 ds1) (Some w2 ds2) = Some ... ...
-}

