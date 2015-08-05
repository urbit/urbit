
/*============================================================================

This C source file is part of the SoftFloat IEEE Floating-Point Arithmetic
Package, Release 3, by John R. Hauser.

Copyright 2011, 2012, 2013, 2014 The Regents of the University of California
(Regents).  All Rights Reserved.  Redistribution and use in source and binary
forms, with or without modification, are permitted provided that the following
conditions are met:

Redistributions of source code must retain the above copyright notice,
this list of conditions, and the following two paragraphs of disclaimer.
Redistributions in binary form must reproduce the above copyright notice,
this list of conditions, and the following two paragraphs of disclaimer in the
documentation and/or other materials provided with the distribution.  Neither
the name of the Regents nor the names of its contributors may be used to
endorse or promote products derived from this software without specific prior
written permission.

IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING
OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF REGENTS HAS
BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE.  THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF ANY, PROVIDED
HEREUNDER IS PROVIDED "AS IS".  REGENTS HAS NO OBLIGATION TO PROVIDE
MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

=============================================================================*/

#include <stdbool.h>
#include <stdint.h>
#include "platform.h"
#include "internals.h"
#include "specialize.h"
#include "softfloat.h"

#ifdef SOFTFLOAT_FAST_INT64

void
 f128M_roundToInt(
     const float128_t *aPtr,
     uint_fast8_t roundingMode,
     bool exact,
     float128_t *zPtr
 )
{

    *zPtr = f128_roundToInt( *aPtr, roundingMode, exact );

}

#else

void
 f128M_roundToInt(
     const float128_t *aPtr,
     uint_fast8_t roundingMode,
     bool exact,
     float128_t *zPtr
 )
{
    const uint32_t *aWPtr;
    uint32_t *zWPtr;
    uint32_t ui96;
    int32_t exp;
    uint32_t sigExtra;
    bool sign;
    uint_fast8_t bitPos;
    bool roundNear;
    unsigned int index, lastIndex;
    bool extra;
    uint32_t wordA, bit, wordZ;
    uint_fast8_t carry;
    uint32_t extrasMask;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    aWPtr = (const uint32_t *) aPtr;
    zWPtr = (uint32_t *) zPtr;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    ui96 = aWPtr[indexWordHi( 4 )];
    exp = expF128UI96( ui96 );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( exp < 0x3FFF ) {
        zWPtr[indexWord( 4, 2 )] = 0;
        zWPtr[indexWord( 4, 1 )] = 0;
        zWPtr[indexWord( 4, 0 )] = 0;
        sigExtra = aWPtr[indexWord( 4, 2 )];
        if ( ! sigExtra ) {
            sigExtra = aWPtr[indexWord( 4, 1 )] | aWPtr[indexWord( 4, 0 )];
        }
        if ( ! sigExtra && ! (ui96 & 0x7FFFFFFF) ) goto ui96;
        if ( exact ) softfloat_exceptionFlags |= softfloat_flag_inexact;
        sign = signF128UI96( ui96 );
        switch ( roundingMode ) {
         case softfloat_round_near_even:
            if ( ! fracF128UI96( ui96 ) && ! sigExtra ) break;
         case softfloat_round_near_maxMag:
            if ( exp == 0x3FFE ) goto mag1;
            break;
         case softfloat_round_min:
            if ( sign ) goto mag1;
            break;
         case softfloat_round_max:
            if ( ! sign ) goto mag1;
            break;
        }
        ui96 = packToF128UI96( sign, 0, 0 );
        goto ui96;
     mag1:
        ui96 = packToF128UI96( sign, 0x3FFF, 0 );
        goto ui96;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( 0x406F <= exp ) {
        if (
            (exp == 0x7FFF)
                && (fracF128UI96( ui96 )
                        || (aWPtr[indexWord( 4, 2 )] | aWPtr[indexWord( 4, 1 )]
                                | aWPtr[indexWord( 4, 0 )]))
        ) {
            softfloat_propagateNaNF128M( aWPtr, 0, zWPtr );
            return;
        }
        zWPtr[indexWord( 4, 2 )] = aWPtr[indexWord( 4, 2 )];
        zWPtr[indexWord( 4, 1 )] = aWPtr[indexWord( 4, 1 )];
        zWPtr[indexWord( 4, 0 )] = aWPtr[indexWord( 4, 0 )];
        goto ui96;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    bitPos = 0x406F - exp;
    roundNear =
           (roundingMode == softfloat_round_near_maxMag)
        || (roundingMode == softfloat_round_near_even);
    bitPos -= roundNear;
    index = indexWordLo( 4 );
    lastIndex = indexWordHi( 4 );
    extra = 0;
    for (;;) {
        wordA = aWPtr[index];
        if ( bitPos < 32 ) break;
        if ( wordA ) extra = 1;
        zWPtr[index] = 0;
        index += wordIncr;
        bitPos -= 32;
    }
    bit = (uint32_t) 1<<bitPos;
    if ( roundNear ) {
        wordZ = wordA + bit;
        carry = (wordZ < wordA);
        bit <<= 1;
        extrasMask = bit - 1;
        if (
            (roundingMode == softfloat_round_near_even)
                && ! extra && ! (wordZ & extrasMask)
        ) {
            if ( ! bit ) {
                zWPtr[index] = wordZ;
                index += wordIncr;
                wordZ = aWPtr[index] + carry;
                carry &= ! wordZ;
                zWPtr[index] = wordZ & ~1;
                goto propagateCarry;
            }
            wordZ &= ~bit;
        }
    } else {
        extrasMask = bit - 1;
        wordZ = wordA;
        carry = 0;
        if (
               (roundingMode != softfloat_round_minMag)
            && (signF128UI96( ui96 ) ^ (roundingMode == softfloat_round_max))
        ) {
            if ( extra || (wordA & extrasMask) ) {
                wordZ += bit;
                carry = (wordZ < wordA);
            }
        }
    }
    wordZ &= ~extrasMask;
    zWPtr[index] = wordZ;
 propagateCarry:
    while ( index != lastIndex ) {
        index += wordIncr;
        wordZ = aWPtr[index] + carry;
        zWPtr[index] = wordZ;
        carry &= ! wordZ;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( exact && (softfloat_compare128M( aWPtr, zWPtr ) != 0) ) {
        softfloat_exceptionFlags |= softfloat_flag_inexact;
    }
    return;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 ui96:
    zWPtr[indexWordHi( 4 )] = ui96;

}

#endif

