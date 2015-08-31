
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

float128_t
 f128_roundToInt( float128_t a, uint_fast8_t roundingMode, bool exact )
{
    union ui128_f128 uA;
    uint_fast64_t uiA64, uiA0;
    int_fast32_t exp;
    struct uint128 uiZ;
    uint_fast64_t lastBitMask, roundBitsMask;
    bool roundNearEven;
    union ui128_f128 uZ;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uA.f = a;
    uiA64 = uA.ui.v64;
    uiA0  = uA.ui.v0;
    exp = expF128UI64( uiA64 );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( 0x402F <= exp ) {
        /*--------------------------------------------------------------------
        *--------------------------------------------------------------------*/
        if ( 0x406F <= exp ) {
            if ( (exp == 0x7FFF) && (fracF128UI64( uiA64 ) | uiA0) ) {
                uiZ = softfloat_propagateNaNF128UI( uiA64, uiA0, 0, 0 );
                goto uiZ;
            }
            return a;
        }
        /*--------------------------------------------------------------------
        *--------------------------------------------------------------------*/
        lastBitMask = (uint_fast64_t) 2<<(0x406E - exp);
        roundBitsMask = lastBitMask - 1;
        uiZ.v64 = uiA64;
        uiZ.v0  = uiA0;
        roundNearEven = (roundingMode == softfloat_round_near_even);
        if ( roundNearEven || (roundingMode == softfloat_round_near_maxMag) ) {
            if ( exp == 0x402F ) {
                if ( UINT64_C( 0x8000000000000000 ) <= uiZ.v0 ) {
                    ++uiZ.v64;
                    if (
                        roundNearEven
                            && (uiZ.v0 == UINT64_C( 0x8000000000000000 ))
                    ) {
                        uiZ.v64 &= ~1;
                    }
                }
            } else {
                uiZ = softfloat_add128( uiZ.v64, uiZ.v0, 0, lastBitMask>>1 );
                if ( roundNearEven && ! (uiZ.v0 & roundBitsMask) ) {
                    uiZ.v0 &= ~lastBitMask;
                }
            }
        } else if ( roundingMode != softfloat_round_minMag ) {
            if (
                signF128UI64( uiZ.v64 ) ^ (roundingMode == softfloat_round_max)
            ) {
                uiZ = softfloat_add128( uiZ.v64, uiZ.v0, 0, roundBitsMask );
            }
        }
        uiZ.v0 &= ~roundBitsMask;
    } else {
        /*--------------------------------------------------------------------
        *--------------------------------------------------------------------*/
        if ( exp < 0x3FFF ) {
            if ( ! ((uiA64 & UINT64_C( 0x7FFFFFFFFFFFFFFF )) | uiA0) ) {
                return a;
            }
            if ( exact ) softfloat_exceptionFlags |= softfloat_flag_inexact;
            uiZ.v64 = uiA64 & packToF128UI64( 1, 0, 0 );
            uiZ.v0  = 0;
            switch ( roundingMode ) {
             case softfloat_round_near_even:
                if ( ! (fracF128UI64( uiA64 ) | uiA0) ) break;
             case softfloat_round_near_maxMag:
                if ( exp == 0x3FFE ) uiZ.v64 |= packToF128UI64( 0, 0x3FFF, 0 );
                break;
             case softfloat_round_min:
                if ( uiZ.v64 ) uiZ.v64 = packToF128UI64( 1, 0x3FFF, 0 );
                break;
             case softfloat_round_max:
                if ( ! uiZ.v64 ) uiZ.v64 = packToF128UI64( 0, 0x3FFF, 0 );
                break;
            }
            goto uiZ;
        }
        /*--------------------------------------------------------------------
        *--------------------------------------------------------------------*/
        uiZ.v64 = uiA64;
        uiZ.v0  = 0;
        lastBitMask = (uint_fast64_t) 1<<(0x402F - exp);
        roundBitsMask = lastBitMask - 1;
        if ( roundingMode == softfloat_round_near_maxMag ) {
            uiZ.v64 += lastBitMask>>1;
        } else if ( roundingMode == softfloat_round_near_even ) {
            uiZ.v64 += lastBitMask>>1;
            if ( ! ((uiZ.v64 & roundBitsMask) | uiA0) ) {
                uiZ.v64 &= ~lastBitMask;
            }
        } else if ( roundingMode != softfloat_round_minMag ) {
            if (
                signF128UI64( uiZ.v64 ) ^ (roundingMode == softfloat_round_max)
            ) {
                uiZ.v64 = (uiZ.v64 | (uiA0 != 0)) + roundBitsMask;
            }
        }
        uiZ.v64 &= ~roundBitsMask;
    }
    if ( exact && ((uiZ.v64 != uiA64) || (uiZ.v0 != uiA0)) ) {
        softfloat_exceptionFlags |= softfloat_flag_inexact;
    }
 uiZ:
    uZ.ui = uiZ;
    return uZ.f;

}

