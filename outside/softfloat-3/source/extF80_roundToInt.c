
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

extFloat80_t
 extF80_roundToInt( extFloat80_t a, uint_fast8_t roundingMode, bool exact )
{
    union { struct extFloat80M s; extFloat80_t f; } uA;
    uint_fast16_t uiA64, signUI64;
    int_fast32_t exp;
    uint_fast64_t sigA;
    uint_fast16_t uiZ64;
    uint_fast64_t sigZ;
    struct exp32_sig64 normExpSig;
    struct uint128 uiZ;
    uint_fast64_t lastBitMask, roundBitsMask;
    union { struct extFloat80M s; extFloat80_t f; } uZ;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uA.f = a;
    uiA64 = uA.s.signExp;
    signUI64 = uiA64 & packToExtF80UI64( 1, 0 );
    exp = expExtF80UI64( uiA64 );
    sigA = uA.s.signif;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( ! (sigA & UINT64_C( 0x8000000000000000 )) && (exp != 0x7FFF) ) {
        if ( ! sigA ) {
            uiZ64 = signUI64;
            sigZ = 0;
            goto uiZ;
        }
        normExpSig = softfloat_normSubnormalExtF80Sig( sigA );
        exp += normExpSig.exp;
        sigA = normExpSig.sig;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( 0x403E <= exp ) {
        if ( exp == 0x7FFF ) {
            if ( sigA & UINT64_C( 0x7FFFFFFFFFFFFFFF ) ) {
                uiZ = softfloat_propagateNaNExtF80UI( uiA64, sigA, 0, 0 );
                uiZ64 = uiZ.v64;
                sigZ  = uiZ.v0;
                goto uiZ;
            }
            sigZ = UINT64_C( 0x8000000000000000 );
        } else {
            sigZ = sigA;
        }
        uiZ64 = signUI64 | exp;
        goto uiZ;
    }
    if ( exp <= 0x3FFE ) {
        if ( exact ) softfloat_exceptionFlags |= softfloat_flag_inexact;
        switch ( roundingMode ) {
         case softfloat_round_near_even:
            if ( ! (sigA & UINT64_C( 0x7FFFFFFFFFFFFFFF )) ) break;
         case softfloat_round_near_maxMag:
            if ( exp == 0x3FFE ) goto mag1;
            break;
         case softfloat_round_min:
            if ( signUI64 ) goto mag1;
            break;
         case softfloat_round_max:
            if ( ! signUI64 ) goto mag1;
            break;
        }
        uiZ64 = signUI64;
        sigZ  = 0;
        goto uiZ;
     mag1:
        uiZ64 = signUI64 | 0x3FFF;
        sigZ  = UINT64_C( 0x8000000000000000 );
        goto uiZ;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uiZ64 = signUI64 | exp;
    lastBitMask = (uint_fast64_t) 1<<(0x403E - exp);
    roundBitsMask = lastBitMask - 1;
    sigZ = sigA;
    if ( roundingMode == softfloat_round_near_maxMag ) {
        sigZ += lastBitMask>>1;
    } else if ( roundingMode == softfloat_round_near_even ) {
        sigZ += lastBitMask>>1;
        if ( ! (sigZ & roundBitsMask) ) sigZ &= ~lastBitMask;
    } else if ( roundingMode != softfloat_round_minMag ) {
        if ( (signUI64 != 0) ^ (roundingMode == softfloat_round_max) ) {
            sigZ += roundBitsMask;
        }
    }
    sigZ &= ~roundBitsMask;
    if ( ! sigZ ) {
        ++uiZ64;
        sigZ = UINT64_C( 0x8000000000000000 );
    }
    if ( exact && (sigZ != sigA) ) {
        softfloat_exceptionFlags |= softfloat_flag_inexact;
    }
 uiZ:
    uZ.s.signExp = uiZ64;
    uZ.s.signif = sigZ;
    return uZ.f;

}

