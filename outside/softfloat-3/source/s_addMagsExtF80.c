
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
 softfloat_addMagsExtF80(
     uint_fast16_t uiA64,
     uint_fast64_t uiA0,
     uint_fast16_t uiB64,
     uint_fast64_t uiB0,
     bool signZ
 )
{
    int_fast32_t expA;
    uint_fast64_t sigA;
    int_fast32_t expB;
    uint_fast64_t sigB;
    int_fast32_t expDiff;
    uint_fast16_t uiZ64;
    uint_fast64_t uiZ0, sigZ, sigZExtra;
    struct exp32_sig64 normExpSig;
    int_fast32_t expZ;
    struct uint64_extra sig64Extra;
    struct uint128 uiZ;
    union { struct extFloat80M s; extFloat80_t f; } uZ;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    expA = expExtF80UI64( uiA64 );
    sigA = uiA0;
    expB = expExtF80UI64( uiB64 );
    sigB = uiB0;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    expDiff = expA - expB;
    if ( ! expDiff ) {
        if ( expA == 0x7FFF ) {
            if ( (sigA | sigB) & UINT64_C( 0x7FFFFFFFFFFFFFFF ) ) {
                goto propagateNaN;
            }
            uiZ64 = uiA64;
            uiZ0  = uiA0;
            goto uiZ;
        }
        sigZ = sigA + sigB;
        sigZExtra = 0;
        if ( ! expA ) {
            normExpSig = softfloat_normSubnormalExtF80Sig( sigZ );
            expZ = normExpSig.exp + 1;
            sigZ = normExpSig.sig;
            goto roundAndPack;
        }
        expZ = expA;
        goto shiftRight1;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( expDiff < 0 ) {
        if ( expB == 0x7FFF ) {
            if ( sigB & UINT64_C( 0x7FFFFFFFFFFFFFFF ) ) goto propagateNaN;
            uiZ64 = packToExtF80UI64( signZ, 0x7FFF );
            uiZ0  = uiB0;
            goto uiZ;
        }
        expZ = expB;
        if ( ! expA ) {
            ++expDiff;
            sigZExtra = 0;
            if ( ! expDiff ) goto newlyAligned;
        }
        sig64Extra = softfloat_shiftRightJam64Extra( sigA, 0, -expDiff );
        sigA = sig64Extra.v;
        sigZExtra = sig64Extra.extra;
    } else {
        if ( expA == 0x7FFF ) {
            if ( sigA & UINT64_C( 0x7FFFFFFFFFFFFFFF ) ) goto propagateNaN;
            uiZ64 = uiA64;
            uiZ0  = uiA0;
            goto uiZ;
        }
        expZ = expA;
        if ( ! expB ) {
            --expDiff;
            sigZExtra = 0;
            if ( ! expDiff ) goto newlyAligned;
        }
        sig64Extra = softfloat_shiftRightJam64Extra( sigB, 0, expDiff );
        sigB = sig64Extra.v;
        sigZExtra = sig64Extra.extra;
    }
 newlyAligned:
    sigZ = sigA + sigB;
    if ( sigZ & UINT64_C( 0x8000000000000000 ) ) goto roundAndPack;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 shiftRight1:
    sig64Extra = softfloat_shortShiftRightJam64Extra( sigZ, sigZExtra, 1 );
    sigZ = sig64Extra.v | UINT64_C( 0x8000000000000000 );
    sigZExtra = sig64Extra.extra;
    ++expZ;
 roundAndPack:
    return
        softfloat_roundPackToExtF80(
            signZ, expZ, sigZ, sigZExtra, extF80_roundingPrecision );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 propagateNaN:
    uiZ = softfloat_propagateNaNExtF80UI( uiA64, uiA0, uiB64, uiB0 );
    uiZ64 = uiZ.v64;
    uiZ0  = uiZ.v0;
 uiZ:
    uZ.s.signExp = uiZ64;
    uZ.s.signif  = uiZ0;
    return uZ.f;

}

