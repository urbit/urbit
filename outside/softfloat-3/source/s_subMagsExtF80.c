
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
 softfloat_subMagsExtF80(
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
    uint_fast64_t uiZ0;
    int_fast32_t expZ;
    uint_fast64_t sigExtra;
    struct uint128 sig128, uiZ;
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
    if ( 0 < expDiff ) goto expABigger;
    if ( expDiff < 0 ) goto expBBigger;
    if ( expA == 0x7FFF ) {
        if ( (sigA | sigB) & UINT64_C( 0x7FFFFFFFFFFFFFFF ) ) {
            goto propagateNaN;
        }
        softfloat_raiseFlags( softfloat_flag_invalid );
        uiZ64 = defaultNaNExtF80UI64;
        uiZ0  = defaultNaNExtF80UI0;
        goto uiZ;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    expZ = expA;
    if ( ! expZ ) expZ = 1;
    sigExtra = 0;
    if ( sigB < sigA ) goto aBigger;
    if ( sigA < sigB ) goto bBigger;
    uiZ64 =
        packToExtF80UI64( (softfloat_roundingMode == softfloat_round_min), 0 );
    uiZ0 = 0;
    goto uiZ;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 expBBigger:
    if ( expB == 0x7FFF ) {
        if ( sigB & UINT64_C( 0x7FFFFFFFFFFFFFFF ) ) goto propagateNaN;
        uiZ64 = packToExtF80UI64( signZ ^ 1, 0x7FFF );
        uiZ0  = UINT64_C( 0x8000000000000000 );
        goto uiZ;
    }
    if ( ! expA ) {
        ++expDiff;
        sigExtra = 0;
        if ( ! expDiff ) goto newlyAlignedBBigger;
    }
    sig128 = softfloat_shiftRightJam128( sigA, 0, -expDiff );
    sigA = sig128.v64;
    sigExtra = sig128.v0;
 newlyAlignedBBigger:
    expZ = expB;
 bBigger:
    signZ ^= 1;
    sig128 = softfloat_sub128( sigB, 0, sigA, sigExtra );
    goto normRoundPack;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 expABigger:
    if ( expA == 0x7FFF ) {
        if ( sigA & UINT64_C( 0x7FFFFFFFFFFFFFFF ) ) goto propagateNaN;
        uiZ64 = uiA64;
        uiZ0  = uiA0;
        goto uiZ;
    }
    if ( ! expB ) {
        --expDiff;
        sigExtra = 0;
        if ( ! expDiff ) goto newlyAlignedABigger;
    }
    sig128 = softfloat_shiftRightJam128( sigB, 0, expDiff );
    sigB = sig128.v64;
    sigExtra = sig128.v0;
 newlyAlignedABigger:
    expZ = expA;
 aBigger:
    sig128 = softfloat_sub128( sigA, 0, sigB, sigExtra );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 normRoundPack:
    return
        softfloat_normRoundPackToExtF80(
            signZ, expZ, sig128.v64, sig128.v0, extF80_roundingPrecision );
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

