
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
 softfloat_subMagsF128(
     uint_fast64_t uiA64,
     uint_fast64_t uiA0,
     uint_fast64_t uiB64,
     uint_fast64_t uiB0,
     bool signZ
 )
{
    int_fast32_t expA;
    struct uint128 sigA;
    int_fast32_t expB;
    struct uint128 sigB, sigZ;
    int_fast32_t expDiff, expZ;
    struct uint128 uiZ;
    union ui128_f128 uZ;

    expA = expF128UI64( uiA64 );
    sigA.v64 = fracF128UI64( uiA64 );
    sigA.v0  = uiA0;
    expB = expF128UI64( uiB64 );
    sigB.v64 = fracF128UI64( uiB64 );
    sigB.v0  = uiB0;
    sigA = softfloat_shortShiftLeft128( sigA.v64, sigA.v0, 4 );
    sigB = softfloat_shortShiftLeft128( sigB.v64, sigB.v0, 4 );
    expDiff = expA - expB;
    if ( 0 < expDiff ) goto expABigger;
    if ( expDiff < 0 ) goto expBBigger;
    if ( expA == 0x7FFF ) {
        if ( sigA.v64 | sigA.v0 | sigB.v64 | sigB.v0 ) goto propagateNaN;
        softfloat_raiseFlags( softfloat_flag_invalid );
        uiZ.v64 = defaultNaNF128UI64;
        uiZ.v0  = defaultNaNF128UI0;
        goto uiZ;
    }
    expZ = expA;
    if ( ! expZ ) expZ = 1;
    if ( sigB.v64 < sigA.v64 ) goto aBigger;
    if ( sigA.v64 < sigB.v64 ) goto bBigger;
    if ( sigB.v0 < sigA.v0 ) goto aBigger;
    if ( sigA.v0 < sigB.v0 ) goto bBigger;
    uiZ.v64 =
        packToF128UI64(
            (softfloat_roundingMode == softfloat_round_min), 0, 0 );
    uiZ.v0 = 0;
    goto uiZ;
 expBBigger:
    if ( expB == 0x7FFF ) {
        if ( sigB.v64 | sigB.v0 ) goto propagateNaN;
        uiZ.v64 = packToF128UI64( signZ ^ 1, 0x7FFF, 0 );
        uiZ.v0  = 0;
        goto uiZ;
    }
    if ( expA ) {
        sigA.v64 |= UINT64_C( 0x0010000000000000 );
    } else {
        ++expDiff;
        if ( ! expDiff ) goto newlyAlignedBBigger;
    }
    sigA = softfloat_shiftRightJam128( sigA.v64, sigA.v0, -expDiff );
 newlyAlignedBBigger:
    expZ = expB;
    sigB.v64 |= UINT64_C( 0x0010000000000000 );
 bBigger:
    signZ ^= 1;
    sigZ = softfloat_sub128( sigB.v64, sigB.v0, sigA.v64, sigA.v0 );
    goto normRoundPack;
 expABigger:
    if ( expA == 0x7FFF ) {
        if ( sigA.v64 | sigA.v0 ) goto propagateNaN;
        uiZ.v64 = uiA64;
        uiZ.v0  = uiA0;
        goto uiZ;
    }
    if ( expB ) {
        sigB.v64 |= UINT64_C( 0x0010000000000000 );
    } else {
        --expDiff;
        if ( ! expDiff ) goto newlyAlignedABigger;
    }
    sigB = softfloat_shiftRightJam128( sigB.v64, sigB.v0, expDiff );
 newlyAlignedABigger:
    expZ = expA;
    sigA.v64 |= UINT64_C( 0x0010000000000000 );
 aBigger:
    sigZ = softfloat_sub128( sigA.v64, sigA.v0, sigB.v64, sigB.v0 );
 normRoundPack:
    return softfloat_normRoundPackToF128( signZ, expZ - 5, sigZ.v64, sigZ.v0 );
 propagateNaN:
    uiZ = softfloat_propagateNaNF128UI( uiA64, uiA0, uiB64, uiB0 );
 uiZ:
    uZ.ui = uiZ;
    return uZ.f;

}

