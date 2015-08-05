
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

float128_t
 softfloat_addMagsF128(
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
    struct uint128 sigB;
    int_fast32_t expDiff;
    struct uint128 uiZ, sigZ;
    int_fast32_t expZ;
    uint_fast64_t sigZExtra;
    struct uint128_extra sig128Extra;
    union ui128_f128 uZ;

    expA = expF128UI64( uiA64 );
    sigA.v64 = fracF128UI64( uiA64 );
    sigA.v0  = uiA0;
    expB = expF128UI64( uiB64 );
    sigB.v64 = fracF128UI64( uiB64 );
    sigB.v0  = uiB0;
    expDiff = expA - expB;
    if ( ! expDiff ) {
        if ( expA == 0x7FFF ) {
            if ( sigA.v64 | sigA.v0 | sigB.v64 | sigB.v0 ) goto propagateNaN;
            uiZ.v64 = uiA64;
            uiZ.v0  = uiA0;
            goto uiZ;
        }
        sigZ = softfloat_add128( sigA.v64, sigA.v0, sigB.v64, sigB.v0 );
        if ( ! expA ) {
            uiZ.v64 = packToF128UI64( signZ, 0, sigZ.v64 );
            uiZ.v0  = sigZ.v0;
            goto uiZ;
        }
        expZ = expA;
        sigZ.v64 |= UINT64_C( 0x0002000000000000 );
        sigZExtra = 0;
        goto shiftRight1;
    }
    if ( expDiff < 0 ) {
        if ( expB == 0x7FFF ) {
            if ( sigB.v64 | sigB.v0 ) goto propagateNaN;
            uiZ.v64 = packToF128UI64( signZ, 0x7FFF, 0 );
            uiZ.v0  = 0;
            goto uiZ;
        }
        expZ = expB;
        if ( expA ) {
            sigA.v64 |= UINT64_C( 0x0001000000000000 );
        } else {
            ++expDiff;
            sigZExtra = 0;
            if ( ! expDiff ) goto newlyAligned;
        }
        sig128Extra =
            softfloat_shiftRightJam128Extra( sigA.v64, sigA.v0, 0, -expDiff );
        sigA = sig128Extra.v;
        sigZExtra = sig128Extra.extra;
    } else {
        if ( expA == 0x7FFF ) {
            if ( sigA.v64 | sigA.v0 ) goto propagateNaN;
            uiZ.v64 = uiA64;
            uiZ.v0  = uiA0;
            goto uiZ;
        }
        expZ = expA;
        if ( expB ) {
            sigB.v64 |= UINT64_C( 0x0001000000000000 );
        } else {
            --expDiff;
            sigZExtra = 0;
            if ( ! expDiff ) goto newlyAligned;
        }
        sig128Extra =
            softfloat_shiftRightJam128Extra( sigB.v64, sigB.v0, 0, expDiff );
        sigB = sig128Extra.v;
        sigZExtra = sig128Extra.extra;
    }
 newlyAligned:
    sigZ =
        softfloat_add128(
            sigA.v64 | UINT64_C( 0x0001000000000000 ),
            sigA.v0,
            sigB.v64,
            sigB.v0
        );
    --expZ;
    if ( sigZ.v64 < UINT64_C( 0x0002000000000000 ) ) goto roundAndPack;
    ++expZ;
 shiftRight1:
    sig128Extra =
        softfloat_shortShiftRightJam128Extra(
            sigZ.v64, sigZ.v0, sigZExtra, 1 );
    sigZ = sig128Extra.v;
    sigZExtra = sig128Extra.extra;
 roundAndPack:
    return
        softfloat_roundPackToF128( signZ, expZ, sigZ.v64, sigZ.v0, sigZExtra );
 propagateNaN:
    uiZ = softfloat_propagateNaNF128UI( uiA64, uiA0, uiB64, uiB0 );
 uiZ:
    uZ.ui = uiZ;
    return uZ.f;

}

