
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

float32_t
 softfloat_subMagsF32( uint_fast32_t uiA, uint_fast32_t uiB, bool signZ )
{
    int_fast16_t expA;
    uint_fast32_t sigA;
    int_fast16_t expB;
    uint_fast32_t sigB;
    int_fast16_t expDiff;
    uint_fast32_t uiZ;
    int_fast16_t expZ;
    uint_fast32_t sigZ;
    union ui32_f32 uZ;

    expA = expF32UI( uiA );
    sigA = fracF32UI( uiA );
    expB = expF32UI( uiB );
    sigB = fracF32UI( uiB );
    expDiff = expA - expB;
    sigA <<= 7;
    sigB <<= 7;
    if ( 0 < expDiff ) goto expABigger;
    if ( expDiff < 0 ) goto expBBigger;
    if ( expA == 0xFF ) {
        if ( sigA | sigB ) goto propagateNaN;
        softfloat_raiseFlags( softfloat_flag_invalid );
        uiZ = defaultNaNF32UI;
        goto uiZ;
    }
    if ( ! expA ) {
        expA = 1;
        expB = 1;
    }
    if ( sigB < sigA ) goto aBigger;
    if ( sigA < sigB ) goto bBigger;
    uiZ = packToF32UI( softfloat_roundingMode == softfloat_round_min, 0, 0 );
    goto uiZ;
 expBBigger:
    if ( expB == 0xFF ) {
        if ( sigB ) goto propagateNaN;
        uiZ = packToF32UI( signZ ^ 1, 0xFF, 0 );
        goto uiZ;
    }
    sigA += expA ? 0x40000000 : sigA;
    sigA = softfloat_shiftRightJam32( sigA, -expDiff );
    sigB |= 0x40000000;
 bBigger:
    signZ ^= 1;
    expZ = expB;
    sigZ = sigB - sigA;
    goto normRoundPack;
 expABigger:
    if ( expA == 0xFF ) {
        if ( sigA ) goto propagateNaN;
        uiZ = uiA;
        goto uiZ;
    }
    sigB += expB ? 0x40000000 : sigB;
    sigB = softfloat_shiftRightJam32( sigB, expDiff );
    sigA |= 0x40000000;
 aBigger:
    expZ = expA;
    sigZ = sigA - sigB;
 normRoundPack:
    return softfloat_normRoundPackToF32( signZ, expZ - 1, sigZ );
 propagateNaN:
    uiZ = softfloat_propagateNaNF32UI( uiA, uiB );
 uiZ:
    uZ.ui = uiZ;
    return uZ.f;

}

