
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

float128_t f128_rem( float128_t a, float128_t b )
{
    union ui128_f128 uA;
    uint_fast64_t uiA64, uiA0;
    bool signA;
    int_fast32_t expA;
    struct uint128 sigA;
    union ui128_f128 uB;
    uint_fast64_t uiB64, uiB0;
    bool signB;
    int_fast32_t expB;
    struct uint128 sigB;
    struct exp32_sig128 normExpSig;
    struct uint128 rem;
    int_fast32_t expDiff;
    uint_fast32_t q, recip32;
    uint_fast64_t q64;
    struct uint128 term, altRem, meanRem;
    bool signRem;
    struct uint128 uiZ;
    union ui128_f128 uZ;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uA.f = a;
    uiA64 = uA.ui.v64;
    uiA0  = uA.ui.v0;
    signA = signF128UI64( uiA64 );
    expA  = expF128UI64( uiA64 );
    sigA.v64 = fracF128UI64( uiA64 );
    sigA.v0  = uiA0;
    uB.f = b;
    uiB64 = uB.ui.v64;
    uiB0  = uB.ui.v0;
    signB = signF128UI64( uiB64 );
    expB  = expF128UI64( uiB64 );
    sigB.v64 = fracF128UI64( uiB64 );
    sigB.v0  = uiB0;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( expA == 0x7FFF ) {
        if (
            (sigA.v64 | sigA.v0) || ((expB == 0x7FFF) && (sigB.v64 | sigB.v0))
        ) {
            goto propagateNaN;
        }
        goto invalid;
    }
    if ( expB == 0x7FFF ) {
        if ( sigB.v64 | sigB.v0 ) goto propagateNaN;
        return a;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( ! expB ) {
        if ( ! (sigB.v64 | sigB.v0) ) goto invalid;
        normExpSig = softfloat_normSubnormalF128Sig( sigB.v64, sigB.v0 );
        expB = normExpSig.exp;
        sigB = normExpSig.sig;
    }
    if ( ! expA ) {
        if ( ! (sigA.v64 | sigA.v0) ) return a;
        normExpSig = softfloat_normSubnormalF128Sig( sigA.v64, sigA.v0 );
        expA = normExpSig.exp;
        sigA = normExpSig.sig;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    sigA.v64 |= UINT64_C( 0x0001000000000000 );
    sigB.v64 |= UINT64_C( 0x0001000000000000 );
    rem = sigA;
    expDiff = expA - expB;
    if ( expDiff < 1 ) {
        if ( expDiff < -1 ) return a;
        if ( expDiff ) {
            --expB;
            sigB = softfloat_add128( sigB.v64, sigB.v0, sigB.v64, sigB.v0 );
            q = 0;
        } else {
            q = softfloat_le128( sigB.v64, sigB.v0, rem.v64, rem.v0 );
            if ( q ) {
                rem = softfloat_sub128( rem.v64, rem.v0, sigB.v64, sigB.v0 );
            }
        }
    } else {
        recip32 = softfloat_approxRecip32_1( sigB.v64>>17 );
        expDiff -= 30;
        for (;;) {
            q64 = (uint_fast64_t) (uint32_t) (rem.v64>>19) * recip32;
            if ( expDiff < 0 ) break;
            q = (q64 + 0x80000000)>>32;
            rem = softfloat_shortShiftLeft128( rem.v64, rem.v0, 29 );
            term = softfloat_mul128By32( sigB.v64, sigB.v0, q );
            rem = softfloat_sub128( rem.v64, rem.v0, term.v64, term.v0 );
            if ( rem.v64 & UINT64_C( 0x8000000000000000 ) ) {
                rem = softfloat_add128( rem.v64, rem.v0, sigB.v64, sigB.v0 );
            }
            expDiff -= 29;
        }
        /*--------------------------------------------------------------------
        | (`expDiff' cannot be less than -29 here.)
        *--------------------------------------------------------------------*/
        q = (uint32_t) (q64>>32)>>(~expDiff & 31);
        rem = softfloat_shortShiftLeft128( rem.v64, rem.v0, expDiff + 30 );
        term = softfloat_mul128By32( sigB.v64, sigB.v0, q );
        rem = softfloat_sub128( rem.v64, rem.v0, term.v64, term.v0 );
        if ( rem.v64 & UINT64_C( 0x8000000000000000 ) ) {
            altRem = softfloat_add128( rem.v64, rem.v0, sigB.v64, sigB.v0 );
            goto selectRem;
        }
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    do {
        altRem = rem;
        ++q;
        rem = softfloat_sub128( rem.v64, rem.v0, sigB.v64, sigB.v0 );
    } while ( ! (rem.v64 & UINT64_C( 0x8000000000000000 )) );
 selectRem:
    meanRem = softfloat_add128( rem.v64, rem.v0, altRem.v64, altRem.v0 );
    if (
        (meanRem.v64 & UINT64_C( 0x8000000000000000 ))
            || (! (meanRem.v64 | meanRem.v0) && (q & 1))
    ) {
        rem = altRem;
    }
    signRem = signA;
    if ( rem.v64 & UINT64_C( 0x8000000000000000 ) ) {
        signRem = ! signRem;
        rem = softfloat_sub128( 0, 0, rem.v64, rem.v0 );
    }
    return softfloat_normRoundPackToF128( signRem, expB - 1, rem.v64, rem.v0 );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 propagateNaN:
    uiZ = softfloat_propagateNaNF128UI( uiA64, uiA0, uiB64, uiB0 );
    goto uiZ;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 invalid:
    softfloat_raiseFlags( softfloat_flag_invalid );
    uiZ.v64 = defaultNaNF128UI64;
    uiZ.v0  = defaultNaNF128UI0;
 uiZ:
    uZ.ui = uiZ;
    return uZ.f;

}

