
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

float128_t f128_sqrt( float128_t a )
{
    union ui128_f128 uA;
    uint_fast64_t uiA64, uiA0;
    bool signA;
    int_fast32_t expA;
    struct uint128 sigA, uiZ;
    struct exp32_sig128 normExpSig;
    int_fast32_t expZ;
    uint_fast32_t sig32A, recipSqrt32, sig32Z;
    struct uint128 rem;
    uint32_t qs[3];
    uint_fast32_t q;
    uint_fast64_t x64, sig64Z;
    struct uint128 term, y;
    uint_fast64_t sigZExtra;
    struct uint128 sigZ;
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
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( expA == 0x7FFF ) {
        if ( sigA.v64 | sigA.v0 ) {
            uiZ = softfloat_propagateNaNF128UI( uiA64, uiA0, 0, 0 );
            goto uiZ;
        }
        if ( ! signA ) return a;
        goto invalid;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( signA ) {
        if ( ! (expA | sigA.v64 | sigA.v0) ) return a;
        goto invalid;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( ! expA ) {
        if ( ! (sigA.v64 | sigA.v0) ) return a;
        normExpSig = softfloat_normSubnormalF128Sig( sigA.v64, sigA.v0 );
        expA = normExpSig.exp;
        sigA = normExpSig.sig;
    }
    /*------------------------------------------------------------------------
    | (`sig32Z' is guaranteed to be a lower bound on the square root of
    | `sig32A', which makes `sig32Z' also a lower bound on the square root of
    | `sigA'.)
    *------------------------------------------------------------------------*/
    expZ = ((expA - 0x3FFF)>>1) + 0x3FFE;
    expA &= 1;
    sigA.v64 |= UINT64_C( 0x0001000000000000 );
    sig32A = sigA.v64>>17;
    recipSqrt32 = softfloat_approxRecipSqrt32_1( expA, sig32A );
    sig32Z = ((uint_fast64_t) sig32A * recipSqrt32)>>32;
    if ( expA ) {
        sig32Z >>= 1;
        rem = softfloat_shortShiftLeft128( sigA.v64, sigA.v0, 12 );
    } else {
        rem = softfloat_shortShiftLeft128( sigA.v64, sigA.v0, 13 );
    }
    qs[2] = sig32Z;
    rem.v64 -= (uint_fast64_t) sig32Z * sig32Z;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    q = ((uint_fast64_t) (uint32_t) (rem.v64>>2) * recipSqrt32)>>32;
    qs[1] = q;
    x64 = (uint_fast64_t) sig32Z<<32;
    sig64Z = x64 + ((uint_fast64_t) q<<3);
    x64 += sig64Z;
    rem = softfloat_shortShiftLeft128( rem.v64, rem.v0, 29 );
    term = softfloat_mul64ByShifted32To128( x64, q );
    rem = softfloat_sub128( rem.v64, rem.v0, term.v64, term.v0 );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    q = ((uint_fast64_t) (uint32_t) (rem.v64>>2) * recipSqrt32)>>32;
    y = softfloat_shortShiftLeft128( rem.v64, rem.v0, 29 );
    sig64Z <<= 1;
    /*------------------------------------------------------------------------
    | (Repeating this loop is a rare occurrence.)
    *------------------------------------------------------------------------*/
    for (;;) {
        term = softfloat_shortShiftLeft128( 0, sig64Z, 32 );
        term = softfloat_add128( term.v64, term.v0, 0, (uint_fast64_t) q<<6 );
        term = softfloat_mul128By32( term.v64, term.v0, q );
        rem = softfloat_sub128( y.v64, y.v0, term.v64, term.v0 );
        if ( ! (rem.v64 & UINT64_C( 0x8000000000000000 )) ) break;
        --q;
    }
    qs[0] = q;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    q = (((uint_fast64_t) (uint32_t) (rem.v64>>2) * recipSqrt32)>>32) + 2;
    sigZExtra = (uint64_t) ((uint_fast64_t) q<<59);
    term = softfloat_shortShiftLeft128( 0, qs[1], 53 );
    sigZ =
        softfloat_add128(
            (uint_fast64_t) qs[2]<<18, ((uint_fast64_t) qs[0]<<24) + (q>>5),
            term.v64, term.v0
        );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( (q & 0xF) <= 2 ) {
        q &= ~3;
        sigZExtra = (uint64_t) ((uint_fast64_t) q<<59);
        y = softfloat_shortShiftLeft128( sigZ.v64, sigZ.v0, 6 );
        y.v0 |= sigZExtra>>58;
        term = softfloat_sub128( y.v64, y.v0, 0, q );
        y    = softfloat_mul64ByShifted32To128( term.v0,  q );
        term = softfloat_mul64ByShifted32To128( term.v64, q );
        term = softfloat_add128( term.v64, term.v0, 0, y.v64 );
        rem = softfloat_shortShiftLeft128( rem.v64, rem.v0, 20 );
        term = softfloat_sub128( term.v64, term.v0, rem.v64, rem.v0 );
        /*--------------------------------------------------------------------
        | The concatenation of `term' and `y.v0' is now the negative remainder
        | (3 words altogether).
        *--------------------------------------------------------------------*/
        if ( term.v64 & UINT64_C( 0x8000000000000000 ) ) {
            sigZExtra |= 1;
        } else {
            if ( term.v64 | term.v0 | y.v0 ) {
                if ( sigZExtra ) {
                    --sigZExtra;
                } else {
                    sigZ = softfloat_sub128( sigZ.v64, sigZ.v0, 0, 1 );
                    sigZExtra = ~0;
                }
            }
        }
    }
    return softfloat_roundPackToF128( 0, expZ, sigZ.v64, sigZ.v0, sigZExtra );
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

