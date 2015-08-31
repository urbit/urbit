
/*============================================================================

This C source file is part of the SoftFloat IEEE Floating-Point Arithmetic
Package, Release 3, by John R. Hauser.

Copyright 2011, 2012, 2013, 2014, 2015 The Regents of the University of
California (Regents).  All Rights Reserved.  Redistribution and use in source
and binary forms, with or without modification, are permitted provided that
the following conditions are met:

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

extFloat80_t extF80_sqrt( extFloat80_t a )
{
    union { struct extFloat80M s; extFloat80_t f; } uA;
    uint_fast16_t uiA64;
    uint_fast64_t uiA0;
    bool signA;
    int_fast32_t expA;
    uint_fast64_t sigA;
    struct uint128 uiZ;
    uint_fast16_t uiZ64;
    uint_fast64_t uiZ0;
    struct exp32_sig64 normExpSig;
    int_fast32_t expZ;
    uint_fast32_t sig32A, recipSqrt32, sig32Z;
    struct uint128 rem;
    uint_fast64_t q, sigZ, x64;
    struct uint128 term;
    uint_fast64_t sigZExtra;
    union { struct extFloat80M s; extFloat80_t f; } uZ;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uA.f = a;
    uiA64 = uA.s.signExp;
    uiA0  = uA.s.signif;
    signA = signExtF80UI64( uiA64 );
    expA  = expExtF80UI64( uiA64 );
    sigA  = uiA0;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( expA == 0x7FFF ) {
        if ( sigA & UINT64_C( 0x7FFFFFFFFFFFFFFF ) ) {
            uiZ = softfloat_propagateNaNExtF80UI( uiA64, uiA0, 0, 0 );
            uiZ64 = uiZ.v64;
            uiZ0  = uiZ.v0;
            goto uiZ;
        }
        if ( ! signA ) return a;
        goto invalid;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( signA ) {
        if ( ! sigA ) goto zero;
        goto invalid;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( ! expA ) expA = 1;
    if ( ! (sigA & UINT64_C( 0x8000000000000000 )) ) {
        if ( ! sigA ) goto zero;
        normExpSig = softfloat_normSubnormalExtF80Sig( sigA );
        expA += normExpSig.exp;
        sigA = normExpSig.sig;
    }
    /*------------------------------------------------------------------------
    | (`sig32Z' is guaranteed to be a lower bound on the square root of
    | `sig32A', which makes `sig32Z' also a lower bound on the square root of
    | `sigA'.)
    *------------------------------------------------------------------------*/
    expZ = ((expA - 0x3FFF)>>1) + 0x3FFF;
    expA &= 1;
    sig32A = sigA>>32;
    recipSqrt32 = softfloat_approxRecipSqrt32_1( expA, sig32A );
    sig32Z = ((uint_fast64_t) sig32A * recipSqrt32)>>32;
    if ( expA ) {
        sig32Z >>= 1;
        rem = softfloat_shortShiftLeft128( 0, sigA, 61 );
    } else {
        rem = softfloat_shortShiftLeft128( 0, sigA, 62 );
    }
    rem.v64 -= (uint_fast64_t) sig32Z * sig32Z;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    q = ((uint_fast64_t) (uint32_t) (rem.v64>>2) * recipSqrt32)>>32;
    sigZ = ((uint_fast64_t) sig32Z<<32) + (q<<3);
    x64 = ((uint_fast64_t) sig32Z<<32) + sigZ;
    term = softfloat_mul64ByShifted32To128( x64, q );
    rem = softfloat_shortShiftLeft128( rem.v64, rem.v0, 29 );
    rem = softfloat_sub128( rem.v64, rem.v0, term.v64, term.v0 );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    q = (((uint_fast64_t) (uint32_t) (rem.v64>>2) * recipSqrt32)>>32) + 2;
    x64 = sigZ;
    sigZ = (sigZ<<1) + (q>>25);
    sigZExtra = (uint64_t) (q<<39);
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( (q & 0xFFFFFF) <= 2 ) {
        q &= ~(uint_fast64_t) 0xFFFF;
        sigZExtra = (uint64_t) (q<<39);
        term = softfloat_mul64ByShifted32To128( x64 + (q>>27), q );
        x64 = (uint_fast64_t) (uint32_t) (q<<5) * (uint32_t) q;
        term = softfloat_add128( term.v64, term.v0, 0, x64 );
        rem = softfloat_shortShiftLeft128( rem.v64, rem.v0, 28 );
        rem = softfloat_sub128( rem.v64, rem.v0, term.v64, term.v0 );
        if ( rem.v64 & UINT64_C( 0x8000000000000000 ) ) {
            if ( ! sigZExtra ) --sigZ;
            --sigZExtra;
        } else {
            if ( rem.v64 | rem.v0 ) sigZExtra |= 1;
        }
    }
    return
        softfloat_roundPackToExtF80(
            0, expZ, sigZ, sigZExtra, extF80_roundingPrecision );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 invalid:
    softfloat_raiseFlags( softfloat_flag_invalid );
    uiZ64 = defaultNaNExtF80UI64;
    uiZ0  = defaultNaNExtF80UI0;
    goto uiZ;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 zero:
    uiZ64 = packToExtF80UI64( signA, 0 );
    uiZ0  = 0;
 uiZ:
    uZ.s.signExp = uiZ64;
    uZ.s.signif  = uiZ0;
    return uZ.f;

}

