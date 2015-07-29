
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

float64_t f64_sqrt( float64_t a )
{
    union ui64_f64 uA;
    uint_fast64_t uiA;
    bool signA;
    int_fast16_t expA;
    uint_fast64_t sigA, uiZ;
    struct exp16_sig64 normExpSig;
    int_fast16_t expZ;
    uint32_t sig32A, recipSqrt32, sig32Z;
    uint_fast64_t rem;
    uint32_t q;
    uint_fast64_t sigZ, shiftedSigZ;
    union ui64_f64 uZ;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uA.f = a;
    uiA = uA.ui;
    signA = signF64UI( uiA );
    expA  = expF64UI( uiA );
    sigA  = fracF64UI( uiA );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( expA == 0x7FF ) {
        if ( sigA ) {
            uiZ = softfloat_propagateNaNF64UI( uiA, 0 );
            goto uiZ;
        }
        if ( ! signA ) return a;
        goto invalid;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( signA ) {
        if ( ! (expA | sigA) ) return a;
        goto invalid;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( ! expA ) {
        if ( ! sigA ) return a;
        normExpSig = softfloat_normSubnormalF64Sig( sigA );
        expA = normExpSig.exp;
        sigA = normExpSig.sig;
    }
    /*------------------------------------------------------------------------
    | (`sig32Z' is guaranteed to be a lower bound on the square root of
    | `sig32A', which makes `sig32Z' also a lower bound on the square root of
    | `sigA'.)
    *------------------------------------------------------------------------*/
    expZ = ((expA - 0x3FF)>>1) + 0x3FE;
    expA &= 1;
    sigA |= UINT64_C( 0x0010000000000000 );
    sig32A = sigA>>21;
    recipSqrt32 = softfloat_approxRecipSqrt32_1( expA, sig32A );
    sig32Z = ((uint_fast64_t) sig32A * recipSqrt32)>>32;
    if ( expA ) {
        sigA <<= 8;
        sig32Z >>= 1;
    } else {
        sigA <<= 9;
    }
    rem = sigA - (uint_fast64_t) sig32Z * sig32Z;
    q = ((uint32_t) (rem>>2) * (uint_fast64_t) recipSqrt32)>>32;
    sigZ = ((uint_fast64_t) sig32Z<<32 | 1<<5) + ((uint_fast64_t) q<<3);
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( (sigZ & 0x1FF) < 1<<5 ) {
        sigZ &= ~(uint_fast64_t) 0x3F;
        shiftedSigZ = sigZ>>6;
        rem = (sigA<<52) - shiftedSigZ * shiftedSigZ;
        if ( rem & UINT64_C( 0x8000000000000000 ) ) {
            --sigZ;
        } else {
            if ( rem ) sigZ |= 1;
        }
    }
    return softfloat_roundPackToF64( 0, expZ, sigZ );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 invalid:
    softfloat_raiseFlags( softfloat_flag_invalid );
    uiZ = defaultNaNF64UI;
 uiZ:
    uZ.ui = uiZ;
    return uZ.f;

}

