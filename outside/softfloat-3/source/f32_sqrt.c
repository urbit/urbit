
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

float32_t f32_sqrt( float32_t a )
{
    union ui32_f32 uA;
    uint_fast32_t uiA;
    bool signA;
    int_fast16_t expA;
    uint_fast32_t sigA, uiZ;
    struct exp16_sig32 normExpSig;
    int_fast16_t expZ;
    uint_fast32_t sigZ, shiftedSigZ;
    uint32_t negRem;
    union ui32_f32 uZ;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uA.f = a;
    uiA = uA.ui;
    signA = signF32UI( uiA );
    expA  = expF32UI( uiA );
    sigA  = fracF32UI( uiA );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( expA == 0xFF ) {
        if ( sigA ) {
            uiZ = softfloat_propagateNaNF32UI( uiA, 0 );
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
        normExpSig = softfloat_normSubnormalF32Sig( sigA );
        expA = normExpSig.exp;
        sigA = normExpSig.sig;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    expZ = ((expA - 0x7F)>>1) + 0x7E;
    expA &= 1;
    sigA = (sigA | 0x00800000)<<8;
    sigZ =
        ((uint_fast64_t) sigA * softfloat_approxRecipSqrt32_1( expA, sigA ))
            >>32;
    if ( expA ) sigZ >>= 1;
    sigZ += 2;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( (sigZ & 0x3F) < 2 ) {
        shiftedSigZ = sigZ>>2;
        negRem = shiftedSigZ * shiftedSigZ;
        sigZ = shiftedSigZ<<2;
        if ( negRem & 0x80000000 ) {
            sigZ |= 1;
        } else {
            if ( negRem ) --sigZ;
        }
    }
    return softfloat_roundPackToF32( 0, expZ, sigZ );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 invalid:
    softfloat_raiseFlags( softfloat_flag_invalid );
    uiZ = defaultNaNF32UI;
 uiZ:
    uZ.ui = uiZ;
    return uZ.f;

}

