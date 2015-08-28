
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

float128_t f32_to_f128( float32_t a )
{
    union ui32_f32 uA;
    uint_fast32_t uiA;
    bool sign;
    int_fast16_t exp;
    uint_fast32_t sig;
    struct commonNaN commonNaN;
    struct uint128 uiZ;
    struct exp16_sig32 normExpSig;
    union ui128_f128 uZ;

    uA.f = a;
    uiA = uA.ui;
    sign = signF32UI( uiA );
    exp  = expF32UI( uiA );
    sig  = fracF32UI( uiA );
    if ( exp == 0xFF ) {
        if ( sig ) {
            softfloat_f32UIToCommonNaN( uiA, &commonNaN );
            uiZ = softfloat_commonNaNToF128UI( &commonNaN );
        } else {
            uiZ.v64 = packToF128UI64( sign, 0x7FFF, 0 );
            uiZ.v0  = 0;
        }
        goto uiZ;
    }
    if ( ! exp ) {
        if ( ! sig ) {
            uiZ.v64 = packToF128UI64( sign, 0, 0 );
            uiZ.v0  = 0;
            goto uiZ;
        }
        normExpSig = softfloat_normSubnormalF32Sig( sig );
        exp = normExpSig.exp - 1;
        sig = normExpSig.sig;
    }
    uiZ.v64 = packToF128UI64( sign, exp + 0x3F80, (uint_fast64_t) sig<<25 );
    uiZ.v0  = 0;
 uiZ:
    uZ.ui = uiZ;
    return uZ.f;

}

