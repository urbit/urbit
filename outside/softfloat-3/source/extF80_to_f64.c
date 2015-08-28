
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

float64_t extF80_to_f64( extFloat80_t a )
{
    union { struct extFloat80M s; extFloat80_t f; } uA;
    uint_fast16_t uiA64;
    uint_fast64_t uiA0;
    bool sign;
    int_fast32_t exp;
    uint_fast64_t sig;
    struct commonNaN commonNaN;
    uint_fast64_t uiZ;
    union ui64_f64 uZ;

    uA.f = a;
    uiA64 = uA.s.signExp;
    uiA0  = uA.s.signif;
    sign = signExtF80UI64( uiA64 );
    exp  = expExtF80UI64( uiA64 );
    sig  = uiA0;
    if ( ! (exp | sig) ) {
        uiZ = packToF64UI( sign, 0, 0 );
        goto uiZ;
    }
    if ( exp == 0x7FFF ) {
        if ( sig & UINT64_C( 0x7FFFFFFFFFFFFFFF ) ) {
            softfloat_extF80UIToCommonNaN( uiA64, uiA0, &commonNaN );
            uiZ = softfloat_commonNaNToF64UI( &commonNaN );
        } else {
            uiZ = packToF64UI( sign, 0x7FF, 0 );
        }
        goto uiZ;
    }
    sig = softfloat_shortShiftRightJam64( sig, 1 );
    exp -= 0x3C01;
    if ( sizeof (int_fast16_t) < sizeof (int_fast32_t) ) {
        if ( exp < -0x1000 ) exp = -0x1000;
    }
    return softfloat_roundPackToF64( sign, exp, sig );
 uiZ:
    uZ.ui = uiZ;
    return uZ.f;

}

