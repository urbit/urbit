
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

#ifdef SOFTFLOAT_FAST_INT64

void f64_to_f128M( float64_t a, float128_t *zPtr )
{

    *zPtr = f64_to_f128( a );

}

#else

void f64_to_f128M( float64_t a, float128_t *zPtr )
{
    uint32_t *zWPtr;
    union ui64_f64 uA;
    uint64_t uiA;
    bool sign;
    int_fast16_t exp;
    uint64_t sig;
    struct commonNaN commonNaN;
    uint32_t uiZ96;
    struct exp16_sig64 normExpSig;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    zWPtr = (uint32_t *) zPtr;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uA.f = a;
    uiA = uA.ui;
    sign = signF64UI( uiA );
    exp  = expF64UI( uiA );
    sig  = fracF64UI( uiA );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    zWPtr[indexWord( 4, 0 )] = 0;
    if ( exp == 0x7FF ) {
        if ( sig ) {
            softfloat_f64UIToCommonNaN( uiA, &commonNaN );
            softfloat_commonNaNToF128M( &commonNaN, zWPtr );
            return;
        }
        uiZ96 = packToF128UI96( sign, 0x7FFF, 0 );
        goto uiZ;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( ! exp ) {
        if ( ! sig ) {
            uiZ96 = packToF128UI96( sign, 0, 0 );
            goto uiZ;
        }
        normExpSig = softfloat_normSubnormalF64Sig( sig );
        exp = normExpSig.exp - 1;
        sig = normExpSig.sig;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    zWPtr[indexWord( 4, 1 )] = (uint32_t) sig<<28;
    sig >>= 4;
    zWPtr[indexWordHi( 4 )] = packToF128UI96( sign, exp + 0x3C00, sig>>32 );
    zWPtr[indexWord( 4, 2 )] = sig;
    return;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 uiZ:
    zWPtr[indexWord( 4, 3 )] = uiZ96;
    zWPtr[indexWord( 4, 2 )] = 0;
    zWPtr[indexWord( 4, 1 )] = 0;

}

#endif

