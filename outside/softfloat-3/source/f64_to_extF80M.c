
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

void f64_to_extF80M( float64_t a, extFloat80_t *zPtr )
{

    *zPtr = f64_to_extF80( a );

}

#else

void f64_to_extF80M( float64_t a, extFloat80_t *zPtr )
{
    struct extFloat80M *zSPtr;
    union ui64_f64 uA;
    uint64_t uiA;
    bool sign;
    int_fast16_t exp;
    uint64_t sig;
    struct commonNaN commonNaN;
    uint_fast16_t uiZ64;
    uint64_t uiZ0;
    struct exp16_sig64 normExpSig;

    zSPtr = (struct extFloat80M *) zPtr;
    uA.f = a;
    uiA = uA.ui;
    sign = signF64UI( uiA );
    exp  = expF64UI( uiA );
    sig  = fracF64UI( uiA );
    if ( exp == 0x7FF ) {
        if ( sig ) {
            softfloat_f64UIToCommonNaN( uiA, &commonNaN );
            softfloat_commonNaNToExtF80M( &commonNaN, zSPtr );
            return;
        }
        uiZ64 = packToExtF80UI64( sign, 0x7FFF );
        uiZ0  = UINT64_C( 0x8000000000000000 );
        goto uiZ;
    }
    if ( ! exp ) {
        if ( ! sig ) {
            uiZ64 = packToExtF80UI64( sign, 0 );
            uiZ0  = 0;
            goto uiZ;
        }
        normExpSig = softfloat_normSubnormalF64Sig( sig );
        exp = normExpSig.exp;
        sig = normExpSig.sig;
    }
    uiZ64 = packToExtF80UI64( sign, exp + 0x3C00 );
    uiZ0  = UINT64_C( 0x8000000000000000 ) | sig<<11;
 uiZ:
    zSPtr->signExp = uiZ64;
    zSPtr->signif  = uiZ0;

}

#endif

