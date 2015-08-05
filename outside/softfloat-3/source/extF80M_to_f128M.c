
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

void extF80M_to_f128M( const extFloat80_t *aPtr, float128_t *zPtr )
{

    *zPtr = extF80_to_f128( *aPtr );

}

#else

void extF80M_to_f128M( const extFloat80_t *aPtr, float128_t *zPtr )
{
    const struct extFloat80M *aSPtr;
    uint32_t *zWPtr;
    uint_fast16_t uiA64;
    bool sign;
    int32_t exp;
    uint64_t sig;
    struct commonNaN commonNaN;
    uint32_t uiZ96;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    aSPtr = (const struct extFloat80M *) aPtr;
    zWPtr = (uint32_t *) zPtr;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uiA64 = aSPtr->signExp;
    sign = signExtF80UI64( uiA64 );
    exp  = expExtF80UI64( uiA64 );
    sig = aSPtr->signif;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    zWPtr[indexWord( 4, 0 )] = 0;
    if ( exp == 0x7FFF ) {
        if ( sig & UINT64_C( 0x7FFFFFFFFFFFFFFF ) ) {
            softfloat_extF80MToCommonNaN( aSPtr, &commonNaN );
            softfloat_commonNaNToF128M( &commonNaN, zWPtr );
            return;
        }
        uiZ96 = packToF128UI96( sign, 0x7FFF, 0 );
        goto uiZ;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( exp ) --exp;
    if ( ! (sig & UINT64_C( 0x8000000000000000 )) ) {
        if ( ! sig ) {
            uiZ96 = packToF128UI96( sign, 0, 0 );
            goto uiZ;
        }
        exp += softfloat_normExtF80SigM( &sig );
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    zWPtr[indexWord( 4, 1 )] = (uint32_t) sig<<17;
    sig >>= 15;
    zWPtr[indexWord( 4, 2 )] = sig;
    if ( exp < 0 ) {
        zWPtr[indexWordHi( 4 )] = sig>>32;
        softfloat_shiftRight96M(
            &zWPtr[indexMultiwordHi( 4, 3 )],
            -exp,
            &zWPtr[indexMultiwordHi( 4, 3 )]
        );
        exp = 0;
        sig = (uint64_t) zWPtr[indexWordHi( 4 )]<<32;
    }
    zWPtr[indexWordHi( 4 )] = packToF128UI96( sign, exp, sig>>32 );
    return;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 uiZ:
    zWPtr[indexWord( 4, 3 )] = uiZ96;
    zWPtr[indexWord( 4, 2 )] = 0;
    zWPtr[indexWord( 4, 1 )] = 0;

}

#endif

