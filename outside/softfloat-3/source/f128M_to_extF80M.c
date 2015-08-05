
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

void f128M_to_extF80M( const float128_t *aPtr, extFloat80_t *zPtr )
{

    *zPtr = f128_to_extF80( *aPtr );

}

#else

void f128M_to_extF80M( const float128_t *aPtr, extFloat80_t *zPtr )
{
    const uint32_t *aWPtr;
    struct extFloat80M *zSPtr;
    uint32_t uiA96;
    bool sign;
    int32_t exp;
    struct commonNaN commonNaN;
    uint32_t sig[4];

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    aWPtr = (const uint32_t *) aPtr;
    zSPtr = (struct extFloat80M *) zPtr;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uiA96 = aWPtr[indexWordHi( 4 )];
    sign = signF128UI96( uiA96 );
    exp  = expF128UI96( uiA96 );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( exp == 0x7FFF ) {
        if ( softfloat_isNaNF128M( aWPtr ) ) {
            softfloat_f128MToCommonNaN( aWPtr, &commonNaN );
            softfloat_commonNaNToExtF80M( &commonNaN, zSPtr );
            return;
        }
        zSPtr->signExp = packToExtF80UI64( sign, 0x7FFF );
        zSPtr->signif = UINT64_C( 0x8000000000000000 );
        return;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    exp = softfloat_shiftNormSigF128M( aWPtr, 15, sig );
    if ( exp == -128 ) {
        zSPtr->signExp = packToExtF80UI64( sign, 0 );
        zSPtr->signif = 0;
        return;
    }
    if ( sig[indexWord( 4, 0 )] ) sig[indexWord( 4, 1 )] |= 1;
    softfloat_roundPackMToExtF80M(
        sign, exp, &sig[indexMultiwordHi( 4, 3 )], 80, zSPtr );

}

#endif

