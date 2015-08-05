
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

void
 f128M_rem( const float128_t *aPtr, const float128_t *bPtr, float128_t *zPtr )
{

    *zPtr = f128_rem( *aPtr, *bPtr );

}

#else

void
 f128M_rem( const float128_t *aPtr, const float128_t *bPtr, float128_t *zPtr )
{
    const uint32_t *aWPtr, *bWPtr;
    uint32_t *zWPtr, uiA96;
    int32_t expA, expB;
    uint32_t x[4], rem1[5], *remPtr;
    bool signRem;
    int32_t expDiff;
    uint32_t q, recip32;
    uint64_t q64;
    uint32_t rem2[5], *altRemPtr, *newRemPtr, wordMeanRem;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    aWPtr = (const uint32_t *) aPtr;
    bWPtr = (const uint32_t *) bPtr;
    zWPtr = (uint32_t *) zPtr;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uiA96 = aWPtr[indexWordHi( 4 )];
    expA = expF128UI96( uiA96 );
    expB = expF128UI96( bWPtr[indexWordHi( 4 )] );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( (expA == 0x7FFF) || (expB == 0x7FFF) ) {
        if ( softfloat_tryPropagateNaNF128M( aWPtr, bWPtr, zWPtr ) ) return;
        if ( expA == 0x7FFF ) goto invalid;
        goto copyA;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( expA < expB - 1 ) goto copyA;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    expB = softfloat_shiftNormSigF128M( bWPtr, 13, x );
    if ( expB == -128 ) goto invalid;
    remPtr = &rem1[indexMultiwordLo( 5, 4 )];
    expA = softfloat_shiftNormSigF128M( aWPtr, 13, remPtr );
    if ( expA == -128 ) goto copyA;
    signRem = signF128UI96( uiA96 );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    expDiff = expA - expB;
    if ( expDiff < 1 ) {
        if ( expDiff < -1 ) goto copyA;
        if ( expDiff ) {
            --expB;
            softfloat_add128M( x, x, x );
            q = 0;
        } else {
            q = (softfloat_compare128M( x, remPtr ) <= 0);
            if ( q ) softfloat_sub128M( remPtr, x, remPtr );
        }
    } else {
        recip32 =
            softfloat_approxRecip32_1(
                ((uint64_t) x[indexWord( 4, 3 )]<<32 | x[indexWord( 4, 2 )])
                    >>30
            );
        expDiff -= 30;
        for (;;) {
            q64 = (uint64_t) remPtr[indexWordHi( 4 )] * recip32;
            if ( expDiff < 0 ) break;
            q = (q64 + 0x80000000)>>32;
            softfloat_remStep128MBy32( remPtr, 29, x, q, remPtr );
            if ( remPtr[indexWordHi( 4 )] & 0x80000000 ) {
                softfloat_add128M( remPtr, x, remPtr );
            }
            expDiff -= 29;
        }
        /*--------------------------------------------------------------------
        | (`expDiff' cannot be less than -29 here.)
        *--------------------------------------------------------------------*/
        q = (uint32_t) (q64>>32)>>(~expDiff & 31);
        softfloat_remStep128MBy32( remPtr, expDiff + 30, x, q, remPtr );
        if ( remPtr[indexWordHi( 4 )] & 0x80000000 ) {
            altRemPtr = &rem2[indexMultiwordLo( 5, 4 )];
            softfloat_add128M( remPtr, x, altRemPtr );
            goto selectRem;
        }
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    altRemPtr = &rem2[indexMultiwordLo( 5, 4 )];
    do {
        ++q;
        newRemPtr = altRemPtr;
        softfloat_sub128M( remPtr, x, newRemPtr );
        altRemPtr = remPtr;
        remPtr = newRemPtr;
    } while ( ! (remPtr[indexWordHi( 4 )] & 0x80000000) );
 selectRem:
    softfloat_add128M( remPtr, altRemPtr, x );
    wordMeanRem = x[indexWordHi( 4 )];
    if (
        (wordMeanRem & 0x80000000)
            || (! wordMeanRem && (q & 1) && ! x[indexWord( 4, 0 )]
                    && ! (x[indexWord( 4, 2 )] | x[indexWord( 4, 1 )]))
    ) {
        remPtr = altRemPtr;
    }
    if ( remPtr[indexWordHi( 4 )] & 0x80000000 ) {
        signRem = ! signRem;
        softfloat_negX128M( remPtr );
    }
    remPtr -= indexMultiwordLo( 5, 4 );
    remPtr[indexWordHi( 5 )] = 0;
    softfloat_normRoundPackMToF128M( signRem, expB + 18, remPtr, zWPtr );
    return;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 invalid:
    softfloat_invalidF128M( zWPtr );
    return;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 copyA:
    zWPtr[indexWordHi( 4 )] = uiA96;
    zWPtr[indexWord( 4, 2 )] = aWPtr[indexWord( 4, 2 )];
    zWPtr[indexWord( 4, 1 )] = aWPtr[indexWord( 4, 1 )];
    zWPtr[indexWord( 4, 0 )] = aWPtr[indexWord( 4, 0 )];

}

#endif

