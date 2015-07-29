
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
 f128M_div( const float128_t *aPtr, const float128_t *bPtr, float128_t *zPtr )
{

    *zPtr = f128_div( *aPtr, *bPtr );

}

#else

void
 f128M_div( const float128_t *aPtr, const float128_t *bPtr, float128_t *zPtr )
{
    const uint32_t *aWPtr, *bWPtr;
    uint32_t *zWPtr, uiA96;
    bool signA;
    int32_t expA;
    uint32_t uiB96;
    bool signB;
    int32_t expB;
    bool signZ;
    uint32_t y[5], sigB[4];
    int32_t expZ;
    uint32_t recip32;
    int ix;
    uint64_t q64;
    uint32_t q, qs[3], uiZ96;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    aWPtr = (const uint32_t *) aPtr;
    bWPtr = (const uint32_t *) bPtr;
    zWPtr = (uint32_t *) zPtr;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uiA96 = aWPtr[indexWordHi( 4 )];
    signA = signF128UI96( uiA96 );
    expA  = expF128UI96( uiA96 );
    uiB96 = bWPtr[indexWordHi( 4 )];
    signB = signF128UI96( uiB96 );
    expB  = expF128UI96( uiB96 );
    signZ = signA ^ signB;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( (expA == 0x7FFF) || (expB == 0x7FFF) ) {
        if ( softfloat_tryPropagateNaNF128M( aWPtr, bWPtr, zWPtr ) ) return;
        if ( expA == 0x7FFF ) {
            if ( expB == 0x7FFF ) goto invalid;
            goto infinity;
        }
        goto zero;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    expA = softfloat_shiftNormSigF128M( aWPtr, 13, y );
    expB = softfloat_shiftNormSigF128M( bWPtr, 13, sigB );
    if ( expA == -128 ) {
        if ( expB == -128 ) goto invalid;
        goto zero;
    }
    if ( expB == -128 ) {
        softfloat_raiseFlags( softfloat_flag_infinite );
        goto infinity;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    expZ = expA - expB + 0x3FFE;
    if ( softfloat_compare128M( y, sigB ) < 0 ) {
        --expZ;
        softfloat_add128M( y, y, y );
    }
    recip32 =
        softfloat_approxRecip32_1(
            ((uint64_t) sigB[indexWord( 4, 3 )]<<32 | sigB[indexWord( 4, 2 )])
                >>30
        );
    ix = 3;
    for (;;) {
        q64 = (uint64_t) y[indexWordHi( 4 )] * recip32;
        q = (q64 + 0x80000000)>>32;
        --ix;
        if ( ix < 0 ) break;
        softfloat_remStep128MBy32( y, 29, sigB, q, y );
        if ( y[indexWordHi( 4 )] & 0x80000000 ) {
            --q;
            softfloat_add128M( y, sigB, y );
        }
        qs[ix] = q;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( ((q + 1) & 7) < 2 ) {
        softfloat_remStep128MBy32( y, 29, sigB, q, y );
        if ( y[indexWordHi( 4 )] & 0x80000000 ) {
            --q;
            softfloat_add128M( y, sigB, y );
        } else if ( softfloat_compare128M( sigB, y ) <= 0 ) {
            ++q;
            softfloat_sub128M( y, sigB, y );
        }
        if (
            y[indexWordLo( 4 )] || y[indexWord( 4, 1 )]
                || (y[indexWord( 4, 2 )] | y[indexWord( 4, 3 )])
        ) {
            q |= 1;
        }
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    q64 = (uint64_t) q<<28;
    y[indexWord( 5, 0 )] = q64;
    q64 = ((uint64_t) qs[0]<<25) + (q64>>32);
    y[indexWord( 5, 1 )] = q64;
    q64 = ((uint64_t) qs[1]<<22) + (q64>>32);
    y[indexWord( 5, 2 )] = q64;
    q64 = ((uint64_t) qs[2]<<19) + (q64>>32);
    y[indexWord( 5, 3 )] = q64;
    y[indexWord( 5, 4 )] = q64>>32;
    softfloat_roundPackMToF128M( signZ, expZ, y, zWPtr );
    return;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 invalid:
    softfloat_invalidF128M( zWPtr );
    return;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 infinity:
    uiZ96 = packToF128UI96( signZ, 0x7FFF, 0 );
    goto uiZ96;
 zero:
    uiZ96 = packToF128UI96( signZ, 0, 0 );
 uiZ96:
    zWPtr[indexWordHi( 4 )] = uiZ96;
    zWPtr[indexWord( 4, 2 )] = 0;
    zWPtr[indexWord( 4, 1 )] = 0;
    zWPtr[indexWord( 4, 0 )] = 0;

}

#endif

