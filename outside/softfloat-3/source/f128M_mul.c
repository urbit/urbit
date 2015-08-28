
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
 f128M_mul( const float128_t *aPtr, const float128_t *bPtr, float128_t *zPtr )
{

    *zPtr = f128_mul( *aPtr, *bPtr );

}

#else

void
 f128M_mul( const float128_t *aPtr, const float128_t *bPtr, float128_t *zPtr )
{
    const uint32_t *aWPtr, *bWPtr;
    uint32_t *zWPtr;
    uint32_t uiA96;
    int32_t expA;
    uint32_t uiB96;
    int32_t expB;
    bool signZ;
    const uint32_t *ptr;
    uint32_t uiZ96, sigA[4];
    uint_fast8_t shiftCount;
    uint32_t sigB[4];
    int32_t expZ;
    uint32_t sigProd[8], *extSigZPtr;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    aWPtr = (const uint32_t *) aPtr;
    bWPtr = (const uint32_t *) bPtr;
    zWPtr = (uint32_t *) zPtr;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uiA96 = aWPtr[indexWordHi( 4 )];
    expA = expF128UI96( uiA96 );
    uiB96 = bWPtr[indexWordHi( 4 )];
    expB = expF128UI96( uiB96 );
    signZ = signF128UI96( uiA96 ) ^ signF128UI96( uiB96 );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( (expA == 0x7FFF) || (expB == 0x7FFF) ) {
        if ( softfloat_tryPropagateNaNF128M( aWPtr, bWPtr, zWPtr ) ) return;
        ptr = aWPtr;
        if ( ! expA ) goto possiblyInvalid;
        if ( ! expB ) {
            ptr = bWPtr;
     possiblyInvalid:
            if (
                ! fracF128UI96( ptr[indexWordHi( 4 )] )
                    && ! (ptr[indexWord( 4, 2 )] | ptr[indexWord( 4, 1 )]
                              | ptr[indexWord( 4, 0 )])
            ) {
                softfloat_invalidF128M( zWPtr );
                return;
            }
        }
        uiZ96 = packToF128UI96( signZ, 0x7FFF, 0 );
        goto uiZ96;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( expA ) {
        sigA[indexWordHi( 4 )] = fracF128UI96( uiA96 ) | 0x00010000;
        sigA[indexWord( 4, 2 )] = aWPtr[indexWord( 4, 2 )];
        sigA[indexWord( 4, 1 )] = aWPtr[indexWord( 4, 1 )];
        sigA[indexWord( 4, 0 )] = aWPtr[indexWord( 4, 0 )];
    } else {
        expA = softfloat_shiftNormSigF128M( aWPtr, 0, sigA );
        if ( expA == -128 ) goto zero;
    }
    if ( expB ) {
        sigB[indexWordHi( 4 )] = fracF128UI96( uiB96 ) | 0x00010000;
        sigB[indexWord( 4, 2 )] = bWPtr[indexWord( 4, 2 )];
        sigB[indexWord( 4, 1 )] = bWPtr[indexWord( 4, 1 )];
        sigB[indexWord( 4, 0 )] = bWPtr[indexWord( 4, 0 )];
    } else {
        expB = softfloat_shiftNormSigF128M( bWPtr, 0, sigB );
        if ( expB == -128 ) goto zero;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    expZ = expA + expB - 0x4000;
    softfloat_mul128MTo256M( sigA, sigB, sigProd );
    if (
        sigProd[indexWord( 8, 2 )]
            || (sigProd[indexWord( 8, 1 )] | sigProd[indexWord( 8, 0 )])
    ) {
        sigProd[indexWord( 8, 3 )] |= 1;
    }
    extSigZPtr = &sigProd[indexMultiwordHi( 8, 5 )];
    shiftCount = 16;
    if ( extSigZPtr[indexWordHi( 5 )] & 2 ) {
        ++expZ;
        shiftCount = 15;
    }
    softfloat_shortShiftLeft160M( extSigZPtr, shiftCount, extSigZPtr );
    softfloat_roundPackMToF128M( signZ, expZ, extSigZPtr, zWPtr );
    return;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 zero:
    uiZ96 = packToF128UI96( signZ, 0, 0 );
 uiZ96:
    zWPtr[indexWordHi( 4 )] = uiZ96;
    zWPtr[indexWord( 4, 2 )] = 0;
    zWPtr[indexWord( 4, 1 )] = 0;
    zWPtr[indexWord( 4, 0 )] = 0;

}

#endif

