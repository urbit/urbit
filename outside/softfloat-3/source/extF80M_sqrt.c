
/*============================================================================

This C source file is part of the SoftFloat IEEE Floating-Point Arithmetic
Package, Release 3, by John R. Hauser.

Copyright 2011, 2012, 2013, 2014, 2015 The Regents of the University of
California (Regents).  All Rights Reserved.  Redistribution and use in source
and binary forms, with or without modification, are permitted provided that
the following conditions are met:

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

void extF80M_sqrt( const extFloat80_t *aPtr, extFloat80_t *zPtr )
{

    *zPtr = extF80_sqrt( *aPtr );

}

#else

void extF80M_sqrt( const extFloat80_t *aPtr, extFloat80_t *zPtr )
{
    const struct extFloat80M *aSPtr;
    struct extFloat80M *zSPtr;
    uint_fast16_t uiA64, signUI64;
    int32_t expA;
    uint64_t rem64;
    int32_t expZ;
    uint32_t rem[4], sig32A, recipSqrt32, sig32Z, q;
    uint64_t sig64Z, x64;
    uint32_t term[4], extSigZ[3];

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    aSPtr = (const struct extFloat80M *) aPtr;
    zSPtr = (struct extFloat80M *) zPtr;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uiA64 = aSPtr->signExp;
    signUI64 = uiA64 & packToExtF80UI64( 1, 0 );
    expA = expExtF80UI64( uiA64 );
    rem64 = aSPtr->signif;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( expA == 0x7FFF ) {
        if ( rem64 & UINT64_C( 0x7FFFFFFFFFFFFFFF ) ) {
            softfloat_propagateNaNExtF80M( aSPtr, 0, zSPtr );
            return;
        }
        if ( signUI64 ) goto invalid;
        rem64 = UINT64_C( 0x8000000000000000 );
        goto copyA;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( ! expA ) expA = 1;
    if ( ! (rem64 & UINT64_C( 0x8000000000000000 )) ) {
        if ( ! rem64 ) {
            uiA64 = signUI64;
            goto copyA;
        }
        expA += softfloat_normExtF80SigM( &rem64 );
    }
    if ( signUI64 ) goto invalid;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    expZ = ((expA - 0x3FFF)>>1) + 0x3FFF;
    expA &= 1;
    softfloat_shortShiftLeft64To96M(
        rem64, 30 - expA, &rem[indexMultiwordHi( 4, 3 )] );
    sig32A = rem64>>32;
    recipSqrt32 = softfloat_approxRecipSqrt32_1( expA, sig32A );
    sig32Z = ((uint64_t) sig32A * recipSqrt32)>>32;
    if ( expA ) sig32Z >>= 1;
    rem64 =
        ((uint64_t) rem[indexWord( 4, 3 )]<<32 | rem[indexWord( 4, 2 )])
            - (uint64_t) sig32Z * sig32Z;
    rem[indexWord( 4, 3 )] = rem64>>32;
    rem[indexWord( 4, 2 )] = rem64;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    q = ((uint32_t) (rem64>>2) * (uint64_t) recipSqrt32)>>32;
    sig64Z = ((uint64_t) sig32Z<<32) + ((uint64_t) q<<3);
    x64 = ((uint64_t) sig32Z<<32) + sig64Z;
    term[indexWord( 3, 2 )] = 0;
    term[indexWord( 3, 1 )] = x64>>32;
    term[indexWord( 3, 0 )] = x64;
    softfloat_remStep96MBy32(
        &rem[indexMultiwordHi( 4, 3 )],
        29,
        term,
        q,
        &rem[indexMultiwordHi( 4, 3 )]
    );
    rem64 = (uint64_t) rem[indexWord( 4, 3 )]<<32 | rem[indexWord( 4, 2 )];
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    q = (((uint32_t) (rem64>>2) * (uint64_t) recipSqrt32)>>32) + 2;
    x64 = (uint64_t) q<<7;
    extSigZ[indexWord( 3, 0 )] = x64;
    x64 = (sig64Z<<1) + (x64>>32);
    extSigZ[indexWord( 3, 2 )] = x64>>32;
    extSigZ[indexWord( 3, 1 )] = x64;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( (q & 0xFFFFFF) <= 2 ) {
        q &= ~(uint32_t) 0xFFFF;
        extSigZ[indexWordLo( 3 )] = q<<7;
        x64 = sig64Z + (q>>27);
        term[indexWord( 4, 3 )] = 0;
        term[indexWord( 4, 2 )] = x64>>32;
        term[indexWord( 4, 1 )] = x64;
        term[indexWord( 4, 0 )] = q<<5;
        rem[indexWord( 4, 0 )] = 0;
        softfloat_remStep128MBy32( rem, 28, term, q, rem );
        q = rem[indexWordHi( 4 )];
        if ( q & 0x80000000 ) {
            softfloat_sub1X96M( extSigZ );
        } else {
            if ( q || rem[indexWord( 4, 1 )] || rem[indexWord( 4, 2 )] ) {
                extSigZ[indexWordLo( 3 )] |= 1;
            }
        }
    }
    softfloat_roundPackMToExtF80M(
        0, expZ, extSigZ, extF80_roundingPrecision, zSPtr );
    return;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 invalid:
    softfloat_invalidExtF80M( zSPtr );
    return;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 copyA:
    zSPtr->signExp = uiA64;
    zSPtr->signif  = rem64;

}

#endif

