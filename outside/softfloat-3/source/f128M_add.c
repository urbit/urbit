
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
#include "softfloat.h"

#ifdef SOFTFLOAT_FAST_INT64

void
 f128M_add( const float128_t *aPtr, const float128_t *bPtr, float128_t *zPtr )
{
    const uint64_t *aWPtr, *bWPtr;
    uint_fast64_t uiA64, uiA0;
    bool signA;
    uint_fast64_t uiB64, uiB0;
    bool signB;
#if ! defined INLINE_LEVEL || (INLINE_LEVEL < 2)
    float128_t
        (*magsFuncPtr)(
            uint_fast64_t, uint_fast64_t, uint_fast64_t, uint_fast64_t, bool );
#endif

    aWPtr = (const uint64_t *) aPtr;
    bWPtr = (const uint64_t *) bPtr;
    uiA64 = aWPtr[indexWord( 2, 1 )];
    uiA0  = aWPtr[indexWord( 2, 0 )];
    signA = signF128UI64( uiA64 );
    uiB64 = bWPtr[indexWord( 2, 1 )];
    uiB0  = bWPtr[indexWord( 2, 0 )];
    signB = signF128UI64( uiB64 );
#if defined INLINE_LEVEL && (2 <= INLINE_LEVEL)
    if ( signA == signB ) {
        *zPtr = softfloat_addMagsF128( uiA64, uiA0, uiB64, uiB0, signA );
    } else {
        *zPtr = softfloat_subMagsF128( uiA64, uiA0, uiB64, uiB0, signA );
    }
#else
    magsFuncPtr =
        (signA == signB) ? softfloat_addMagsF128 : softfloat_subMagsF128;
    *zPtr = (*magsFuncPtr)( uiA64, uiA0, uiB64, uiB0, signA );
#endif

}

#else

void
 f128M_add( const float128_t *aPtr, const float128_t *bPtr, float128_t *zPtr )
{

    softfloat_addF128M(
        (const uint32_t *) aPtr,
        (const uint32_t *) bPtr,
        (uint32_t *) zPtr,
        false
    );

}

#endif

