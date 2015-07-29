
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

#include <stdint.h>
#include "platform.h"
#include "internals.h"
#include "softfloat.h"

#ifdef SOFTFLOAT_FAST_INT64

void ui32_to_f128M( uint32_t a, float128_t *zPtr )
{

    *zPtr = ui32_to_f128( a );

}

#else

void ui32_to_f128M( uint32_t a, float128_t *zPtr )
{
    uint32_t *zWPtr, uiZ96, uiZ64;
    int_fast8_t shiftCount;
    uint64_t normA;

    zWPtr = (uint32_t *) zPtr;
    uiZ96 = 0;
    uiZ64 = 0;
    if ( a ) {
        shiftCount = softfloat_countLeadingZeros32( a ) + 17;
        normA = (uint64_t) a<<shiftCount;
        uiZ96 = packToF128UI96( 0, 0x402E - shiftCount, normA>>32 );
        uiZ64 = normA;
    }
    zWPtr[indexWord( 4, 3 )] = uiZ96;
    zWPtr[indexWord( 4, 2 )] = uiZ64;
    zWPtr[indexWord( 4, 1 )] = 0;
    zWPtr[indexWord( 4, 0 )] = 0;

}

#endif

