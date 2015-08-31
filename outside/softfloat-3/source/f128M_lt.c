
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

bool f128M_lt( const float128_t *aPtr, const float128_t *bPtr )
{

    return f128_lt( *aPtr, *bPtr );

}

#else

bool f128M_lt( const float128_t *aPtr, const float128_t *bPtr )
{
    const uint32_t *aWPtr, *bWPtr;
    uint32_t uiA96, uiB96;
    bool signA, signB;
    uint32_t wordA, wordB;

    aWPtr = (const uint32_t *) aPtr;
    bWPtr = (const uint32_t *) bPtr;
    if ( softfloat_isNaNF128M( aWPtr ) || softfloat_isNaNF128M( bWPtr ) ) {
        softfloat_raiseFlags( softfloat_flag_invalid );
        return false;
    }
    uiA96 = aWPtr[indexWordHi( 4 )];
    uiB96 = bWPtr[indexWordHi( 4 )];
    signA = signF128UI96( uiA96 );
    signB = signF128UI96( uiB96 );
    if ( signA != signB ) {
        if ( signB ) return false;
        if ( (uiA96 | uiB96) & 0x7FFFFFFF ) return true;
        wordA = aWPtr[indexWord( 4, 2 )];
        wordB = bWPtr[indexWord( 4, 2 )];
        if ( wordA | wordB ) return true;
        wordA = aWPtr[indexWord( 4, 1 )];
        wordB = bWPtr[indexWord( 4, 1 )];
        if ( wordA | wordB ) return true;
        wordA = aWPtr[indexWord( 4, 0 )];
        wordB = bWPtr[indexWord( 4, 0 )];
        return ((wordA | wordB) != 0);
    }
    if ( signA ) {
        aWPtr = (const uint32_t *) bPtr;
        bWPtr = (const uint32_t *) aPtr;
    }
    return (softfloat_compare128M( aWPtr, bWPtr ) < 0);

}

#endif

