
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

uint_fast32_t f128M_to_ui32_r_minMag( const float128_t *aPtr, bool exact )
{

    return f128_to_ui32_r_minMag( *aPtr, exact );

}

#else

uint_fast32_t f128M_to_ui32_r_minMag( const float128_t *aPtr, bool exact )
{
    const uint32_t *aWPtr;
    uint32_t uiA96;
    int32_t exp;
    uint64_t sig64;
    int32_t shiftCount;
    uint32_t z;

    aWPtr = (const uint32_t *) aPtr;
    uiA96 = aWPtr[indexWordHi( 4 )];
    exp = expF128UI96( uiA96 );
    sig64 = (uint64_t) fracF128UI96( uiA96 )<<32 | aWPtr[indexWord( 4, 2 )];
    if ( aWPtr[indexWord( 4, 1 )] | aWPtr[indexWord( 4, 0 )] ) sig64 |= 1;
    shiftCount = 0x402F - exp;
    if ( 49 <= shiftCount ) {
        if ( exact && (exp | sig64) ) {
            softfloat_exceptionFlags |= softfloat_flag_inexact;
        }
        return 0;
    }
    if ( signF128UI96( uiA96 ) || (shiftCount < 17) ) {
        softfloat_raiseFlags( softfloat_flag_invalid );
        return 0xFFFFFFFF;
    }
    sig64 |= UINT64_C( 0x0001000000000000 );
    z = sig64>>shiftCount;
    if ( exact && ((uint64_t) z<<shiftCount != sig64) ) {
        softfloat_exceptionFlags |= softfloat_flag_inexact;
    }
    return z;

}

#endif

