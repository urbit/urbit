
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

uint_fast32_t
 f128M_to_ui32( const float128_t *aPtr, uint_fast8_t roundingMode, bool exact )
{

    return f128_to_ui32( *aPtr, roundingMode, exact );

}

#else

uint_fast32_t
 f128M_to_ui32( const float128_t *aPtr, uint_fast8_t roundingMode, bool exact )
{
    const uint32_t *aWPtr;
    uint32_t uiA96;
    int32_t exp;
    uint64_t sig64;
    int32_t shiftCount;

    aWPtr = (const uint32_t *) aPtr;
    uiA96 = aWPtr[indexWordHi( 4 )];
    exp = expF128UI96( uiA96 );
    sig64 = (uint64_t) fracF128UI96( uiA96 )<<32 | aWPtr[indexWord( 4, 2 )];
    if ( exp ) sig64 |= UINT64_C( 0x0001000000000000 );
    if ( aWPtr[indexWord( 4, 1 )] | aWPtr[indexWord( 4, 0 )] ) sig64 |= 1;
    shiftCount = 0x4028 - exp;
    if ( 0 < shiftCount ) {
        sig64 = softfloat_shiftRightJam64( sig64, shiftCount );
    }
    return
        softfloat_roundPackToUI32(
            signF128UI96( uiA96 ), sig64, roundingMode, exact );

}

#endif

