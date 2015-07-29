
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

int_fast32_t
 extF80M_to_i32(
     const extFloat80_t *aPtr, uint_fast8_t roundingMode, bool exact )
{

    return extF80_to_i32( *aPtr, roundingMode, exact );

}

#else

int_fast32_t
 extF80M_to_i32(
     const extFloat80_t *aPtr, uint_fast8_t roundingMode, bool exact )
{
    const struct extFloat80M *aSPtr;
    uint_fast16_t uiA64;
    bool sign;
    int32_t exp;
    uint64_t sig;
    int32_t shiftCount;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    aSPtr = (const struct extFloat80M *) aPtr;
    uiA64 = aSPtr->signExp;
    sign = signExtF80UI64( uiA64 );
    exp  = expExtF80UI64( uiA64 );
    sig = aSPtr->signif;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( (exp == 0x7FFF) && (sig & UINT64_C( 0x7FFFFFFFFFFFFFFF )) ) sign = 0;
    shiftCount = 0x4037 - exp;
    if ( shiftCount <= 0 ) {
        if ( sig>>32 ) goto invalid;
        if ( -32 < shiftCount ) {
            sig <<= -shiftCount;
        } else {
            if ( (uint32_t) sig ) goto invalid;
        }
    } else {
        sig = softfloat_shiftRightJam64( sig, shiftCount );
    }
    return softfloat_roundPackToI32( sign, sig, roundingMode, exact );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 invalid:
    softfloat_raiseFlags( softfloat_flag_invalid );
    return sign ? -0x7FFFFFFF - 1 : 0x7FFFFFFF;

}

#endif

