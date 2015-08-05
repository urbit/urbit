
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

uint_fast32_t extF80M_to_ui32_r_minMag( const extFloat80_t *aPtr, bool exact )
{

    return extF80_to_ui32_r_minMag( *aPtr, exact );

}

#else

uint_fast32_t extF80M_to_ui32_r_minMag( const extFloat80_t *aPtr, bool exact )
{
    const struct extFloat80M *aSPtr;
    uint_fast16_t uiA64;
    int32_t exp;
    uint64_t sig;
    int32_t shiftCount;
    bool sign;
    uint64_t shiftedSig;
    uint32_t z;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    aSPtr = (const struct extFloat80M *) aPtr;
    uiA64 = aSPtr->signExp;
    exp = expExtF80UI64( uiA64 );
    sig = aSPtr->signif;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( ! sig && (exp != 0x7FFF) ) return 0;
    shiftCount = 0x403E - exp;
    if ( 64 <= shiftCount ) {
        if ( exact ) softfloat_exceptionFlags |= softfloat_flag_inexact;
        return 0;
    }
    sign = signExtF80UI64( uiA64 );
    if ( shiftCount < 0 ) {
        if ( sign || sig>>32 || (shiftCount <= -31) ) goto invalid;
        shiftedSig = (uint64_t) (uint32_t) sig<<-shiftCount;
        if ( shiftedSig>>32 ) goto invalid;
        z = shiftedSig;
    } else {
        shiftedSig = sig;
        if ( shiftCount ) shiftedSig >>= shiftCount;
        if ( shiftedSig>>32 ) goto invalid;
        z = shiftedSig;
        if ( sign && z ) goto invalid;
        if ( exact && shiftCount && ((uint64_t) z<<shiftCount != sig) ) {
            softfloat_exceptionFlags |= softfloat_flag_inexact;
        }
    }
    return z;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 invalid:
    softfloat_raiseFlags( softfloat_flag_invalid );
    return 0xFFFFFFFF;

}

#endif

