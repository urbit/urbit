
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

int_fast64_t extF80_to_i64_r_minMag( extFloat80_t a, bool exact )
{
    union { struct extFloat80M s; extFloat80_t f; } uA;
    uint_fast16_t uiA64;
    int_fast32_t exp;
    uint_fast64_t sig;
    int_fast32_t shiftCount;
    bool sign;
    int_fast64_t absZ;

    uA.f = a;
    uiA64 = uA.s.signExp;
    exp = expExtF80UI64( uiA64 );
    sig = uA.s.signif;
    shiftCount = 0x403E - exp;
    if ( 64 <= shiftCount ) {
        if ( exact && (exp | sig) ) {
            softfloat_exceptionFlags |= softfloat_flag_inexact;
        }
        return 0;
    }
    sign = signExtF80UI64( uiA64 );
    if ( shiftCount <= 0 ) {
        if (
            (uiA64 != packToExtF80UI64( 1, 0x403E ))
                || (sig != UINT64_C( 0x8000000000000000 ))
        ) {
            softfloat_raiseFlags( softfloat_flag_invalid );
            if (
                   ! sign
                || ((exp == 0x7FFF) && (sig & UINT64_C( 0x7FFFFFFFFFFFFFFF )))
            ) {
                return INT64_C( 0x7FFFFFFFFFFFFFFF );
            }
        }
        return -INT64_C( 0x7FFFFFFFFFFFFFFF ) - 1;
    }
    absZ = sig>>shiftCount;
    if ( exact && (uint64_t) (sig<<(-shiftCount & 63)) ) {
        softfloat_exceptionFlags |= softfloat_flag_inexact;
    }
    return sign ? -absZ : absZ;

}

