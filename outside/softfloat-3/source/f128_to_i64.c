
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

int_fast64_t f128_to_i64( float128_t a, uint_fast8_t roundingMode, bool exact )
{
    union ui128_f128 uA;
    uint_fast64_t uiA64, uiA0;
    bool sign;
    int_fast32_t exp;
    uint_fast64_t sig64, sig0;
    int_fast32_t shiftCount;
    struct uint128 sig128;
    struct uint64_extra sigExtra;

    uA.f = a;
    uiA64 = uA.ui.v64;
    uiA0  = uA.ui.v0;
    sign  = signF128UI64( uiA64 );
    exp   = expF128UI64( uiA64 );
    sig64 = fracF128UI64( uiA64 );
    sig0  = uiA0;
    shiftCount = 0x402F - exp;
    if ( shiftCount <= 0 ) {
        if ( shiftCount < -15 ) {
            softfloat_raiseFlags( softfloat_flag_invalid );
            return
                ! sign || ((exp == 0x7FFF) && (sig64 | sig0))
                    ? INT64_C( 0x7FFFFFFFFFFFFFFF )
                    : -INT64_C( 0x7FFFFFFFFFFFFFFF ) - 1;
        }
        sig64 |= UINT64_C( 0x0001000000000000 );
        if ( shiftCount ) {
            sig128 = softfloat_shortShiftLeft128( sig64, sig0, -shiftCount );
            sig64 = sig128.v64;
            sig0  = sig128.v0;
        }
    } else {
        if ( exp ) sig64 |= UINT64_C( 0x0001000000000000 );
        sigExtra = softfloat_shiftRightJam64Extra( sig64, sig0, shiftCount );
        sig64 = sigExtra.v;
        sig0  = sigExtra.extra;
    }
    return softfloat_roundPackToI64( sign, sig64, sig0, roundingMode, exact );

}

