
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

int_fast64_t f64_to_i64( float64_t a, uint_fast8_t roundingMode, bool exact )
{
    union ui64_f64 uA;
    uint_fast64_t uiA;
    bool sign;
    int_fast16_t exp;
    uint_fast64_t sig;
    int_fast16_t shiftCount;
#ifdef SOFTFLOAT_FAST_INT64
    struct uint64_extra sigExtra;
#else
    uint32_t extSig[3];
#endif

    uA.f = a;
    uiA = uA.ui;
    sign = signF64UI( uiA );
    exp  = expF64UI( uiA );
    sig  = fracF64UI( uiA );
    if ( exp ) sig |= UINT64_C( 0x0010000000000000 );
    shiftCount = 0x433 - exp;
#ifdef SOFTFLOAT_FAST_INT64
    if ( shiftCount <= 0 ) {
        if ( shiftCount < -11 ) {
            softfloat_raiseFlags( softfloat_flag_invalid );
            return
                ! sign
                    || ((exp == 0x7FF)
                            && (sig != UINT64_C( 0x0010000000000000 )))
                    ? INT64_C( 0x7FFFFFFFFFFFFFFF )
                    : -INT64_C( 0x7FFFFFFFFFFFFFFF ) - 1;
        }
        sigExtra.v = sig<<-shiftCount;
        sigExtra.extra = 0;
    } else {
        sigExtra = softfloat_shiftRightJam64Extra( sig, 0, shiftCount );
    }
    return
        softfloat_roundPackToI64(
            sign, sigExtra.v, sigExtra.extra, roundingMode, exact );
#else
    extSig[indexWord( 3, 0 )] = 0;
    if ( shiftCount <= 0 ) {
        if ( shiftCount < -11 ) {
            softfloat_raiseFlags( softfloat_flag_invalid );
            return
                ! sign
                    || ((exp == 0x7FF)
                            && (sig != UINT64_C( 0x0010000000000000 )))
                    ? INT64_C( 0x7FFFFFFFFFFFFFFF )
                    : -INT64_C( 0x7FFFFFFFFFFFFFFF ) - 1;
        }
        sig <<= -shiftCount;
        extSig[indexWord( 3, 2 )] = sig>>32;
        extSig[indexWord( 3, 1 )] = sig;
    } else {
        extSig[indexWord( 3, 2 )] = sig>>32;
        extSig[indexWord( 3, 1 )] = sig;
        softfloat_shiftRightJam96M( extSig, shiftCount, extSig );
    }
    return softfloat_roundPackMToI64( sign, extSig, roundingMode, exact );
#endif

}

