
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

float128_t
 softfloat_normRoundPackToF128(
     bool sign, int_fast32_t exp, uint_fast64_t sig64, uint_fast64_t sig0 )
{
    int_fast8_t shiftCount;
    struct uint128 sig128;
    union ui128_f128 uZ;
    uint_fast64_t sigExtra;
    struct uint128_extra sig128Extra;

    if ( ! sig64 ) {
        exp -= 64;
        sig64 = sig0;
        sig0 = 0;
    }
    shiftCount = softfloat_countLeadingZeros64( sig64 ) - 15;
    exp -= shiftCount;
    if ( 0 <= shiftCount ) {
        if ( shiftCount ) {
            sig128 = softfloat_shortShiftLeft128( sig64, sig0, shiftCount );
            sig64 = sig128.v64;
            sig0  = sig128.v0;
        }
        if ( (uint32_t) exp < 0x7FFD ) {
            uZ.ui.v64 = packToF128UI64( sign, sig64 | sig0 ? exp : 0, sig64 );
            uZ.ui.v0  = sig0;
            return uZ.f;
        }
        sigExtra = 0;
    } else {
        sig128Extra =
            softfloat_shortShiftRightJam128Extra(
                sig64, sig0, 0, -shiftCount );
        sig64 = sig128Extra.v.v64;
        sig0  = sig128Extra.v.v0;
        sigExtra = sig128Extra.extra;
    }
    return softfloat_roundPackToF128( sign, exp, sig64, sig0, sigExtra );

}

