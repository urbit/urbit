
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

float128_t ui64_to_f128( uint64_t a )
{
    uint_fast64_t uiZ64, uiZ0;
    int_fast8_t shiftCount;
    struct uint128 zSig;
    union ui128_f128 uZ;

    if ( ! a ) {
        uiZ64 = 0;
        uiZ0  = 0;
    } else {
        shiftCount = softfloat_countLeadingZeros64( a ) + 49;
        if ( 64 <= shiftCount ) {
            zSig.v64 = a<<(shiftCount - 64);
            zSig.v0  = 0;
        } else {
            zSig = softfloat_shortShiftLeft128( 0, a, shiftCount );
        }
        uiZ64 = packToF128UI64( 0, 0x406E - shiftCount, zSig.v64 );
        uiZ0  = zSig.v0;
    }
    uZ.ui.v64 = uiZ64;
    uZ.ui.v0  = uiZ0;
    return uZ.f;

}

