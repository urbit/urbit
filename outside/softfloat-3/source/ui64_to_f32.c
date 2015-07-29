
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

float32_t ui64_to_f32( uint64_t a )
{
    int_fast8_t shiftCount;
    union ui32_f32 u;
    uint_fast32_t sig;

    shiftCount = softfloat_countLeadingZeros64( a ) - 40;
    if ( 0 <= shiftCount ) {
        u.ui =
            a ? packToF32UI(
                    0, 0x95 - shiftCount, (uint_fast32_t) a<<shiftCount )
                : 0;
        return u.f;
    } else {
        shiftCount += 7;
        sig =
            (shiftCount < 0) ? softfloat_shortShiftRightJam64( a, -shiftCount )
                : (uint_fast32_t) a<<shiftCount;
        return softfloat_roundPackToF32( 0, 0x9C - shiftCount, sig );
    }

}

