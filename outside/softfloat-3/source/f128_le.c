
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

bool f128_le( float128_t a, float128_t b )
{
    union ui128_f128 uA;
    uint_fast64_t uiA64, uiA0;
    union ui128_f128 uB;
    uint_fast64_t uiB64, uiB0;
    bool signA, signB;

    uA.f = a;
    uiA64 = uA.ui.v64;
    uiA0  = uA.ui.v0;
    uB.f = b;
    uiB64 = uB.ui.v64;
    uiB0  = uB.ui.v0;
    if ( isNaNF128UI( uiA64, uiA0 ) || isNaNF128UI( uiB64, uiB0 ) ) {
        softfloat_raiseFlags( softfloat_flag_invalid );
        return false;
    }
    signA = signF128UI64( uiA64 );
    signB = signF128UI64( uiB64 );
    return
        (signA != signB)
            ? signA
                  || ! (((uiA64 | uiB64) & UINT64_C( 0x7FFFFFFFFFFFFFFF ))
                            | uiA0 | uiB0)
            : ((uiA64 == uiB64) && (uiA0 == uiB0))
                  || (signA ^ softfloat_lt128( uiA64, uiA0, uiB64, uiB0 ));

}

