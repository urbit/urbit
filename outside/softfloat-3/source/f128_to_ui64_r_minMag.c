
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

uint_fast64_t f128_to_ui64_r_minMag( float128_t a, bool exact )
{
    union ui128_f128 uA;
    uint_fast64_t uiA64, uiA0;
    int_fast32_t exp, shiftCount;
    uint_fast64_t sig64, sig0;
    int_fast16_t negShiftCount;
    uint_fast64_t z;

    uA.f = a;
    uiA64 = uA.ui.v64;
    uiA0  = uA.ui.v0;
    exp = expF128UI64( uiA64 );
    shiftCount = 0x402F - exp;
    if ( shiftCount < 0 ) {
        if ( signF128UI64( uiA64 ) || (shiftCount < -15) ) goto invalid;
        sig64 = fracF128UI64( uiA64 ) | UINT64_C( 0x0001000000000000 );
        sig0  = uiA0;
        negShiftCount = -shiftCount;
        z = sig64<<negShiftCount | sig0>>(shiftCount & 63);
        if ( exact && (uint64_t) (sig0<<negShiftCount) ) {
            softfloat_exceptionFlags |= softfloat_flag_inexact;
        }
    } else {
        sig64 = fracF128UI64( uiA64 );
        sig0  = uiA0;
        if ( 49 <= shiftCount ) {
            if ( exact && (exp | sig64 | sig0) ) {
                softfloat_exceptionFlags |= softfloat_flag_inexact;
            }
            return 0;
        }
        if ( signF128UI64( uiA64 ) ) goto invalid;
        sig64 |= UINT64_C( 0x0001000000000000 );
        z = sig64>>shiftCount;
        if ( exact && (sig0 || (z<<shiftCount != sig64)) ) {
            softfloat_exceptionFlags |= softfloat_flag_inexact;
        }
    }
    return z;
 invalid:
    softfloat_raiseFlags( softfloat_flag_invalid );
    return UINT64_C( 0xFFFFFFFFFFFFFFFF );

}

