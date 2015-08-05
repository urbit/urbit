
/*============================================================================

This C source file is part of the SoftFloat IEEE Floating-Point Arithmetic
Package, Release 3, by John R. Hauser.

Copyright 2011, 2012, 2013, 2014, 2015 The Regents of the University of
California (Regents).  All Rights Reserved.  Redistribution and use in source
and binary forms, with or without modification, are permitted provided that
the following conditions are met:

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

float128_t
 softfloat_roundPackToF128(
     bool sign,
     int_fast32_t exp,
     uint_fast64_t sig64,
     uint_fast64_t sig0,
     uint_fast64_t sigExtra
 )
{
    uint_fast8_t roundingMode;
    bool roundNearEven, doIncrement, isTiny;
    struct uint128_extra sig128Extra;
    uint_fast64_t uiZ64, uiZ0;
    struct uint128 sig128;
    union ui128_f128 uZ;

    roundingMode = softfloat_roundingMode;
    roundNearEven = (roundingMode == softfloat_round_near_even);
    doIncrement = (UINT64_C( 0x8000000000000000 ) <= sigExtra);
    if ( ! roundNearEven && (roundingMode != softfloat_round_near_maxMag) ) {
        doIncrement =
            (roundingMode
                 == (sign ? softfloat_round_min : softfloat_round_max))
                && sigExtra;
    }
    if ( 0x7FFD <= (uint32_t) exp ) {
        if ( exp < 0 ) {
            isTiny =
                   (softfloat_detectTininess
                        == softfloat_tininess_beforeRounding)
                || (exp < -1)
                || ! doIncrement
                || softfloat_lt128(
                       sig64,
                       sig0,
                       UINT64_C( 0x0001FFFFFFFFFFFF ),
                       UINT64_C( 0xFFFFFFFFFFFFFFFF )
                   );
            sig128Extra =
                softfloat_shiftRightJam128Extra( sig64, sig0, sigExtra, -exp );
            sig64 = sig128Extra.v.v64;
            sig0  = sig128Extra.v.v0;
            sigExtra = sig128Extra.extra;
            exp = 0;
            if ( isTiny && sigExtra ) {
                softfloat_raiseFlags( softfloat_flag_underflow );
            }
            doIncrement = (UINT64_C( 0x8000000000000000 ) <= sigExtra);
            if (
                   ! roundNearEven
                && (roundingMode != softfloat_round_near_maxMag)
            ) {
                doIncrement =
                    (roundingMode
                         == (sign ? softfloat_round_min : softfloat_round_max))
                        && sigExtra;
            }
        } else if (
               (0x7FFD < exp)
            || ((exp == 0x7FFD)
                    && softfloat_eq128( 
                           sig64,
                           sig0,
                           UINT64_C( 0x0001FFFFFFFFFFFF ),
                           UINT64_C( 0xFFFFFFFFFFFFFFFF )
                       )
                    && doIncrement)
        ) {
            softfloat_raiseFlags(
                softfloat_flag_overflow | softfloat_flag_inexact );
            if (
                   roundNearEven
                || (roundingMode == softfloat_round_near_maxMag)
                || (roundingMode
                        == (sign ? softfloat_round_min : softfloat_round_max))
            ) {
                uiZ64 = packToF128UI64( sign, 0x7FFF, 0 );
                uiZ0  = 0;
            } else {
                uiZ64 =
                    packToF128UI64(
                        sign, 0x7FFE, UINT64_C( 0x0000FFFFFFFFFFFF ) );
                uiZ0 = UINT64_C( 0xFFFFFFFFFFFFFFFF );
            }
            goto uiZ;
        }
    }
    if ( sigExtra ) softfloat_exceptionFlags |= softfloat_flag_inexact;
    if ( doIncrement ) {
        sig128 = softfloat_add128( sig64, sig0, 0, 1 );
        sig64 = sig128.v64;
        sig0 =
            sig128.v0
                & ~(uint64_t)
                       (! (sigExtra & UINT64_C( 0x7FFFFFFFFFFFFFFF ))
                            & roundNearEven);
    } else {
        if ( ! (sig64 | sig0) ) exp = 0;
    }
    uiZ64 = packToF128UI64( sign, exp, sig64 );
    uiZ0  = sig0;
 uiZ:
    uZ.ui.v64 = uiZ64;
    uZ.ui.v0  = uiZ0;
    return uZ.f;

}

