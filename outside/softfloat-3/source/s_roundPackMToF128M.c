
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

void
 softfloat_roundPackMToF128M(
     bool sign, int32_t exp, uint32_t *extSigPtr, uint32_t *zWPtr )
{
    uint_fast8_t roundingMode;
    bool roundNearEven;
    uint32_t sigExtra;
    bool doIncrement, isTiny;
    static const uint32_t maxSig[4] =
        INIT_UINTM4( 0x0001FFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF );
    uint32_t ui, uj;

    roundingMode = softfloat_roundingMode;
    roundNearEven = (roundingMode == softfloat_round_near_even);
    sigExtra = extSigPtr[indexWordLo( 5 )];
    doIncrement = (0x80000000 <= sigExtra);
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
                || (softfloat_compare128M(
                        extSigPtr + indexMultiwordHi( 5, 4 ), maxSig )
                        < 0);
            softfloat_shiftRightJam160M( extSigPtr, -exp, extSigPtr );
            exp = 0;
            sigExtra = extSigPtr[indexWordLo( 5 )];
            if ( isTiny && sigExtra ) {
                softfloat_raiseFlags( softfloat_flag_underflow );
            }
            doIncrement = (0x80000000 <= sigExtra);
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
            || ((exp == 0x7FFD) && doIncrement
                    && (softfloat_compare128M(
                            extSigPtr + indexMultiwordHi( 5, 4 ), maxSig )
                            == 0))
        ) {
            softfloat_raiseFlags(
                softfloat_flag_overflow | softfloat_flag_inexact );
            if (
                   roundNearEven
                || (roundingMode == softfloat_round_near_maxMag)
                || (roundingMode
                        == (sign ? softfloat_round_min : softfloat_round_max))
            ) {
                ui = packToF128UI96( sign, 0x7FFF, 0 );
                uj = 0;
            } else {
                ui = packToF128UI96( sign, 0x7FFE, 0x0000FFFF );
                uj = 0xFFFFFFFF;
            }
            zWPtr[indexWordHi( 4 )] = ui;
            zWPtr[indexWord( 4, 2 )] = uj;
            zWPtr[indexWord( 4, 1 )] = uj;
            zWPtr[indexWord( 4, 0 )] = uj;
            return;
        }
    }
    if ( sigExtra ) softfloat_exceptionFlags |= softfloat_flag_inexact;
    uj = extSigPtr[indexWord( 5, 1 )];
    if ( doIncrement ) {
        ++uj;
        if ( uj ) {
            if ( ! (sigExtra & 0x7FFFFFFF) && roundNearEven ) uj &= ~1;
            zWPtr[indexWord( 4, 2 )] = extSigPtr[indexWord( 5, 3 )];
            zWPtr[indexWord( 4, 1 )] = extSigPtr[indexWord( 5, 2 )];
            zWPtr[indexWord( 4, 0 )] = uj;
            ui = extSigPtr[indexWordHi( 5 )];
        } else {
            zWPtr[indexWord( 4, 0 )] = uj;
            ui = extSigPtr[indexWord( 5, 2 )] + 1;
            zWPtr[indexWord( 4, 1 )] = ui;
            uj = extSigPtr[indexWord( 5, 3 )];
            if ( ui ) {
                zWPtr[indexWord( 4, 2 )] = uj;
                ui = extSigPtr[indexWordHi( 5 )];
            } else {
                ++uj;
                zWPtr[indexWord( 4, 2 )] = uj;
                ui = extSigPtr[indexWordHi( 5 )];
                if ( ! uj ) ++ui;
            }
        }
    } else {
        zWPtr[indexWord( 4, 0 )] = uj;
        ui = extSigPtr[indexWord( 5, 2 )];
        zWPtr[indexWord( 4, 1 )] = ui;
        uj |= ui;
        ui = extSigPtr[indexWord( 5, 3 )];
        zWPtr[indexWord( 4, 2 )] = ui;
        uj |= ui;
        ui = extSigPtr[indexWordHi( 5 )];
        uj |= ui;
        if ( ! uj ) exp = 0;
    }
    zWPtr[indexWordHi( 4 )] = packToF128UI96( sign, exp, ui );

}

