
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
#include "specialize.h"
#include "softfloat.h"

float32_t f32_roundToInt( float32_t a, uint_fast8_t roundingMode, bool exact )
{
    union ui32_f32 uA;
    uint_fast32_t uiA;
    int_fast16_t exp;
    uint_fast32_t uiZ, lastBitMask, roundBitsMask;
    union ui32_f32 uZ;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uA.f = a;
    uiA = uA.ui;
    exp = expF32UI( uiA );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( exp <= 0x7E ) {
        if ( ! (uint32_t) (uiA<<1) ) return a;
        if ( exact ) softfloat_exceptionFlags |= softfloat_flag_inexact;
        uiZ = uiA & packToF32UI( 1, 0, 0 );
        switch ( roundingMode ) {
         case softfloat_round_near_even:
            if ( ! fracF32UI( uiA ) ) break;
         case softfloat_round_near_maxMag:
            if ( exp == 0x7E ) uiZ |= packToF32UI( 0, 0x7F, 0 );
            break;
         case softfloat_round_min:
            if ( uiZ ) uiZ = packToF32UI( 1, 0x7F, 0 );
            break;
         case softfloat_round_max:
            if ( ! uiZ ) uiZ = packToF32UI( 0, 0x7F, 0 );
            break;
        }
        goto uiZ;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( 0x96 <= exp ) {
        if ( (exp == 0xFF) && fracF32UI( uiA ) ) {
            uiZ = softfloat_propagateNaNF32UI( uiA, 0 );
            goto uiZ;
        }
        return a;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uiZ = uiA;
    lastBitMask = (uint_fast32_t) 1<<(0x96 - exp);
    roundBitsMask = lastBitMask - 1;
    if ( roundingMode == softfloat_round_near_maxMag ) {
        uiZ += lastBitMask>>1;
    } else if ( roundingMode == softfloat_round_near_even ) {
        uiZ += lastBitMask>>1;
        if ( ! (uiZ & roundBitsMask) ) uiZ &= ~lastBitMask;
    } else if ( roundingMode != softfloat_round_minMag ) {
        if ( signF32UI( uiZ ) ^ (roundingMode == softfloat_round_max) ) {
            uiZ += roundBitsMask;
        }
    }
    uiZ &= ~roundBitsMask;
    if ( exact && (uiZ != uiA) ) {
        softfloat_exceptionFlags |= softfloat_flag_inexact;
    }
 uiZ:
    uZ.ui = uiZ;
    return uZ.f;

}

