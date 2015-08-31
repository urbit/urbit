
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

int
 softfloat_shiftNormSigF128M(
     const uint32_t *wPtr, uint_fast8_t shiftCount, uint32_t *sigPtr )
{
    uint32_t wordSig;
    int32_t exp;
    uint32_t leadingBit;

    wordSig = wPtr[indexWordHi( 4 )];
    exp = expF128UI96( wordSig );
    if ( exp ) {
        softfloat_shortShiftLeft128M( wPtr, shiftCount, sigPtr );
        leadingBit = 0x00010000<<shiftCount;
        sigPtr[indexWordHi( 4 )] =
            (sigPtr[indexWordHi( 4 )] & (leadingBit - 1)) | leadingBit;
    } else {
        exp = 16;
        wordSig &= 0x7FFFFFFF;
        if ( ! wordSig ) {
            exp = -16;
            wordSig = wPtr[indexWord( 4, 2 )];
            if ( ! wordSig ) {
                exp = -48;
                wordSig = wPtr[indexWord( 4, 1 )];
                if ( ! wordSig ) {
                    wordSig = wPtr[indexWord( 4, 0 )];
                    if ( ! wordSig ) return -128;
                    exp = -80;
                }
            }
        }
        exp -= softfloat_countLeadingZeros32( wordSig );
        softfloat_shiftLeft128M( wPtr, 1 - exp + shiftCount, sigPtr );
    }
    return exp;

}

