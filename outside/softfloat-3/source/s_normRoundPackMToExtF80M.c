
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

void
 softfloat_normRoundPackMToExtF80M(
     bool sign,
     int32_t exp,
     uint32_t *extSigPtr,
     uint_fast8_t roundingPrecision,
     struct extFloat80M *zSPtr
 )
{
    int_fast16_t shiftCount;
    uint32_t wordSig;

    shiftCount = 0;
    wordSig = extSigPtr[indexWord( 3, 2 )];
    if ( ! wordSig ) {
        shiftCount = 32;
        wordSig = extSigPtr[indexWord( 3, 1 )];
        if ( ! wordSig ) {
            shiftCount = 64;
            wordSig = extSigPtr[indexWord( 3, 0 )];
            if ( ! wordSig ) {
                zSPtr->signExp = packToExtF80UI64( sign, 0 );
                zSPtr->signif = 0;
                return;
            }
        }
    }
    shiftCount += softfloat_countLeadingZeros32( wordSig );
    if ( shiftCount ) {
        exp -= shiftCount;
        softfloat_shiftLeft96M( extSigPtr, shiftCount, extSigPtr );
    }
    softfloat_roundPackMToExtF80M(
        sign, exp, extSigPtr, roundingPrecision, zSPtr );

}

