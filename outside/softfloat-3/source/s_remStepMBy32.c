
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

#include <stdint.h>
#include "platform.h"
#include "primitiveTypes.h"

#ifndef softfloat_remStepMBy32

void
 softfloat_remStepMBy32(
     uint_fast8_t size_words,
     const uint32_t *remPtr,
     uint_fast8_t count,
     const uint32_t *bPtr,
     uint32_t q,
     uint32_t *zPtr
 )
{
    uint_fast8_t negCount;
    unsigned int index, lastIndex;
    uint64_t dwordProd;
    uint32_t wordRem, wordShiftedRem, wordProd;
    uint_fast8_t borrow;

    negCount = -count;
    index = indexWordLo( size_words );
    lastIndex = indexWordHi( size_words );
    dwordProd = (uint64_t) bPtr[index] * q;
    wordRem = remPtr[index];
    wordShiftedRem = wordRem<<count;
    wordProd = dwordProd;
    zPtr[index] = wordShiftedRem - wordProd;
    if ( index != lastIndex ) {
        borrow = (wordShiftedRem < wordProd);
        for (;;) {
            wordShiftedRem = wordRem>>(negCount & 31);
            index += wordIncr;
            dwordProd = (uint64_t) bPtr[index] * q + (dwordProd>>32);
            wordRem = remPtr[index];
            wordShiftedRem |= wordRem<<count;
            wordProd = dwordProd;
            zPtr[index] = wordShiftedRem - wordProd - borrow;
            if ( index == lastIndex ) break;
            borrow =
                borrow ? (wordShiftedRem <= wordProd)
                    : (wordShiftedRem < wordProd);
        }
    }

}

#endif

