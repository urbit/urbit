
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

#ifndef softfloat_shiftLeftM

#define softfloat_shiftLeftM softfloat_shiftLeftM
#include "primitives.h"

void
 softfloat_shiftLeftM(
     uint_fast8_t size_words,
     const uint32_t *aPtr,
     uint32_t count,
     uint32_t *zPtr
 )
{
    uint32_t wordCount;
    uint_fast8_t innerCount;
    uint32_t *destPtr;
    uint_fast8_t i;

    wordCount = count>>5;
    if ( wordCount < size_words ) {
        aPtr += indexMultiwordLoBut( size_words, wordCount );
        innerCount = count & 31;
        if ( innerCount ) {
            softfloat_shortShiftLeftM(
                size_words - wordCount,
                aPtr,
                innerCount,
                zPtr + indexMultiwordHiBut( size_words, wordCount )
            );
            if ( ! wordCount ) return;
        } else {
            aPtr += indexWordHi( size_words - wordCount );
            destPtr = zPtr + indexWordHi( size_words );
            for ( i = size_words - wordCount; i; --i ) {
                *destPtr = *aPtr;
                aPtr -= wordIncr;
                destPtr -= wordIncr;
            }
        }
        zPtr += indexMultiwordLo( size_words, wordCount );
    } else {
        wordCount = size_words;
    }
    do {
        *zPtr++ = 0;
        --wordCount;
    } while ( wordCount );

}

#endif

