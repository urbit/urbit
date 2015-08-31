
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

#ifndef softfloat_shiftRightJamM

#define softfloat_shiftRightJamM softfloat_shiftRightJamM
#include "primitives.h"

void
 softfloat_shiftRightJamM(
     uint_fast8_t size_words,
     const uint32_t *aPtr,
     uint32_t count,
     uint32_t *zPtr
 )
{
    uint32_t wordJam, wordCount, *ptr;
    uint_fast8_t i, innerCount;

    wordJam = 0;
    wordCount = count>>5;
    if ( wordCount ) {
        if ( size_words < wordCount ) wordCount = size_words;
        ptr = (uint32_t *) (aPtr + indexMultiwordLo( size_words, wordCount ));
        i = wordCount;
        do {
            wordJam = *ptr++;
            if ( wordJam ) break;
            --i;
        } while ( i );
        ptr = zPtr;
    }
    if ( wordCount < size_words ) {
        aPtr += indexMultiwordHiBut( size_words, wordCount );
        innerCount = count & 31;
        if ( innerCount ) {
            softfloat_shortShiftRightJamM(
                size_words - wordCount,
                aPtr,
                innerCount,
                zPtr + indexMultiwordLoBut( size_words, wordCount )
            );
            if ( ! wordCount ) goto wordJam;
        } else {
            aPtr += indexWordLo( size_words - wordCount );
            ptr = zPtr + indexWordLo( size_words );
            for ( i = size_words - wordCount; i; --i ) {
                *ptr = *aPtr;
                aPtr += wordIncr;
                ptr += wordIncr;
            }
        }
        ptr = zPtr + indexMultiwordHi( size_words, wordCount );
    }
    do {
        *ptr++ = 0;
        --wordCount;
    } while ( wordCount );
 wordJam:
    if ( wordJam ) zPtr[indexWordLo( size_words )] |= 1;

}

#endif

