
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

#ifndef softfloat_shiftRightJam256M

static
 void
  softfloat_shortShiftRightJamM(
      uint_fast8_t size_words,
      const uint64_t *aPtr,
      uint_fast8_t count,
      uint64_t *zPtr
  )
{
    uint_fast8_t negCount;
    unsigned int index, lastIndex;
    uint64_t partWordZ, wordA;

    negCount = -count;
    index = indexWordLo( size_words );
    lastIndex = indexWordHi( size_words );
    wordA = aPtr[index];
    partWordZ = wordA>>count;
    if ( partWordZ<<count != wordA ) partWordZ |= 1;
    while ( index != lastIndex ) {
        wordA = aPtr[index + wordIncr];
        zPtr[index] = wordA<<(negCount & 63) | partWordZ;
        index += wordIncr;
        partWordZ = wordA>>count;
    }
    zPtr[index] = partWordZ;

}

void
 softfloat_shiftRightJam256M(
     const uint64_t *aPtr, uint_fast32_t count, uint64_t *zPtr )
{
    uint64_t wordJam;
    uint_fast32_t wordCount;
    uint64_t *ptr;
    uint_fast8_t i, innerCount;

    wordJam = 0;
    wordCount = count>>6;
    if ( wordCount ) {
        if ( 4 < wordCount ) wordCount = 4;
        ptr = (uint64_t *) (aPtr + indexMultiwordLo( 4, wordCount ));
        i = wordCount;
        do {
            wordJam = *ptr++;
            if ( wordJam ) break;
            --i;
        } while ( i );
        ptr = zPtr;
    }
    if ( wordCount < 4 ) {
        aPtr += indexMultiwordHiBut( 4, wordCount );
        innerCount = count & 63;
        if ( innerCount ) {
            softfloat_shortShiftRightJamM(
                4 - wordCount,
                aPtr,
                innerCount,
                zPtr + indexMultiwordLoBut( 4, wordCount )
            );
            if ( ! wordCount ) goto wordJam;
        } else {
            aPtr += indexWordLo( 4 - wordCount );
            ptr = zPtr + indexWordLo( 4 );
            for ( i = 4 - wordCount; i; --i ) {
                *ptr = *aPtr;
                aPtr += wordIncr;
                ptr += wordIncr;
            }
        }
        ptr = zPtr + indexMultiwordHi( 4, wordCount );
    }
    do {
        *ptr++ = 0;
        --wordCount;
    } while ( wordCount );
 wordJam:
    if ( wordJam ) zPtr[indexWordLo( 4 )] |= 1;

}

#endif

