
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

#ifndef softfloat_shiftRightJam128Extra

struct uint128_extra
 softfloat_shiftRightJam128Extra(
     uint64_t a64, uint64_t a0, uint64_t extra, uint_fast32_t count )
{
    int_fast32_t negCount;
    struct uint128_extra z;

    negCount = -count;
    if ( count < 64 ) {
        z.v.v64 = a64>>count;
        z.v.v0 = a64<<(negCount & 63) | a0>>count;
        z.extra = a0<<(negCount & 63);
    } else {
        z.v.v64 = 0;
        if ( count == 64 ) {
            z.v.v0 = a64;
            z.extra = a0;
        } else {
            extra |= a0;
            if ( count < 128 ) {
                z.v.v0 = a64>>(count & 63);
                z.extra = a64<<(negCount & 63);
            } else {
                z.v.v0 = 0;
                z.extra = (count == 128) ? a64 : (a64 != 0);
            }
        }
    }
    z.extra |= (extra != 0);
    return z;

}

#endif

