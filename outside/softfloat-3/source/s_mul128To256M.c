
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

#ifndef softfloat_mul128To256M

#define softfloat_mul128To256M softfloat_mul128To256M
#include "primitives.h"

void
 softfloat_mul128To256M(
     uint64_t a64, uint64_t a0, uint64_t b64, uint64_t b0, uint64_t *zPtr )
{
    struct uint128 p0, p64, p128;
    uint_fast64_t z64, z128, z192;

    p0 = softfloat_mul64To128( a0, b0 );
    zPtr[indexWord( 4, 0 )] = p0.v0;
    p64 = softfloat_mul64To128( a64, b0 );
    z64 = p64.v0 + p0.v64;
    z128 = p64.v64 + (z64 < p64.v0);
    p128 = softfloat_mul64To128( a64, b64 );
    z128 += p128.v0;
    z192 = p128.v64 + (z128 < p128.v0);
    p64 = softfloat_mul64To128( a0, b64 );
    z64 += p64.v0;
    zPtr[indexWord( 4, 1 )] = z64;
    p64.v64 += (z64 < p64.v0);
    z128 += p64.v64;
    zPtr[indexWord( 4, 2 )] = z128;
    zPtr[indexWord( 4, 3 )] = z192 + (z128 < p64.v64);

}

#endif

