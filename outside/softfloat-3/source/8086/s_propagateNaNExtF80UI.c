
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

/*----------------------------------------------------------------------------
| Interpreting the unsigned integer formed from concatenating `uiA64' and
| `uiA0' as an 80-bit extended floating-point value, and likewise interpreting
| the unsigned integer formed from concatenating `uiB64' and `uiB0' as another
| 80-bit extended floating-point value, and assuming at least on of these
| floating-point values is a NaN, returns the bit pattern of the combined NaN
| result.  If either original floating-point value is a signaling NaN, the
| invalid exception is raised.
*----------------------------------------------------------------------------*/
struct uint128
 softfloat_propagateNaNExtF80UI(
     uint_fast16_t uiA64,
     uint_fast64_t uiA0,
     uint_fast16_t uiB64,
     uint_fast64_t uiB0
 )
{
    bool isSigNaNA, isSigNaNB;
    uint_fast64_t uiNonsigA0, uiNonsigB0;
    uint_fast16_t uiMagA64, uiMagB64;
    struct uint128 uiZ;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    isSigNaNA = softfloat_isSigNaNExtF80UI( uiA64, uiA0 );
    isSigNaNB = softfloat_isSigNaNExtF80UI( uiB64, uiB0 );
    /*------------------------------------------------------------------------
    | Make NaNs non-signaling.
    *------------------------------------------------------------------------*/
    uiNonsigA0 = uiA0 | UINT64_C( 0xC000000000000000 );
    uiNonsigB0 = uiB0 | UINT64_C( 0xC000000000000000 );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( isSigNaNA | isSigNaNB ) {
        softfloat_raiseFlags( softfloat_flag_invalid );
        if ( isSigNaNA ) {
            if ( isSigNaNB ) goto returnLargerMag;
            if ( isNaNExtF80UI( uiB64, uiB0 ) ) goto returnB;
            goto returnA;
        } else {
            if ( isNaNExtF80UI( uiA64, uiA0 ) ) goto returnA;
            goto returnB;
        }
    }
 returnLargerMag:
    uiMagA64 = uiA64 & 0x7FFF;
    uiMagB64 = uiB64 & 0x7FFF;
    if ( uiMagA64 < uiMagB64 ) goto returnB;
    if ( uiMagB64 < uiMagA64 ) goto returnA;
    if ( uiNonsigA0 < uiNonsigB0 ) goto returnB;
    if ( uiNonsigB0 < uiNonsigA0 ) goto returnA;
    if ( uiA64 < uiB64 ) goto returnA;
 returnB:
    uiZ.v64 = uiB64;
    uiZ.v0  = uiNonsigB0;
    return uiZ;
 returnA:
    uiZ.v64 = uiA64;
    uiZ.v0  = uiNonsigA0;
    return uiZ;

}

