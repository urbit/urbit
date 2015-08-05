
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
| Interpreting `uiA' and `uiB' as the bit patterns of two 64-bit floating-
| point values, at least one of which is a NaN, returns the bit pattern of
| the combined NaN result.  If either `uiA' or `uiB' has the pattern of a
| signaling NaN, the invalid exception is raised.
*----------------------------------------------------------------------------*/
uint_fast64_t
 softfloat_propagateNaNF64UI( uint_fast64_t uiA, uint_fast64_t uiB )
{
    bool isSigNaNA, isSigNaNB;
    uint_fast64_t uiNonsigA, uiNonsigB, uiMagA, uiMagB;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    isSigNaNA = softfloat_isSigNaNF64UI( uiA );
    isSigNaNB = softfloat_isSigNaNF64UI( uiB );
    /*------------------------------------------------------------------------
    | Make NaNs non-signaling.
    *------------------------------------------------------------------------*/
    uiNonsigA = uiA | UINT64_C( 0x0008000000000000 );
    uiNonsigB = uiB | UINT64_C( 0x0008000000000000 );
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( isSigNaNA | isSigNaNB ) {
        softfloat_raiseFlags( softfloat_flag_invalid );
        if ( isSigNaNA ) {
            if ( isSigNaNB ) goto returnLargerMag;
            return isNaNF64UI( uiB ) ? uiNonsigB : uiNonsigA;
        } else {
            return isNaNF64UI( uiA ) ? uiNonsigA : uiNonsigB;
        }
    }
 returnLargerMag:
    uiMagA = uiNonsigA & UINT64_C( 0x7FFFFFFFFFFFFFFF );
    uiMagB = uiNonsigB & UINT64_C( 0x7FFFFFFFFFFFFFFF );
    if ( uiMagA < uiMagB ) return uiNonsigB;
    if ( uiMagB < uiMagA ) return uiNonsigA;
    return (uiNonsigA < uiNonsigB) ? uiNonsigA : uiNonsigB;

}

