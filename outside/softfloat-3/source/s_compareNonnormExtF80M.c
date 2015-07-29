
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
#include "softfloat_types.h"

int
 softfloat_compareNonnormExtF80M(
     const struct extFloat80M *aSPtr, const struct extFloat80M *bSPtr )
{
    uint_fast16_t uiA64, uiB64;
    uint64_t sigA;
    bool signB;
    uint64_t sigB;
    int32_t expA, expB;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uiA64 = aSPtr->signExp;
    uiB64 = bSPtr->signExp;
    sigA = aSPtr->signif;
    signB = signExtF80UI64( uiB64 );
    sigB = bSPtr->signif;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( (uiA64 ^ uiB64) & 0x8000 ) {
        if ( ! (sigA | sigB) ) return 0;
        goto resultFromSignB;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    expA = expExtF80UI64( uiA64 );
    expB = expExtF80UI64( uiB64 );
    if ( expA == 0x7FFF ) {
        if (expB == 0x7FFF) return 0;
        signB = ! signB;
        goto resultFromSignB;
    }
    if ( expB == 0x7FFF ) {
        goto resultFromSignB;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( ! expA ) expA = 1;
    if ( ! (sigA & UINT64_C( 0x8000000000000000 )) ) {
        if ( sigA ) {
            expA += softfloat_normExtF80SigM( &sigA );
        } else {
            expA = -128;
        }
    }
    if ( ! expB ) expB = 1;
    if ( ! (sigB & UINT64_C( 0x8000000000000000 )) ) {
        if ( sigB ) {
            expB += softfloat_normExtF80SigM( &sigB );
        } else {
            expB = -128;
        }
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( signB ) {
        if ( expA < expB ) return 1;
        if ( (expB < expA) || (sigB < sigA) ) return -1;
    } else {
        if ( expB < expA ) return 1;
        if ( (expA < expB) || (sigA < sigB) ) return -1;
    }
    return (sigA != sigB);
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
 resultFromSignB:
    return signB ? 1 : -1;

}

