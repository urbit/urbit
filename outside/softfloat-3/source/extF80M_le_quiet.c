
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

#ifdef SOFTFLOAT_FAST_INT64

bool extF80M_le_quiet( const extFloat80_t *aPtr, const extFloat80_t *bPtr )
{

    return extF80_le_quiet( *aPtr, *bPtr );

}

#else

bool extF80M_le_quiet( const extFloat80_t *aPtr, const extFloat80_t *bPtr )
{
    const struct extFloat80M *aSPtr, *bSPtr;
    uint_fast16_t uiA64;
    uint64_t uiA0;
    uint_fast16_t uiB64;
    uint64_t uiB0;
    bool signA, ltMags;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    aSPtr = (const struct extFloat80M *) aPtr;
    bSPtr = (const struct extFloat80M *) bPtr;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    uiA64 = aSPtr->signExp;
    uiA0  = aSPtr->signif;
    uiB64 = bSPtr->signExp;
    uiB0  = bSPtr->signif;
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    if ( isNaNExtF80UI( uiA64, uiA0 ) || isNaNExtF80UI( uiB64, uiB0 ) ) {
        if (
               softfloat_isSigNaNExtF80UI( uiA64, uiA0 )
            || softfloat_isSigNaNExtF80UI( uiB64, uiB0 )
        ) {
            softfloat_raiseFlags( softfloat_flag_invalid );
        }
        return false;
    }
    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    signA = signExtF80UI64( uiA64 );
    if ( (uiA64 ^ uiB64) & 0x8000 ) {
        /*--------------------------------------------------------------------
        | Signs are different.
        *--------------------------------------------------------------------*/
        return signA || ! (uiA0 | uiB0);
    } else {
        /*--------------------------------------------------------------------
        | Signs are the same.
        *--------------------------------------------------------------------*/
        if ( ! ((uiA0 & uiB0) & UINT64_C( 0x8000000000000000 )) ) {
            return (softfloat_compareNonnormExtF80M( aSPtr, bSPtr ) <= 0);
        }
        if ( uiA64 == uiB64 ) {
            if ( uiA0 == uiB0 ) return true;
            ltMags = (uiA0 < uiB0);
        } else {
            ltMags = (uiA64 < uiB64);
        }
        return signA ^ ltMags;
    }

}

#endif

