
/*============================================================================

This C header file is part of the SoftFloat IEEE Floating-Point Arithmetic
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

#ifndef softfloat_types_h
#define softfloat_types_h 1

#include <stdint.h>

/*----------------------------------------------------------------------------
| Types used to pass 32-bit, 64-bit, and 128-bit floating-point arguments and
| results to/from functions.  These types must be exactly 32 bits, 64 bits,
| and 128 bits in size, respectively.  Where a platform has "native" support
| for IEEE-Standard floating-point formats, the types below may, if desired,
| be defined as aliases for the native types (typically `float' and `double',
| and possibly `long double').
*----------------------------------------------------------------------------*/
typedef struct { uint32_t v; } float32_t;
typedef struct { uint64_t v; } float64_t;
typedef struct { uint64_t v[2]; } float128_t;

/*----------------------------------------------------------------------------
| The format of an 80-bit extended floating-point number in memory.  This
| structure must contain a 16-bit field named `signExp' and a 64-bit field
| named `signif'.
*----------------------------------------------------------------------------*/
#ifdef LITTLEENDIAN
struct extFloat80M { uint64_t signif; uint16_t signExp; };
#else
struct extFloat80M { uint16_t signExp; uint64_t signif; };
#endif

/*----------------------------------------------------------------------------
| The type used to pass 80-bit extended floating-point arguments and
| results to/from functions.  This type must have size identical to
| `struct extFloat80M'.  Type `extFloat80_t' can be defined as an alias for
| `struct extFloat80M'.  Alternatively, if a platform has "native" support
| for IEEE-Standard 80-bit extended floating-point, it may be possible,
| if desired, to define `extFloat80_t' as an alias for the native type
| (presumably either `long double' or a nonstandard compiler-intrinsic type).
| In that case, the `signif' and `signExp' fields of `struct extFloat80M'
| must align exactly with the locations in memory of the sign, exponent, and
| significand of the native type.
*----------------------------------------------------------------------------*/
typedef struct extFloat80M extFloat80_t;

#endif

