
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

#ifndef internals_h
#define internals_h 1

#include <stdbool.h>
#include <stdint.h>
#include "primitives.h"
#include "softfloat_types.h"

union ui32_f32 { uint32_t ui; float32_t f; };
union ui64_f64 { uint64_t ui; float64_t f; };

#ifdef SOFTFLOAT_FAST_INT64
union extF80M_extF80 { struct extFloat80M fM; extFloat80_t f; };
union ui128_f128 { struct uint128 ui; float128_t f; };
#endif

enum {
    softfloat_mulAdd_subC    = 1,
    softfloat_mulAdd_subProd = 2
};

/*----------------------------------------------------------------------------
*----------------------------------------------------------------------------*/
uint_fast32_t
 softfloat_roundPackToUI32( bool, uint_fast64_t, uint_fast8_t, bool );

#ifdef SOFTFLOAT_FAST_INT64
uint_fast64_t
 softfloat_roundPackToUI64(
     bool, uint_fast64_t, uint_fast64_t, uint_fast8_t, bool );
#else
uint_fast64_t
 softfloat_roundPackMToUI64( bool, uint32_t *, uint_fast8_t, bool );
#endif

int_fast32_t
 softfloat_roundPackToI32( bool, uint_fast64_t, uint_fast8_t, bool );

#ifdef SOFTFLOAT_FAST_INT64
int_fast64_t
 softfloat_roundPackToI64(
     bool, uint_fast64_t, uint_fast64_t, uint_fast8_t, bool );
#else
int_fast64_t softfloat_roundPackMToI64( bool, uint32_t *, uint_fast8_t, bool );
#endif

/*----------------------------------------------------------------------------
*----------------------------------------------------------------------------*/
#define signF32UI( a ) ((bool) ((uint32_t) (a)>>31))
#define expF32UI( a ) ((int_fast16_t) ((a)>>23) & 0xFF)
#define fracF32UI( a ) ((a) & 0x007FFFFF)
#define packToF32UI( sign, exp, sig ) (((uint32_t) (sign)<<31) + ((uint32_t) (exp)<<23) + (sig))

#define isNaNF32UI( a ) ((((a) & 0x7F800000) == 0x7F800000) && ((a) & 0x007FFFFF))

struct exp16_sig32 { int_fast16_t exp; uint_fast32_t sig; };
struct exp16_sig32 softfloat_normSubnormalF32Sig( uint_fast32_t );

float32_t softfloat_roundPackToF32( bool, int_fast16_t, uint_fast32_t );
float32_t softfloat_normRoundPackToF32( bool, int_fast16_t, uint_fast32_t );

float32_t softfloat_addMagsF32( uint_fast32_t, uint_fast32_t, bool );
float32_t softfloat_subMagsF32( uint_fast32_t, uint_fast32_t, bool );
float32_t
 softfloat_mulAddF32(
     uint_fast32_t, uint_fast32_t, uint_fast32_t, uint_fast8_t );

/*----------------------------------------------------------------------------
*----------------------------------------------------------------------------*/
#define signF64UI( a ) ((bool) ((uint64_t) (a)>>63))
#define expF64UI( a ) ((int_fast16_t) ((a)>>52) & 0x7FF)
#define fracF64UI( a ) ((a) & UINT64_C( 0x000FFFFFFFFFFFFF ))
#define packToF64UI( sign, exp, sig ) ((uint64_t) (((uint_fast64_t) (sign)<<63) + ((uint_fast64_t) (exp)<<52) + (sig)))

#define isNaNF64UI( a ) ((((a) & UINT64_C( 0x7FF0000000000000 )) == UINT64_C( 0x7FF0000000000000 )) && ((a) & UINT64_C( 0x000FFFFFFFFFFFFF )))

struct exp16_sig64 { int_fast16_t exp; uint_fast64_t sig; };
struct exp16_sig64 softfloat_normSubnormalF64Sig( uint_fast64_t );

float64_t softfloat_roundPackToF64( bool, int_fast16_t, uint_fast64_t );
float64_t softfloat_normRoundPackToF64( bool, int_fast16_t, uint_fast64_t );

float64_t softfloat_addMagsF64( uint_fast64_t, uint_fast64_t, bool );
float64_t softfloat_subMagsF64( uint_fast64_t, uint_fast64_t, bool );
float64_t
 softfloat_mulAddF64(
     uint_fast64_t, uint_fast64_t, uint_fast64_t, uint_fast8_t );

/*----------------------------------------------------------------------------
*----------------------------------------------------------------------------*/
#define signExtF80UI64( a64 ) ((bool) ((uint16_t) (a64)>>15))
#define expExtF80UI64( a64 ) ((a64) & 0x7FFF)
#define packToExtF80UI64( sign, exp ) ((uint_fast16_t) (sign)<<15 | (exp))

#define isNaNExtF80UI( a64, a0 ) ((((a64) & 0x7FFF) == 0x7FFF) && ((a0) & UINT64_C( 0x7FFFFFFFFFFFFFFF )))

#ifdef SOFTFLOAT_FAST_INT64

/*----------------------------------------------------------------------------
*----------------------------------------------------------------------------*/

struct exp32_sig64 { int_fast32_t exp; uint64_t sig; };
struct exp32_sig64 softfloat_normSubnormalExtF80Sig( uint_fast64_t );

extFloat80_t
 softfloat_roundPackToExtF80(
     bool, int_fast32_t, uint_fast64_t, uint_fast64_t, uint_fast8_t );
extFloat80_t
 softfloat_normRoundPackToExtF80(
     bool, int_fast32_t, uint_fast64_t, uint_fast64_t, uint_fast8_t );

extFloat80_t
 softfloat_addMagsExtF80(
     uint_fast16_t, uint_fast64_t, uint_fast16_t, uint_fast64_t, bool );
extFloat80_t
 softfloat_subMagsExtF80(
     uint_fast16_t, uint_fast64_t, uint_fast16_t, uint_fast64_t, bool );

/*----------------------------------------------------------------------------
*----------------------------------------------------------------------------*/
#define signF128UI64( a64 ) ((bool) ((uint64_t) (a64)>>63))
#define expF128UI64( a64 ) ((int_fast32_t) ((a64)>>48) & 0x7FFF)
#define fracF128UI64( a64 ) ((a64) & UINT64_C( 0x0000FFFFFFFFFFFF ))
#define packToF128UI64( sign, exp, sig64 ) (((uint_fast64_t) (sign)<<63) + ((uint_fast64_t) (exp)<<48) + (sig64))

#define isNaNF128UI( a64, a0 ) ((((a64) & UINT64_C( 0x7FFF000000000000 )) == UINT64_C( 0x7FFF000000000000 )) && (a0 || ((a64) & UINT64_C( 0x0000FFFFFFFFFFFF ))))

struct exp32_sig128 { int_fast32_t exp; struct uint128 sig; };
struct exp32_sig128
 softfloat_normSubnormalF128Sig( uint_fast64_t, uint_fast64_t );

float128_t
 softfloat_roundPackToF128(
     bool, int_fast32_t, uint_fast64_t, uint_fast64_t, uint_fast64_t );
float128_t
 softfloat_normRoundPackToF128(
     bool, int_fast32_t, uint_fast64_t, uint_fast64_t );

float128_t
 softfloat_addMagsF128(
     uint_fast64_t, uint_fast64_t, uint_fast64_t, uint_fast64_t, bool );
float128_t
 softfloat_subMagsF128(
     uint_fast64_t, uint_fast64_t, uint_fast64_t, uint_fast64_t, bool );
float128_t
 softfloat_mulAddF128(
     uint_fast64_t,
     uint_fast64_t,
     uint_fast64_t,
     uint_fast64_t,
     uint_fast64_t,
     uint_fast64_t,
     uint_fast8_t
 );

#else

/*----------------------------------------------------------------------------
*----------------------------------------------------------------------------*/

bool
 softfloat_tryPropagateNaNExtF80M(
     const struct extFloat80M *,
     const struct extFloat80M *,
     struct extFloat80M *
 );
void softfloat_invalidExtF80M( struct extFloat80M * );

int softfloat_normExtF80SigM( uint64_t * );

void
 softfloat_roundPackMToExtF80M(
     bool, int32_t, uint32_t *, uint_fast8_t, struct extFloat80M * );
void
 softfloat_normRoundPackMToExtF80M(
     bool, int32_t, uint32_t *, uint_fast8_t, struct extFloat80M * );

void
 softfloat_addExtF80M(
     const struct extFloat80M *,
     const struct extFloat80M *,
     struct extFloat80M *,
     bool
 );

int
 softfloat_compareNonnormExtF80M(
     const struct extFloat80M *, const struct extFloat80M * );

/*----------------------------------------------------------------------------
*----------------------------------------------------------------------------*/
#define signF128UI96( a96 ) ((bool) ((uint32_t) (a96)>>31))
#define expF128UI96( a96 ) ((int32_t) ((a96)>>16) & 0x7FFF)
#define fracF128UI96( a96 ) ((a96) & 0x0000FFFF)
#define packToF128UI96( sign, exp, sig96 ) (((uint32_t) (sign)<<31) + ((uint32_t) (exp)<<16) + (sig96))

bool softfloat_isNaNF128M( const uint32_t * );

bool
 softfloat_tryPropagateNaNF128M(
     const uint32_t *, const uint32_t *, uint32_t * );
void softfloat_invalidF128M( uint32_t * );

int softfloat_shiftNormSigF128M( const uint32_t *, uint_fast8_t, uint32_t * );

void softfloat_roundPackMToF128M( bool, int32_t, uint32_t *, uint32_t * );
void softfloat_normRoundPackMToF128M( bool, int32_t, uint32_t *, uint32_t * );

void
 softfloat_addF128M( const uint32_t *, const uint32_t *, uint32_t *, bool );
void
 softfloat_mulAddF128M(
     const uint32_t *,
     const uint32_t *,
     const uint32_t *,
     uint32_t *,
     uint_fast8_t
 );

#endif

#endif

