//! @file types.h
//! c3 types.

#ifndef C3_TYPES_H
#define C3_TYPES_H

#include "c/portable.h"

//==============================================================================
// Canonical integers
//==============================================================================

typedef uint64_t c3_d;
typedef int64_t  c3_ds;

typedef uint32_t c3_w;
typedef int32_t  c3_ws;

typedef uint16_t c3_s;
typedef int16_t  c3_ss;

typedef uint8_t  c3_y;  //!< byte
typedef int8_t   c3_ys; //!< signed byte
typedef uint8_t  c3_b;  //!< bit

typedef uint8_t c3_t; //!< boolean
typedef uint8_t c3_o; //!< loobean
#define c3y 0         //!< loobean yes
#define c3n 1         //!< loobean no

typedef uint8_t  c3_g; //!< 32-bit log - 0-31 bits

typedef uint32_t c3_l; //!< little; 31-bit unsigned integer

typedef uint32_t c3_m; //!< mote; also c3_l; LSB first a-z 4-char string.

//==============================================================================
// Deprecated integers
//==============================================================================

typedef char      c3_c;  //!< does not match int8_t or uint8_t
typedef int       c3_i;  //!< int - really bad
typedef uintptr_t c3_p;  //!< pointer-length uint - really really bad
typedef intptr_t  c3_ps; //!< pointer-length int - really really bad

#endif /* ifndef C3_TYPES_H */
