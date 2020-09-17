#ifndef UR_DEFS_H
#define UR_DEFS_H

#include <inttypes.h>
#include <assert.h>
#include <limits.h>

typedef uint8_t  ur_bool_t;

#define ur_min(a, b)   ( ((a) < (b)) ? (a) : (b) )
#define ur_max(a, b)   ( ((a) > (b)) ? (a) : (b) )

/*
**  fibonacci constants, for convenient initialization of
**  objects intended to be reallocated with fibonacci growth
*/
#define ur_fib10          55
#define ur_fib11          89
#define ur_fib12         144
#define ur_fib27      196418
#define ur_fib28      317811
#define ur_fib33     3524578
#define ur_fib34     5702887

/*
**  bit-masking helpers
*/
#define ur_mask_3(a)   (a & 0x7)
#define ur_mask_8(a)   (a & 0xff)
#define ur_mask_31(a)  (a & 0x7fffffff)
#define ur_mask_62(a)  (a & 0x3fffffffffffffffULL)

/*
**  bloq (binary exponent) conversions
*/
#define ur_bloq_up1(a) ( (a + 0x1) >> 1 )
#define ur_bloq_up2(a) ( (a + 0x3) >> 2 )
#define ur_bloq_up3(a) ( (a + 0x7) >> 3 )

/*
**  atom measurement
*/
#if    (32 == (CHAR_BIT * __SIZEOF_INT__))
#  define  ur_lz32  __builtin_clz
#  define  ur_tz32  __builtin_ctz
#elif  (32 == (CHAR_BIT * __SIZEOF_LONG__))
#  define  ur_lz32  __builtin_clzl
#  define  ur_tz32  __builtin_ctzl
#else
#  error   "port me"
#endif

#if    (64 == (CHAR_BIT * __SIZEOF_LONG__))
#  define  ur_lz64  __builtin_clzl
#  define  ur_tz64  __builtin_ctzl
#elif  (64 == (CHAR_BIT * __SIZEOF_LONG_LONG__))
#  define  ur_lz64  __builtin_clzll
#  define  ur_tz64  __builtin_ctzll
#else
#  error   "port me"
#endif

#define ur_lz8(a)      ( ur_lz32(a) - 24 )
#define ur_tz8         ur_tz32

#define ur_met0_8(a)   ( (a) ? 8  - ur_lz8(a)  : 0 )
#define ur_met0_32(a)  ( (a) ? 32 - ur_lz32(a) : 0 )
#define ur_met0_64(a)  ( (a) ? 64 - ur_lz64(a) : 0 )

/*
**  unsafe wrt trailing null bytes, which are invalid
*/
inline uint64_t
ur_met0_bytes_unsafe(uint64_t len, uint8_t *byt)
{
  uint64_t last = len - 1;
  return (last << 3) + ur_met0_8(byt[last]);
}

#define ur_met3_8(a)   ur_bloq_up3(ur_met0_8(a))
#define ur_met3_32(a)  ur_bloq_up3(ur_met0_32(a))
#define ur_met3_64(a)  ur_bloq_up3(ur_met0_64(a))

#endif
