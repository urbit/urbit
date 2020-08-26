#ifndef UR_DEFS_H
#define UR_DEFS_H

#include <inttypes.h>
#include <assert.h>
#include <limits.h>

#define ur_fib10          55
#define ur_fib11          89
#define ur_fib12         144
#define ur_fib33     3524578
#define ur_fib34     5702887

typedef uint8_t  ur_bool_t;

#define ur_min(a, b)   ( ((a) < (b)) ? (a) : (b) )
#define ur_max(a, b)   ( ((a) > (b)) ? (a) : (b) )

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

#define ur_mask_3(a)   (a & 0x7)
#define ur_mask_8(a)   (a & 0xff)
#define ur_mask_31(a)  (a & 0x7fffffff)
#define ur_mask_62(a)  (a & 0x3fffffffffffffffULL)

#define ur_met0_8(a)   ( (a) ? 8  - ur_lz8(a)  : 0 )
#define ur_met0_32(a)  ( (a) ? 32 - ur_lz32(a) : 0 )
#define ur_met0_64(a)  ( (a) ? 64 - ur_lz64(a) : 0 )

inline uint64_t
ur_met0_bytes(uint8_t *byt, uint64_t len)
{
  //  XX requires no trailing null bytes
  //
  uint64_t last = len - 1;
  return (last << 3) + ur_met0_8(byt[last]);
}

#define ur_met3_8(a)                         \
        ({ uint8_t _a = ur_met0_8(a);        \
           ( (_a >> 3) + !!ur_mask_3(_a) ); })

#define ur_met3_32(a)                        \
        ({ uint8_t _a = ur_met0_32(a);       \
           ( (_a >> 3) + !!ur_mask_3(_a) ); })

#define ur_met3_64(a)                        \
        ({ uint8_t _a = ur_met0_64(a);       \
           ( (_a >> 3) + !!ur_mask_3(_a) ); })

#endif
