#include <inttypes.h>
#include <assert.h>
#include <limits.h>

typedef uint8_t  ur_bool_t;

#if    (32 == (CHAR_BIT * __SIZEOF_INT__))
#  define  ur_lz32  __builtin_ctz
#elif  (32 == (CHAR_BIT * __SIZEOF_LONG__))
#  define  ur_lz32  __builtin_ctzl
#else
#  error   "port me"
#endif

#if    (64 == (CHAR_BIT * __SIZEOF_LONG__))
#  define  ur_lz64  __builtin_ctzl
#elif  (64 == (CHAR_BIT * __SIZEOF_LONG_LONG__))
#  define  ur_lz64  __builtin_ctzll
#else
#  error   "port me"
#endif

#define ur_mask_3(a)   (a & 0x7)
#define ur_mask_8(a)   (a & 0xff)
#define ur_mask_31(a)  (a & 0x7fffffff)
#define ur_mask_62(a)  (a & 0x3fffffffffffffffULL)

#define ur_met0_32(a)  ( 32 - ur_lz32(a) )
#define ur_met0_64(a)  ( 64 - ur_lz64(a) )

#define ur_met3_32(a)                           \
        ({ uint8_t _a = ur_met0_32(a);          \
           ( (_a >> 3) + !!ur_mask_3(_a) ); })  \

#define ur_met3_64(a)                           \
        ({ uint8_t _a = ur_met0_64(a);          \
           ( (_a >> 3) + !!ur_mask_3(_a) ); })  \

#define ur_nref_tag(ref)       ( ref >> 62 )
#define ur_nref_idx(ref)       ur_mask_62(ref)

typedef struct ur_pail32_s {
  uint8_t  fill;
  uint32_t data[10];
} ur_pail32_t;

typedef struct ur_dict32_s {
  uint64_t     prev;
  uint64_t     size;
  ur_pail32_t *buckets;
} ur_dict32_t;

typedef struct ur_pail64_s {
  uint8_t  fill;
  uint64_t data[10];
} ur_pail64_t;

typedef struct ur_dict64_s {
  uint64_t     prev;
  uint64_t     size;
  ur_pail64_t *buckets;
} ur_dict64_t;

typedef uint32_t ur_mug;
typedef uint64_t ur_nref;
typedef enum {
  ur_direct = 0,
  ur_iatom = 1,
  ur_icell = 2,
} ur_tag;

typedef struct ur_cells_s {
  ur_dict64_t dict;
  uint64_t    prev;
  uint64_t    size;
  uint64_t    fill;
  ur_mug     *mugs;
  ur_nref   *heads;
  ur_nref   *tails;
} ur_cells_t;

typedef struct ur_atoms_s {
  ur_dict64_t dict;
  uint64_t    prev;
  uint64_t    size;
  uint64_t    fill;
  ur_mug     *mugs;
  uint8_t  **bytes;
  uint64_t   *lens;
} ur_atoms_t;

typedef struct ur_root_s {
  ur_cells_t cells;
  ur_atoms_t atoms;
} ur_root_t;
