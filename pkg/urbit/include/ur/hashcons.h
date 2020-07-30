#include <inttypes.h>
#include <assert.h>
#include <limits.h>

typedef uint8_t  ur_bool_t;

#if    (32 == (CHAR_BIT * __SIZEOF_INT__))
#  define  ur_lz32  __builtin_clz
#elif  (32 == (CHAR_BIT * __SIZEOF_LONG__))
#  define  ur_lz32  __builtin_clzl
#else
#  error   "port me"
#endif

#if    (64 == (CHAR_BIT * __SIZEOF_LONG__))
#  define  ur_lz64  __builtin_clzl
#elif  (64 == (CHAR_BIT * __SIZEOF_LONG_LONG__))
#  define  ur_lz64  __builtin_clzll
#else
#  error   "port me"
#endif

#define ur_mask_3(a)   (a & 0x7)
#define ur_mask_8(a)   (a & 0xff)
#define ur_mask_31(a)  (a & 0x7fffffff)
#define ur_mask_62(a)  (a & 0x3fffffffffffffffULL)

#define ur_met0_32(a)  ( (a) ? 32 - ur_lz32(a) : 0 )
#define ur_met0_64(a)  ( (a) ? 64 - ur_lz64(a) : 0 )

#define ur_met3_32(a)                           \
        ({ uint8_t _a = ur_met0_32(a);          \
           ( (_a >> 3) + !!ur_mask_3(_a) ); })  \

#define ur_met3_64(a)                           \
        ({ uint8_t _a = ur_met0_64(a);          \
           ( (_a >> 3) + !!ur_mask_3(_a) ); })  \

#define ur_nref_tag(ref)       ( ref >> 62 )
#define ur_nref_idx(ref)       ur_mask_62(ref)

typedef uint32_t ur_mug;
typedef uint64_t ur_nref;
typedef enum {
  ur_direct = 0,
  ur_iatom = 1,
  ur_icell = 2,
} ur_tag;

typedef struct ur_nvec_s {
  uint64_t fill;
  ur_nref* refs;
} ur_nvec_t;

typedef struct ur_pail_s {
  uint8_t  fill;
  ur_nref  refs[10];
} ur_pail_t;

typedef struct ur_dict_s {
  uint64_t      prev;
  uint64_t      size;
  ur_pail_t *buckets;
} ur_dict_t;

typedef struct ur_cells_s {
  ur_dict_t dict;
  uint64_t  prev;
  uint64_t  size;
  uint64_t  fill;
  ur_mug   *mugs;
  ur_nref *heads;
  ur_nref *tails;
} ur_cells_t;

typedef struct ur_atoms_s {
  ur_dict_t  dict;
  uint64_t   prev;
  uint64_t   size;
  uint64_t   fill;
  ur_mug    *mugs;
  uint8_t **bytes;
  uint64_t  *lens;
} ur_atoms_t;

typedef struct ur_root_s {
  ur_cells_t cells;
  ur_atoms_t atoms;
} ur_root_t;

ur_nref
ur_coin_bytes(ur_root_t *r, uint8_t *byt, uint64_t len);

ur_nref
ur_coin64(ur_root_t *r, uint64_t n);

ur_nref
ur_cons(ur_root_t *r, ur_nref hed, ur_nref tal);

void
ur_hcon_info(FILE *f, ur_root_t *r);

void
ur_hcon_free(ur_root_t *r);

ur_root_t*
ur_hcon_init(void);

void
ur_nvec_free(ur_nvec_t *v);

void
ur_nvec_init(ur_nvec_t *v, uint64_t size);
