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

#define ur_lz8(a)      ( ur_lz32(a) - 24 )

#define ur_mask_3(a)   (a & 0x7)
#define ur_mask_8(a)   (a & 0xff)
#define ur_mask_31(a)  (a & 0x7fffffff)
#define ur_mask_62(a)  (a & 0x3fffffffffffffffULL)

#define ur_met0_8(a)   ( (a) ? 8  - ur_lz8(a)  : 0 )
#define ur_met0_32(a)  ( (a) ? 32 - ur_lz32(a) : 0 )
#define ur_met0_64(a)  ( (a) ? 64 - ur_lz64(a) : 0 )

#define ur_met3_8(a)                            \
        ({ uint8_t _a = ur_met0_8(a);           \
           ( (_a >> 3) + !!ur_mask_3(_a) ); })  \

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

typedef struct ur_pail32_s {
  uint8_t  fill;
  ur_nref  refs[10];
  uint32_t vals[10];
} ur_pail32_t;

typedef struct ur_dict32_s {
  uint64_t        prev;
  uint64_t        size;
  ur_pail32_t *buckets;
} ur_dict32_t;

typedef struct ur_pail64_s {
  uint8_t  fill;
  ur_nref  refs[10];
  uint64_t vals[10];
} ur_pail64_t;

typedef struct ur_dict64_s {
  uint64_t        prev;
  uint64_t        size;
  ur_pail64_t *buckets;
} ur_dict64_t;

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

void
ur_dict32_grow(ur_root_t *r, ur_dict32_t *dict, uint64_t prev, uint64_t size);

ur_bool_t
ur_dict32_get(ur_root_t *r, ur_dict32_t *dict, ur_nref ref, uint32_t *out);

void
ur_dict32_put(ur_root_t *r, ur_dict32_t *dict, ur_nref ref, uint32_t val);

void
ur_dict64_grow(ur_root_t *r, ur_dict64_t *dict, uint64_t prev, uint64_t size);

void
ur_dict_free(ur_dict_t *dict);

ur_bool_t
ur_dict64_get(ur_root_t *r, ur_dict64_t *dict, ur_nref ref, uint64_t *out);

void
ur_dict64_put(ur_root_t *r, ur_dict64_t *dict, ur_nref ref, uint64_t val);

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

void
ur_walk_fore(ur_root_t *r,
             ur_nref  ref,
             void      *v,
             void      (*atom)(ur_root_t*, ur_nref, void*),
             ur_bool_t (*cell)(ur_root_t*, ur_nref, void*));


typedef struct ur_bsw_s {
  uint64_t    prev;
  uint64_t    size;
  uint64_t    fill;
  uint64_t    bits;
  uint8_t      off;
  uint8_t   *bytes;
} ur_bsw_t;

void
ur_bsw_grow(ur_bsw_t *bsw);

ur_bool_t
ur_bsw_sane(ur_bsw_t *bsw);

void
ur_bsw_bit(ur_bsw_t *bsw, uint8_t bit);

void
ur_bsw8(ur_bsw_t *bsw, uint8_t len, uint8_t byt);

void
ur_bsw32(ur_bsw_t *bsw, uint8_t len, uint32_t val);

void
ur_bsw64(ur_bsw_t *bsw, uint8_t len_bit, uint64_t val);

void
ur_bsw_bytes(ur_bsw_t *bsw, uint64_t len, uint8_t *byt);

void
ur_bsw_bex(ur_bsw_t *bsw, uint8_t n);


typedef struct ur_bsr_s {
  uint64_t        left;
  uint64_t        bits;
  uint8_t          off;
  const uint8_t *bytes;
} ur_bsr_t;

typedef enum {
  ur_cue_good = 0,
  ur_cue_gone = 1
} ur_cue_res_e;

ur_bool_t
ur_bsr_sane(ur_bsr_t *bsr);

ur_cue_res_e
ur_bsr_bit(ur_bsr_t *bsr, uint8_t *out);

uint8_t
ur_bsr_bit_any(ur_bsr_t *bsr);

uint8_t
ur_bsr8_any(ur_bsr_t *bsr, uint8_t len);

uint32_t
ur_bsr32_any(ur_bsr_t *bsr, uint8_t len);

uint64_t
ur_bsr64_any(ur_bsr_t *bsr, uint8_t len);

uint64_t
ur_jam(ur_root_t *r, ur_nref ref, uint64_t *len, uint8_t **byt);

typedef enum {
  ur_jam_atom = 0,
  ur_jam_cell = 1,
  ur_jam_back = 2
} ur_cue_tag_e;

ur_cue_res_e
ur_cue(ur_root_t *r, uint64_t len, const uint8_t *byt, ur_nref *out);
