#ifndef UR_HASHCONS_H
#define UR_HASHCONS_H

#include <inttypes.h>
#include <assert.h>
#include <limits.h>
#include <stdio.h>

#include "ur/defs.h"

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

uint64_t
ur_met(ur_root_t *r, uint8_t bloq, ur_nref ref);

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
ur_coin_bytes_unsafe(ur_root_t *r, uint8_t *byt, uint64_t len);

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

#endif