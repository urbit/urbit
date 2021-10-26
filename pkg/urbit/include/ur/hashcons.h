#ifndef UR_HASHCONS_H
#define UR_HASHCONS_H

#include <inttypes.h>
#include <assert.h>
#include <limits.h>
#include <stdio.h>

#include "ur/defs.h"

/*
**  64-bit noun references, with the top 2 bits reserved for type tags.
*/
typedef uint64_t ur_nref;

typedef enum {
  ur_direct = 0,
  ur_iatom = 1,
  ur_icell = 2,
} ur_tag;

#define ur_nref_tag(ref)       ( ref >> 62 )
#define ur_nref_idx(ref)       ur_mask_62(ref)

/*
**  31-bit, non-zero, murmur3-based noun hash.
*/
typedef uint32_t ur_mug;

/*
**  associative structures (dictionaries) of noun references,
**  distributed by mug across fixed-size buckets (pails),
**  reallocated with fibonacci growth once a bucket is full.
**
**    - ur_dict_t:   set of noun references
**    - ur_dict32_t: map from noun reference to uint32
**    - ur_dict32_t: map from noun reference to uint64
*/

#define ur_pail_max            10

typedef struct ur_pail32_s {
  ur_nref  refs[ur_pail_max];
  uint32_t vals[ur_pail_max];
} ur_pail32_t;

typedef struct ur_dict32_s {
  uint64_t        prev;
  uint64_t        size;
  uint8_t       *fills;
  ur_pail32_t *buckets;
} ur_dict32_t;

typedef struct ur_pail64_s {
  ur_nref  refs[ur_pail_max];
  uint64_t vals[ur_pail_max];
} ur_pail64_t;

typedef struct ur_dict64_s {
  uint64_t        prev;
  uint64_t        size;
  uint8_t       *fills;
  ur_pail64_t *buckets;
} ur_dict64_t;

typedef struct ur_pail_s {
  ur_nref  refs[ur_pail_max];
} ur_pail_t;

typedef struct ur_dict_s {
  uint64_t      prev;
  uint64_t      size;
  uint8_t     *fills;
  ur_pail_t *buckets;
} ur_dict_t;

/*
**  cells are hash-consed, atoms are deduplicated (byte-array comparison),
**  mug hashes are stored, and noun references are unique within a root.
*/
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

/*
**  a vector of noun references.
*/
typedef struct ur_nvec_s {
  uint64_t fill;
  ur_nref* refs;
} ur_nvec_t;

/*
**  opaque handle for repeated traversal.
*/
typedef struct ur_walk_fore_s ur_walk_fore_t;

/*
**  type-specific dictionary operations.
**
**    NB: [r] is only used to retrieve the stored mug of cells and
**    indirect atoms. If all references are direct atoms (62-bits or less),
**    [r] can be null. This option is used extensively in cue (de-serialization)
**    implementations, where the dictionary keys are bit-cursors.
*/
void
ur_dict32_grow(ur_root_t *r, ur_dict32_t *dict, uint64_t prev, uint64_t size);

ur_bool_t
ur_dict32_get(ur_root_t *r, ur_dict32_t *dict, ur_nref ref, uint32_t *out);

void
ur_dict32_put(ur_root_t *r, ur_dict32_t *dict, ur_nref ref, uint32_t val);

void
ur_dict32_wipe(ur_dict32_t *dict);

void
ur_dict64_grow(ur_root_t *r, ur_dict64_t *dict, uint64_t prev, uint64_t size);

ur_bool_t
ur_dict64_get(ur_root_t *r, ur_dict64_t *dict, ur_nref ref, uint64_t *out);

void
ur_dict64_put(ur_root_t *r, ur_dict64_t *dict, ur_nref ref, uint64_t val);

void
ur_dict64_wipe(ur_dict64_t *dict);

void
ur_dict_grow(ur_root_t *r, ur_dict_t *dict, uint64_t prev, uint64_t size);

ur_bool_t
ur_dict_get(ur_root_t *r, ur_dict_t *dict, ur_nref ref);

void
ur_dict_put(ur_root_t *r, ur_dict_t *dict, ur_nref ref);

void
ur_dict_wipe(ur_dict_t *dict);

/*
**  free the buckets of any dictionary (cast to ur_dict_t*).
*/
void
ur_dict_free(ur_dict_t *dict);

/*
**  measure the bloq (binary-exponent) length of an atom in [r]
*/
uint64_t
ur_met(ur_root_t *r, uint8_t bloq, ur_nref ref);

/*
**  find or allocate an atom in [r]
**
**  unsafe variant is unsafe wrt allocation (byte arrays must be
**  allocated with system malloc) and trailing null bytes (not allowed).
*/
ur_nref
ur_coin_bytes_unsafe(ur_root_t *r, uint64_t len, uint8_t *byt);

ur_nref
ur_coin_bytes(ur_root_t *r, uint64_t len, uint8_t *byt);

ur_nref
ur_coin64(ur_root_t *r, uint64_t n);

/*
**  find or construct a cell in [r]
*/
ur_nref
ur_cons(ur_root_t *r, ur_nref hed, ur_nref tal);

/*
**  calculate the mug of [ref], or produce the stored value in [r].
*/
ur_mug
ur_nref_mug(ur_root_t *r, ur_nref ref);

/*
**  initialize a noun arena (root).
*/
ur_root_t*
ur_root_init(void);

/*
**  print root details to [f]
*/
void
ur_root_info(FILE *f, ur_root_t *r);

/*
**  dispose all allocations in [r]
*/
void
ur_root_free(ur_root_t *r);

/*
**  initialize or dispose a vector of noun references
*/
void
ur_nvec_init(ur_nvec_t *v, uint64_t size);

void
ur_nvec_free(ur_nvec_t *v);

/*
**  depth-first, pre-order noun traversal, cells can short-circuit.
*/
void
ur_walk_fore(ur_root_t     *r,
             ur_nref      ref,
             void          *v,
             void      (*atom)(ur_root_t*, ur_nref, void*),
             ur_bool_t (*cell)(ur_root_t*, ur_nref, void*));

ur_walk_fore_t*
ur_walk_fore_init_with(ur_root_t    *r,
                       uint32_t s_prev,
                       uint32_t s_size);

ur_walk_fore_t*
ur_walk_fore_init(ur_root_t *r);

void
ur_walk_fore_with(ur_walk_fore_t *w,
                  ur_nref       ref,
                  void           *v,
                  void       (*atom)(ur_root_t*, ur_nref, void*),
                  ur_bool_t  (*cell)(ur_root_t*, ur_nref, void*));

void
ur_walk_fore_done(ur_walk_fore_t *w);

#endif
