#include <inttypes.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

#include <limits.h>

#include <murmur3.h>

#include "ur/hashcons.h"

ur_mug
ur_mug_bytes(const uint8_t *byt, uint64_t len)
{
  uint32_t seed = 0xcafebabe;
  ur_mug    mug;

  while ( 1 ) {
    uint32_t raw;
    MurmurHash3_x86_32(byt, len, seed, &raw);
    mug = (raw >> 31) ^ ( ur_mask_31(raw) );

    if ( 0 == mug ) {
      seed++;
    }
    else {
      return mug;
    }
  }
}

ur_mug
ur_mug32(uint32_t x)
{
  uint8_t byt[4] = {
    ur_mask_8(x >>  0),
    ur_mask_8(x >>  8),
    ur_mask_8(x >> 16),
    ur_mask_8(x >> 24)
  };

  return ur_mug_bytes(byt, ur_met3_32(x));
}

ur_mug
ur_mug64(uint64_t x)
{
  uint8_t byt[8] = {
    ur_mask_8(x >>  0),
    ur_mask_8(x >>  8),
    ur_mask_8(x >> 16),
    ur_mask_8(x >> 24),
    ur_mask_8(x >> 32),
    ur_mask_8(x >> 40),
    ur_mask_8(x >> 48),
    ur_mask_8(x >> 56)
  };

  return ur_mug_bytes(byt, ur_met3_64(x));
}

ur_mug
ur_mug_both(ur_mug hed, ur_mug tal)
{
  //  XX not correct per u3r_mug, but necessary to avoid collisions
  //
  return ur_mug32(hed ^ (0x7fffffff ^ ur_mug32(tal)));
}

ur_mug
ur_nref_mug(ur_root_t *r, ur_nref ref)
{
  switch ( ur_nref_tag(ref) ) {
    default: assert(0);

    case ur_direct: return ur_mug64(ref);
    case ur_iatom:  return r->atoms.mugs[ur_nref_idx(ref)];
    case ur_icell:  return r->cells.mugs[ur_nref_idx(ref)];
  }
}

ur_bool_t
ur_deep(ur_nref ref)
{
  return ur_icell == ur_nref_tag(ref);
}

ur_nref
ur_head(ur_root_t *r, ur_nref ref)
{
  assert( ur_deep(ref) );
  return r->cells.heads[ur_nref_idx(ref)];
}

ur_nref
ur_tail(ur_root_t *r, ur_nref ref)
{
  assert( ur_deep(ref) );
  return r->cells.tails[ur_nref_idx(ref)];
}
void
ur_dict32_grow(ur_root_t *r, ur_dict32_t *dict, uint64_t prev, uint64_t size)
{
  ur_pail32_t *buckets, *old_buckets = dict->buckets;
  uint64_t old_size = dict->size;
  uint64_t  i, next = prev + size;

  buckets = calloc(next, sizeof(*buckets));

  for ( i = 0; i < old_size; i++ ) {
    ur_pail32_t *old_bucket = &(old_buckets[i]);
    uint8_t     j, old_fill = old_bucket->fill;

    for ( j = 0; j < old_fill; j++ ) {
      uint32_t val = old_bucket->vals[j];
      ur_nref  ref = old_bucket->refs[j];
      ur_mug   mug = ur_nref_mug(r, ref);

      uint64_t        idx = ( mug % next );
      ur_pail32_t *bucket = &(buckets[idx]);
      uint8_t    new_fill = bucket->fill;

      if ( 10 == new_fill ) {
        free(buckets);
        return ur_dict32_grow(r, dict, size, next);
      }

      bucket->refs[new_fill] = ref;
      bucket->vals[new_fill] = val;
      bucket->fill = 1 + new_fill;
    }
  }

  free(old_buckets);

  dict->prev = size;
  dict->size = next;
  dict->buckets = buckets;
}

ur_bool_t
ur_dict32_get(ur_root_t *r, ur_dict32_t *dict, ur_nref ref, uint32_t *out)
{
  ur_mug   mug = ur_nref_mug(r, ref);
  uint64_t idx = ( mug % dict->size );

  ur_pail32_t *bucket = &(dict->buckets[idx]);
  uint8_t     i, fill = bucket->fill;

  for ( i = 0; i < fill; i++ ) {
    if ( ref == bucket->refs[i] ) {
      *out = bucket->vals[i];
      return 1;
    }
  }

  return 0;
}

void
ur_dict32_put(ur_root_t *r, ur_dict32_t *dict, ur_nref ref, uint32_t val)
{
  ur_mug mug = ur_nref_mug(r, ref);

  while ( 1 ) {
    uint64_t        idx = ( mug % dict->size );
    ur_pail32_t *bucket = &(dict->buckets[idx]);
    uint8_t     i, fill = bucket->fill;

    for ( i = 0; i < fill; i++ ) {
      if ( ref == bucket->refs[i] ) {
        bucket->vals[i] = val;
        return;
      }
    }

    if ( 10 == fill ) {
      ur_dict32_grow(r, dict, dict->prev, dict->size);
      continue;
    }

    bucket->refs[fill] = ref;
    bucket->vals[fill] = val;
    bucket->fill = 1 + fill;
    break;
  }
}

void
ur_dict64_grow(ur_root_t *r, ur_dict64_t *dict, uint64_t prev, uint64_t size)
{
  ur_pail64_t *buckets, *old_buckets = dict->buckets;
  uint64_t old_size = dict->size;
  uint64_t  i, next = prev + size;

  buckets = calloc(next, sizeof(*buckets));

  for ( i = 0; i < old_size; i++ ) {
    ur_pail64_t *old_bucket = &(old_buckets[i]);
    uint8_t     j, old_fill = old_bucket->fill;

    for ( j = 0; j < old_fill; j++ ) {
      uint64_t val = old_bucket->vals[j];
      ur_nref  ref = old_bucket->refs[j];
      ur_mug   mug = ur_nref_mug(r, ref);

      uint64_t        idx = ( mug % next );
      ur_pail64_t *bucket = &(buckets[idx]);
      uint8_t    new_fill = bucket->fill;

      if ( 10 == new_fill ) {
        free(buckets);
        return ur_dict64_grow(r, dict, size, next);
      }

      bucket->refs[new_fill] = ref;
      bucket->vals[new_fill] = val;
      bucket->fill = 1 + new_fill;
    }
  }

  free(old_buckets);

  dict->prev = size;
  dict->size = next;
  dict->buckets = buckets;
}

ur_bool_t
ur_dict64_get(ur_root_t *r, ur_dict64_t *dict, ur_nref ref, uint64_t *out)
{
  ur_mug   mug = ur_nref_mug(r, ref);
  uint64_t idx = ( mug % dict->size );

  ur_pail64_t *bucket = &(dict->buckets[idx]);
  uint8_t     i, fill = bucket->fill;

  for ( i = 0; i < fill; i++ ) {
    if ( ref == bucket->refs[i] ) {
      *out = bucket->vals[i];
      return 1;
    }
  }

  return 0;
}

void
ur_dict64_put(ur_root_t *r, ur_dict64_t *dict, ur_nref ref, uint64_t val)
{
  ur_mug mug = ur_nref_mug(r, ref);

  while ( 1 ) {
    uint64_t        idx = ( mug % dict->size );
    ur_pail64_t *bucket = &(dict->buckets[idx]);
    uint8_t     i, fill = bucket->fill;

    for ( i = 0; i < fill; i++ ) {
      if ( ref == bucket->refs[i] ) {
        bucket->vals[i] = val;
        return;
      }
    }

    if ( 10 == fill ) {
      ur_dict64_grow(r, dict, dict->prev, dict->size);
      continue;
    }

    bucket->refs[fill] = ref;
    bucket->vals[fill] = val;
    bucket->fill = 1 + fill;
    break;
  }
}

void
ur_dict_grow(ur_root_t *r, ur_dict_t *dict, uint64_t prev, uint64_t size)
{
  ur_pail_t *buckets, *old_buckets = dict->buckets;
  uint64_t old_size = dict->size;
  uint64_t  i, next = prev + size;

  buckets = calloc(next, sizeof(*buckets));

  for ( i = 0; i < old_size; i++ ) {
    ur_pail_t *old_bucket = &(old_buckets[i]);
    uint8_t   j, old_fill = old_bucket->fill;

    for ( j = 0; j < old_fill; j++ ) {
      ur_nref ref = old_bucket->refs[j];
      ur_mug  mug = ur_nref_mug(r, ref);
      
      uint64_t      idx = ( mug % next );
      ur_pail_t *bucket = &(buckets[idx]);
      uint8_t  new_fill = bucket->fill;

      if ( 10 == new_fill ) {
        free(buckets);
        return ur_dict_grow(r, dict, size, next);
      }

      bucket->refs[new_fill] = ref;
      bucket->fill = 1 + new_fill;
    }
  }

  free(old_buckets);

  dict->prev = size;
  dict->size = next;
  dict->buckets = buckets;
}

void
ur_dict_free(ur_dict_t *dict)
{
  free(dict->buckets);
}

void
ur_atoms_grow(ur_atoms_t *atoms)
{
  uint64_t   prev = atoms->prev;
  uint64_t   size = atoms->size;
  uint64_t   next = prev + size;
  uint8_t **bytes = atoms->bytes;
  uint64_t  *lens = atoms->lens;
  ur_mug    *mugs = atoms->mugs;

  atoms->bytes = malloc(next * ( sizeof(*atoms->bytes)
                               + sizeof(*atoms->lens)
                               + sizeof(*atoms->mugs) ));
  atoms->lens  = (void*)((char*)atoms->bytes + (next * sizeof(*atoms->bytes)));
  atoms->mugs  = (void*)((char*)atoms->lens  + (next * sizeof(*atoms->lens)));

  assert( atoms->bytes );

  if ( bytes ) {
    memcpy(atoms->bytes, bytes, size * (sizeof(*bytes)));
    memcpy(atoms->lens,   lens, size * (sizeof(*lens)));
    memcpy(atoms->mugs,   mugs, size * (sizeof(*mugs)));

    free(bytes);
  }

  atoms->prev = size;
  atoms->size = next;
}

void
ur_cells_grow(ur_cells_t *cells)
{
  uint64_t  prev = cells->prev;
  uint64_t  size = cells->size;
  uint64_t  next = prev + size;
  ur_nref *heads = cells->heads;
  ur_nref *tails = cells->tails;
  ur_mug   *mugs = cells->mugs;

  cells->heads = malloc(next * ( sizeof(*cells->heads)
                               + sizeof(*cells->heads)
                               + sizeof(*cells->mugs) ));
  cells->tails = (void*)((char*)cells->heads + (next * sizeof(*cells->heads)));
  cells->mugs  = (void*)((char*)cells->tails + (next * sizeof(*cells->tails)));

  assert( cells->heads );

  if ( heads ) {
    memcpy(cells->heads, heads, size * (sizeof(*heads)));
    memcpy(cells->tails, tails, size * (sizeof(*tails)));
    memcpy(cells->mugs,   mugs, size * (sizeof(*mugs)));

    free(heads);
  }

  cells->prev = size;
  cells->size = next;
}

void
ur_bytes(ur_root_t *r, ur_nref ref, uint8_t **byt, uint64_t *len)
{
  assert( !ur_deep(ref) );
  switch ( ur_nref_tag(ref) ) {
    default: assert(0);

    case ur_direct: {
      *len = ur_met3_64(ref);
      //  XX little-endian
      //
      *byt = (uint8_t*)&ref;
    } break;

    case ur_iatom: {
      uint64_t idx = ur_nref_idx(ref);
      *len = r->atoms.lens[idx];
      *byt = r->atoms.bytes[idx];
    } break;
  }
}

static inline uint64_t
_met0_bytes(uint8_t *byt, uint64_t len)
{
  //  XX requires no trailing null bytes
  //
  uint64_t last = len - 1;
  return (last << 3) + ur_met0_8(byt[last]);
}

uint64_t
ur_met(ur_root_t *r, uint8_t bloq, ur_nref ref)
{
  assert( !ur_deep(ref) );

  //  these cases are the same, except for the
  //  bit-width calculation and the width of their operands
  //
  switch ( ur_nref_tag(ref) ) {
    default: assert(0);

    case ur_direct: {
      uint8_t m_bit = ur_met0_64(ref);

      switch ( bloq ) {
        case 0: return m_bit;
        case 1: return (m_bit + 1) >> 1;
        case 2: return (m_bit + 3) >> 2;

        {
          //  hand-inline of ur_met3_64
          //
          uint8_t m_byt = (m_bit >> 3) + !!ur_mask_3(m_bit);

          case 3: return m_byt;
          default: {
            uint8_t off = (bloq - 3);
            return (m_byt + ((1 << off) - 1)) >> off;
          }
        }
      }
    } break;

    case ur_iatom: {
      uint64_t m_bit;

      {
        uint64_t idx = ur_nref_idx(ref);
        uint64_t len = r->atoms.lens[idx];
        uint8_t *byt = r->atoms.bytes[idx];

        m_bit = _met0_bytes(byt, len);
      }

      switch ( bloq ) {
        case 0: return m_bit;
        case 1: return (m_bit + 1) >> 1;
        case 2: return (m_bit + 3) >> 2;

        {
          //  hand-inline of ur_met3_64
          //
          uint64_t m_byt = (m_bit >> 3) + !!ur_mask_3(m_bit);

          case 3: return m_byt;
          default: {
            uint8_t off = (bloq - 3);
            return (m_byt + ((1ULL << off) - 1)) >> off;
          }
        }
      }
    } break;
  }
}

static ur_nref
_coin_unsafe(ur_atoms_t *atoms, ur_mug mug, uint8_t *byt, uint64_t len)
{
  uint64_t fill = atoms->fill;
  ur_tag    tag = ur_iatom;
  ur_nref   tom = ( fill | ((uint64_t)tag << 62) );

  //  XX necessary?
  //
  assert( 62 >= ur_met0_64(fill) );

  atoms->bytes[fill] = byt;
  atoms->lens[fill]  = len;
  atoms->mugs[fill]  = mug;
  atoms->fill = 1 + fill;

  return tom;
}

static ur_nref
_cons_unsafe(ur_cells_t *cells, ur_mug mug, ur_nref hed, ur_nref tal)
{
  uint64_t fill = cells->fill;
  ur_tag    tag = ur_icell;
  ur_nref   cel = ( fill | ((uint64_t)tag << 62) );

  //  XX necessary?
  //
  assert( 62 >= ur_met0_64(fill) );

  cells->mugs[fill]  = mug;
  cells->heads[fill] = hed;
  cells->tails[fill] = tal;
  cells->fill = 1 + fill;

  return cel;
}

static ur_nref
_coin_bytes_unsafe(ur_root_t *r, uint8_t *byt, uint64_t len)
{
  ur_atoms_t *atoms = &(r->atoms);
  ur_dict_t   *dict = &(atoms->dict);
  ur_mug        mug = ur_mug_bytes(byt, len);

  while ( 1 ) {
    uint64_t      idx = ( mug % dict->size );
    ur_pail_t *bucket = &(dict->buckets[idx]);
    uint8_t i, b_fill = bucket->fill;
    ur_nref tom;

    for ( i = 0; i < b_fill; i++ ) {
      uint8_t *t_byt;
      uint64_t t_len;
      tom = bucket->refs[i];

      ur_bytes(r, tom, &t_byt, &t_len);

      if (  (t_len == len)
         && (0 == memcmp(t_byt, byt, len)) )
      {
        return tom;
      }
    }

    if ( 10 == b_fill ) {
      ur_dict_grow(r, dict, dict->prev, dict->size);
      continue;
    }

    if ( atoms->fill == atoms->size ) {
      ur_atoms_grow(atoms);
    }

    tom = _coin_unsafe(atoms, mug, byt, len);
    
    bucket->refs[b_fill] = tom;
    bucket->fill = 1 + b_fill;

    return tom;
  }
}

ur_nref
ur_coin_bytes(ur_root_t *r, uint8_t *byt, uint64_t len)
{
  //  strip trailing zeroes
  //
  while ( len && !byt[len - 1] ) {
    len--;
  }

  //  produce a direct atom if possible
  //
  if ( 62 >= _met0_bytes(byt, len) ) {
    uint64_t i, direct = 0;

    for ( i = 0; i < len; i++ ) {
      direct |= byt[i] << (8 * i);
    }

    return (ur_nref)direct;
  }
  else {
    uint8_t *copy = malloc(len);
    assert( copy );
    memcpy(copy, byt, len);

    return _coin_bytes_unsafe(r, copy, len);
  }
}

ur_nref
ur_coin64(ur_root_t *r, uint64_t n)
{
  if ( ur_direct == ur_nref_tag(n) ) {
    return n;
  }
  else {
    uint8_t *byt = malloc(8);
    assert( byt );
    assert( 8 == ur_met3_64(n) );

    byt[0] = ur_mask_8(n);
    byt[1] = ur_mask_8(n >>  8);
    byt[2] = ur_mask_8(n >> 16);
    byt[3] = ur_mask_8(n >> 24);
    byt[4] = ur_mask_8(n >> 32);
    byt[5] = ur_mask_8(n >> 40);
    byt[6] = ur_mask_8(n >> 48);
    byt[7] = ur_mask_8(n >> 56);

    return _coin_bytes_unsafe(r, byt, 8);
  }
}

ur_nref
ur_cons(ur_root_t *r, ur_nref hed, ur_nref tal)
{
  ur_cells_t *cells = &(r->cells);
  ur_dict_t   *dict = &(cells->dict);
  ur_mug mug        = ur_mug_both(ur_nref_mug(r, hed),
                                  ur_nref_mug(r, tal));

  while ( 1 ) {
    uint64_t      idx = ( mug % dict->size );
    ur_pail_t *bucket = &(dict->buckets[idx]);
    uint8_t i, b_fill = bucket->fill;
    ur_nref cel;

    for ( i = 0; i < b_fill; i++ ) {
      cel = bucket->refs[i];

      if (  (hed == ur_head(r, cel))
         && (tal == ur_tail(r, cel)) )
      {
        return cel;
      }
    }

    if ( 10 == b_fill ) {
      ur_dict_grow(r, dict, dict->prev, dict->size);
      continue;
    }

    if ( cells->fill == cells->size ) {
      ur_cells_grow(cells);
    }

    cel = _cons_unsafe(cells, mug, hed, tal);

    bucket->refs[b_fill] = cel;
    bucket->fill = 1 + b_fill;

    return cel;
  }
}

static void
_print_memory(FILE *f, const char *c, uint64_t bytes)
{
  if ( !bytes ) {
    fprintf(f, "%s: B/0\r\n", c);
  }
  else {
    uint32_t g = (bytes / 1000000000);
    uint32_t m = (bytes % 1000000000) / 1000000;
    uint32_t k = (bytes % 1000000) / 1000;
    uint32_t b = (bytes % 1000);

    if ( g ) {
      fprintf(f, "%s: GB/%d.%03d.%03d.%03d\r\n", c, g, m, k, b);
    }
    else if ( m ) {
      fprintf(f, "%s: MB/%d.%03d.%03d\r\n", c, m, k, b);
    }
    else if ( k ) {
      fprintf(f, "%s: KB/%d.%03d\r\n", c, k, b);
    }
    else if ( b ) {
      fprintf(f, "%s: B/%d\r\n", c, b);
    }
  }
}

static uint64_t
_dict_info(FILE *f, ur_dict_t *dict)
{
  uint64_t data = dict->size * sizeof(*dict->buckets);
  _print_memory(f, "    dict", data);
  return data;
}

static uint64_t
_atoms_info(FILE *f, ur_atoms_t *atoms)
{
  uint64_t total = 0;
  uint64_t  size = atoms->size;
  uint64_t  fill = atoms->fill;
  uint64_t  refs = size * ( sizeof(*atoms->bytes)
                          + sizeof(*atoms->lens)
                          + sizeof(*atoms->mugs) );
  uint64_t i, data = 0;

  fprintf(f, "  atoms (%" PRIu64 "):\r\n", fill);

  _print_memory(f, "    refs", refs);
  total += refs;

  for ( i = 0; i < fill; i++ ) {
    data += atoms->lens[i];
  }
  _print_memory(f, "    data", data);
  total += data;

  total += _dict_info(f, &(atoms->dict));
  _print_memory(f, "  total", total);
  return total;
}

static uint64_t
_cells_info(FILE *f, ur_cells_t *cells)
{
  uint64_t total = 0;
  uint64_t  size = cells->size;
  uint64_t  fill = cells->fill;
  uint64_t  refs = size * ( sizeof(*cells->heads)
                          + sizeof(*cells->heads)
                          + sizeof(*cells->mugs) );

  fprintf(f, "  cells (%" PRIu64 "):\r\n", fill);

  _print_memory(f, "    refs", refs);
  total += refs;

  total += _dict_info(f, &(cells->dict));
  _print_memory(f, "  total", total);
  return total;
}

void
ur_hcon_info(FILE *f, ur_root_t *r)
{
  uint64_t total = 0;

  fprintf(stderr, "hash-cons arena:\r\n");

  {
    uint64_t root = sizeof(*r);
    _print_memory(f, "  root", root);
    total += root;
  }

  total += _atoms_info(f, &(r->atoms));
  total += _cells_info(f, &(r->cells));

  _print_memory(f, "total", total);
}

static void
_atoms_free(ur_atoms_t *atoms)
{
  uint8_t  **bytes = atoms->bytes;
  uint64_t i, fill = atoms->fill;

  for ( i = 0; i < fill; i++ ) {
    free(bytes[i]);
  }

  ur_dict_free(&(atoms->dict));
  free(bytes);
}

static void
_cells_free(ur_cells_t *cells)
{
  ur_dict_free(&(cells->dict));
  free(cells->heads);
}

void
ur_hcon_free(ur_root_t *r)
{
  _atoms_free(&(r->atoms));
  _cells_free(&(r->cells));
  free(r);
}

ur_root_t*
ur_hcon_init(void)
{
  ur_root_t *r = calloc(1, sizeof(*r));
  assert( r );

  {
    ur_dict_t *dict;
    uint64_t fib11 = 89, fib12 = 144;

    //  allocate atom storage
    //
    r->atoms.prev  = fib11;
    r->atoms.size  = fib12;
    ur_atoms_grow(&(r->atoms));

    //  allocate atom hashtable
    //
    dict = &(r->atoms.dict);
    ur_dict_grow(r, dict, fib11, fib12);

    //  allocate cell storage
    //
    r->cells.prev  = fib11;
    r->cells.size  = fib12;
    ur_cells_grow(&(r->cells));

    //  allocate cell hashtable
    //
    dict = &(r->cells.dict);
    ur_dict_grow(r, dict, fib11, fib12);
  }

  return r;
}

void
ur_nvec_free(ur_nvec_t *v)
{
  free(v->refs);
}

void
ur_nvec_init(ur_nvec_t *v, uint64_t size)
{
  v->fill = 0;
  v->refs = calloc(size, sizeof(ur_nref));
}

void
ur_walk_fore(ur_root_t *r,
             ur_nref  ref,
             void      *v,
             void      (*atom)(ur_root_t*, ur_nref, void*),
             ur_bool_t (*cell)(ur_root_t*, ur_nref, void*))
{
  uint64_t prev = 89, size = 144, fill = 0;
  ur_nref *top, *don;

  don  = malloc(size * sizeof(*don));
  top  = don + ++fill;
  *top = ref;

  while ( top != don ) {
    //  visit atom, pop stack
    //
    if ( !ur_deep(ref) ) {
      atom(r, ref, v);
      top--; fill--;
    }
    //  visit cell, pop stack if false
    //
    else if ( !cell(r, ref, v) ) {
      top--; fill--;
    }
    //  push the tail, continue into the head
    //
    else {
      *top = ur_tail(r, ref);

      //  reallocate "stack" if full
      //
      if ( size == fill ) {
        uint64_t next = prev + size;
        don  = realloc(don, next * sizeof(*don));
        top  = don + fill;
        prev = size;
        size = next;
      }

      top++; fill++;
      *top = ur_head(r, ref);
    }

    ref = *top;
  }

  free(don);
}

void
ur_bsw_grow(ur_bsw_t *bsw)
{
  uint64_t prev = bsw->prev;
  uint64_t size = bsw->size;
  uint64_t next = prev + size;

  bsw->bytes = realloc(bsw->bytes, next);
  assert(bsw->bytes);
  memset(bsw->bytes + size, 0, prev);

  bsw->prev  = size;
  bsw->size  = next;
}

ur_bool_t
ur_bsw_sane(ur_bsw_t *bsw)
{
  return (  (8 > bsw->off)
         && ((bsw->fill << 3) + bsw->off == bsw->bits) );
}

static inline void
_bsw_bit_unsafe(ur_bsw_t *bsw, uint8_t bit)
{
  uint64_t fill = bsw->fill;
  uint8_t   off = bsw->off;

  bsw->bytes[fill] ^= (bit & 1) << off;

  if ( 7 == off ) {
    bsw->fill = 1 + fill;
    bsw->off  = 0;
  }
  else {
    bsw->off  = 1 + off;
  }

  bsw->bits++;
}

void
ur_bsw_bit(ur_bsw_t *bsw, uint8_t bit)
{
  if (  (7 == bsw->off)
     && ((1 + bsw->fill) == bsw->size) )
  {
    ur_bsw_grow(bsw);
  }

  _bsw_bit_unsafe(bsw, bit);
}

static inline void
_bsw8_unsafe(ur_bsw_t *bsw, uint8_t len, uint8_t byt)
{
  uint64_t fill = bsw->fill;
  uint8_t   off = bsw->off;
  uint8_t rest = 8 - off;
  uint8_t  old = bsw->bytes[fill];

  if ( len < rest ) {
    uint8_t left = (byt & ((1 << len) - 1)) << off;

    bsw->bytes[fill] = old ^ left;
    bsw->off = off + len;
  }
  else {
    uint8_t left, right;

    left  = (byt & ((1 << rest) - 1)) << off;
    off   = len - rest;
    right = (byt >> rest) & ((1 << off) - 1);

    bsw->bytes[fill] = old ^ left;
    fill++;
    bsw->bytes[fill] = right;

    bsw->fill = fill;
    bsw->off  = off;
  }

  bsw->bits += len;
}

void
ur_bsw8(ur_bsw_t *bsw, uint8_t len, uint8_t byt)
{
  if ( bsw->fill + !!((bsw->off + len) >> 3) >= bsw->size ) {
    ur_bsw_grow(bsw);
  }

  _bsw8_unsafe(bsw, (len > 8) ? 8 : len, byt);
}

void
ur_bsw8_slow(ur_bsw_t *bsw, uint8_t len, uint8_t byt)
{
  while ( len ) {
    ur_bsw_bit(bsw, byt);
    byt >>= 1;
    len--;
  }
}

static inline void
_bsw_bytes_unsafe(ur_bsw_t *bsw, uint64_t len, uint8_t *byt)
{
  uint64_t fill = bsw->fill;
  uint8_t   off = bsw->off;

  if ( !off ) {
    memcpy(bsw->bytes + fill, byt, len);
    fill += len;
  }
  else {
    uint8_t rest = 8 - off;
    uint8_t left, right, old = bsw->bytes[fill];
    uint64_t i;

    for ( i = 0; i < len; i++ ) {
      left  = (byt[i] & ((1 << rest) - 1)) << off;
      right = (byt[i] >> rest) & ((1 << off) - 1);

      bsw->bytes[fill++] = old ^ left;
      old = right;
    }

    bsw->bytes[fill] = old;
  }

  bsw->fill  = fill;
  bsw->bits += len << 3;
}

void
ur_bsw_bytes(ur_bsw_t *bsw, uint64_t len, uint8_t *byt)
{
  if ( (bsw->fill + len + !!bsw->off) >= bsw->size ) {
    uint64_t prev = bsw->prev;

    //  be sure to grow sufficiently
    //
    if ( len > prev ) {
      bsw->prev = len;
    }

    ur_bsw_grow(bsw);
  }

  _bsw_bytes_unsafe(bsw, len, byt);
}

static inline void
_bsw64_unsafe(ur_bsw_t *bsw, uint8_t len_bit, uint64_t val)
{
  //  assumes little-endian
  //
  uint8_t    *byt = (uint8_t*)&val;
  uint8_t len_byt = len_bit >> 3;
  uint8_t     low = ur_mask_3(len_bit);

  if ( len_byt ) {
    _bsw_bytes_unsafe(bsw, len_byt, byt);
  }

  if ( low ) {
    _bsw8_unsafe(bsw, low, byt[len_byt]);
  }
}

void
ur_bsw64(ur_bsw_t *bsw, uint8_t len, uint64_t val)
{
  uint8_t bits = bsw->off + len;

  if ( bsw->fill + (bits >> 3) + !!ur_mask_3(bits) >= bsw->size ) {
    ur_bsw_grow(bsw);
  }

  _bsw64_unsafe(bsw, (len > 64) ? 64 : len, val);
}

void
ur_bsw64_slow(ur_bsw_t *bsw, uint8_t len, uint64_t val)
{
  while ( len ) {
    ur_bsw_bit(bsw, val & 0xff);
    val >>= 1;
    len--;
  }
}

static inline void
_bsw_mat64_unsafe(ur_bsw_t *bsw, uint8_t len_len, uint8_t len, uint64_t val)
{
  if ( 0 == val ) {
    _bsw_bit_unsafe(bsw, 1);
  }
  else {
    _bsw64_unsafe(bsw, len_len + 1, 1ULL << len_len);
    _bsw64_unsafe(bsw, len_len - 1, len);
    _bsw64_unsafe(bsw, len, val);
  }
}

void
ur_bsw_mat64(ur_bsw_t *bsw, uint8_t len, uint64_t val)
{
  len = ( len > 64 ) ? 64 : len;

  {
    uint8_t len_len = ur_met0_64(len);
    uint8_t    next = ( 0 == val ) ? 1 : len + (2 * len_len);
    uint8_t    bits = bsw->off + next;

    if ( bsw->fill + (bits >> 3) + !!ur_mask_3(bits) >= bsw->size ) {
      ur_bsw_grow(bsw);
    }

    _bsw_mat64_unsafe(bsw, len_len, len, val);
  }
}

static inline void
_bsw_mat_bytes_unsafe(ur_bsw_t *bsw, uint8_t len_len, uint64_t len_bit, uint64_t len, uint8_t *byt)
{
  //  write run-length
  //
  _bsw64_unsafe(bsw, len_len + 1, 1ULL << len_len);
  _bsw64_unsafe(bsw, len_len - 1, len_bit);

  //  write bytes
  //
  {
    uint8_t low = ur_mask_3(len_bit);

    if ( !low ) {
      _bsw_bytes_unsafe(bsw, len, byt);
    }
    else {
      len--;
      _bsw_bytes_unsafe(bsw, len, byt);
      _bsw8_unsafe(bsw, low, byt[len]);
    }
  }
}

void
ur_bsw_mat_bytes(ur_bsw_t *bsw, uint64_t len_bit, uint64_t len, uint8_t *byt)
{
  uint8_t len_len = ur_met0_64(len_bit);

  //  XX
  assert( 64 > len_len );

  {
    uint8_t  bits = bsw->off + (2 * len_len);
    uint64_t need = len + (bits >> 3) + !!ur_mask_3(bits);

    if ( (bsw->fill + need) >= bsw->size ) {
      uint64_t prev = bsw->prev;

      //  be sure to grow sufficiently
      //
      if ( need > prev ) {
        bsw->prev = need;
      }

      ur_bsw_grow(bsw);
    }

    _bsw_mat_bytes_unsafe(bsw, len_len, len_bit, len, byt);
  }
}

typedef struct ur_bsr_s {
  uint64_t    left;
  uint64_t    bits;
  uint8_t      off;
  const uint8_t *bytes;
} ur_bsr_t;

static inline ur_cue_res_e
ur_bsr_bit(ur_bsr_t *bsr, uint8_t *out)
{
  uint8_t left = bsr->left;

  if ( !left ) {
    return ur_cue_gone;
  }
  else {
    uint8_t byt = bsr->bytes[0];
    uint8_t off = bsr->off;
    uint8_t bit = (byt >> off) & 1;

    if ( 7 == off ) {
      left--;

      if ( left ) {
        bsr->bytes++;
        bsr->left = left;
      }
      else {
        bsr->bytes = 0;
        bsr->left  = 0;
      }

      bsr->off = 0;
    }
    else {
      bsr->off = 1 + off;
    }

    bsr->bits++;

    *out = bit;

    return ur_cue_good;
  }
}

static inline ur_cue_res_e
ur_bsr_tag(ur_bsr_t *bsr, ur_cue_tag_e *out)
{
  ur_cue_res_e res;
  uint8_t      bit;

  if ( ur_cue_good != (res = ur_bsr_bit(bsr, &bit)) ) {
    return res;
  }
  else if ( 0 == bit ) {
    *out = ur_jam_atom;
    return ur_cue_good;
  }
  else if ( ur_cue_good != (res = ur_bsr_bit(bsr, &bit)) ) {
    return res;
  }

  *out = ( 0 == bit ) ? ur_jam_cell : ur_jam_back;
  return ur_cue_good;
}

static inline ur_cue_res_e
ur_bsr_zeros(ur_bsr_t *bsr, uint8_t *out)
{
  ur_cue_res_e res;
  uint8_t bit, len = 0;

  while ( (ur_cue_good == (res = ur_bsr_bit(bsr, &bit))) && (0 == bit) ) {
    len++;
  }

  if ( ur_cue_good != res ) {
    return res;
  }
  else {
    *out = len;
    return ur_cue_good;
  }
}

static inline uint64_t
ur_bsr64(ur_bsr_t *bsr, uint8_t len)
{
  uint64_t acc = 0;
  uint64_t   i;
  uint8_t  bit;

  for ( i = 0; i < len; i++ ) {
    if ( ur_cue_good != ur_bsr_bit(bsr, &bit) ) {
      bsr->bits += len - i;
      bsr->bytes = 0;
      return acc;
    }

    acc ^= (uint64_t)bit << i;
  }

  return acc;
}

static inline void
ur_bsr_bytes(ur_bsr_t *bsr, uint64_t len, uint8_t *out)
{
  uint8_t  left = bsr->left;
  uint8_t   off = bsr->off;
  ur_bool_t end = len >= left;

  if ( !left ) {
    return;
  }

  if ( !off ) {
    if ( end ) {
      memcpy(out, bsr->bytes, left);
      bsr->bytes = 0;
      left = 0;
    }
    else {
      memcpy(out, bsr->bytes, len);
      bsr->bytes += len;
      left -= len;
    }
  }
  //  the most-significant bits from a byte in the stream
  //  become the least-significant bits of an output byte, and vice-versa
  //
  else {
    uint8_t   rest = 8 - off;
    const uint8_t *bytes = bsr->bytes;
    uint8_t    byt = bytes[0];
    uint8_t   l, m;
    uint64_t   max = end ? (left - 1) : len;
    uint64_t     i;

    for ( i = 0; i < max; i++ ) {
      m      = byt >> off;
      byt    = bytes[1 + i];
      l      = byt & ((1 << off) - 1);
      out[i] = m ^ (l << rest);
    }

    if ( end ) {
      out[max] = bytes[max] >> off;

      bsr->bytes = 0;
      left = 0;
    }
    else {
      bsr->bytes += max;
      left -= max;
    }
  }

  bsr->left  = left;
  bsr->bits += len << 3;
}

static inline ur_cue_res_e
ur_bsr_mat(ur_bsr_t *bsr, uint64_t *out)
{
  uint8_t len;

  if ( ur_cue_gone == ur_bsr_zeros(bsr, &len) ) {
    return ur_cue_gone;
  }

  //  XX
  assert( 64 > len );

  if ( !len ) {
    *out = 0;
  }
  else {
    len--;
    *out = ur_bsr64(bsr, len) ^ (1ULL << len);
  }

  return ur_cue_good;
}

static inline void
_jam_mat(ur_root_t *r, ur_nref ref, ur_bsw_t *bsw, uint64_t len_bit)
{
  switch ( ur_nref_tag(ref) ) {
    default: assert(0);

    case ur_direct: {
      ur_bsw_mat64(bsw, len_bit, ref);
    } break;

    case ur_iatom: {
      uint64_t len;
      uint8_t *byt;
      ur_bytes(r, ref, &byt, &len);
      ur_bsw_mat_bytes(bsw, len_bit, len, byt);
    } break;
  }
}

typedef struct _jam_s {
  ur_dict64_t dict;
  ur_bsw_t     bsw;
} _jam_t;

static void
_jam_atom(ur_root_t *r, ur_nref ref, void *ptr)
{
  _jam_t         *j = ptr;
  ur_dict64_t *dict = &(j->dict);
  ur_bsw_t     *bsw = &j->bsw;
  uint64_t bak, len_bit;

  len_bit = ur_met(r, 0, ref);

  if ( !ur_dict64_get(r, dict, ref, &bak) ) {
    ur_dict64_put(r, dict, ref, bsw->bits);

    ur_bsw_bit(bsw, 0);
    _jam_mat(r, ref, bsw, len_bit);
  }
  else {
    uint64_t bak_bit = ur_met0_64(bak);

    if ( len_bit <= bak_bit ) {
      ur_bsw_bit(bsw, 0);
      _jam_mat(r, ref, bsw, len_bit);
    }
    else {
      ur_bsw_bit(bsw, 1);
      ur_bsw_bit(bsw, 1);
      ur_bsw_mat64(bsw, bak_bit, bak);
    }
  }
}

static ur_bool_t
_jam_cell(ur_root_t *r, ur_nref ref, void *ptr)
{
  _jam_t         *j = ptr;
  ur_dict64_t *dict = &(j->dict);
  ur_bsw_t     *bsw = &j->bsw;
  uint64_t      bak;

  if ( !ur_dict64_get(r, dict, ref, &bak) ) {
    ur_dict64_put(r, dict, ref, bsw->bits);

    ur_bsw_bit(bsw, 1);
    ur_bsw_bit(bsw, 0);

    return 1; // true
  }
  else {
    ur_bsw_bit(bsw, 1);
    ur_bsw_bit(bsw, 1);
    ur_bsw_mat64(bsw, ur_met0_64(bak), bak);

    return 0; // false
  }
}

uint64_t
ur_jam(ur_root_t *r, ur_nref ref, uint64_t *len, uint8_t **byt)
{
  _jam_t j = {0};
  {
    uint64_t fib11 = 89, fib12 = 144;

    j.bsw.prev  = fib11;
    j.bsw.size  = fib12;
    j.bsw.bytes = calloc(j.bsw.size, 1);

    ur_dict64_grow(r, &j.dict, fib11, fib12);
  }

  ur_walk_fore(r, ref, &j, _jam_atom, _jam_cell);
  ur_dict_free((ur_dict_t*)&j.dict);

  *len = j.bsw.fill + !!j.bsw.off;
  *byt = j.bsw.bytes;

  return j.bsw.bits;
}

typedef struct _cue_s {
  ur_dict64_t dict;
  ur_bsr_t     bsr;
} _cue_t;

static inline ur_cue_res_e
_cue_atom(ur_root_t *r, _cue_t *c, ur_nref *out)
{
  ur_bsr_t    *bsr = &c->bsr;
  ur_cue_res_e res;
  uint64_t     len;

  if ( ur_cue_good != (res = ur_bsr_mat(bsr, &len)) ) {
    return res;
  }

  if ( 62 >= len ) {
    *out = (ur_nref)ur_bsr64(bsr, len);
  }
  else {
    uint8_t *byt = calloc(len, 1);
    ur_bsr_bytes(bsr, len, byt);

    //  strip trailing zeroes
    //
    while ( len && !byt[len - 1] ) {
      len--;
    }

    *out = _coin_bytes_unsafe(r, byt, len);
  }

  return ur_cue_good;
}

static inline ur_cue_res_e
_cue_back(ur_bsr_t *bsr, uint64_t *out)
{
  ur_cue_res_e res;
  uint64_t     len;

  if ( ur_cue_good != (res = ur_bsr_mat(bsr, &len)) ) {
    return res;
  }

  //  XX
  assert( 62 >= len );

  *out = ur_bsr64(bsr, len);
  return ur_cue_good;
}

#define STACK_ROOT 0
#define STACK_HEAD 1
#define STACK_TAIL 2

//  stack frame for recording head vs tail iteration
//
//    In Hoon, this structure would be as follows:
//
//    $%  [%root ~]
//        [%head cursor=@]
//        [%tail cursor=@ hed-ref=*]
//    ==
//
typedef struct _cue_frame_s {
  uint8_t   tag;
  uint64_t bits;
  ur_nref   ref;
} _cue_frame_t;

typedef struct _cue_stack_s {
  uint32_t   prev;
  uint32_t   size;
  uint32_t   fill;
  _cue_frame_t* f;
} _cue_stack_t;

static inline void
_cue_stack_push(_cue_stack_t *s, uint8_t tag, uint64_t bits, ur_nref ref)
{
  if ( s->fill == s->size ) {
    uint32_t next = s->prev + s->size;
    s->f = realloc(s->f, next * sizeof(*s->f));
    s->prev = s->size;
    s->size = next;
  }

  _cue_frame_t* f = &(s->f[s->fill++]);
  f->tag  = tag;
  f->bits = bits;
  f->ref  = ref;
}

ur_cue_res_e
ur_cue(ur_root_t *r, uint64_t len, const uint8_t *byt, ur_nref *out)
{
  ur_cue_res_e res;
  ur_nref      ref;
  _cue_t         c = {0};
  _cue_stack_t   s = { .prev = 89, .size = 144, .fill = 0, .f = 0 };

  //  init bitstream-reader
  //
  c.bsr.left  = len;
  c.bsr.bytes = byt;

  //  init dictionary
  //
  {
    uint64_t fib11 = 89, fib12 = 144;
    ur_dict64_grow(r, &c.dict, fib11, fib12);
  }

  //  setup stack
  //
  s.f = malloc(s.size * sizeof(*s.f));
  _cue_stack_push(&s, STACK_ROOT, 0, 0);

  //  advance into buffer
  //
  advance: {
    uint64_t    bits = c.bsr.bits;
    ur_cue_tag_e tag;

    if ( ur_cue_good != (res = ur_bsr_tag(&c.bsr, &tag)) ) {
      goto perfect;
    }

    switch ( tag ) {
      default: assert(0);

      case ur_jam_atom: {
        if ( ur_cue_good != (res = _cue_atom(r, &c, &ref)) ) {
          goto perfect;
        }
        else {
          ur_dict64_put(r, &c.dict, bits, (uint64_t)ref);
          goto retreat;
        }
      }

      case ur_jam_back: {
        uint64_t bak, val;

        if ( ur_cue_good != (res = _cue_back(&c.bsr, &bak)) ) {
          goto perfect;
        }
        else if ( !ur_dict64_get(r, &c.dict, bak, &val) ) {
          //  XX distinguish bad backref?
          //
          res = ur_cue_gone;
          goto perfect;
        }

        ref = (ur_nref)val;
        goto retreat;
      }

      case ur_jam_cell: {
        _cue_stack_push(&s, STACK_HEAD, bits, 0);
        goto advance;
      }
    }
  }

  //  retreat down the stack
  //
  retreat: {
    _cue_frame_t f = s.f[--s.fill];

    switch ( f.tag ) {
      default: assert(0);

      case STACK_ROOT: {
        res = ur_cue_good;
        goto perfect;
      }

      case STACK_HEAD: {
        _cue_stack_push(&s, STACK_TAIL, f.bits, ref);
        goto advance;
      }

      case STACK_TAIL: {
        ref = ur_cons(r, f.ref, ref);
        ur_dict64_put(r, &c.dict, f.bits, (uint64_t)ref);
        goto retreat;
      }
    }
  }

  //  we done
  //
  perfect: {
    ur_dict_free((ur_dict_t*)&c.dict);
    free(s.f);

    if ( ur_cue_good == res ) {
      *out = ref;
    }

    return res;
  }
}
