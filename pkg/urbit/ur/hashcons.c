#include <inttypes.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

#include <limits.h>

#include <murmur3.h>

#include "ur/defs.h"
#include "ur/hashcons.h"

//  declarations of inline functions
//
uint64_t
ur_met0_bytes_unsafe(uint64_t len, uint8_t *byt);

static void*
_oom(const char* cap, void* v)
{
  if ( !v ) {
    fprintf(stderr,
            "ur: hashcons: %s: allocation failed, out of memory\r\n", cap);
    abort();
  }

  return v;
}

void
ur_dict32_grow(ur_root_t *r, ur_dict32_t *dict, uint64_t prev, uint64_t size)
{
  ur_pail32_t *buckets, *old_buckets = dict->buckets;
  uint64_t old_size = dict->size;
  uint64_t  i, next = prev + size;

  buckets = _oom("dict32_grow", calloc(next, sizeof(*buckets)));

  if ( old_buckets ) {
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

        if ( ur_pail_max == new_fill ) {
          free(buckets);
          return ur_dict32_grow(r, dict, size, next);
        }

        bucket->refs[new_fill] = ref;
        bucket->vals[new_fill] = val;
        bucket->fill = 1 + new_fill;
      }
    }

    free(old_buckets);
  }

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

    if ( ur_pail_max == fill ) {
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
ur_dict32_wipe(ur_dict32_t *dict)
{
  ur_pail32_t *buckets = dict->buckets;
  uint64_t     i, size = dict->size;

  for ( i = 0; i < size; i++ ) {
    buckets[i].fill = 0;
  }
}

void
ur_dict64_grow(ur_root_t *r, ur_dict64_t *dict, uint64_t prev, uint64_t size)
{
  ur_pail64_t *buckets, *old_buckets = dict->buckets;
  uint64_t old_size = dict->size;
  uint64_t  i, next = prev + size;

  buckets = _oom("dict64_grow", calloc(next, sizeof(*buckets)));

  if ( old_buckets ) {
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

        if ( ur_pail_max == new_fill ) {
          free(buckets);
          return ur_dict64_grow(r, dict, size, next);
        }

        bucket->refs[new_fill] = ref;
        bucket->vals[new_fill] = val;
        bucket->fill = 1 + new_fill;
      }
    }

    free(old_buckets);
  }

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

    if ( ur_pail_max == fill ) {
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
ur_dict64_wipe(ur_dict64_t *dict)
{
  ur_pail64_t *buckets = dict->buckets;
  uint64_t     i, size = dict->size;

  for ( i = 0; i < size; i++ ) {
    buckets[i].fill = 0;
  }
}

void
ur_dict_grow(ur_root_t *r, ur_dict_t *dict, uint64_t prev, uint64_t size)
{
  ur_pail_t *buckets, *old_buckets = dict->buckets;
  uint64_t old_size = dict->size;
  uint64_t  i, next = prev + size;

  buckets = _oom("dict_grow", calloc(next, sizeof(*buckets)));

  if ( old_buckets ) {
    for ( i = 0; i < old_size; i++ ) {
      ur_pail_t *old_bucket = &(old_buckets[i]);
      uint8_t   j, old_fill = old_bucket->fill;

      for ( j = 0; j < old_fill; j++ ) {
        ur_nref ref = old_bucket->refs[j];
        ur_mug  mug = ur_nref_mug(r, ref);

        uint64_t      idx = ( mug % next );
        ur_pail_t *bucket = &(buckets[idx]);
        uint8_t  new_fill = bucket->fill;

        if ( ur_pail_max == new_fill ) {
          free(buckets);
          return ur_dict_grow(r, dict, size, next);
        }

        bucket->refs[new_fill] = ref;
        bucket->fill = 1 + new_fill;
      }
    }

    free(old_buckets);
  }

  dict->prev = size;
  dict->size = next;
  dict->buckets = buckets;
}

ur_bool_t
ur_dict_get(ur_root_t *r, ur_dict_t *dict, ur_nref ref)
{
  ur_mug   mug = ur_nref_mug(r, ref);
  uint64_t idx = ( mug % dict->size );

  ur_pail_t *bucket = &(dict->buckets[idx]);
  uint8_t   i, fill = bucket->fill;

  for ( i = 0; i < fill; i++ ) {
    if ( ref == bucket->refs[i] ) {
      return 1;
    }
  }

  return 0;
}

void
ur_dict_put(ur_root_t *r, ur_dict_t *dict, ur_nref ref)
{
  ur_mug mug = ur_nref_mug(r, ref);

  while ( 1 ) {
    uint64_t      idx = ( mug % dict->size );
    ur_pail_t *bucket = &(dict->buckets[idx]);
    uint8_t   i, fill = bucket->fill;

    for ( i = 0; i < fill; i++ ) {
      if ( ref == bucket->refs[i] ) {
        return;
      }
    }

    if ( ur_pail_max == fill ) {
      ur_dict_grow(r, dict, dict->prev, dict->size);
      continue;
    }

    bucket->refs[fill] = ref;
    bucket->fill = 1 + fill;
    break;
  }
}

void
ur_dict_wipe(ur_dict_t *dict)
{
  ur_pail_t *buckets = dict->buckets;
  uint64_t   i, size = dict->size;

  for ( i = 0; i < size; i++ ) {
    buckets[i].fill = 0;
  }
}

void
ur_dict_free(ur_dict_t *dict)
{
  free(dict->buckets);
  dict->buckets = 0;
}

ur_mug
ur_mug_bytes(const uint8_t *byt, uint64_t len)
{
  uint32_t seed = 0xcafebabe;
  uint8_t     i = 0;

  while ( i < 8 ) {
    ur_mug   mug;
    uint32_t raw;
    MurmurHash3_x86_32(byt, len, seed, &raw);
    mug = (raw >> 31) ^ ( ur_mask_31(raw) );

    if ( 0 == mug ) {
      seed++; i++;
    }
    else {
      return mug;
    }
  }

  return (ur_mug)0x7fff;
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
  uint32_t  seed = 0xdeadbeef;
  uint8_t    len = 4 + ur_bloq_up3(ur_met0_32(tal));
  uint8_t      i = 0;
  uint8_t byt[8] = {
    ur_mask_8(hed >>  0),
    ur_mask_8(hed >>  8),
    ur_mask_8(hed >> 16),
    ur_mask_8(hed >> 24),
    ur_mask_8(tal >>  0),
    ur_mask_8(tal >>  8),
    ur_mask_8(tal >> 16),
    ur_mask_8(tal >> 24)
  };

  while ( i < 8 ) {
    ur_mug   mug;
    uint32_t raw;
    MurmurHash3_x86_32(byt, len, seed, &raw);
    mug = (raw >> 31) ^ ( ur_mask_31(raw) );

    if ( 0 == mug ) {
      seed++; i++;
    }
    else {
      return mug;
    }
  }

  return (ur_mug)0xfffe;
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
ur_atoms_grow(ur_atoms_t *atoms)
{
  uint64_t   prev = atoms->prev;
  uint64_t   size = atoms->size;
  uint64_t   next = prev + size;
  uint8_t **bytes = atoms->bytes;
  uint64_t  *lens = atoms->lens;
  ur_mug    *mugs = atoms->mugs;

  atoms->bytes = _oom("atoms_grow", malloc(next * ( sizeof(*atoms->bytes)
                                                  + sizeof(*atoms->lens)
                                                  + sizeof(*atoms->mugs) )));
  atoms->lens  = (void*)((char*)atoms->bytes + (next * sizeof(*atoms->bytes)));
  atoms->mugs  = (void*)((char*)atoms->lens  + (next * sizeof(*atoms->lens)));

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

  cells->heads = _oom("cells_grow", malloc(next * ( sizeof(*cells->heads)
                                                  + sizeof(*cells->heads)
                                                  + sizeof(*cells->mugs) )));
  cells->tails = (void*)((char*)cells->heads + (next * sizeof(*cells->heads)));
  cells->mugs  = (void*)((char*)cells->tails + (next * sizeof(*cells->tails)));

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

uint64_t
ur_met(ur_root_t *r, uint8_t bloq, ur_nref ref)
{
  uint64_t m_bit;

  //  XX return bool for cells, length in out parameter
  //
  assert( !ur_deep(ref) );

  if ( ur_direct == ur_nref_tag(ref) ) {
    m_bit = ur_met0_64(ref);
  }
  else {
    uint64_t idx = ur_nref_idx(ref);
    uint64_t len = r->atoms.lens[idx];
    uint8_t *byt = r->atoms.bytes[idx];

    m_bit = ur_met0_bytes_unsafe(len, byt);
  }

  switch ( bloq ) {
    case 0: return m_bit;
    case 1: return ur_bloq_up1(m_bit);
    case 2: return ur_bloq_up2(m_bit);
    case 3: return ur_bloq_up3(m_bit);

    default: {
      uint64_t m_byt = ur_bloq_up3(m_bit);
      uint8_t    off = (bloq - 3);
      return (m_byt + ((1ULL << off) - 1)) >> off;
    }
  }
}

static ur_nref
_coin_unsafe(ur_atoms_t *atoms, ur_mug mug, uint64_t len, uint8_t *byt)
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

ur_nref
ur_coin_bytes_unsafe(ur_root_t *r, uint64_t len, uint8_t *byt)
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

    if ( ur_pail_max == b_fill ) {
      ur_dict_grow(r, dict, dict->prev, dict->size);
      continue;
    }

    if ( atoms->fill == atoms->size ) {
      ur_atoms_grow(atoms);
    }

    tom = _coin_unsafe(atoms, mug, len, byt);

    bucket->refs[b_fill] = tom;
    bucket->fill = 1 + b_fill;

    return tom;
  }
}

ur_nref
ur_coin_bytes(ur_root_t *r, uint64_t len, uint8_t *byt)
{
  //  strip trailing zeroes
  //
  while ( len && !byt[len - 1] ) {
    len--;
  }

  //  produce a direct atom if possible
  //
  if ( 62 >= ur_met0_bytes_unsafe(len, byt) ) {
    uint64_t i, direct = 0;

    for ( i = 0; i < len; i++ ) {
      direct |= byt[i] << (8 * i);
    }

    return (ur_nref)direct;
  }
  else {
    uint8_t *copy = _oom("coin_bytes", malloc(len));
    memcpy(copy, byt, len);

    return ur_coin_bytes_unsafe(r, len, copy);
  }
}

ur_nref
ur_coin64(ur_root_t *r, uint64_t n)
{
  if ( ur_direct == ur_nref_tag(n) ) {
    return n;
  }
  else {
    uint8_t *byt;
    assert( 8 == ur_met3_64(n) );

    byt = _oom("coin64", malloc(8));

    byt[0] = ur_mask_8(n);
    byt[1] = ur_mask_8(n >>  8);
    byt[2] = ur_mask_8(n >> 16);
    byt[3] = ur_mask_8(n >> 24);
    byt[4] = ur_mask_8(n >> 32);
    byt[5] = ur_mask_8(n >> 40);
    byt[6] = ur_mask_8(n >> 48);
    byt[7] = ur_mask_8(n >> 56);

    return ur_coin_bytes_unsafe(r, 8, byt);
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

    if ( ur_pail_max == b_fill ) {
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
ur_root_info(FILE *f, ur_root_t *r)
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
ur_root_free(ur_root_t *r)
{
  _atoms_free(&(r->atoms));
  _cells_free(&(r->cells));
  free(r);
}

ur_root_t*
ur_root_init(void)
{
  ur_root_t *r = _oom("root_init", calloc(1, sizeof(*r)));

  {
    ur_dict_t *dict;

    //  allocate atom storage
    //
    r->atoms.prev = ur_fib11;
    r->atoms.size = ur_fib12;
    ur_atoms_grow(&(r->atoms));

    //  allocate atom hashtable
    //
    dict = &(r->atoms.dict);
    ur_dict_grow(r, dict, ur_fib11, ur_fib12);

    //  allocate cell storage
    //
    r->cells.prev = ur_fib11;
    r->cells.size = ur_fib12;
    ur_cells_grow(&(r->cells));

    //  allocate cell hashtable
    //
    dict = &(r->cells.dict);
    ur_dict_grow(r, dict, ur_fib11, ur_fib12);
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
  v->refs = _oom("nvec_init", calloc(size, sizeof(ur_nref)));
}

/*
**  define opaque struct ur_walk_fore_s (ie, ur_walk_fore_t)
*/
struct ur_walk_fore_s {
  ur_root_t  *r;
  uint32_t prev;
  uint32_t size;
  uint32_t fill;
  ur_nref  *top;
};

ur_walk_fore_t*
ur_walk_fore_init_with(ur_root_t    *r,
                       uint32_t s_prev,
                       uint32_t s_size)
{
  ur_walk_fore_t *w = _oom("walk_fore", malloc(sizeof(*w)));
  w->top = _oom("walk_fore", malloc(s_size * sizeof(*w->top)));
  w->prev = s_prev;
  w->size = s_size;
  w->fill = 0;
  w->r    = r;

  return w;
}

ur_walk_fore_t*
ur_walk_fore_init(ur_root_t *r)
{
  return ur_walk_fore_init_with(r, ur_fib10, ur_fib11);
}

void
ur_walk_fore_with(ur_walk_fore_t *w,
                  ur_nref       ref,
                  void           *v,
                  void       (*atom)(ur_root_t*, ur_nref, void*),
                  ur_bool_t  (*cell)(ur_root_t*, ur_nref, void*))
{
  ur_root_t *r = w->r;
  ur_nref *don = w->top;

  w->top += ++w->fill;
  *w->top = ref;

  while ( w->top != don ) {
    //  visit atom, pop stack
    //
    if ( !ur_deep(ref) ) {
      atom(r, ref, v);
      w->top--; w->fill--;
    }
    //  visit cell, pop stack if false
    //
    else if ( !cell(r, ref, v) ) {
      w->top--; w->fill--;
    }
    //  push the tail, continue into the head
    //
    else {
      *w->top = ur_tail(r, ref);

      //  reallocate "stack" if full
      //
      if ( w->size == w->fill ) {
        uint32_t next = w->prev + w->size;
        don     = _oom("walk_fore", realloc(don, next * sizeof(*don)));
        w->top  = don + w->fill;
        w->prev = w->size;
        w->size = next;
      }

      w->top++; w->fill++;
      *w->top = ur_head(r, ref);
    }

    ref = *w->top;
  }
}

void
ur_walk_fore_done(ur_walk_fore_t *w)
{
  free(w->top);
  free(w);
}

void
ur_walk_fore(ur_root_t *r,
             ur_nref  ref,
             void      *v,
             void      (*atom)(ur_root_t*, ur_nref, void*),
             ur_bool_t (*cell)(ur_root_t*, ur_nref, void*))
{
  ur_walk_fore_t *w = ur_walk_fore_init(r);
  ur_walk_fore_with(w, ref, v, atom, cell);
  ur_walk_fore_done(w);
}
