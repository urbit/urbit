#include <stdlib.h>
#include <assert.h>

#include "ur/ur.h"

static inline void
_bsw_atom(ur_root_t *r, ur_nref ref, ur_bsw_t *bsw, uint64_t len)
{
  switch ( ur_nref_tag(ref) ) {
    default: assert(0);

    case ur_direct: return ur_bsw_atom64(bsw, len, ref);

    case ur_iatom: {
      uint8_t *byt = r->atoms.bytes[ur_nref_idx(ref)];
      return ur_bsw_atom_bytes(bsw, len, byt);
    }
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
  uint64_t bak, len = ur_met(r, 0, ref);

  if ( !ur_dict64_get(r, dict, ref, &bak) ) {
    ur_dict64_put(r, dict, ref, bsw->bits);

    _bsw_atom(r, ref, bsw, len);
  }
  else {
    uint64_t len_bak = ur_met0_64(bak);

    if ( len <= len_bak ) {
      _bsw_atom(r, ref, bsw, len);
    }
    else {
      ur_bsw_back64(bsw, len_bak, bak);
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

    ur_bsw_cell(bsw);
    return 1;
  }
  else {
    ur_bsw_back64(bsw, ur_met0_64(bak), bak);
    return 0;
  }
}

uint64_t
ur_jam(ur_root_t *r, ur_nref ref, uint64_t *len, uint8_t **byt)
{
  _jam_t j = {0};

  j.bsw.prev  = ur_fib11;
  j.bsw.size  = ur_fib12;
  j.bsw.bytes = calloc(j.bsw.size, 1);
  ur_dict64_grow(r, &j.dict, ur_fib11, ur_fib12);

  ur_walk_fore(r, ref, &j, _jam_atom, _jam_cell);
  ur_dict_free((ur_dict_t*)&j.dict);

  *len = j.bsw.fill + !!j.bsw.off;
  *byt = j.bsw.bytes;

  return j.bsw.bits;
}

#define CUE_HEAD64 0xffffffffffffffffULL

/*
**  stack frame for recording head vs tail iteration
**
**    $?  [CUE_HEAD64 bits=@]
**    [hed=* bits=@]
*/
typedef struct _cue64_frame_s {
  uint64_t  ref;
  uint64_t bits;
} _cue64_frame_t;

typedef struct _cue64_stack_s {
  uint32_t     prev;
  uint32_t     size;
  uint32_t     fill;
  _cue64_frame_t* f;
} _cue64_stack_t;

static inline void
_cue64_stack_push(_cue64_stack_t *s, uint64_t ref, uint64_t bits)
{
  if ( s->fill == s->size ) {
    uint32_t next = s->prev + s->size;
    s->f = realloc(s->f, next * sizeof(*s->f));
    s->prev = s->size;
    s->size = next;
  }

  _cue64_frame_t* f = &(s->f[s->fill++]);
  f->ref  = ref;
  f->bits = bits;
}

static inline ur_cue_res_e
_cue_walk64_advance(ur_root_t      *r,
                    _cue64_stack_t *s,
                    ur_bsr_t     *bsr,
                    ur_dict64_t *dict,
                    uint64_t   (*coin)(ur_root_t*, ur_bsr_t*, uint64_t),
                    uint64_t     *out)
{
  while ( 1 ) {
    uint64_t len, bits = bsr->bits;
    ur_cue_tag_e   tag;
    ur_cue_res_e   res;

    if ( ur_cue_good != (res = ur_bsr_tag(bsr, &tag)) ) {
      return res;
    }

    switch ( tag ) {
      default: assert(0);

      case ur_jam_atom: {
        if ( ur_cue_good != (res = ur_bsr_rub_len(bsr, &len)) ) {
          return res;
        }

        *out = coin(r, bsr, len);
        ur_dict64_put(r, dict, bits, *out);
        return ur_cue_good;
      }

      case ur_jam_back: {
        if ( ur_cue_good != (res = ur_bsr_rub_len(bsr, &len)) ) {
          return res;
        }
        else if ( 62 < len ) {
          return ur_cue_meme;
        }
        else {
          uint64_t bak = ur_bsr64_any(bsr, len);

          if ( !ur_dict64_get(r, dict, bak, out) ) {
            //  XX distinguish bad backref?
            //
            return ur_cue_gone;
          }

          return ur_cue_good;
        }
      }

      case ur_jam_cell: {
        _cue64_stack_push(s, CUE_HEAD64, bits);
        continue;
      }
    }
  }
}

ur_cue_res_e
ur_cue_walk64(ur_root_t       *r,
              uint64_t       len,
              const uint8_t *byt,
              uint64_t      *out,
              uint64_t    (*coin)(ur_root_t*, ur_bsr_t*, uint64_t),
              uint64_t    (*cons)(ur_root_t*, uint64_t, uint64_t))
{
  ur_cue_res_e res;
  uint64_t     ref;
  ur_dict64_t dict = {0};
  ur_bsr_t     bsr = {0};
  _cue64_stack_t s = {0};

  //  init dictionary
  //
  ur_dict64_grow(r, &dict, ur_fib11, ur_fib12);

  //  init bitstream-reader
  //
  bsr.left  = len;
  bsr.bytes = byt;

  //  setup stack
  //
  s.prev = ur_fib10;
  s.size = ur_fib11;
  s.f = malloc(s.size * sizeof(*s.f));

  //  advance into stream
  //
  res = _cue_walk64_advance(r, &s, &bsr, &dict, coin, &ref);

  //  retreat down the stack
  //
  while ( s.fill && (ur_cue_good == res) ) {
    _cue64_frame_t f = s.f[--s.fill];

    //  f is a head-frame
    //
    if ( CUE_HEAD64 == f.ref ) {
      _cue64_stack_push(&s, ref, f.bits);
      res = _cue_walk64_advance(r, &s, &bsr, &dict, coin, &ref);
    }
    //  f is a tail-frame
    //
    else {
      ref = cons(r, f.ref, ref);
      ur_dict64_put(r, &dict, f.bits, ref);
    }
  }

  //  finalize result
  //
  ur_dict_free((ur_dict_t*)&dict);
  free(s.f);

  if ( ur_cue_good == res ) {
    *out = ref;
  }

  return res;
}

static inline uint64_t
_cue_coin(ur_root_t *r, ur_bsr_t *bsr, uint64_t len)
{
  if ( 62 >= len ) {
    return ur_bsr64_any(bsr, len);
  }
  else {
    uint64_t len_byt = (len >> 3) + !!ur_mask_3(len);
    uint8_t *byt = calloc(len_byt, 1);
    ur_bsr_bytes_any(bsr, len, byt);

    //  strip trailing zeroes
    //
    while ( len_byt && !byt[len_byt - 1] ) {
      len_byt--;
    }

    return (uint64_t)ur_coin_bytes_unsafe(r, byt, len_byt);
  }
}

ur_cue_res_e
ur_cue(ur_root_t *r, uint64_t len, const uint8_t *byt, ur_nref *out)
{
  return ur_cue_walk64(r, len, byt, (uint64_t*)out, _cue_coin, ur_cons);
}
