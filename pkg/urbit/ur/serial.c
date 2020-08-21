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

  if ( ur_cue_good != (res = ur_bsr_rub_len(bsr, &len)) ) {
    return res;
  }

  if ( 62 >= len ) {
    *out = (ur_nref)ur_bsr64_any(bsr, len);
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

    *out = ur_coin_bytes_unsafe(r, byt, len_byt);
  }

  return ur_cue_good;
}

static inline ur_cue_res_e
_cue_back(ur_bsr_t *bsr, uint64_t *out)
{
  ur_cue_res_e res;
  uint64_t     len;

  if ( ur_cue_good != (res = ur_bsr_rub_len(bsr, &len)) ) {
    return res;
  }
  else if ( 62 < len ) {
    return ur_cue_meme;
  }

  *out = ur_bsr64_any(bsr, len);
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
