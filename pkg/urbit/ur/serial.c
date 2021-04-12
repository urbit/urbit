#include <stdlib.h>
#include <assert.h>

#include "ur/ur.h"

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

/*
**  define opaque struct ur_jam_s (ie, ur_jam_t)
*/
struct ur_jam_s {
  ur_root_t      *r;
  ur_walk_fore_t *w;
  ur_dict64_t  dict;
  ur_bsw_t      bsw;
};

static void
_jam_atom(ur_root_t *r, ur_nref ref, void *ptr)
{
  ur_jam_t       *j = ptr;
  ur_dict64_t *dict = &j->dict;
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
  ur_jam_t       *j = ptr;
  ur_dict64_t *dict = &j->dict;
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

static uint64_t
_jam(ur_jam_t   *j,
     ur_nref   ref,
     uint64_t *len,
     uint8_t **byt)
{
  ur_bsw_init(&j->bsw, ur_fib11, ur_fib12);
  ur_walk_fore_with(j->w, ref, j, _jam_atom, _jam_cell);
  return ur_bsw_done(&j->bsw, len, byt);
}

ur_jam_t*
ur_jam_init_with(ur_root_t    *r,
                 uint64_t d_prev,
                 uint64_t d_size,
                 uint32_t s_prev,
                 uint32_t s_size)
{
  ur_jam_t *j = _oom("jam_init", calloc(sizeof(*j), 1));
  j->w = ur_walk_fore_init_with(r, s_prev, s_size);
  j->r = r;

  ur_dict64_grow(r, &j->dict, d_prev, d_size);

  return j;
}

ur_jam_t*
ur_jam_init(ur_root_t *r)
{
  return ur_jam_init_with(r, ur_fib11, ur_fib12,    //  dict sizes
                             ur_fib10, ur_fib11);   //  stack sizes
}

uint64_t
ur_jam_with(ur_jam_t   *j,
            ur_nref   ref,
            uint64_t *len,
            uint8_t **byt)
{
  uint64_t bits = _jam(j, ref, len, byt);
  ur_dict64_wipe(&j->dict);
  return bits;
}

void
ur_jam_done(ur_jam_t *j)
{
  ur_dict_free((ur_dict_t*)&j->dict);
  free(j->w);
  free(j);
}

uint64_t
ur_jam(ur_root_t  *r,
       ur_nref   ref,
       uint64_t *len,
       uint8_t **byt)
{
  ur_jam_t   *j = ur_jam_init(r);
  uint64_t bits = _jam(j, ref, len, byt);
  ur_jam_done(j);
  return bits;
}

/*
**  stack frame for recording head vs tail iteration
**
**    $?  [CUE_HEAD bits=@]
**    [hed=* bits=@]
*/

#define CUE_HEAD 0xffffffffffffffffULL

typedef struct _cue_frame_s {
  uint64_t  ref;
  uint64_t bits;
} _cue_frame_t;

typedef struct _cue_stack_s {
  uint32_t   prev;
  uint32_t   size;
  uint32_t   fill;
  _cue_frame_t* f;
} _cue_stack_t;

static inline ur_cue_res_e
_cue_next(ur_root_t      *r,
          _cue_stack_t   *s,
          ur_bsr_t     *bsr,
          ur_dict64_t *dict,
          ur_nref      *out)
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

      case ur_jam_cell: {
        //  reallocate the stack if full
        //
        if ( s->fill == s->size ) {
          uint32_t next = s->prev + s->size;
          s->f = _oom("cue_next stack", realloc(s->f, next * sizeof(*s->f)));
          s->prev = s->size;
          s->size = next;
        }

        //  save a head-frame and read the head from the stream
        //
        {
          _cue_frame_t* f = &(s->f[s->fill++]);
          f->ref  = CUE_HEAD;
          f->bits = bits;
        }
        continue;
      }

      case ur_jam_back: {
        if ( ur_cue_good != (res = ur_bsr_rub_len(bsr, &len)) ) {
          return res;
        }
        else if ( 62 < len ) {
          return ur_cue_meme;
        }
        else {
          uint64_t val, bak = ur_bsr64_any(bsr, len);

          if ( !ur_dict64_get(r, dict, bak, &val) ) {
            return  ur_cue_back;
          }

          *out = (ur_nref)val;
          return ur_cue_good;
        }
      }

      case ur_jam_atom: {
        if ( ur_cue_good != (res = ur_bsr_rub_len(bsr, &len)) ) {
          return res;
        }
        else if ( 62 >= len ) {
          *out = (ur_nref)ur_bsr64_any(bsr, len);
        }
        else {
          uint64_t len_byt = (len >> 3) + !!ur_mask_3(len);
          uint8_t     *byt = _oom("cue_next bytes", calloc(len_byt, 1));

          ur_bsr_bytes_any(bsr, len, byt);

          //  strip trailing zeroes
          //
          while ( len_byt && !byt[len_byt - 1] ) {
            len_byt--;
          }

          *out = ur_coin_bytes_unsafe(r, len_byt, byt);
        }

        ur_dict64_put(r, dict, bits, (uint64_t)*out);
        return ur_cue_good;
      }
    }
  }
}

/*
**  define opaque struct ur_cue_s (ie, ur_cue_t)
*/
struct ur_cue_s {
  ur_root_t     *r;
  _cue_stack_t   s;
  ur_dict64_t dict;
};

static ur_cue_res_e
_cue(ur_cue_t        *c,
     uint64_t       len,
     const uint8_t *byt,
     ur_nref       *out)
{
  ur_bsr_t      bsr = {0};
  ur_root_t      *r = c->r;
  _cue_stack_t   *s = &c->s;
  ur_dict64_t *dict = &c->dict;
  ur_cue_res_e res;
  ur_nref      ref;

  //  init bitstream-reader
  //
  if ( ur_cue_good != (res = ur_bsr_init(&bsr, len, byt)) ) {
    return res;
  }
  //  bit-cursor (and backreferences) must fit in 62-bit direct atoms
  //
  else if ( 0x7ffffffffffffffULL < len ) {
    return ur_cue_meme;
  }

  //  advance into stream
  //
  res = _cue_next(r, s, &bsr, dict, &ref);

  //  process result
  //
  while ( s->fill && (ur_cue_good == res) ) {
    //  peek at the top of the stack
    //
    _cue_frame_t *f = &(s->f[s->fill - 1]);

    //  f is a head-frame; stash result and read the tail from the stream
    //
    if ( CUE_HEAD == f->ref ) {
      f->ref = ref;
      res    = _cue_next(r, s, &bsr, dict, &ref);
    }
    //  f is a tail-frame; pop the stack and continue
    //
    else {
      ref = ur_cons(r, f->ref, ref);
      ur_dict64_put(r, dict, f->bits, (uint64_t)ref);
      s->fill--;
    }
  }

  if ( ur_cue_good == res ) {
    *out = ref;
  }
  return res;
}

ur_cue_t*
ur_cue_init_with(ur_root_t    *r,
                 uint64_t d_prev,
                 uint64_t d_size,
                 uint32_t s_prev,
                 uint32_t s_size)
{
  ur_cue_t* c = _oom("cue_init", calloc(sizeof(*c), 1));
  c->r = r;

  ur_dict64_grow(r, &c->dict, d_prev, d_size);

  c->s.prev = s_prev;
  c->s.size = s_size;
  c->s.f = _oom("cue_test_init", malloc(s_size * sizeof(*c->s.f)));

  return c;
}

ur_cue_t*
ur_cue_init(ur_root_t *r)
{
  return ur_cue_init_with(r, ur_fib11, ur_fib12,    //  dict sizes
                             ur_fib10, ur_fib11);   //  stack sizes
}

ur_cue_res_e
ur_cue_with(ur_cue_t        *c,
            uint64_t       len,
            const uint8_t *byt,
            ur_nref       *out)
{
  ur_cue_res_e res = _cue(c, len, byt, out);

  //  XX check size, shrink if above threshold
  //
  ur_dict64_wipe(&c->dict);
  c->s.fill = 0;

  return res;
}

void
ur_cue_done(ur_cue_t *c)
{

  ur_dict_free((ur_dict_t*)&c->dict);
  free(c->s.f);
  free(c);
}

ur_cue_res_e
ur_cue(ur_root_t       *r,
       uint64_t       len,
       const uint8_t *byt,
       ur_nref       *out)
{
  ur_cue_t       *c = ur_cue_init(r);
  ur_cue_res_e res = _cue(c, len, byt, out);

  ur_cue_done(c);
  return res;
}

/*
**  stack frame for recording head vs tail iteration
**
**    [hed=? bits=@]
**
*/

typedef struct _cue_test_frame_s {
  ur_bool_t tal;
  uint64_t bits;
} _cue_test_frame_t;

typedef struct _cue_test_stack_s {
  uint32_t        prev;
  uint32_t        size;
  uint32_t        fill;
  _cue_test_frame_t* f;
} _cue_test_stack_t;

static inline ur_cue_res_e
_cue_test_next(_cue_test_stack_t *s,
               ur_bsr_t        *bsr,
               ur_dict_t      *dict)
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

      case ur_jam_cell: {
        //  reallocate the stack if full
        //
        if ( s->fill == s->size ) {
          uint32_t next = s->prev + s->size;
          s->f = _oom("cue_test", realloc(s->f, next * sizeof(*s->f)));
          s->prev = s->size;
          s->size = next;
        }

        //  save a head-frame and read the head from the stream
        //
        {
          _cue_test_frame_t* f = &(s->f[s->fill++]);
          f->tal  = 0;
          f->bits = bits;
        }
        continue;
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
          return ur_dict_get((ur_root_t*)0, dict, bak)
               ? ur_cue_good
               : ur_cue_back;
        }
      }

      case ur_jam_atom: {
        if ( ur_cue_good != (res = ur_bsr_rub_len(bsr, &len)) ) {
          return res;
        }

        ur_bsr_skip_any(bsr, len);
        ur_dict_put((ur_root_t*)0, dict, bits);
        return ur_cue_good;
      }
    }
  }
}

/*
**  define opaque struct ur_cue_test_s (ie, ur_cue_test_t)
*/
struct ur_cue_test_s {
  _cue_test_stack_t s;
  ur_dict_t      dict;
};

static ur_cue_res_e
_cue_test(ur_cue_test_t   *t,
          uint64_t       len,
          const uint8_t *byt)
{
  ur_bsr_t         bsr = {0};
  _cue_test_stack_t *s = &t->s;
  ur_dict_t      *dict = &t->dict;
  ur_cue_res_e     res;

  //  init bitstream-reader
  //
  if ( ur_cue_good != (res = ur_bsr_init(&bsr, len, byt)) ) {
    return res;
  }
  //  bit-cursor (and backreferences) must fit in 62-bit direct atoms
  //
  else if ( 0x7ffffffffffffffULL < len ) {
    return ur_cue_meme;
  }

  //  advance into stream
  //
  res = _cue_test_next(s, &bsr, dict);

  //  process result
  //
  while ( s->fill && (ur_cue_good == res) ) {
    //  peek at the top of the stack
    //
    _cue_test_frame_t *f = &(s->f[s->fill - 1]);

    //  f is a head-frame; stash result and read the tail from the stream
    //
    if ( !f->tal ) {
      f->tal = 1;
      res    = _cue_test_next(s, &bsr, dict);
    }
    //  f is a tail-frame; pop the stack and continue
    //
    else {
      ur_dict_put((ur_root_t*)0, dict, f->bits);
      s->fill--;
    }
  }

  return res;
}

ur_cue_test_t*
ur_cue_test_init_with(uint64_t d_prev,
                      uint64_t d_size,
                      uint32_t s_prev,
                      uint32_t s_size)
{
  ur_cue_test_t* t = _oom("cue_test_init", calloc(sizeof(*t), 1));

  ur_dict_grow((ur_root_t*)0, &t->dict, d_prev, d_size);

  t->s.prev = s_prev;
  t->s.size = s_size;
  t->s.f = _oom("cue_test_init", malloc(s_size * sizeof(*t->s.f)));

  return t;
}

ur_cue_test_t*
ur_cue_test_init(void)
{
  return ur_cue_test_init_with(ur_fib11, ur_fib12,    //  dict sizes
                               ur_fib10, ur_fib11);   //  stack sizes
}

ur_bool_t
ur_cue_test_with(ur_cue_test_t   *t,
                 uint64_t       len,
                 const uint8_t *byt)
{
  ur_bool_t ret = ur_cue_good == _cue_test(t, len, byt);

  //  XX check size, shrink if above threshold
  //
  ur_dict_wipe(&t->dict);
  t->s.fill = 0;

  return ret;
}

void
ur_cue_test_done(ur_cue_test_t *t)
{
  ur_dict_free(&t->dict);
  free(t->s.f);
  free(t);
}

ur_bool_t
ur_cue_test(uint64_t len, const uint8_t *byt)
{
  ur_cue_test_t *t = ur_cue_test_init();
  ur_bool_t    ret = ur_cue_good == _cue_test(t, len, byt);

  ur_cue_test_done(t);
  return ret;
}
