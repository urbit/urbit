/* noun/serial.c
**
*/

#include <errno.h>
#include <fcntl.h>

#include "all.h"
#include "ur/ur.h"

/* _cs_jam_buf: struct for tracking the fibonacci-allocated jam of a noun
*/
struct _cs_jam_fib {
  u3i_slab*     sab_u;
  u3p(u3h_root) har_p;
  c3_w          a_w;
  c3_w          b_w;
  c3_w          bit_w;
};

/* _cs_jam_fib_grow(): reallocate buffer with fibonacci growth
*/
static void
_cs_jam_fib_grow(struct _cs_jam_fib* fib_u, c3_w mor_w)
{
  c3_w wan_w = fib_u->bit_w + mor_w;

  // check for c3_w overflow
  //
  if ( wan_w < mor_w ) {
    u3m_bail(c3__fail);
  }

  if ( wan_w > fib_u->a_w ) {
    c3_w old_w = fib_u->sab_u->len_w;
    c3_w   c_w = 0;

    //  fibonacci growth
    //
    while ( c_w < wan_w ) {
      c_w        = fib_u->a_w + fib_u->b_w;
      fib_u->b_w = fib_u->a_w;
      fib_u->a_w = c_w;
    }

    u3i_slab_grow(fib_u->sab_u, 0, c_w);
  }
}

/* _cs_jam_fib_chop(): chop [met_w] bits of [a] into [fib_u]
*/
static void
_cs_jam_fib_chop(struct _cs_jam_fib* fib_u, c3_w met_w, u3_noun a)
{
  c3_w bit_w = fib_u->bit_w;
  _cs_jam_fib_grow(fib_u, met_w);
  fib_u->bit_w += met_w;

  {
    c3_w* buf_w = fib_u->sab_u->buf_w;
    u3r_chop(0, 0, met_w, bit_w, buf_w, a);
  }
}

/* _cs_jam_fib_mat(): length-prefixed encode (mat) [a] into [fib_u]
*/
static void
_cs_jam_fib_mat(struct _cs_jam_fib* fib_u, u3_noun a)
{
  if ( 0 == a ) {
    _cs_jam_fib_chop(fib_u, 1, 1);
  }
  else {
    c3_w a_w = u3r_met(0, a);
    c3_w b_w = c3_bits_word(a_w);

    _cs_jam_fib_chop(fib_u, b_w+1, 1 << b_w);
    _cs_jam_fib_chop(fib_u, b_w-1, a_w & ((1 << (b_w-1)) - 1));
    _cs_jam_fib_chop(fib_u, a_w, a);
  }
}

/* _cs_jam_fib_atom_cb(): encode atom or backref
*/
static void
_cs_jam_fib_atom_cb(u3_atom a, void* ptr_v)
{
  struct _cs_jam_fib* fib_u = ptr_v;
  u3_weak b = u3h_git(fib_u->har_p, a);

  //  if [a] has no backref, encode atom and put cursor into [har_p]
  //
  if ( u3_none == b ) {
    u3h_put(fib_u->har_p, a, u3i_words(1, &(fib_u->bit_w)));
    _cs_jam_fib_chop(fib_u, 1, 0);
    _cs_jam_fib_mat(fib_u, a);
  }
  else {
    c3_w a_w = u3r_met(0, a);
    c3_w b_w = u3r_met(0, b);

    //  if [a] is smaller than the backref, encode atom
    //
    if ( a_w <= b_w ) {
      _cs_jam_fib_chop(fib_u, 1, 0);
      _cs_jam_fib_mat(fib_u, a);
    }
    //  otherwise, encode backref
    //
    else {
      _cs_jam_fib_chop(fib_u, 2, 3);
      _cs_jam_fib_mat(fib_u, b);
    }
  }
}

/* _cs_jam_fib_cell_cb(): encode cell or backref
*/
static c3_o
_cs_jam_fib_cell_cb(u3_noun a, void* ptr_v)
{
  struct _cs_jam_fib* fib_u = ptr_v;
  u3_weak b = u3h_git(fib_u->har_p, a);

  //  if [a] has no backref, encode cell and put cursor into [har_p]
  //
  if ( u3_none == b ) {
    u3h_put(fib_u->har_p, a, u3i_words(1, &(fib_u->bit_w)));
    _cs_jam_fib_chop(fib_u, 2, 1);
    return c3y;
  }
  //  otherwise, encode backref and shortcircuit traversal
  //
  else {
    _cs_jam_fib_chop(fib_u, 2, 3);
    _cs_jam_fib_mat(fib_u, b);
    return c3n;
  }
}

/* u3s_jam_fib(): jam without atom allocation.
**
**   returns atom-suitable words, and *bit_w will have
**   the length (in bits). return should be freed with u3a_wfree().
*/
c3_w
u3s_jam_fib(u3i_slab* sab_u, u3_noun a)
{
  struct _cs_jam_fib fib_u;
  fib_u.har_p = u3h_new();
  fib_u.sab_u = sab_u;

  //  fib(12) is small enough to be reasonably fast to allocate.
  //  fib(11) is needed to get fib(13).
  //
  //
  fib_u.a_w   = ur_fib12;
  fib_u.b_w   = ur_fib11;
  fib_u.bit_w = 0;
  u3i_slab_init(sab_u, 0, fib_u.a_w);

  //  as this is a hot path, we unsafely elide overflow checks
  //
  //    a page-fault overflow detection system is urgently needed ...
  //
  u3a_walk_fore_unsafe(a, &fib_u, _cs_jam_fib_atom_cb,
                                  _cs_jam_fib_cell_cb);

  u3h_free(fib_u.har_p);

  return fib_u.bit_w;
}

typedef struct _jam_xeno_s {
  u3p(u3h_root) har_p;
  ur_bsw_t      rit_u;
} _jam_xeno_t;

/* _cs_coin_chub(): shortcircuit u3i_chubs().
*/
static inline u3_atom
_cs_coin_chub(c3_d a_d)
{
  return ( 0x7fffffffULL >= a_d ) ? a_d : u3i_chubs(1, &a_d);
}

/* _cs_jam_xeno_atom(): encode in/direct atom in bitstream.
*/
static inline void
_cs_jam_bsw_atom(ur_bsw_t* rit_u, c3_w met_w, u3_atom a)
{
  if ( c3y == u3a_is_cat(a) ) {
    //  XX need a ur_bsw_atom32()
    //
    ur_bsw_atom64(rit_u, (c3_y)met_w, (c3_d)a);
  }
  else {
    u3a_atom* vat_u = u3a_to_ptr(a);
    //  XX assumes little-endian
    //  XX need a ur_bsw_atom_words()
    //
    c3_y*     byt_y = (c3_y*)vat_u->buf_w;
    ur_bsw_atom_bytes(rit_u, (c3_d)met_w, byt_y);
  }
}

/* _cs_jam_bsw_back(): encode in/direct backref in bitstream.
*/
static inline void
_cs_jam_bsw_back(ur_bsw_t* rit_u, c3_w met_w, u3_atom a)
{
  c3_d bak_d = ( c3y == u3a_is_cat(a) )
             ? (c3_d)a
             : u3r_chub(0, a);

  //  XX need a ur_bsw_back32()
  //
  ur_bsw_back64(rit_u, (c3_y)met_w, bak_d);
}

/* _cs_jam_xeno_atom(): encode atom or backref in bitstream.
*/
static void
_cs_jam_xeno_atom(u3_atom a, void* ptr_v)
{
  _jam_xeno_t* jam_u = ptr_v;
  ur_bsw_t*    rit_u = &(jam_u->rit_u);
  u3_weak        bak = u3h_git(jam_u->har_p, a);
  c3_w         met_w = u3r_met(0, a);

  if ( u3_none == bak ) {
    u3h_put(jam_u->har_p, a, _cs_coin_chub(rit_u->bits));
    _cs_jam_bsw_atom(rit_u, met_w, a);
  }
  else {
    c3_w bak_w = u3r_met(0, bak);

    if ( met_w <= bak_w ) {
      _cs_jam_bsw_atom(rit_u, met_w, a);
    }
    else {
      _cs_jam_bsw_back(rit_u, bak_w, bak);
    }
  }
}

/* _cs_jam_xeno_cell(): encode cell or backref in bitstream.
*/
static c3_o
_cs_jam_xeno_cell(u3_noun a, void* ptr_v)
{
  _jam_xeno_t* jam_u = ptr_v;
  ur_bsw_t*    rit_u = &(jam_u->rit_u);
  u3_weak        bak = u3h_git(jam_u->har_p, a);

  if ( u3_none == bak ) {
    u3h_put(jam_u->har_p, a, _cs_coin_chub(rit_u->bits));
    ur_bsw_cell(rit_u);
    return c3y;
  }
  else {
    _cs_jam_bsw_back(rit_u, u3r_met(0, bak), bak);
    return c3n;
  }
}

/* u3s_jam_xeno(): jam with off-loom buffer (re-)allocation.
*/
c3_d
u3s_jam_xeno(u3_noun a, c3_d* len_d, c3_y** byt_y)
{
  _jam_xeno_t jam_u = {0};
  ur_bsw_init(&jam_u.rit_u, ur_fib11, ur_fib12);

  jam_u.har_p = u3h_new();

  //  as this is a hot path, we unsafely elide overflow checks
  //
  //    a page-fault overflow detection system is urgently needed ...
  //
  u3a_walk_fore_unsafe(a, &jam_u, _cs_jam_xeno_atom,
                                  _cs_jam_xeno_cell);

  u3h_free(jam_u.har_p);

  return ur_bsw_done(&jam_u.rit_u, len_d, byt_y);
}

/* _cs_cue: stack frame for tracking intermediate cell results
*/
typedef struct _cs_cue {
  u3_weak hed;  //  head of a cell or u3_none
  u3_atom wid;  //  bitwidth of [hed] or 0
  u3_atom cur;  //  bit-cursor position
} _cs_cue;

/* _cs_rub: rub, TRANSFER [cur], RETAIN [a]
*/
static inline u3_noun
_cs_rub(u3_atom cur, u3_atom a)
{
  u3_noun pro = u3qe_rub(cur, a);
  u3z(cur);
  return pro;
}

/* _cs_cue_next(): advance into [a], reading next value
**                 TRANSFER [cur], RETAIN [a]
*/
static inline u3_noun
_cs_cue_next(u3a_pile*     pil_u,
             u3p(u3h_root) har_p,
             u3_atom         cur,
             u3_atom           a,
             u3_atom*        wid)
{
  while ( 1 ) {
    //  read tag bit at cur
    //
    c3_y tag_y = u3qc_cut(0, cur, 1, a);

    //  low bit unset, (1 + cur) points to an atom
    //
    //    produce atom and the width we read
    //
    if ( 0 == tag_y ) {
      u3_noun bur = _cs_rub(u3i_vint(cur), a);
      u3_noun pro = u3k(u3t(bur));

      u3h_put(har_p, cur, u3k(pro));
      *wid = u3qa_inc(u3h(bur));

      u3z(bur);
      return pro;
    }
    else {
      //  read tag bit at (1 + cur)
      //
      {
        u3_noun x = u3qa_inc(cur);
        tag_y = u3qc_cut(0, x, 1, a);
        u3z(x);
      }

      //  next bit set, (2 + cur) points to a backref
      //
      //    produce referenced value and the width we read
      //
      if ( 1 == tag_y ) {
        u3_noun bur = _cs_rub(u3ka_add(2, cur), a);
        u3_noun pro = u3x_good(u3h_get(har_p, u3t(bur)));

        *wid = u3qa_add(2, u3h(bur));

        u3z(bur);
        return pro;
      }
      //  next bit unset, (2 + cur) points to the head of a cell
      //
      //    push a head-frame onto the road stack and read the head
      //
      else {
        _cs_cue* fam_u = u3a_push(pil_u);
        u3a_pile_sane(pil_u);

        //  NB: fam_u->wid unused in head-frame
        //
        fam_u->hed = u3_none;
        fam_u->cur = cur;

        cur = u3qa_add(2, cur);
        continue;
      }
    }
  }
}

u3_noun
u3s_cue(u3_atom a)
{
  //  pro:   cue'd noun product
  //  wid:   bitwidth read to produce [pro]
  //  fam_u: stack frame
  //  har_p: backreference table
  //  pil_u: stack control structure
  //
  u3_noun         pro;
  u3_atom    wid, cur = 0;
  _cs_cue*      fam_u;
  u3p(u3h_root) har_p = u3h_new();
  u3a_pile      pil_u;

  //  initialize stack control
  //
  u3a_pile_prep(&pil_u, sizeof(*fam_u));

  //  commence cueing at bit-position 0
  //
  pro = _cs_cue_next(&pil_u, har_p, 0, a, &wid);

  //  process cell results
  //
  if ( c3n == u3a_pile_done(&pil_u) ) {
    fam_u = u3a_peek(&pil_u);

    do {
      //  head-frame: stash [pro] and [wid]; continue into the tail
      //
      if ( u3_none == fam_u->hed ) {
        //  NB: fam_u->wid unused in head-frame
        //
        fam_u->hed = pro;
        fam_u->wid = wid;

        //  continue reading at the bit-position after [pro]
        {
          u3_noun cur = u3ka_add(2, u3qa_add(wid, fam_u->cur));
          pro = _cs_cue_next(&pil_u, har_p, cur, a, &wid);
        }

        fam_u = u3a_peek(&pil_u);
      }
      //  tail-frame: cons cell, recalculate [wid], and pop the stack
      //
      else {
        pro   = u3nc(fam_u->hed, pro);
        u3h_put(har_p, fam_u->cur, u3k(pro));
        u3z(fam_u->cur);
        wid   = u3ka_add(2, u3ka_add(wid, fam_u->wid));
        fam_u = u3a_pop(&pil_u);
      }
    } while ( c3n == u3a_pile_done(&pil_u) );
  }

  u3z(wid);
  u3h_free(har_p);

  return pro;
}

/*
**  stack frame for recording head vs tail iteration
**
**    $?  [u3_none bits=@]
**    [hed=* bits=@]
*/
typedef struct _cue_frame_s {
  u3_weak ref;
  c3_d  bit_d;
} _cue_frame_t;

/* _cs_cue_xeno_next(): read next value from bitstream, dictionary off-loom.
*/
static inline ur_cue_res_e
_cs_cue_xeno_next(u3a_pile*    pil_u,
                  ur_bsr_t*    red_u,
                  ur_dict32_t* dic_u,
                  u3_noun*       out)
{
  ur_root_t* rot_u = 0;

  while ( 1 ) {
    c3_d  len_d, bit_d = red_u->bits;
    ur_cue_tag_e tag_e;
    ur_cue_res_e res_e;

    if ( ur_cue_good != (res_e = ur_bsr_tag(red_u, &tag_e)) ) {
      return res_e;
    }

    switch ( tag_e ) {
      default: c3_assert(0);

      case ur_jam_cell: {
        _cue_frame_t* fam_u = u3a_push(pil_u);
        u3a_pile_sane(pil_u);

        fam_u->ref   = u3_none;
        fam_u->bit_d = bit_d;
        continue;
      }

      case ur_jam_back: {
        if ( ur_cue_good != (res_e = ur_bsr_rub_len(red_u, &len_d)) ) {
          return res_e;
        }
        else if ( 62 < len_d ) {
          return ur_cue_meme;
        }
        else {
          c3_d bak_d = ur_bsr64_any(red_u, len_d);
          c3_w bak_w;

          if ( !ur_dict32_get(rot_u, dic_u, bak_d, &bak_w) ) {
            return ur_cue_back;
          }

          *out = u3k((u3_noun)bak_w);
          return ur_cue_good;
        }
      }

      case ur_jam_atom: {
        if ( ur_cue_good != (res_e = ur_bsr_rub_len(red_u, &len_d)) ) {
          return res_e;
        }

        if ( 31 >= len_d ) {
          *out = (u3_noun)ur_bsr32_any(red_u, len_d);
        }
        else {
          c3_d     byt_d = (len_d + 0x7) >> 3;
          u3i_slab sab_u;

          if ( 0xffffffffULL < byt_d) {
            return ur_cue_meme;
          }
          else {
            u3i_slab_init(&sab_u, 3, byt_d);
            ur_bsr_bytes_any(red_u, len_d, sab_u.buf_y);
            *out = u3i_slab_mint_bytes(&sab_u);
          }
        }

        ur_dict32_put(rot_u, dic_u, bit_d, *out);
        return ur_cue_good;
      }
    }
  }
}

struct _u3_cue_xeno {
  ur_dict32_t dic_u;
};

/* _cs_cue_xeno(): cue on-loom, with off-loom dictionary in handle.
*/
static u3_weak
_cs_cue_xeno(u3_cue_xeno* sil_u,
             c3_d         len_d,
             const c3_y*  byt_y)
{
  ur_bsr_t      red_u = {0};
  ur_dict32_t*  dic_u = &sil_u->dic_u;
  u3a_pile      pil_u;
  _cue_frame_t* fam_u;
  ur_cue_res_e  res_e;
  u3_noun         ref;

  //  initialize stack control
  //
  u3a_pile_prep(&pil_u, sizeof(*fam_u));

  //  init bitstream-reader
  //
  if ( ur_cue_good != (res_e = ur_bsr_init(&red_u, len_d, byt_y)) ) {
    return c3n;
  }
  //  bit-cursor (and backreferences) must fit in 62-bit direct atoms
  //
  else if ( 0x7ffffffffffffffULL < len_d ) {
    return c3n;
  }

  //  advance into stream
  //
  res_e = _cs_cue_xeno_next(&pil_u, &red_u, dic_u, &ref);

  //  process cell results
  //
  if (  (c3n == u3a_pile_done(&pil_u))
     && (ur_cue_good == res_e) )
  {
    fam_u = u3a_peek(&pil_u);

    do {
      //  f is a head-frame; stash result and read the tail from the stream
      //
      if ( u3_none == fam_u->ref ) {
        fam_u->ref = ref;
        res_e = _cs_cue_xeno_next(&pil_u, &red_u, dic_u, &ref);
        fam_u = u3a_peek(&pil_u);
      }
      //  f is a tail-frame; pop the stack and continue
      //
      else {
        ur_root_t* rot_u = 0;

        ref   = u3nc(fam_u->ref, ref);
        ur_dict32_put(rot_u, dic_u, fam_u->bit_d, ref);
        fam_u = u3a_pop(&pil_u);
      }
    }
    while (  (c3n == u3a_pile_done(&pil_u))
          && (ur_cue_good == res_e) );
  }

  if ( ur_cue_good == res_e ) {
    return ref;
  }
  //  on failure, unwind the stack and dispose of intermediate nouns
  //
  else if ( c3n == u3a_pile_done(&pil_u) ) {
    do {
      if ( u3_none != fam_u->ref ) {
        u3z(fam_u->ref);
      }
      fam_u = u3a_pop(&pil_u);
    }
    while ( c3n == u3a_pile_done(&pil_u) );
  }

  return u3_none;
}

/* u3s_cue_xeno_init_with(): initialize a cue_xeno handle as specified.
*/
u3_cue_xeno*
u3s_cue_xeno_init_with(c3_d pre_d, c3_d siz_d)
{
  u3_cue_xeno* sil_u;

  c3_assert( &(u3H->rod_u) == u3R );

  sil_u = c3_calloc(sizeof(*sil_u));
  ur_dict32_grow((ur_root_t*)0, &sil_u->dic_u, pre_d, siz_d);

  return sil_u;
}

/* u3s_cue_xeno_init(): initialize a cue_xeno handle.
*/
u3_cue_xeno*
u3s_cue_xeno_init(void)
{
  return u3s_cue_xeno_init_with(ur_fib10, ur_fib11);
}

/* u3s_cue_xeno_init(): cue on-loom, with off-loom dictionary in handle.
*/
u3_weak
u3s_cue_xeno_with(u3_cue_xeno* sil_u,
                  c3_d         len_d,
                  const c3_y*  byt_y)
{
  u3_weak som;

  c3_assert( &(u3H->rod_u) == u3R );

  som = _cs_cue_xeno(sil_u, len_d, byt_y);
  ur_dict32_wipe(&sil_u->dic_u);
  return som;
}

/* u3s_cue_xeno_init(): dispose cue_xeno handle.
*/
void
u3s_cue_xeno_done(u3_cue_xeno* sil_u)
{
  ur_dict_free((ur_dict_t*)&sil_u->dic_u);
  c3_free(sil_u);
}

/* u3s_cue_xeno(): cue on-loom, with off-loom dictionary.
*/
u3_weak
u3s_cue_xeno(c3_d        len_d,
             const c3_y* byt_y)
{
  u3_cue_xeno* sil_u;
  u3_weak        som;

  c3_assert( &(u3H->rod_u) == u3R );

  sil_u = u3s_cue_xeno_init();
  som   = _cs_cue_xeno(sil_u, len_d, byt_y);
  u3s_cue_xeno_done(sil_u);
  return som;
}

/* _cs_cue_need(): bail on ur_cue_* read failures.
*/
static inline void
_cs_cue_need(ur_cue_res_e res_e)
{
  if ( ur_cue_good == res_e ) {
    return;
  }
  else {
    c3_m mot_m = (ur_cue_meme == res_e) ? c3__meme : c3__exit;
    u3m_bail(mot_m);
  }
}

/* _cs_cue_get(): u3h_get wrapper handling allocation and refcounts.
*/
static inline u3_weak
_cs_cue_get(u3p(u3h_root) har_p, c3_d key_d)
{
  u3_atom key = _cs_coin_chub(key_d);
  u3_weak pro = u3h_get(har_p, key);
  u3z(key);
  return pro;
}

/* _cs_cue_put(): u3h_put wrapper handling allocation and refcounts.
*/
static inline u3_noun
_cs_cue_put(u3p(u3h_root) har_p, c3_d key_d, u3_noun val)
{
  u3_atom key = _cs_coin_chub(key_d);
  u3h_put(har_p, key, u3k(val));
  u3z(key);
  return val;
}

/* _cs_cue_bytes_next(): read next value from bitstream.
*/
static inline u3_noun
_cs_cue_bytes_next(u3a_pile*     pil_u,
                   u3p(u3h_root) har_p,
                   ur_bsr_t*     red_u)
{
  while ( 1 ) {
    c3_d  len_d, bit_d = red_u->bits;
    ur_cue_tag_e tag_e;

    _cs_cue_need(ur_bsr_tag(red_u, &tag_e));

    switch ( tag_e ) {
      default: c3_assert(0);

      case ur_jam_cell: {
        _cue_frame_t* fam_u = u3a_push(pil_u);
        u3a_pile_sane(pil_u);

        fam_u->ref   = u3_none;
        fam_u->bit_d = bit_d;
        continue;
      }

      case ur_jam_back: {
        _cs_cue_need(ur_bsr_rub_len(red_u, &len_d));

        if ( 62 < len_d ) {
          return u3m_bail(c3__meme);
        }
        else {
          c3_d  bak_d = ur_bsr64_any(red_u, len_d);
          u3_weak bak = _cs_cue_get(har_p, bak_d);
          return u3x_good(bak);
        }
      }

      case ur_jam_atom: {
        u3_atom vat;

        _cs_cue_need(ur_bsr_rub_len(red_u, &len_d));

        if ( 31 >= len_d ) {
          vat = (u3_noun)ur_bsr32_any(red_u, len_d);
        }
        else {
          u3i_slab sab_u;
          u3i_slab_init(&sab_u, 0, len_d);

          ur_bsr_bytes_any(red_u, len_d, sab_u.buf_y);
          vat = u3i_slab_mint_bytes(&sab_u);
        }

        return _cs_cue_put(har_p, bit_d, vat);
      }
    }
  }
}

/* u3s_cue_bytes(): cue bytes onto the loom.
*/
u3_noun
u3s_cue_bytes(c3_d len_d, const c3_y* byt_y)
{
  ur_bsr_t      red_u = {0};
  u3a_pile      pil_u;
  _cue_frame_t* fam_u;
  u3p(u3h_root) har_p;
  u3_noun         ref;

  //  initialize stack control
  //
  u3a_pile_prep(&pil_u, sizeof(*fam_u));

  //  initialize a hash table for dereferencing backrefs
  //
  har_p = u3h_new();

  //  init bitstream-reader
  //
  _cs_cue_need(ur_bsr_init(&red_u, len_d, byt_y));

  //  bit-cursor (and backreferences) must fit in 62-bit direct atoms
  //
  if ( 0x7ffffffffffffffULL < len_d ) {
    return u3m_bail(c3__meme);
  }

  //  advance into stream
  //
  ref = _cs_cue_bytes_next(&pil_u, har_p, &red_u);

  //  process cell results
  //
  if ( c3n == u3a_pile_done(&pil_u) ) {
    fam_u = u3a_peek(&pil_u);

    do {
      //  f is a head-frame; stash result and read the tail from the stream
      //
      if ( u3_none == fam_u->ref ) {
        fam_u->ref = ref;
        ref        = _cs_cue_bytes_next(&pil_u, har_p, &red_u);
        fam_u      = u3a_peek(&pil_u);
      }
      //  f is a tail-frame; pop the stack and continue
      //
      else {
        ref   = u3nc(fam_u->ref, ref);
        _cs_cue_put(har_p, fam_u->bit_d, ref);
        fam_u = u3a_pop(&pil_u);
      }
    }
    while ( c3n == u3a_pile_done(&pil_u) );
  }

  u3h_free(har_p);

  return ref;
}

/* u3s_cue_atom(): cue atom.
*/
u3_noun
u3s_cue_atom(u3_atom a)
{
  c3_w  len_w = u3r_met(3, a);
  c3_y* byt_y;

  // XX assumes little-endian
  //
  if ( c3y == u3a_is_cat(a) ) {
     byt_y = (c3_y*)&a;
   }
   else {
    u3a_atom* vat_u = u3a_to_ptr(a);
    byt_y = (c3_y*)vat_u->buf_w;
  }

  return u3s_cue_bytes((c3_d)len_w, byt_y);
}

#define NOT_DEC(a) ( ((a) < '0') || ((a) > '9') )

/* u3s_sift_ud_bytes: parse @ud
*/
u3_weak
u3s_sift_ud_bytes(c3_w len_w, c3_y* byt_y)
{
  c3_s val_s;

  if ( !len_w ) return u3_none;

  //  +ape:ag: just 0
  //
  if ( '0' == *byt_y ) {
    return ( 1 == len_w ) ? (u3_noun)0 : u3_none;
  }

  // if ( (1 == len_w) && ('0' == *byt_y) ) return (u3_noun)val_s;

  //  +ted:ab: leading nonzero, 0-2 digits
  //
  if ( NOT_DEC(*byt_y) ) return u3_none;

  val_s = *byt_y++ - '0';
  if ( 0 == --len_w )    return (u3_noun)val_s;

  //  1 digit
  //
  if ( '.' == *byt_y )   goto tid_ab;
  if ( NOT_DEC(*byt_y) ) return u3_none;

  val_s *= 10;
  val_s += *byt_y++ - '0';
  if ( 0 == --len_w )    return (u3_noun)val_s;

  //  2 digits
  //
  if ( '.' == *byt_y )   goto tid_ab;
  if ( NOT_DEC(*byt_y) ) return u3_none;

  val_s *= 10;
  val_s += *byt_y++ - '0';
  if ( 0 == --len_w )    return (u3_noun)val_s;

tid_ab:
  //  +tid:ab: dot-prefixed 3-digit blocks
  //
  if ( len_w % 4 ) return u3_none;

  {
    //  XX estimate size, allocate once
    //
    mpz_t a_mp;
    mpz_init_set_ui(a_mp, val_s);

    while ( len_w ) {
      if (  ('.' != byt_y[0])
         || NOT_DEC(byt_y[1])
         || NOT_DEC(byt_y[2])
         || NOT_DEC(byt_y[1]) )
      {
        mpz_clear(a_mp);
        return u3_none;
      }

      byt_y++;

      val_s  = *byt_y++ - '0';
      val_s *= 10;
      val_s += *byt_y++ - '0';
      val_s *= 10;
      val_s += *byt_y++ - '0';

      mpz_mul_ui(a_mp, a_mp, 1000);
      mpz_add_ui(a_mp, a_mp, val_s);

      len_w -= 4;
    }

    return u3i_mp(a_mp);
  }
}

#undef NOT_DEC

/* u3s_sift_ud: parse @ud.
*/
u3_weak
u3s_sift_ud(u3_atom a)
{
  c3_w  len_w = u3r_met(3, a);
  c3_y* byt_y;

  // XX assumes little-endian
  //
  if ( c3y == u3a_is_cat(a) ) {
     byt_y = (c3_y*)&a;
   }
   else {
    u3a_atom* vat_u = u3a_to_ptr(a);
    byt_y = (c3_y*)vat_u->buf_w;
  }

  return u3s_sift_ud_bytes(len_w, byt_y);
}
