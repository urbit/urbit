/* noun/serial.c
**
*/

#include <errno.h>
#include <fcntl.h>

#include "all.h"
#include "ur/ur.h"

/* _cs_met0_w(): safe bitwidth for any c3_w
*/
static inline c3_w
_cs_met0_w(c3_w wid_w) {
  return ( wid_w >> 31 ) ? 32 : u3r_met(0, wid_w);
}

/* _cs_jam_buf: struct for tracking the fibonacci-allocated jam of a noun
*/
struct _cs_jam_fib {
  u3p(u3h_root) har_p;
  c3_w          a_w;
  c3_w          b_w;
  c3_w          bit_w;
  c3_w*         buf_w;
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
    c3_w old_w, new_w, c_w = 0;

    old_w = fib_u->a_w >> 5;
    if ( (old_w << 5) != fib_u->a_w ) {
      ++old_w;
    }

    //  fibonacci growth
    //
    while ( c_w < wan_w ) {
      c_w        = fib_u->a_w + fib_u->b_w;
      fib_u->b_w = fib_u->a_w;
      fib_u->a_w = c_w;
    }

    new_w = c_w >> 5;
    if ( (new_w << 5) != c_w ) {
      ++new_w;
    }

    fib_u->buf_w = u3a_wealloc(fib_u->buf_w, new_w);
    memset(fib_u->buf_w + old_w, 0, (new_w - old_w) * sizeof(c3_w));
  }
}

/* _cs_jam_fib_chop(): chop [met_w] bits of [a] into [fib_u]
*/
static void
_cs_jam_fib_chop(struct _cs_jam_fib* fib_u, c3_w met_w, u3_noun a)
{
  c3_w bit_w = fib_u->bit_w;
  _cs_jam_fib_grow(fib_u, met_w);
  u3r_chop(0, 0, met_w, bit_w, fib_u->buf_w, a);
  fib_u->bit_w += met_w;
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
    c3_w b_w = _cs_met0_w(a_w);

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
c3_w*
u3s_jam_fib(u3_noun a, c3_w* bit_w)
{
  struct _cs_jam_fib fib_u;
  fib_u.har_p = u3h_new();
  //  fib(12) is small enough to be reasonably fast to allocate.
  //
  fib_u.a_w   = 144;
  //  fib(11) is needed to get fib(13).
  //
  fib_u.b_w   = 89;
  fib_u.bit_w = 0;

  {
    c3_w len_w = fib_u.a_w >> 5;
    if ( (len_w << 5) != fib_u.a_w ) {
      ++len_w;
    }

    fib_u.buf_w = u3a_walloc(len_w);
    memset(fib_u.buf_w, 0, len_w * sizeof(c3_w));
  }

  //  as this is a hot path, we unsafely elide overflow checks
  //
  //    a page-fault overflow detection system is urgently needed ...
  //
  u3a_walk_fore_unsafe(a, &fib_u, _cs_jam_fib_atom_cb,
                                  _cs_jam_fib_cell_cb);

  *bit_w = fib_u.bit_w;
  u3h_free(fib_u.har_p);
  return fib_u.buf_w;
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

#define CUE_ROOT 0
#define CUE_HEAD 1
#define CUE_TAIL 2

//  stack frame for recording head vs tail iteration
//
//    In Hoon, this structure would be as follows:
//
//    $%  [%root ~]
//        [%head cell-cursor=@]
//        [%tail cell-cursor=@ hed-width=@ hed-value=*]
//    ==
//
typedef struct _cs_cue_frame
{
  c3_y    tag_y;
  u3_atom cur;
  u3_atom wid;
  u3_noun hed;
} cueframe;

/* _cs_cue_push(): construct a cueframe and push it onto the road stack.
*/
static inline void
_cs_cue_push(c3_ys   mov,
             c3_ys   off,
             c3_y    tag_y,
             u3_atom cur,
             u3_atom wid,
             u3_noun hed)
{
  u3R->cap_p += mov;

  //  ensure we haven't overflowed the stack
  //  (off==0 means we're on a north road)
  //
  if ( 0 == off ) {
    if( !(u3R->cap_p > u3R->hat_p) ) {
      u3m_bail(c3__meme);
    }
  }
  else {
    if( !(u3R->cap_p < u3R->hat_p) ) {
      u3m_bail(c3__meme);
    }
  }

  cueframe* fam_u = u3to(cueframe, u3R->cap_p + off);
  fam_u->tag_y = tag_y;
  fam_u->cur   = cur;
  fam_u->wid   = wid;
  fam_u->hed   = hed;
}

/* _cs_cue_pop(): pop a cueframe off the road stack and return it.
*/
static inline cueframe
_cs_cue_pop(c3_ys mov, c3_ys off)
{
  cueframe* fam_u = u3to(cueframe, u3R->cap_p + off);
  u3R->cap_p -= mov;

  return *fam_u;
}

/* u3s_cue(): cue [a]
*/
u3_noun
u3s_cue(u3_atom a)
{
  //  initialize signed stack offsets (relative to north/south road)
  //
  c3_ys mov, off;
  {
    c3_y wis_y = c3_wiseof(cueframe);
    c3_o nor_o = u3a_is_north(u3R);
    mov = ( c3y == nor_o ? -wis_y : wis_y );
    off = ( c3y == nor_o ? 0 : -wis_y );
  }

  //  initialize a hash table for dereferencing backrefs
  //
  u3p(u3h_root) har_p = u3h_new();

  //  stash the current stack post
  //
  u3p(cueframe) cap_p = u3R->cap_p;

  //  push the (only) ROOT stack frame (our termination condition)
  //
  _cs_cue_push(mov, off, CUE_ROOT, 0, 0, 0);

  // initialize cursor to bit-position 0
  //
  u3_atom cur = 0;

  //  the bitwidth and product from reading at cursor
  //
  u3_atom wid, pro;

  //  read from atom at cursor
  //
  //    TRANSFER .cur
  //
  advance: {
    //  read tag bit at cur
    //
    c3_y tag_y = u3qc_cut(0, cur, 1, a);

    //  low bit unset, (1 + cur) points to an atom
    //
    //    produce atom and the width we read
    //
    if ( 0 == tag_y ) {
      u3_noun bur;
      {
        u3_noun x = u3qa_inc(cur);
        bur = u3qe_rub(x, a);
        u3z(x);
      }

      pro = u3k(u3t(bur));
      u3h_put(har_p, cur, u3k(pro));
      wid = u3qa_inc(u3h(bur));

      u3z(bur);
      goto retreat;
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
        u3_noun bur;
        {
          u3_noun x = u3ka_add(2, cur);
          bur = u3qe_rub(x, a);
          u3z(x);
        }

        pro = u3h_get(har_p, u3k(u3t(bur)));

        if ( u3_none == pro ) {
          return u3m_bail(c3__exit);
        }

        wid = u3qa_add(2, u3h(bur));

        u3z(bur);
        goto retreat;
      }
      //  next bit unset, (2 + cur) points to the head of a cell
      //
      //    push a frame to mark HEAD recursion and read the head
      //
      else {
        _cs_cue_push(mov, off, CUE_HEAD, cur, 0, 0);

        cur = u3qa_add(2, cur);
        goto advance;
      }
    }
  }

  //  consume: popped stack frame, .wid and .pro from above.
  //
  //    TRANSFER .wid, .pro, and contents of .fam_u
  //    (.cur is in scope, but we have already lost our reference to it)
  //
  retreat: {
    cueframe fam_u = _cs_cue_pop(mov, off);

    switch ( fam_u.tag_y ) {
      default: {
        c3_assert(0);
      }

      //  fam_u is our stack root, we're done.
      //
      case CUE_ROOT: {
        break;
      }

      //  .wid and .pro are the head of the cell at fam_u.cur.
      //  save them (and the cell cursor) in a TAIL frame,
      //  set the cursor to the tail and read there.
      //
      case CUE_HEAD: {
        _cs_cue_push(mov, off, CUE_TAIL, fam_u.cur, wid, pro);

        cur = u3ka_add(2, u3qa_add(wid, fam_u.cur));
        goto advance;
      }

      //  .wid and .pro are the tail of the cell at fam_u.cur,
      //  construct the cell, memoize it, and produce it along with
      //  its total width (as if it were a read from above).
      //
      case CUE_TAIL: {
        pro = u3nc(fam_u.hed, pro);
        u3h_put(har_p, fam_u.cur, u3k(pro));
        wid = u3ka_add(2, u3ka_add(wid, fam_u.wid));
        goto retreat;
      }
    }
  }

  u3z(wid);
  u3h_free(har_p);

  //  sanity check
  //
  c3_assert( u3R->cap_p == cap_p );

  return pro;
}

/* _cs_cue_sill_back(): cue a backref, off-loom dictionary.
*/
static inline ur_cue_res_e
_cs_cue_sill_back(ur_bsr_t*    red_u,
                  ur_dict32_t* dic_u,
                  u3_noun*       out)
{
  ur_root_t*   rot_u = 0;
  ur_cue_res_e res_e;
  c3_d         len_d;

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

/* _cs_cue_sill_atom(): cue an atom, off-loom dictionary.
*/
static inline ur_cue_res_e
_cs_cue_sill_atom(ur_bsr_t*    red_u,
                  ur_dict32_t* dic_u,
                  c3_d         bit_d,
                  u3_noun*       out)
{
  ur_root_t*   rot_u = 0;
  ur_cue_res_e res_e;
  c3_d         len_d;

  if ( ur_cue_good != (res_e = ur_bsr_rub_len(red_u, &len_d)) ) {
    return res_e;
  }

  if ( 31 >= len_d ) {
    *out = (u3_noun)ur_bsr32_any(red_u, len_d);
  }
  //  XX need a ur_bsr_words_any()
  //
  else {
    c3_w* wor_w;
    c3_y* byt_y;

    {
      c3_d byt_d = (len_d >> 3) + !!ur_mask_3(len_d);

      if ( 0xffffffffULL < byt_d) {
        return u3m_bail(c3__meme);
      }

      //  XX assumes little-endian
      //
      wor_w = u3a_slaq(3, byt_d);
      byt_y = (c3_y*)wor_w;
    }

    ur_bsr_bytes_any(red_u, len_d, byt_y);
    *out = u3a_malt(wor_w);
  }

  ur_dict32_put(rot_u, dic_u, bit_d, *out);
  return ur_cue_good;
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

/* _cs_cue_sill_next(): read next value from bitstream, dictionary off-loom.
**                  NB: this is _cs_cue_xeno_next, using the road stack.
*/
static inline ur_cue_res_e
_cs_cue_sill_next(u3a_pile*    pil_u,
                  ur_bsr_t*    red_u,
                  ur_dict32_t* dic_u,
                  u3_noun*       out)
{
  while ( 1 ) {
    c3_d         bit_d = red_u->bits;
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
        return _cs_cue_sill_back(red_u, dic_u, out);
      }

      case ur_jam_atom: {
        return _cs_cue_sill_atom(red_u, dic_u, bit_d, out);
      }
    }
  }
}

struct _u3_cue_sill {
  ur_dict32_t dic_u;
};

/* _cs_cue_sill(): cue on-loom, with off-loom dictionary in handle.
*/
static u3_weak
_cs_cue_sill(u3_cue_sill* sil_u,
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
  res_e = _cs_cue_sill_next(&pil_u, &red_u, dic_u, &ref);

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
        res_e = _cs_cue_sill_next(&pil_u, &red_u, dic_u, &ref);
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

/* u3s_cue_sill_init_with(): initialize a cue_sill handle as specified.
*/
u3_cue_sill*
u3s_cue_sill_init_with(c3_d pre_d, c3_d siz_d)
{
  u3_cue_sill* sil_u;

  c3_assert( &(u3H->rod_u) == u3R );

  sil_u = c3_calloc(sizeof(*sil_u));
  ur_dict32_grow((ur_root_t*)0, &sil_u->dic_u, pre_d, siz_d);

  return sil_u;
}

/* u3s_cue_sill_init(): initialize a cue_sill handle.
*/
u3_cue_sill*
u3s_cue_sill_init(void)
{
  return u3s_cue_sill_init_with(ur_fib10, ur_fib11);
}

/* u3s_cue_sill_init(): cue on-loom, with off-loom dictionary in handle.
*/
u3_weak
u3s_cue_sill_with(u3_cue_sill* sil_u,
                  c3_d         len_d,
                  const c3_y*  byt_y)
{
  u3_weak som;

  c3_assert( &(u3H->rod_u) == u3R );

  som = _cs_cue_sill(sil_u, len_d, byt_y);
  ur_dict32_wipe(&sil_u->dic_u);
  return som;
}

/* u3s_cue_sill_init(): dispose cue_sill handle.
*/
void
u3s_cue_sill_done(u3_cue_sill* sil_u)
{
  ur_dict_free((ur_dict_t*)&sil_u->dic_u);
  c3_free(sil_u);
}

/* u3s_cue_sill(): cue on-loom, with off-loom dictionary.
*/
u3_weak
u3s_cue_sill(c3_d        len_d,
             const c3_y* byt_y)
{
  u3_cue_sill* sil_u;
  u3_weak        som;

  c3_assert( &(u3H->rod_u) == u3R );

  sil_u = u3s_cue_sill_init();
  som   = _cs_cue_sill(sil_u, len_d, byt_y);
  u3s_cue_sill_done(sil_u);
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
        //  XX need a ur_bsr_words_any()
        //
        else {
          c3_w* wor_w;
          c3_y* byt_y;

          {
            c3_d byt_d = (len_d >> 3) + !!ur_mask_3(len_d);

            if ( 0xffffffffULL < byt_d) {
              return u3m_bail(c3__meme);
            }

            //  XX assumes little-endian
            //
            wor_w = u3a_slaq(3, byt_d);
            byt_y = (c3_y*)wor_w;
          }

          ur_bsr_bytes_any(red_u, len_d, byt_y);
          vat = u3a_malt(wor_w);
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
