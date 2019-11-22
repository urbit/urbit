/* noun/serial.c
**
*/

#include <errno.h>
#include <fcntl.h>

#include "all.h"

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

/* _cs_jam_met_mat(): the jam bitwidth of an atom of bitwidth [wid_w]
**
**   equivalent to (head (rub a))
*/
static c3_d
_cs_jam_met_mat(c3_w wid_w)
{
  return ( 0 == wid_w ) ? 1ULL :
         (c3_d)wid_w + (2ULL * (c3_d)_cs_met0_w(wid_w));
}

/* _cs_jam_met: struct for tracking the jam bitwidth of a noun
*/
struct _cs_jam_met {
  u3p(u3h_root) har_p;
  u3p(u3h_root) bak_p;
  c3_d          len_d;
};

/* _cs_jam_met_atom_cb(): bitwidth of atom or backref encoding for [a]
*/
static void
_cs_jam_met_atom_cb(u3_atom a, void* ptr_v)
{
  struct _cs_jam_met* met_u = ptr_v;
  c3_w  a_w = u3r_met(0, a);
  u3_weak b = u3h_git(met_u->har_p, a);

  //  if we haven't haven't seen [a], put cursor into [har_p]
  //
  if ( u3_none == b ) {
    u3h_put(met_u->har_p, a, u3i_chubs(1, &(met_u->len_d)));
    met_u->len_d += 1ULL + _cs_jam_met_mat(a_w);
  }
  else {
    c3_w b_w = u3r_met(0, b);

    //  if [a] is smaller than a backref, use directly
    //
    if ( a_w <= b_w ) {
      met_u->len_d += 1ULL + _cs_jam_met_mat(a_w);
    }
    //  otherwise, save backref
    //
    else {
      u3h_put(met_u->bak_p, a, u3k(b));
      met_u->len_d += 2ULL + _cs_jam_met_mat(b_w);
    }
  }
}

/* _cs_jam_met_cell_cb(): bitwidth of cell or backref encoding for [a]
*/
static c3_o
_cs_jam_met_cell_cb(u3_noun a, void* ptr_v)
{
  struct _cs_jam_met* met_u = ptr_v;
  u3_weak b = u3h_git(met_u->har_p, a);

  //  if we haven't haven't seen [a], put cursor into [har_p]
  //
  if ( u3_none == b ) {
    u3h_put(met_u->har_p, a, u3i_chubs(1, &(met_u->len_d)));
    met_u->len_d += 2ULL;
    return c3y;
  }
  //  otherwise, save backref and shortcircuit traversal
  //
  else {
    c3_w b_w = u3r_met(0, b);
    u3h_put(met_u->bak_p, a, u3k(b));
    met_u->len_d += 2ULL + _cs_jam_met_mat(b_w);
    return c3n;
  }
}

/* u3s_jam_met(): measure a noun for jam, calculating backrefs
*/
c3_d
u3s_jam_met(u3_noun a, u3p(u3h_root)* bak_p)
{
  struct _cs_jam_met met_u;
  met_u.har_p = u3h_new();
  met_u.bak_p = u3h_new();
  met_u.len_d = 0ULL;

  u3a_walk_fore(a, &met_u, _cs_jam_met_atom_cb,
                           _cs_jam_met_cell_cb);
  u3h_free(met_u.har_p);
  *bak_p = met_u.bak_p;

  return met_u.len_d;
}

/* _cs_jam_buf: struct for tracking the pre-measured jam of a noun
*/
struct _cs_jam_buf {
  u3p(u3h_root) bak_p;
  c3_w          bit_w;
  c3_w*         buf_w;
};

/* _cs_jam_buf_chop(): chop [met_w] bits of [a] into [buf_u]
*/
static void
_cs_jam_buf_chop(struct _cs_jam_buf* buf_u, c3_w met_w, u3_noun a)
{
  u3r_chop(0, 0, met_w, buf_u->bit_w, buf_u->buf_w, a);
  buf_u->bit_w += met_w;
}

/* _cs_jam_buf_mat(): length-prefixed encode (mat) [a] into [buf_u]
*/
static void
_cs_jam_buf_mat(struct _cs_jam_buf* buf_u, u3_atom a)
{
  if ( 0 == a ) {
    _cs_jam_buf_chop(buf_u, 1, 1);
  }
  else {
    c3_w a_w = u3r_met(0, a);
    c3_w b_w = _cs_met0_w(a_w);

    _cs_jam_buf_chop(buf_u, b_w+1, 1 << b_w);
    _cs_jam_buf_chop(buf_u, b_w-1, a_w & ((1 << (b_w-1)) - 1));
    _cs_jam_buf_chop(buf_u, a_w, a);
  }
}

/* _cs_jam_buf_atom_cb(): encode atom or backref
*/
static void
_cs_jam_buf_atom_cb(u3_atom a, void* ptr_v)
{
  struct _cs_jam_buf* buf_u = ptr_v;
  u3_weak b = u3h_git(buf_u->bak_p, a);

  //  if [a] has no backref (or this is the referent), encode atom
  //
  if ( (u3_none == b) ||
       (u3r_word(0, b) == buf_u->bit_w) )
  {
    _cs_jam_buf_chop(buf_u, 1, 0);
    _cs_jam_buf_mat(buf_u, a);
  }
  else {
    c3_w a_w = u3r_met(0, a);
    c3_w b_w = u3r_met(0, b);

    //  if [a] is smaller than the backref, encode atom
    //
    if ( a_w <= b_w ) {
      _cs_jam_buf_chop(buf_u, 1, 0);
      _cs_jam_buf_mat(buf_u, a);
    }
    //  otherwise, encode backref
    //
    else {
      _cs_jam_buf_chop(buf_u, 2, 3);
      _cs_jam_buf_mat(buf_u, b);
    }
  }
}

/* _cs_jam_buf_cell_cb(): encode cell or backref
*/
static c3_o
_cs_jam_buf_cell_cb(u3_noun a, void* ptr_v)
{
  struct _cs_jam_buf* buf_u = ptr_v;
  u3_weak b = u3h_git(buf_u->bak_p, a);

  //  if [a] has no backref (or this is the referent), encode cell
  //
  if ( (u3_none == b) ||
       (u3r_word(0, b) == buf_u->bit_w) )
  {
    _cs_jam_buf_chop(buf_u, 2, 1);
    return c3y;
  }
  //  otherwise, encode backref and shortcircuit traversal
  //
  else {
    _cs_jam_buf_chop(buf_u, 2, 3);
    _cs_jam_buf_mat(buf_u, b);
    return c3n;
  }
}

/* u3s_jam_buf(): jam [a] into pre-allocated [buf_w], without allocation
**
**   using backrefs in [bak_p], as computed by u3s_jam_met()
**   NB [buf_w] must be pre-allocated with sufficient space
**
**   XX can only encode up to c3_w bits, due to use of chop
*/
void
u3s_jam_buf(u3_noun a, u3p(u3h_root) bak_p, c3_w* buf_w)
{
  struct _cs_jam_buf buf_u;
  buf_u.bak_p = bak_p;
  buf_u.buf_w = buf_w;
  buf_u.bit_w = 0;

  //  this is in fact safe under normal usage, as
  //  the stack will have been checked in u3s_jam_met()
  //
  u3a_walk_fore_unsafe(a, &buf_u, _cs_jam_buf_atom_cb,
                                  _cs_jam_buf_cell_cb);
}

/* u3s_jam_file(): jam [a] into a file, overwriting
*/
c3_o
u3s_jam_file(u3_noun a, c3_c* pas_c)
{
  u3p(u3h_root) bak_p;
  c3_i fid_i = open(pas_c, O_RDWR | O_CREAT | O_TRUNC, 0644);
  c3_w byt_w, wor_w, len_w;

  if ( fid_i < 0 ) {
    fprintf(stderr, "jam: open %s: %s\r\n", pas_c, strerror(errno));
    return c3n;
  }

  {
    c3_d len_d = u3s_jam_met(a, &bak_p);

    if ( len_d > 0xffffffffULL ) {
      fprintf(stderr, "jam: overflow c3_w: %" PRIu64 "\r\n", len_d);
      u3h_free(bak_p);
      return c3n;
    }

    //  length in bytes a la u3i_bytes
    //
    byt_w = (c3_w)(len_d >> 3ULL);
    if ( len_d > (c3_d)(byt_w << 3) ) {
      byt_w++;
    }

    //  length in words
    //
    wor_w = (c3_w)(len_d >> 5ULL);
    if ( len_d > (c3_d)(wor_w << 5) ) {
      wor_w++;
    }

    //  byte-length of word-length
    //
    len_w = 4 * wor_w;
  }

  //  grow [fid_i] to [len_w]
  //
  if ( 0 != ftruncate(fid_i, len_w) ) {
    fprintf(stderr, "jam: ftruncate grow %s: %s\r\n", pas_c, strerror(errno));
    goto error;
  }

  //  mmap [fid_i], jam into it, sync, and unmap
  //
  {
    c3_w* buf_w;
    void* ptr_v = mmap(0, len_w, PROT_READ|PROT_WRITE, MAP_SHARED, fid_i, 0);

    if ( MAP_FAILED == ptr_v ) {
      fprintf(stderr, "jam: mmap %s: %s\r\n", pas_c, strerror(errno));
      goto error;
    }

    buf_w = ptr_v;
    u3s_jam_buf(a, bak_p, buf_w);

    if ( 0 != msync(ptr_v, len_w, MS_SYNC) ) {
      fprintf(stderr, "jam: msync %s: %s\r\n", pas_c, strerror(errno));
      //  XX ignore return?
      //
      munmap(ptr_v, len_w);
      goto error;
    }

    if ( 0 != munmap(ptr_v, len_w) ) {
      fprintf(stderr, "jam: munmap %s: %s\r\n", pas_c, strerror(errno));
      //  XX fatal error?
      //
      goto error;
    }
  }

  //  shrink [fid_i] to [byt_w]
  //
  if ( 0 != ftruncate(fid_i, byt_w) ) {
    fprintf(stderr, "jam: ftruncate shrink %s: %s\r\n", pas_c, strerror(errno));
    goto error;
  }

  {
    close(fid_i);
    u3h_free(bak_p);
    return c3y;
  }

  error: {
    close(fid_i);
    unlink(pas_c);
    u3h_free(bak_p);
    return c3n;
  }
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
