/* noun/serial.c
**
*/
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
