/* j/5/jam.c
**
*/
#include "all.h"

typedef struct {
  c3_w  a_w;
  c3_w  b_w;
  c3_w  bit_w;
  c3_w* wor_w;
} _jam_buf;

static void
_jam_buf_grow(_jam_buf* buf_u, c3_w mor_w)
{
  c3_w wan_w = buf_u->bit_w + mor_w;

  if ( wan_w < mor_w ) {
    // overflowed c3_w bits
    u3m_bail(c3__fail);
  }

  if ( wan_w > buf_u->a_w ) {
    c3_w old_w, new_w, c_w = 0;

    old_w = buf_u->a_w >> 5;
    if ( (old_w << 5) != buf_u->a_w ) {
      ++old_w;
    }

    // fibonacci growth
    while ( c_w < wan_w ) {
      c_w        = buf_u->a_w + buf_u->b_w;
      buf_u->b_w = buf_u->a_w;
      buf_u->a_w = c_w;
    }

    new_w = c_w >> 5;
    if ( (new_w << 5) != c_w ) {
      ++new_w;
    }

    buf_u->wor_w = u3a_realloc(buf_u->wor_w, new_w * sizeof(c3_w));
    memset(buf_u->wor_w + old_w, 0, (new_w - old_w) * sizeof(c3_w));
  }
}

static void
_jam_buf_chop(_jam_buf* buf_u, c3_w met_w, u3_noun a)
{
  c3_w bit_w = buf_u->bit_w;
  _jam_buf_grow(buf_u, met_w);
  u3r_chop(0, 0, met_w, bit_w, buf_u->wor_w, a);
  buf_u->bit_w += met_w;
}

static void
_jam_buf_atom(_jam_buf* buf_u, u3_noun a)
{
  u3_noun mat = u3qe_mat(a);
  _jam_buf_chop(buf_u, u3h(mat), u3t(mat));
  u3z(mat);
}

static u3_noun
_jam_buf_top(u3_noun a)
{
  u3p(u3h_root) har_p = u3h_new();
  c3_o          nor_o = u3a_is_north(u3R);
  c3_y          wis_y = c3_wiseof(u3_noun);
  c3_ys         mov   = ( c3y == nor_o ? -wis_y : wis_y );
  c3_ys         off   = ( c3y == nor_o ? 0 : -wis_y );
  u3_noun*      top, *don = u3to(u3_noun, u3R->cap_p + off);
  _jam_buf      buf_u;
  u3_weak       c;
  c3_o          cel_o;
  c3_w*         sal_w, len_w;

  buf_u.a_w   = 144;  // fib(12) is small enough to be reasonably fast to allocate.
  buf_u.b_w   = 89;   // fib(11) is needed to get fib(13).
  len_w       = buf_u.a_w >> 5;
  if ( (len_w << 5) != buf_u.a_w ) {
    ++len_w;
  }
  buf_u.wor_w = u3a_calloc(len_w, sizeof(c3_w));
  buf_u.bit_w = 0;

  u3R->cap_p += mov;
  top         = u3to(u3_noun, u3R->cap_p + off);
  *top        = a;

  while ( top != don ) {
    a     = *top;
    cel_o = u3du(a);
    c     = u3h_git(har_p, a);
    if ( u3_none != c ) {
      if ( c3y == cel_o ) {
          _jam_buf_chop(&buf_u, 2, 3);
          _jam_buf_atom(&buf_u, c);
      }
      else {
        if ( u3r_met(0, a) <= u3r_met(0, c) ) {
          _jam_buf_chop(&buf_u, 1, 0);
          _jam_buf_atom(&buf_u, a);
        }
        else {
          _jam_buf_chop(&buf_u, 2, 3);
          _jam_buf_atom(&buf_u, c);
        }
      }
      u3R->cap_p -= mov;
      top = u3to(u3_noun, u3R->cap_p + off);
    }
    else {
      u3h_put(har_p, a, buf_u.bit_w);
      if ( c3n == cel_o ) {
        _jam_buf_chop(&buf_u, 1, 0);
        _jam_buf_atom(&buf_u, a);
        u3R->cap_p -= mov;
        top = u3to(u3_noun, u3R->cap_p + off);
      }
      else {
        _jam_buf_chop(&buf_u, 2, 1);
        *top         = u3t(a);

        u3R->cap_p  += mov;
        top          = u3to(u3_noun, u3R->cap_p + off);
        *top         = u3h(a);
      }
    }
  }

  len_w = buf_u.bit_w >> 5;
  if ( (len_w << 5) != buf_u.bit_w ) {
    ++len_w;
  }
  sal_w = u3a_slab(len_w);
  memcpy(sal_w, buf_u.wor_w, len_w*sizeof(c3_w));
  u3a_free(buf_u.wor_w);
  u3h_free(har_p);
  return u3a_moot(sal_w);
}

/* functions
*/
  u3_noun
  u3qe_jam(u3_atom a)
  {
    return _jam_buf_top(a);
  }
  u3_noun
  u3we_jam(u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3r_at(u3x_sam, cor))) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qe_jam(a);
    }
  }
  u3_atom
  u3ke_jam(u3_noun a)
  {
    u3_atom b = u3qe_jam(a);

    u3z(a);
    return b;
  }

