/* j/5/shux.c
**
*/
#include "all.h"

static u3_atom
_shux_shax(void* dat_u, u3_atom a)
{
  u3_atom pro = u3qe_shax(a);
  u3z(a);
  return pro;
}

static u3_atom
_shux_mix(void* dat_u, u3_atom a, u3_atom b)
{
  u3_atom tac = u3qc_cat(8, a, b),
          pro = u3qe_shay(32, tac);
  u3z(a); u3z(b); u3z(tac);
  return pro;
}

/* functions
*/
  u3_noun
  u3qe_shux(u3_atom a)
  {
    return u3kc_mux_f(u3k(a), _shux_shax, NULL, _shux_mix, NULL);
  }
  u3_noun
  u3we_shux(u3_noun cor)
  {
    u3_noun sam;

    if ( u3_none == (sam = u3r_at(u3x_sam, cor)) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qe_shux(sam);
    }
  }
