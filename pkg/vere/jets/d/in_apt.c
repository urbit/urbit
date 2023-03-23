/* j/4/in_apt.c
**
*/
#include "all.h"

static c3_o
_in_apt(u3_noun a, u3_weak l, u3_weak r)
{
  if ( u3_nul == a ) {
    return c3y;
  }
  else {
    u3_noun n_a, l_a, r_a;
    u3x_trel(a, &n_a, &l_a, &r_a);

    if ( (u3_none != l) && (c3n == u3qc_gor(n_a, l)) ) {
      return c3n;
    }

    if ( (u3_none != r) && (c3n == u3qc_gor(r, n_a)) ) {
      return c3n;
    }

    if ( u3_nul != l_a ) {
      if ( c3n == u3qc_mor(n_a, u3h(l_a)) ) {
        return c3n;
      }

      if ( c3n == _in_apt(l_a, n_a, r) ) {
        return c3n;
      }
    }

    if ( u3_nul != r_a ) {
      if ( c3n == u3qc_mor(n_a, u3h(r_a)) ) {
        return c3n;
      }

      return _in_apt(r_a, l, n_a);
    }

    return c3y;
  }
}

u3_noun
u3qdi_apt(u3_noun a)
{
  return _in_apt(a, u3_none, u3_none);
}

u3_noun
u3wdi_apt(u3_noun cor)
{
  return u3qdi_apt(u3x_at(u3x_con_sam, cor));
}
