/* j/4/by_apt.c
**
*/
#include "all.h"

static c3_o
_by_apt(u3_noun a, u3_weak l, u3_weak r)
{
  if ( u3_nul == a ) {
    return c3y;
  }
  else {
    u3_noun p_n_a, l_a, r_a;
    {
      u3_noun n_a;
      u3x_trel(a, &n_a, &l_a, &r_a);
      p_n_a = u3h(n_a);
    }

    if ( u3_none != l ) {
      if (  (c3n == u3qc_gor(p_n_a, l))
         || (c3y == u3r_sing(p_n_a, l)) )
      {
        return c3n;
      }
    }

    if ( u3_none != r ) {
      if (  (c3n == u3qc_gor(r, p_n_a))
         || (c3y == u3r_sing(r, p_n_a)) )
      {
        return c3n;
      }
    }

    if ( u3_nul != l_a ) {
      u3_noun p_n_l_a = u3h(u3h(l_a));

      if (  (c3n == u3qc_mor(p_n_a, p_n_l_a))
         || (c3y == u3r_sing(p_n_a, p_n_l_a)) )
      {
        return c3n;
      }

      if ( c3n == _by_apt(l_a, p_n_a, r) ) {
        return c3n;
      }
    }

    if ( u3_nul != r_a ) {
      u3_noun p_n_r_a = u3h(u3h(r_a));

      if (  (c3n == u3qc_mor(p_n_a, p_n_r_a))
         || (c3y == u3r_sing(p_n_a, p_n_r_a)) )
      {
        return c3n;
      }

      return _by_apt(r_a, l, p_n_a);
    }

    return c3y;
  }
}

u3_noun
u3qdb_apt(u3_noun a)
{
  return _by_apt(a, u3_none, u3_none);
}

u3_noun
u3wdb_apt(u3_noun cor)
{
  return u3qdb_apt(u3x_at(u3x_con_sam, cor));
}
