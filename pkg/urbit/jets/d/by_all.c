/* j/4/by_all.c
**
*/
#include "all.h"

static u3_noun
_by_all(u3_noun a, u3j_site* sit_u)
{
  if ( u3_nul == a ) {
    return c3y;
  }
  else {
    u3_noun n_a, l_a, r_a;
    u3x_trel(a, &n_a, &l_a, &r_a);

    switch ( u3j_gate_slam(sit_u, u3k(u3t(n_a))) ) {
      case c3y: break;
      case c3n: return c3n;
      default:  return u3m_bail(c3__exit);
    }

    if ( c3n == _by_all(l_a, sit_u) ) {
      return c3n;
    }

    return _by_all(r_a, sit_u);
  }
}

u3_noun
u3qdb_all(u3_noun a, u3_noun b)
{
  u3_noun    pro;
  u3j_site sit_u;

  u3j_gate_prep(&sit_u, u3k(b));
  pro = _by_all(a, &sit_u);
  u3j_gate_lose(&sit_u);

  return pro;
}

u3_noun
u3wdb_all(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);
  return u3qdb_all(a, b);
}
