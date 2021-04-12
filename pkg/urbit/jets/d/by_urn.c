/* j/4/by_urn.c
**
*/
#include "all.h"

static u3_noun
_by_urn(u3_noun a, u3j_site* sit_u)
{
  if ( u3_nul == a ) {
    return u3_nul;
  }
  else {
    u3_noun n_a, l_a, r_a;
    u3x_trel(a, &n_a, &l_a, &r_a);

    return u3nt(u3nc(u3k(u3h(n_a)), u3j_gate_slam(sit_u, u3k(n_a))),
                _by_urn(l_a, sit_u),
                _by_urn(r_a, sit_u));
  }
}

u3_noun
u3qdb_urn(u3_noun a, u3_noun b)
{
  u3_noun    pro;
  u3j_site sit_u;

  u3j_gate_prep(&sit_u, u3k(b));
  pro = _by_urn(a, &sit_u);
  u3j_gate_lose(&sit_u);

  return pro;
}

u3_noun
u3wdb_urn(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);
  return u3qdb_urn(a, b);
}
