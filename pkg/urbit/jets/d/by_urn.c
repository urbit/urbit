/* j/4/by_urn.c
**
*/
#include "all.h"

/* internal functions
 */
static
u3_noun
_by_urn(u3_noun a, u3j_site* sit_u)
{
  if (u3_nul == a) {
    return 0;
  } else {
    u3_noun n_a, l_a, r_a;
    u3_noun p_n_a, q_n_a;
    u3x_trel(a, &n_a, &l_a, &r_a);
    u3x_cell(n_a, &p_n_a, &q_n_a);

    return u3nt(u3nc(u3k(p_n_a), u3j_gate_slam(sit_u, u3k(n_a))),
                _by_urn(l_a, sit_u),
                _by_urn(r_a, sit_u));
  }
}

u3_noun
u3wdb_urn(u3_noun cor)
{
  u3_noun a, b;
  u3j_site sit_u;

  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);

  u3j_gate_prep(&sit_u, u3k(b));
  u3_noun pro = _by_urn(a, &sit_u);
  u3j_gate_lose(&sit_u);
  return pro;
}
