/* j/4/by_all.c
**
*/
#include "all.h"

/* internal functions
 */
static u3_noun
_by_all(u3_noun a, u3j_site* sit_u)
{
  if (u3_nul == a) {
    return c3y;
  } else {
    u3_noun n_a, l_a, r_a;
    u3_noun q_n_a;
    u3x_trel(a, &n_a, &l_a, &r_a);
    u3x_cell(n_a, 0, &q_n_a);

    if (c3n == u3j_gate_slam(sit_u, u3k(q_n_a))) {
      return c3n;
    }

    if (c3n == _by_all(l_a, sit_u)) {
      return c3n;
    }

    if (c3n == _by_all(r_a, sit_u)) {
      return c3n;
    }

    return c3y;
  }
}

u3_noun
u3wdb_all(u3_noun cor)
{
  u3_noun a, b;
  u3j_site sit_u;

  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);

  u3j_gate_prep(&sit_u, u3k(b));
  u3_noun pro = _by_all(a, &sit_u);
  u3j_gate_lose(&sit_u);
  return pro;
}
