/* j/4/by_run.c
**
*/
#include "all.h"

static u3_noun
_by_run(u3_noun a, u3j_site* sit_u)
{
  if ( u3_nul == a ) {
    return u3_nul;
  }
  else {
    u3_noun n_a, l_a, r_a;
    u3_noun p_n_a, q_n_a;
    u3x_trel(a, &n_a, &l_a, &r_a);
    u3x_cell(n_a, &p_n_a, &q_n_a);

    return u3nt(u3nc(u3k(p_n_a), u3j_gate_slam(sit_u, u3k(q_n_a))),
                _by_run(l_a, sit_u),
                _by_run(r_a, sit_u));
  }
}

u3_noun
u3qdb_run(u3_noun a, u3_noun b)
{
  u3_noun    pro;
  u3j_site sit_u;

  u3j_gate_prep(&sit_u, u3k(b));
  pro = _by_run(a, &sit_u);
  u3j_gate_lose(&sit_u);

  return pro;
}

u3_noun
u3wdb_run(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);
  return u3qdb_run(a, b);
}
