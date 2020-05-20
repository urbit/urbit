/* j/4/by_run.c
**
*/
#include "all.h"

/* internal functions
 */
static u3_noun
_by_key(u3_noun a, u3_noun set)
{
  if (u3_nul == a) {
    return u3k(set);
  } else {
    u3_noun n_a, l_a, r_a;
    u3_noun p_n_a, q_n_a;
    u3x_trel(a, &n_a, &l_a, &r_a);
    u3x_cell(n_a, &p_n_a, &q_n_a);

    u3_noun with_set = u3qdi_put(set, p_n_a);
    u3_noun left_set = _by_key(l_a, with_set);
    u3z(with_set);
    u3_noun right_set = _by_key(r_a, left_set);
    u3z(left_set);

    return right_set;
  }
}

u3_noun
u3wdb_key(u3_noun cor)
{
  u3_noun a;
  u3x_mean(cor, u3x_con_sam, &a, 0);
  return _by_key(a, 0);
}
