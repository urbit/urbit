/* j/4/in_rep.c
**
*/
#include "all.h"

/* internal functions
 */
static
void
_in_rep(u3_noun a, u3j_site* sit_u, u3_noun* out)
{
  if (u3_nul == a) {
    return;
  } else {
    u3_noun n_a, l_a, r_a;
    u3x_trel(a, &n_a, &l_a, &r_a);

    // consumes out refcount
    *out = u3j_gate_slam(sit_u, u3nc(u3k(n_a), *out));

    _in_rep(l_a, sit_u, out);
    _in_rep(r_a, sit_u, out);
  }
}

u3_noun
u3wdi_rep(u3_noun cor)
{
  u3_noun a, b;
  u3j_site sit_u;

  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);

  u3j_gate_prep(&sit_u, u3k(b));

  u3_noun out = u3k(u3x_at(u3x_sam_3, b));
  _in_rep(a, &sit_u, &out);

  u3j_gate_lose(&sit_u);
  return out;
}
