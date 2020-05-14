/* j/4/in_run.c
**
*/
#include "all.h"

/* internal functions
 */
static
void
_in_run(u3_noun a, u3j_site* sit_u, u3_noun* out)
{
  if (u3_nul == a) {
    return;
  } else {
    u3_noun n_a, l_a, r_a;
    u3x_trel(a, &n_a, &l_a, &r_a);

    u3_noun nu_n_a = u3j_gate_slam(sit_u, u3k(n_a));

    u3_noun nuout = u3qdi_put(*out, nu_n_a);
    u3z(*out);
    u3z(nu_n_a);
    *out = nuout;

    _in_run(l_a, sit_u, out);
    _in_run(r_a, sit_u, out);
  }
}

u3_noun
u3wdi_run(u3_noun cor)
{
  u3_noun a, b;
  u3j_site sit_u;

  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);

  u3j_gate_prep(&sit_u, u3k(b));
  u3_noun out = 0;
  _in_run(a, &sit_u, &out);
  u3j_gate_lose(&sit_u);
  return out;
}
