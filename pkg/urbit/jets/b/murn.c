/* j/2/murn.c
**
*/
#include "all.h"

u3_noun
u3qb_murn(u3_noun a, u3_noun b)
{
  u3_noun  pro;
  u3_noun* lit = &pro;

  if ( u3_nul != a ) {
    u3_noun*   hed;
    u3_noun*   tel;
    u3_noun res, i, t = a;
    u3j_site sit_u;

    u3j_gate_prep(&sit_u, u3k(b));

    do {
      u3x_cell(t, &i, &t);

      res = u3j_gate_slam(&sit_u, u3k(i));

      if ( u3_nul != res ) {
        *lit = u3i_defcons(&hed, &tel);
        *hed = u3k(u3t(res));
        lit  = tel;
        u3z(res);
      }
    }
    while ( u3_nul != t );

    u3j_gate_lose(&sit_u);
  }

  *lit = u3_nul;

  return pro;
}

u3_noun
u3wb_murn(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0);
  return u3qb_murn(a, b);
}
