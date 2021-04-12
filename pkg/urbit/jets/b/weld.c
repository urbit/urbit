/* j/2/weld.c
**
*/
#include "all.h"

u3_noun
u3qb_weld(u3_noun a, u3_noun b)
{
  u3_noun  pro;
  u3_noun* lit = &pro;

  {
    u3_noun* hed;
    u3_noun* tel;
    u3_noun    i, t = a;

    while ( u3_nul != t ) {
      u3x_cell(t, &i, &t);

      *lit = u3i_defcons(&hed, &tel);
      *hed = u3k(i);
      lit  = tel;
    }
  }

  *lit = u3k(b);

  return pro;
}

u3_noun
u3wb_weld(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0);
  return u3qb_weld(a, b);
}

u3_noun
u3kb_weld(u3_noun a, u3_noun b)
{
  u3_noun c = u3qb_weld(a, b);
  u3z(a); u3z(b);
  return c;
}
