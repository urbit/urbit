/* j/2/zing.c
**
*/
#include "all.h"

u3_noun
u3qb_zing(u3_noun a)
{
  u3_noun  pro;
  u3_noun* lit = &pro;

  if ( u3_nul == a ) {
    *lit = u3_nul;
  }
  else {
    u3_noun i, t = a;
    u3x_cell(t, &i, &t);

    while ( u3_nul != t ) {
      u3_noun* hed;
      u3_noun* tel;
      u3_noun  i_i, t_i = i;

      while ( u3_nul != t_i ) {
        u3x_cell(t_i, &i_i, &t_i);

        *lit = u3i_defcons(&hed, &tel);
        *hed = u3k(i_i);
        lit  = tel;
      }

      u3x_cell(t, &i, &t);
    }

    *lit = u3k(i);
  }

  return pro;
}

u3_noun
u3wb_zing(u3_noun cor)
{
  return u3qb_zing(u3x_at(u3x_sam, cor));
}
