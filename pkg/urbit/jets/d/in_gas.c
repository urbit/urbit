/* j/4/gas.c
**
*/
#include "all.h"

/* functions
*/
u3_noun
u3qdi_gas(u3_noun a,
          u3_noun b)
{
  if ( u3_nul == b ) {
    return u3k(a);
  }
  else {
    u3_noun i_b, t_b;
    u3x_cell(b, &i_b, &t_b);

    u3_noun c = u3qdi_put(a, i_b);
    u3_noun d = u3qdi_gas(c, t_b);
    u3z(c);
    return d;
  }
}

u3_noun
u3wdi_gas(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);
  return u3qdi_gas(a, b);
}

u3_noun
u3kdi_gas(u3_noun a,
          u3_noun b)
{
  u3_noun c = u3qdi_gas(a, b);
  u3z(a); u3z(b);
  return c;
}
