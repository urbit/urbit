/* j/4/in_has.c
**
*/
#include "all.h"

/* functions
*/
u3_noun
u3qdi_has(u3_noun a,
          u3_noun b)
{
  if ( u3_nul == a ) {
    return c3n;
  }
  else {
    u3_noun n_a, lr_a;
    u3x_cell(a, &n_a, &lr_a);

    if ( (c3y == u3r_sing(b, n_a)) ) {
      return c3y;
    }
    else {
      return ( c3y == u3qc_gor(b, n_a) ) ? u3qdi_has(u3h(lr_a), b)
                                         : u3qdi_has(u3t(lr_a), b);
    }
  }
}

u3_noun
u3wdi_has(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);
  return u3qdi_has(a, b);
}

u3_noun
u3kdi_has(u3_noun a,
          u3_noun b)
{
  u3_noun c = u3qdi_has(a, b);
  u3z(a); u3z(b);
  return c;
}
