/* j/2/flop.c
**
*/
#include "all.h"

u3_noun
u3qb_flop(u3_noun a)
{
  u3_noun i, t = a, b = u3_nul;

  while ( u3_nul != t ) {
    u3x_cell(t, &i, &t);
    b = u3nc(u3k(i), b);
  }

  return b;
}

u3_noun
u3wb_flop(u3_noun cor)
{
  return u3qb_flop(u3x_at(u3x_sam, cor));
}

u3_noun
u3kb_flop(u3_noun a)
{
  u3_noun b = u3qb_flop(a);
  u3z(a);
  return b;
}
