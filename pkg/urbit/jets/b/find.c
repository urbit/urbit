/* j/2/find.c
**
*/
#include "all.h"

STATIC_ASSERT( (UINT32_MAX > u3a_cells),
               "list index precision" );

u3_noun
u3qb_find(u3_noun nedl, u3_noun hstk)
{
  if ( u3_nul != nedl ) {
    c3_w  i_w = 0;

    while ( u3_nul != hstk ) {
      u3_noun i_h, t_h = hstk;
      u3_noun i_n, t_n = nedl;
      u3x_cell(t_h, &i_h, &t_h);
      u3x_cell(t_n, &i_n, &t_n);

      while ( c3y == u3r_sing(i_n, i_h) ) {
        if ( u3_nul == t_n ) {
          return u3nc(u3_nul, u3i_word(i_w));
        }
        else if ( u3_nul == t_h ) {
          break;
        }
        else {
          u3x_cell(t_h, &i_h, &t_h);
          u3x_cell(t_n, &i_n, &t_n);
        }
      }

      hstk = u3t(hstk);
      i_w++;
    }
  }

  return u3_nul;
}

u3_noun
u3wb_find(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0);
  return u3qb_find(a, b);
}
