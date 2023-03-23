/* j/2/scag.c
**
*/
#include "all.h"

u3_noun
u3qb_scag(u3_atom a, u3_noun b)
{
  if ( u3_nul == b ) {
    return u3_nul;
  }
  else if ( !_(u3a_is_cat(a)) ) {
    return u3m_bail(c3__fail);
  }
  else {
    u3_noun  pro;
    u3_noun* lit = &pro;

    {
      c3_w   len_w = (c3_w)a;
      u3_noun* hed;
      u3_noun* tel;
      u3_noun i, t = b;

      while ( len_w-- && (u3_nul != t) ) {
        u3x_cell(t, &i, &t);

        *lit = u3i_defcons(&hed, &tel);
        *hed = u3k(i);
        lit  = tel;
      }
    }

    *lit = u3_nul;

    return pro;
  }
}

u3_noun
u3wb_scag(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0);

  if ( (c3n == u3ud(a)) && (u3_nul != b) ) {
    return u3m_bail(c3__exit);
  }

  return u3qb_scag(a, b);
}
