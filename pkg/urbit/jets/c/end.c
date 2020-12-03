/* j/3/end.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qc_end(u3_atom a,
           u3_atom b,
           u3_atom c)
  {
    if ( !_(u3a_is_cat(a)) || (a >= 32) ) {
      return u3m_bail(c3__fail);
    }
    else if ( !_(u3a_is_cat(b)) ) {
      return u3k(c);
    }
    else {
      c3_g a_g   = a;
      c3_w b_w   = b;
      c3_w len_w = u3r_met(a_g, c);

      if ( 0 == b_w ) {
        return 0;
      }
      else if ( b_w >= len_w ) {
        return u3k(c);
      }
      else {
        u3i_slab sab_u;
        u3i_slab_init(&sab_u, a_g, b_w);

        u3r_chop(a_g, 0, b_w, 0, sab_u.buf_w, c);

        return u3i_slab_mint(&sab_u);
      }
    }
  }
  u3_noun
  u3wc_end(u3_noun cor)
  {
    u3_noun a, b, c;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a,
                                u3x_sam_6, &b,
                                u3x_sam_7, &c, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) ||
         (c3n == u3ud(c)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qc_end(a, b, c);
    }
  }

