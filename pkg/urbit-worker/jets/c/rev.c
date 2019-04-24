/* j/3/rev.c
**
*/
#include "all.h"

/* functions
*/

  u3_noun
  u3qc_rev(u3_atom boz,
           u3_atom len,
           u3_atom dat)
  {
    if ( !_(u3a_is_cat(boz)) || (boz >= 32) ||
         !_(u3a_is_cat(len)) ) {
      return u3m_bail(c3__fail);
    }

    dat = u3qc_end(boz, len, dat);
    c3_w met = u3r_met(boz, dat);
    return u3kc_lsh(boz, (len - met), u3kc_swp(boz, dat));
  }

  u3_noun
  u3wc_rev(u3_noun cor)
  {
    u3_noun boz, len, dat;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &boz,
                               u3x_sam_6, &len,
                               u3x_sam_7, &dat, 0)) ||
         (c3n == u3ud(boz)) ||
         (c3n == u3ud(len)) ||
         (c3n == u3ud(dat)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qc_rev(boz, len, dat);
    }
  }

  u3_noun
  u3kc_rev(u3_atom boz,
           u3_atom len,
           u3_atom dat)
  {
    u3_noun res = u3qc_rev(boz, len, dat);
    u3z(boz); u3z(len); u3z(dat);
    return res;
  }
