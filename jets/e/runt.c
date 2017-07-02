/* j/e/runt.c
**
*/
#include "all.h"

/* functions
*/
  u3_noun
  u3qe_runt(u3_noun a,
            u3_noun b,
            u3_noun c)
  {
    if ( 0 == a ) {
      return u3k(c);
    }
    else {
      u3_noun dec = u3qa_dec(a),
              pro = u3nc(u3k(b), u3qe_runt(dec, b, c));
      u3z(dec);
      return pro;
    }
  }

  u3_noun
  u3we_runt(u3_noun cor)
  {
    u3_noun a, b, c;

    if ( (c3n == u3r_mean(cor, u3x_sam_4, &a,
                               u3x_sam_5, &b,
                               u3x_sam_3, &c, 0)) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qe_runt(a, b, c);
    }
  }

  u3_noun
  u3ke_runt(u3_noun a,
            u3_noun b,
            u3_noun c)
  {
    u3_noun pro = u3qe_runt(a, b, c);
    u3z(a); u3z(b); u3z(c);
    return pro;
  }

