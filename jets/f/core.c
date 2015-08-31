/* j/6/core.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qf_core(u3_noun pac,
            u3_noun con)
  {
    if ( (c3__void == pac) ) {
      return c3__void;
    } else {
      return u3nt(c3__core, u3k(pac), u3k(con));
    }
  }
  u3_noun
  u3wf_core(u3_noun cor)
  {
    u3_noun pac, con;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &pac, u3x_sam_3, &con, 0) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_core(pac, con);
    }
  }
