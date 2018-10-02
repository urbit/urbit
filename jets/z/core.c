/* j/6/core.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qz_core(u3_noun pac,
            u3_noun con)
  {
    if ( (c3__void == pac) ) {
      return c3__void;
    } else {
      {
        u3_noun p_con, q_con, r_con, hr_con, tr_con;

        u3r_trel(con, &p_con, &q_con, &r_con);
        u3r_cell(r_con, &hr_con, &tr_con);
        if ( (c3y == u3du(hr_con)) &&
             (u3_nul == u3h(hr_con)) &&
             (u3_nul == u3t(hr_con)) )
        {
          fprintf(stderr, "old core\r\n");
          abort();
        }
      }
      return u3nt(c3__core, u3k(pac), u3k(con));
    }
  }
  u3_noun
  u3wz_core(u3_noun cor)
  {
    u3_noun pac, con;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &pac, u3x_sam_3, &con, 0) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qz_core(pac, con);
    }
  }
