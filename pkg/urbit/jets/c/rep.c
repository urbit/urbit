/* j/3/rep.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qc_rep(u3_atom a,
           u3_noun b)
  {
    if ( !_(u3a_is_cat(a)) || (a >= 32) ) {
      return u3m_bail(c3__exit);
    }
    else {
      c3_g       a_g = a;
      c3_w     tot_w = 0;
      u3i_slab sab_u;

      /* Measure and validate the slab required.
      */
      {
        u3_noun cab = b;

        while ( 1 ) {
          u3_noun h_cab;
          c3_w    len_w;

          if ( 0 == cab ) {
            break;
          }
          else if ( c3n == u3du(cab) ) {
            return u3m_bail(c3__exit);
          }
          else if ( c3n == u3ud(h_cab = u3h(cab)) ) {
            return u3m_bail(c3__exit);
          }
          else if ( (tot_w + (len_w = u3r_met(a_g, h_cab))) < tot_w ) {
            return u3m_bail(c3__fail);
          }
          tot_w++;
          cab = u3t(cab);
        }

        if ( 0 == tot_w ) {
          return 0;
        }

        u3i_slab_init(&sab_u, a_g, tot_w);
      }

      /* Chop the list atoms in.
      */
      {
        u3_noun cab = b;
        c3_w  pos_w = 0;

        while ( 0 != cab ) {
          u3_noun h_cab = u3h(cab);

          u3r_chop(a_g, 0, 1, pos_w, sab_u.buf_w, h_cab);
          pos_w++;
          cab = u3t(cab);
        }
      }

      return u3i_slab_mint(&sab_u);
    }
  }
  u3_noun
  u3wc_rep(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) )
    {
      return u3m_bail(c3__exit);
    } else {
      u3_noun pro;

      pro = u3qc_rep(a, b);
      return pro;
    }
  }
  u3_noun
  u3kc_rep(u3_atom a,
           u3_noun b)
  {
    u3_noun res = u3qc_rep(a, b);
    u3z(a); u3z(b);
    return res;
  }
