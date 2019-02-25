/* j/3/rap.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qc_rap(u3_atom a,
           u3_noun b)
  {
    if ( !_(u3a_is_cat(a)) || (a >= 32) ) {
      return u3m_bail(c3__exit);
    }
    else {
      c3_g  a_g = a;
      c3_w  tot_w = 0;
      c3_w* sal_w;

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
          tot_w += len_w;
          cab = u3t(cab);
        }
        if ( 0 == tot_w ) {
          return 0;
        }
        if ( 0 == (sal_w = u3a_slaq(a_g, tot_w)) ) {
          return u3m_bail(c3__fail);
        }
      }

      /* Chop the list atoms in.
      */
      {
        u3_noun cab = b;
        c3_w    pos_w = 0;

        while ( 0 != cab ) {
          u3_noun h_cab = u3h(cab);
          c3_w    len_w = u3r_met(a_g, h_cab);

          u3r_chop(a_g, 0, len_w, pos_w, sal_w, h_cab);
          pos_w += len_w;
          cab = u3t(cab);
        }
      }
      // return u3a_moot(sal_w);
      return u3a_malt(sal_w);
    }
  }
  u3_noun
  u3wc_rap(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) )
    {
      return u3m_bail(c3__exit);
    } else {
      u3_noun pro;

      pro = u3qc_rap(a, b);
      return pro;
    }
  }

