/* j/6/flor.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qf_flor(u3_noun bos,
            u3_noun nif)
  {
    if ( c3y == u3r_sing(1, u3h(bos)) ) {
      if ( (u3_nul == u3t(bos)) ) {
        return u3k(bos);
      }
      else return u3k(nif);
    }
    else {
      if ( c3y == u3r_sing(1, u3h(nif)) ) {
        if ( (u3_nul == u3t(nif)) ) {
          return u3k(nif);
        }
        else return u3k(bos);
      }
      else {
        return u3nq(6,
                    u3k(bos),
                    u3nc(1, c3y),
                    u3k(nif));
      }
    }
  }
  u3_noun
  u3wf_flor(u3_noun cor)
  {
    u3_noun bos, nif;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &bos, u3x_sam_3, &nif, 0) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_flor(bos, nif);
    }
  }
