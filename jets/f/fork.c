/* j/6/fork.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qf_fork(u3_noun hoz,
            u3_noun bur)
  {
    if ( c3y == u3r_sing(hoz, bur) ) {
      return u3k(hoz);
    }
    else if ( c3__void == bur ) {
      return u3k(hoz);
    }
    else if ( c3__void == hoz ) {
      return u3k(bur);
    }
    else return u3nt
      (c3__fork, u3k(hoz), u3k(bur));
  }
  u3_noun
  u3wf_fork(u3_noun cor)
  {
    u3_noun hoz, bur;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &hoz, u3x_sam_3, &bur, 0) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_fork(hoz, bur);
    }
  }
