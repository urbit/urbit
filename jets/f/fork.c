/* j/6/fork.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qf_forq(u3_noun hoz,
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
    else return u3kf_fork(u3nt(u3k(hoz), u3k(bur), u3_nul));
  }

  u3_noun
  u3qf_fork(u3_noun yed)
  {
    u3_noun lez = u3_nul;

    while ( u3_nul != yed ) {
      u3_noun i_yed = u3h(yed);

      if ( c3__void != i_yed ) {
        if ( (c3y == u3du(i_yed)) && (c3__fork == u3h(i_yed)) ) {
          lez = u3kdi_uni(lez, u3k(u3t(i_yed)));
        } 
        else {
          lez = u3kdi_put(lez, u3k(i_yed));
        }
      }
      
      yed = u3t(yed);
    }
    
    if ( u3_nul == lez ) {
      return c3__void;
    }
    else if ( (u3_nul == u3h(u3t(lez))) && (u3_nul == u3t(u3t(lez))) ) {
      u3_noun ret = u3k(u3h(lez));

      u3z(lez);
      return ret;
    }
    else {
      return u3nc(c3__fork, lez);
    }
  }

  u3_noun
  u3wf_fork(u3_noun cor)
  {
    u3_noun yed;

    if ( c3n == u3r_mean(cor, u3x_sam, &yed, 0) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_fork(yed);
    }
  }

  u3_noun
  u3kf_fork(u3_noun yed)
  {
    u3_noun ret = u3qf_fork(yed);

    u3z(yed);
    return ret;
  }
