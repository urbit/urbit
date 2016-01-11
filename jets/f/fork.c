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
    else return u3nq
      (c3__frog, u3k(hoz), u3k(bur), u3_nul);
  }

  u3_noun
  u3qf_frog(u3_noun yed)
  {
    u3_noun lez = u3_nul;

    while ( u3_nul != yed ) {
      u3_noun i_yed = u3h(yed);

      if ( c3__void != i_yed ) {
        lez = u3kdi_put(lez, u3k(u3h(yed)));
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
      return u3nc(c3__frog, u3kdi_tap(lez, u3_nul));
    }
  }

  u3_noun
  u3qf_grof(u3_noun bez)
  {
    return u3nc(c3__frog, u3qdi_tap(bez, u3_nul));
  }
  u3_noun
  u3kf_grof(u3_noun bez)
  {
    u3_noun ret = u3qf_grof(bez);

    u3z(bez);
    return ret;
  }

  u3_noun
  u3wf_frog(u3_noun cor)
  {
    u3_noun yed;

    if ( c3n == u3r_mean(cor, u3x_sam, &yed, 0) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_frog(yed);
    }
  }

  u3_noun
  u3kf_frog(u3_noun yed)
  {
    u3_noun ret = u3qf_frog(yed);

    u3z(yed);
    return ret;
  }
