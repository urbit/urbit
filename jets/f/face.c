/* j/6/face.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qf_face(
                    u3_noun cog,
                    u3_noun tip)
  {
    if ( c3__void == tip ) {
      return c3__void;
    }
    else return u3nt
      (c3__face, u3k(cog), u3k(tip));
  }
  u3_noun
  u3wf_face(
                   u3_noun cor)
  {
    u3_noun cog, tip;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &cog, u3x_sam_3, &tip, 0) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_face(cog, tip);
    }
  }
