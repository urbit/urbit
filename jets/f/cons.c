/* j/6/cons.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qf_cons(u3_noun vur,
            u3_noun sed)
  {
    u3_noun p_vur, p_sed;

    if ( c3y == u3r_p(vur, 1, &p_vur) &&
         c3y == u3r_p(sed, 1, &p_sed) ) {
      return u3nt(1,
                  u3k(p_vur),
                  u3k(p_sed));
    }
    else if ( c3y == u3r_p(vur, 0, &p_vur) &&
              c3y == u3r_p(sed, 0, &p_sed) &&
              !(c3y == u3r_sing(1, p_vur)) &&
              !(c3y == u3r_sing(p_vur, p_sed)) &&
              (0 == u3r_nord(p_vur, p_sed)) )
    {
      u3_atom fub = u3qa_div(p_vur, 2);
      u3_atom nof = u3qa_div(p_sed, 2);

      if ( c3y == u3r_sing(fub, nof) ) {
        u3z(nof);

        return u3nc(0, fub);
      }
      else {
        u3z(fub);
        u3z(nof);
      }
    }
    return u3nc(u3k(vur), u3k(sed));
  }
  u3_noun
  u3wf_cons(u3_noun cor)
  {
    u3_noun vur, sed;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &vur, u3x_sam_3, &sed, 0) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_cons(vur, sed);
    }
  }
