/* j/3/dor.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qc_dor(u3_atom a,
           u3_atom b)
  {
    if ( c3y == u3r_sing(a, b) ) {
      return c3y;
    }
    else {
      if ( c3y == u3ud(a) ) {
        if ( c3y == u3ud(b) ) {
          return u3qa_lth(a, b);
        }
        else {
          return c3y;
        }
      }
      else {
        if ( c3y == u3ud(b) ) {
          return c3n;
        }
        else {
          if ( c3y == u3r_sing(u3h(a), u3h(b)) ) {
            return u3qc_dor(u3t(a), u3t(b));
          }
          else return u3qc_dor(u3h(a), u3h(b));
        }
      }
    }
  }
  u3_noun
  u3wc_dor(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qc_dor(a, b);
    }
  }

