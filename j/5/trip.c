/* j/5/trip.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqe_trip(u3_atom a)
  {
    if ( c3n == u3ud(a) ) {
      return u3m_bail(c3__exit);
    }
    return u3_cqc_rip(3, a);
  }
  u3_noun
  u3_cwe_trip(u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3r_at(u3v_sam, cor))) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3_cqe_trip(a);
    }
  }
  u3_atom
  u3_cke_trip(u3_noun a)
  {
    u3_atom b = u3_cqe_trip(a);

    u3z(a);
    return b;
  }
