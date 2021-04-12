/* j/5/trip.c
**
*/
#include "all.h"

u3_noun
u3qe_trip(u3_atom a)
{
  return u3qc_rip(3, 1, a);
}

u3_noun
u3we_trip(u3_noun cor)
{
  u3_noun a = u3x_at(u3x_sam, cor);

  if ( c3n == u3ud(a) ) {
    return u3m_bail(c3__exit);
  }

  return u3qe_trip(a);
}

u3_atom
u3ke_trip(u3_noun a)
{
  u3_atom pro = u3qe_trip(a);
  u3z(a);
  return pro;
}
