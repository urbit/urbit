/* j/3/trip.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun
  u2_cqe_trip(u2_atom a)
  {
    if ( u2_no == u2ud(a) ) {
      return u2_cm_bail(c3__exit);
    }
    return u2_cqc_rip(3, a);
  }
  u2_noun
  u2_cwe_trip(u2_noun cor)
  {
    u2_noun a;

    if ( (u2_none == (a = u2_cr_at(u2_cv_sam, cor))) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return u2_cqe_trip(a);
    }
  }
