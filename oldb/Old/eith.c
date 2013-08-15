/* mill/eith.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_eith(): fork pair.
*/
u4_type
_mill_eith(u4_milr m,
           u4_type lef,
           u4_type gap)
{
  if ( u4_n_eq(lef, gap) ) {
    return lef;
  }
  else if ( u4_n_eq(u4_atom_blur, lef) )  {
    return lef;
  }
  else if ( u4_n_eq(u4_atom_blot, lef) ) {
    return gap;
  }
  else if ( u4_n_eq(u4_atom_blur, gap) ) {
    return gap;
  }
  else if ( u4_n_eq(u4_atom_blot, gap) ) {
    return lef;
  }
  else {
    return u4_k_trel(m->lane, u4_atom_fork, lef, gap);
  }
}
