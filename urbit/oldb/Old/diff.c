/* mill/diff.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_diff(): test difference.
*/
u4_form
_mill_diff(u4_milr m,
           u4_axis nar,
           u4_type dol,
           u4_type sef)
{
  u4_type zug = _mill_flap(m, nar, dol);

  if ( _mill_orth(m, sef, zug) ) {
    return u4_k_cell(m->lane, u4_noun_1, u4_noun_1);
  }
  if ( _mill_cong(m, sef, zug) ) {
    return u4_k_cell(m->lane, u4_noun_1, u4_noun_0);
  }

  return _mill_fish(m, nar, dol);
}
