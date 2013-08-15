/* mill/slip.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_slip(): hack bar.
*/
u4_type
_mill_slip(u4_milr m,
           u4_axis feg,
           u4_log  cot)  /* (type) */
{
  if ( u4_n_zero(cot) ) {
    return u4_noun_0;
  }
  else {
    u4_type caf = _mill_hack(m, feg, u4_ch(cot));

    if ( u4_n_eq(u4_atom_blur, caf) ) {
      return _mill_slip(m, feg, u4_ct(cot));
    } else {
      return u4_k_cell(m->lane, caf, _mill_slip(m, feg, u4_ct(cot)));
    }
  }
}
