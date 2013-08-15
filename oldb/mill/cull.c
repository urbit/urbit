/* mill/cull.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_cull(): prune for computation.
*/
u4_t
_mill_cull(u4_milr m,
           u4_rail bar,
           u4_mold tip)
{
  if ( _mill_null(m, tip) ) {
    return 1;
  }
  else {
    while ( !u4_n_zero(bar) ) {
      u4_mold i_bar = u4_ch(bar);

      if ( _mill_orth(m, i_bar, tip) ) {
        return 1;
      }
      else bar = u4_ct(bar);
    }
    return 0;
  }
}
