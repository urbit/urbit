/* mill/cull.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_cull(): prune for computation.
*/
u4_t
_mill_cull(u4_milr m,
           
           u4_type lof,
           u4_log  bup)   /* (type) */
{
  if ( _mill_null(m, lof) ) {
    return 1;
  }
  else {
    while ( !u4_n_zero(bup) ) {
      u4_type i_bup = u4_ch(bup);

      if ( _mill_orth(m, i_bup, lof) ) {
        return 1;
      }
      else bup = u4_ct(bup);
    }
    return 0;
  }
}
