/* mill/pull.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_pull(): fuse with bar.
*/
u4_type
_mill_pull(u4_milr m,
           u4_log  rol,  /* (type) */
           u4_type cag)
{
  if ( u4_n_zero(rol) ) {
    return cag;
  } else {
    return _mill_both(m, u4_ch(rol), _mill_pull(m, u4_ct(rol), cag));
  }
}
