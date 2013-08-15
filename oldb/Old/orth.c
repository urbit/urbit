/* mill/orth.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_orth(): orthogonality.
*/
u4_t
_mill_orth(u4_milr m,
           u4_type ris,
           u4_type gel)
{
  return _mill_null(m, u4_k_trel(m->lane, u4_atom_fuse, ris, gel));
}
