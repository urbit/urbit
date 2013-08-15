/* mill/pack.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_pack: recompose.
*/
u4_type
_mill_pack(u4_milr m,
           u4_log  dun,
           u4_type zog)
{
  if ( u4_n_zero(dun) ) {
    return zog;
  }
  else {
    u4_type i_dun = u4_ch(dun);
    u4_log  t_dun = u4_ct(dun);
    u4_type paf   = _mill_pack(m, t_dun, zog);

    if ( _mill_cong(m, i_dun, paf) ) {
      return paf;
    }
    else return u4_k_trel(m->lane, u4_atom_fuse, i_dun, paf);
  }
}
