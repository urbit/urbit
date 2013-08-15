/* mill/flap.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_flap(): grow a bone.
*/
u4_type
_mill_flap(u4_milr m,
           u4_axis fep,
           u4_type mac)
{
  u4_noun p_mac, q_mac;

  if ( u4_n_eq(u4_noun_1, fep) ) {
    return mac;
  }
  else if ( u4_n_eq(u4_atom_blot, mac) ) {
    return mac;
  }
  else if ( u4_b_pq(mac, u4_atom_bone, &p_mac, &q_mac) ) {
    return u4_k_trel
      (m->lane, u4_atom_bone, u4_op_peg(m->lane, fep, p_mac), q_mac);
  }
  else return u4_k_trel(m->lane, u4_atom_bone, fep, mac);
}
