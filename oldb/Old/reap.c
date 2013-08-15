/* mill/reap.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_reap(): expand type.
*/
u4_type
_mill_reap(u4_milr m,
           u4_type gaz)
{
  u4_lane lane = m->lane;
  u4_noun p_gaz, q_gaz;

  if ( u4_b_pq(gaz, u4_atom_cone, &p_gaz, &q_gaz) ) {
    return u4_k_trel(lane, u4_atom_bone, u4_noun_2, p_gaz);
  }
  else if ( u4_b_p(gaz, u4_atom_cube, &p_gaz) ) {
    if ( u4_n_atom(p_gaz) ) {
      return u4_trip;
    }
    else {
      u4_type foo = u4_k_cell(lane, u4_atom_cube, u4_ch(p_gaz));
      u4_type bar = u4_k_cell(lane, u4_atom_cube, u4_ct(p_gaz));

      return u4_k_trel
        (lane, u4_atom_fuse,
               u4_k_trel(lane, u4_atom_bone, u4_noun_2, foo),
               u4_k_trel(lane, u4_atom_bone, u4_noun_3, bar));
    }
  }
  else if ( u4_b_pq(gaz, u4_atom_pair, &p_gaz, &q_gaz) ) {
    return u4_k_trel
      (lane, u4_atom_fuse,
             u4_k_trel(lane, u4_atom_bone, u4_noun_2, p_gaz),
             u4_k_trel(lane, u4_atom_bone, u4_noun_3, q_gaz));
  }
  else if ( u4_b_pq(gaz, u4_atom_post, &p_gaz, &q_gaz) ) {
    return _mill_repo(m, p_gaz, q_gaz);
  }
  else if ( u4_b_pq(gaz, u4_atom_skin, &p_gaz, &q_gaz) ) {
    return q_gaz;
  }

  else {
    u4_err(lane, "reap", gaz);
    return u4_trip;
  }
}
