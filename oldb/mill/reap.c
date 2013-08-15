/* mill/reap.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_reap(): expand mold.
*/
u4_mold
_mill_reap(u4_milr m,
           u4_mold typ)
{
  u4_lane lane = m->lane;
  u4_noun p_typ, q_typ;

  if ( u4_n_eq(u4_atom_atom, typ) ||
       u4_n_eq(u4_atom_blot, typ) ||
       u4_n_eq(u4_atom_blur, typ) )
  {
    return typ;
  }

  else if ( u4_b_pq(typ, u4_atom_cell, &p_typ, &q_typ) ) {
    return typ;
  }

  else if ( u4_b_pq(typ, u4_atom_mono, &p_typ, &q_typ) ||
            u4_b_pq(typ, u4_atom_poly, &p_typ, &q_typ) ) {
    return u4_k_trel(lane, u4_atom_cell, p_typ, u4_atom_blur);
  }

  else if ( u4_b_p(typ, u4_atom_cube, &p_typ) && !u4_n_atom(p_typ) ) {
    u4_mold foo = u4_k_cell(lane, u4_atom_cube, u4_ch(p_typ));
    u4_mold bar = u4_k_cell(lane, u4_atom_cube, u4_ct(p_typ));

    return u4_k_trel(lane, u4_atom_cell, foo, bar);
  }

  else if ( u4_b_pq(typ, u4_atom_face, &p_typ, &q_typ) ) {
    return q_typ;
  }

  else if ( u4_b_pq(typ, u4_atom_fork, &p_typ, &q_typ) ) {
    return typ;
  }

  else if ( u4_b_pq(typ, u4_atom_fuse, &p_typ, &q_typ) ) {
    return typ;
  }

  else if ( u4_b_pq(typ, u4_atom_hold, &p_typ, &q_typ) ) {
    return _mill_repo(m, p_typ, q_typ);
  }

  else {
    u4_err(lane, "strange reap", typ);
    return u4_trip;
  }
}
