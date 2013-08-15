/* mill/snap.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _snap_cell(): adjust [ger] for assignment into [%cell p_lom q_lom].
*/
static u4_mold
_snap_cell(u4_milr m,
           u4_mold p_lom,
           u4_mold q_lom,
           u4_mold ger)
{
  u4_lane lane = m->lane;
  u4_noun p_ger, q_ger;

  // [%cell p=mold q=mold]
  //
  if ( u4_b_pq(ger, u4_atom_cell, &p_ger, &q_ger) ) {
    return u4_k_trel
      (lane, u4_atom_cell, 
             _mill_snap(m, p_lom, p_ger),
             _mill_snap(m, q_lom, q_ger));
  }
  else if ( u4_b_pq(ger, u4_atom_fork, &p_ger, &q_ger) ) {
    // Not at all clear that this is necessary.  But it can't hurt.
    //
    return u4_k_trel
      (lane, u4_atom_fork,
             _snap_cell(m, p_lom, q_lom, p_ger),
             _snap_cell(m, p_lom, q_lom, q_ger));
  }
  // [%fuse p=mold q=mold]
  //
  else if ( u4_b_pq(ger, u4_atom_fuse, &p_ger, &q_ger) ) {
    // Not at all clear that this is necessary.  But it can't hurt.
    //
    return u4_k_trel
      (lane, u4_atom_fuse,
             _snap_cell(m, p_lom, q_lom, p_ger),
             _snap_cell(m, p_lom, q_lom, q_ger));
  }
  else return ger;
}

/* _snap_face(): adjust [ger] for assignment into [%face p_lom q_lom].
*/
static u4_mold
_snap_face(u4_milr m,
           u4_mark p_lom,
           u4_mold q_lom,
           u4_mold ger)
{
  u4_lane lane = m->lane;
  u4_noun p_ger, q_ger;

  // [%face p=mark q=mold]
  //
  if ( u4_b_pq(ger, u4_atom_face, &p_ger, &q_ger) ) {
    if ( u4_n_eq(p_lom, p_ger) ) {
      return u4_k_trel(lane, u4_atom_face, p_lom, _mill_snap(m, q_lom, q_ger));
    }
    else {
      return u4_k_trel(lane, u4_atom_face, p_lom, _mill_snap(m, q_lom, ger));
    }
  }

  // [%fork p=mold q=mold]
  //
  else if ( u4_b_pq(ger, u4_atom_fork, &p_ger, &q_ger) ) {
    // Not at all clear that this is necessary.  But it can't hurt.
    //
    return u4_k_trel
      (lane, u4_atom_fork,
             _snap_cell(m, p_lom, q_lom, p_ger),
             _snap_cell(m, p_lom, q_lom, q_ger));
  }

  // [%fuse p=mold q=mold]
  //
  else if ( u4_b_pq(ger, u4_atom_fuse, &p_ger, &q_ger) ) {
    // Not at all clear that this is necessary.  But it can't hurt.
    //
    return u4_k_trel
      (lane, u4_atom_fuse,
             _snap_face(m, p_lom, q_lom, p_ger),
             _snap_face(m, p_lom, q_lom, q_ger));
  }

  else {
    return u4_k_trel(lane, u4_atom_face, p_lom, _mill_snap(m, q_lom, ger));
  }
}

/* _mill_snap(): adjust [ger] for assignment into [lom].
*/
u4_mold
_mill_snap(u4_milr m,
           u4_mold lom,
           u4_mold ger)
{
  u4_noun p_lom, q_lom;

  // [%cell p=mold q=mold]
  //
  if ( u4_b_pq(lom, u4_atom_cell, &p_lom, &q_lom) ) {
    return _snap_cell(m, p_lom, q_lom, ger);
  }

  // [%face p=mark q=mold]
  //
  else if ( u4_b_pq(lom, u4_atom_face, &p_lom, &q_lom) ) {
    return _snap_face(m, p_lom, q_lom, ger);
  }

  // [%fuse p=mold q=mold]
  //
  else if ( u4_b_pq(lom, u4_atom_fuse, &p_lom, &q_lom) ) {
    return _mill_snap(m, q_lom, ger);
  }

  // [%fork p=mold q=mold]
  //
  else if ( u4_b_pq(lom, u4_atom_fork, &p_lom, &q_lom) ) {
    u4_t yut = _mill_orth(m, p_lom, ger);
    u4_t riq = _mill_orth(m, q_lom, ger);

    if ( yut ) {
      if ( riq ) {
        // User is doomed, anyway.
        //
        return ger;
      }
      else return _mill_snap(m, q_lom, ger);
    }
    else if ( riq ) {
      return _mill_snap(m, p_lom, ger);
    }
    else return ger;
  }

  // [%hold p=mold q=mold]
  //
  else if ( u4_b_pq(lom, u4_atom_hold, &p_lom, &q_lom) ) {
    return _mill_snap(m, _mill_repo(m, p_lom, q_lom), ger);
  }

  else return ger;
}
