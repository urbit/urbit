/* mill/peek.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _peek_main(): peek, with gil.
*/
u4_mold
_peek_main(u4_milr m,
           u4_axis axe,
           u4_bag  gil,
           u4_rail bar,
           u4_mold tip)
{
  u4_lane lane = m->lane;
  u4_axis sud  = u4_op_tip(axe);
  u4_axis hec  = u4_op_tap(lane, axe);
  u4_noun p_tip, q_tip;

  if ( u4_n_eq(u4_noun_1, axe) ) {
    return tip;
  }
  else {
    // %atom
    // 
    if ( u4_n_eq(u4_atom_atom, tip) ) {
      return u4_atom_blot;
    }
    
    // %blot
    //
    else if ( u4_n_eq(u4_atom_blot, tip) ) {
      return u4_atom_blot;
    }

    // %blur
    //
    else if ( u4_n_eq(u4_atom_blur, tip) ) {
      return u4_atom_blur;
    }

    // [%cell p=mold q=mold]
    //
    else if ( u4_b_pq(tip, u4_atom_cell, &p_tip, &q_tip) ) {
      u4_mold gan;

      if ( u4_n_eq(u4_noun_2, sud) ) {
        gan = p_tip;
      }
      else gan = q_tip;

      return _peek_main(m, hec, u4_noun_0, _mill_slip(m, sud, bar), gan);
    }

    // [%mono p=mold q={bush term mold}]
    // [%poly p=mold q={bush term mold}]
    //
    else if ( u4_b_pq(tip, u4_atom_mono, &p_tip, &q_tip) ||
              u4_b_pq(tip, u4_atom_poly, &p_tip, &q_tip) ) {
      if ( u4_n_eq(u4_noun_2, sud) ) {
        return _peek_main(m, hec, u4_noun_0, _mill_slip(m, sud, bar), p_tip); 
      }
      else return u4_atom_blur;
    }

    // [%cube p=noun]
    // 
    else if ( u4_b_p(tip, u4_atom_cube, &p_tip) ) {
      if ( u4_n_atom(p_tip) ) {
        return u4_atom_blot;
      }
      else return _peek_main(m, axe, gil, bar, _mill_reap(m, tip));
    }

    // [%face p=mark q=mold]
    //
    else if ( u4_b_pq(tip, u4_atom_face, &p_tip, &q_tip) ) {
      return _peek_main(m, axe, gil, bar, q_tip);
    }

    // [%fork p=mold q=mold]
    //
    else if ( u4_b_pq(tip, u4_atom_fork, &p_tip, &q_tip) ) {
      if ( _mill_cull(m, bar, p_tip) ) {
        return _peek_main(m, axe, gil, bar, q_tip);
      } 
      else if ( _mill_cull(m, bar, q_tip) ) {
        return _peek_main(m, axe, gil, bar, p_tip);
      }
      else return _mill_eith
        (m, _peek_main(m, axe, gil, bar, p_tip),
            _peek_main(m, axe, gil, bar, q_tip));
    }

    // [%fuse p=mold q=mold]
    //
    else if ( u4_b_pq(tip, u4_atom_fuse, &p_tip, &q_tip) ) {
      return _mill_both
        (m, _peek_main(m, axe, gil, bar, p_tip),
            _peek_main(m, axe, gil, u4_k_cell(lane, p_tip, bar), q_tip));
    }

    // [%hold p=mold q=gene]
    //
    else if ( u4_b_pq(tip, u4_atom_hold, &p_tip, &q_tip) ) {
      if ( u4_bag_in(tip, gil) ) {
        return u4_atom_blot;
      }
      else return _peek_main(m, axe, gil, bar, _mill_repo(m, p_tip, q_tip));
    }

    else return u4_trip;
  }
}

/* _mill_peek(): cut a railed mold.
*/
u4_mold
_mill_peek(u4_milr m,
           u4_axis axe,
           u4_rail bar,
           u4_mold tip)
{
  return _peek_main(m, axe, u4_noun_0, bar, tip);
}
