/* mill/fish.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

  /** Forward declarations.
  **/
    static u4_nock
    _fish_main(u4_milr, u4_bag, u4_log, u4_axis, u4_mold);


/* _fish_cell(): fish for pair.  Should not be needed, but...
*/
static u4_nock
_fish_cell(u4_milr m,
           u4_bag  gil,
           u4_rail bar,
           u4_axis axe,
           u4_mold p_tip,
           u4_mold q_tip)
{
  u4_lane lane = m->lane;
  u4_axis lec  = u4_op_add(lane, axe, axe);
  u4_axis mir  = u4_op_inc(lane, lec);

  return _mill_and
      (m, u4_k_trel(lane, u4_noun_4, u4_noun_0, axe),
          _mill_and
            (m, _fish_main(m, gil, _mill_slip(m, u4_noun_2, bar), lec, p_tip),
                _fish_main(m, gil, _mill_slip(m, u4_noun_3, bar), mir, q_tip)));
}

/* _fish_forq(): fish for forq.
*/
static u4_nock
_fish_forq(u4_milr m,
           u4_bag  gil,
           u4_rail bar,
           u4_axis axe,
           u4_log  p_tip)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(p_tip) ) {
    return u4_k_cell(lane, u4_noun_1, u4_noun_1);
  }
  else {
    u4_mold ip_tip = u4_ch(p_tip);
    u4_mold tp_tip = u4_ct(p_tip);

    if ( _mill_cull(m, bar, ip_tip) ) {
      return _fish_forq(m, gil, bar, axe, tp_tip);
    }
    else {
      return _mill_or
        (m, _fish_main(m, gil, bar, axe, ip_tip),
            _fish_forq(m, gil, bar, axe, tp_tip));
    }
  }
}

/* _fish_main(): fish with cull and bag.
*/
static u4_nock
_fish_main(u4_milr m,
           u4_bag  gil,
           u4_rail bar,
           u4_axis axe,
           u4_mold tip)
{
  u4_lane lane = m->lane;
  u4_noun p_tip, q_tip;

  if ( u4_n_eq(u4_atom_atom, tip) ) {
    return _mill_not(m, u4_k_trel(lane, u4_noun_4, u4_noun_0, axe));
  }
  else if ( u4_n_eq(u4_atom_blur, tip) ) {
    return u4_k_cell(lane, u4_noun_1, u4_noun_0);
  }
  else if ( u4_n_eq(u4_atom_blot, tip) ) {
    return u4_trip;
  }

  else if ( u4_b_pq(tip, u4_atom_cell, &p_tip, &q_tip) ) {
    return _fish_cell(m, gil, bar, axe, p_tip, q_tip);
  }

  else if ( u4_b_pq(tip, u4_atom_mono, &p_tip, &q_tip) ||
            u4_b_pq(tip, u4_atom_poly, &p_tip, &q_tip) ) {
    return u4_k_cell(lane, u4_noun_0, u4_noun_0);
  }

  else if ( u4_b_p(tip, u4_atom_cube, &p_tip) ) {
    return u4_k_trel
      (lane, u4_noun_6, 
             u4_k_cell(lane, u4_noun_0, axe),
             u4_k_cell(lane, u4_noun_1, p_tip));
  }

  else if ( u4_b_p(tip, u4_atom_forq, &p_tip) ) {
    return _fish_forq(m, gil, bar, axe, p_tip);
  }

  else if ( u4_b_pq(tip, u4_atom_fuse, &p_tip, &q_tip) ) {
    return 
      _mill_and
        (m, _fish_main(m, gil, bar, axe, p_tip),
            _fish_main(m, gil, u4_k_cell(lane, p_tip, bar), axe, q_tip));
  }

  else if ( u4_b_pq(tip, u4_atom_hold, &p_tip, &q_tip) ) {
    u4_noun cuv = u4_k_cell(lane, bar, tip);

    if ( u4_bag_in(cuv, gil) ) {
      // Fish recursion - this can be made to work... or fail.
      //
      return u4_k_cell(lane, u4_noun_0, u4_noun_0);
    }
    else {
      gil = u4_bag_add(m->lane, cuv, gil);

      return _fish_main(m, gil, bar, axe, _mill_repo(m, p_tip, q_tip));
    }
  }
  else return _fish_main(m, gil, bar, axe, _mill_reap(m, tip));
}

/* _mill_fish(): test nock.  Needs considerable improvement.
*/
u4_nock
_mill_fish(u4_milr m,
           u4_axis axe,
           u4_mold tip)
{
  if ( _mill_null(m, tip) ) {
    return u4_k_cell(m->lane, u4_noun_1, u4_noun_1);
  }
  else return _fish_main(m, u4_noun_0, u4_noun_0, axe, tip);
}
