/* mill/fish.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

  /** Forward declarations.
  **/
    static u4_form
    _fish_main(u4_milr, u4_bag, u4_log, u4_axis, u4_type);


/* _fish_bone(): fish for bone.
*/
static u4_form
_fish_bone(u4_milr m,
           u4_bag  hed,
           u4_log  neb,
           u4_axis pol,
           u4_axis p_das,
           u4_type q_das)
{
  u4_lane lane = m->lane;

  if ( u4_n_eq(u4_noun_1, p_das) ) {
    return _fish_main(m, hed, neb, pol, q_das);
  }
  else {
    u4_axis riv = u4_op_tip(p_das);
    u4_axis vop = u4_op_tap(lane, p_das);
    u4_axis lec = u4_op_add(lane, pol, pol);
    u4_axis mir = u4_op_inc(lane, lec);

    return _mill_and
      (m, u4_k_trel(lane, u4_noun_4, u4_noun_0, pol),
          u4_n_eq(u4_noun_2, riv)
            ? _fish_bone
                (m, hed, _mill_slip(m, u4_noun_2, neb), lec, vop, q_das)
            : _fish_bone
                (m, hed, _mill_slip(m, u4_noun_3, neb), mir, vop, q_das));
  }
}

/* _fish_pair(): fish for pair.  Should not be needed, but...
*/
static u4_form
_fish_pair(u4_milr m,
           u4_bag  hed,
           u4_log  neb,
           u4_axis pol,
           u4_type p_das,
           u4_type q_das)
{
  u4_lane lane = m->lane;
  u4_axis lec  = u4_op_add(lane, pol, pol);
  u4_axis mir  = u4_op_inc(lane, lec);

  return _mill_and
      (m, u4_k_trel(lane, u4_noun_4, u4_noun_0, pol),
          _mill_and
            (m, _fish_main(m, hed, _mill_slip(m, u4_noun_2, neb), lec, p_das),
                _fish_main(m, hed, _mill_slip(m, u4_noun_3, neb), mir, q_das)));
}

/* _fish_main(): fish with cull and bag.
*/
static u4_form
_fish_main(u4_milr m,
           u4_bag  hed,
           u4_log  neb,
           u4_axis pol,
           u4_type das)
{
  u4_lane lane = m->lane;
  u4_noun p_das, q_das;

  if ( u4_n_eq(u4_atom_atom, das) ) {
    return _mill_not(m, u4_k_trel(lane, u4_noun_4, u4_noun_0, pol));
  }
  else if ( u4_n_eq(u4_atom_blur, das) ) {
    return u4_k_cell(lane, u4_noun_1, u4_noun_0);
  }
  else if ( u4_b_pq(das, u4_atom_bone, &p_das, &q_das) ) {
    return _fish_bone(m, hed, neb, pol, p_das, q_das);
  }
  else if ( u4_b_pq(das, u4_atom_cone, &p_das, &q_das) ) {
    return u4_k_cell(lane, u4_noun_0, u4_noun_0);
  }
  else if ( u4_b_p(das, u4_atom_cube, &p_das) ) {
    return u4_k_trel
      (lane, u4_noun_6, 
             u4_k_cell(lane, u4_noun_0, pol),
             u4_k_cell(lane, u4_noun_1, p_das));
  }
  else if ( u4_b_pq(das, u4_atom_fork, &p_das, &q_das) ) {
    if ( _mill_cull(m, p_das, neb) ) {
      return _fish_main(m, hed, neb, pol, q_das);
    }
    else if ( _mill_cull(m, q_das, neb) ) {
      return _fish_main(m, hed, neb, pol, p_das);
    }
    else {
      return _mill_or
        (m, _fish_main(m, hed, neb, pol, p_das), 
            _fish_main(m, hed, neb, pol, q_das));
    }
  }
  else if ( u4_b_pq(das, u4_atom_fuse, &p_das, &q_das) ) {
    return 
      _mill_and
        (m, _fish_main(m, hed, neb, pol, p_das),
            _fish_main(m, hed, u4_k_cell(lane, p_das, neb), pol, q_das));
  }
  else if ( u4_b_pq(das, u4_atom_pair, &p_das, &q_das) ) {
    return _fish_pair(m, hed, neb, pol, p_das, q_das);
  }
  else if ( u4_b_pq(das, u4_atom_post, &p_das, &q_das) ) {
    u4_type cuv = _mill_pull(m, neb, das);

    if ( u4_bag_in(cuv, hed) ) {
      printf("fish recursion!\n");
      u4_burp(m->lane, "fish: das", _mill_dump(m, das));

      return u4_trip;
      return _mill_fail(m, "fish recursion");
    }
    else {
      hed = u4_bag_add(m->lane, cuv, hed);

      return _fish_main(m, hed, neb, pol, _mill_repo(m, p_das, q_das));
    }
  }
  else return _fish_main(m, hed, neb, pol, _mill_reap(m, das));
}

/* _mill_fish(): test form.
*/
u4_form
_mill_fish(u4_milr m,
           u4_axis pol,
           u4_type das)
{
  if ( _mill_null(m, das) ) {
    return u4_k_cell(m->lane, u4_noun_1, u4_noun_1);
  }
  else return _fish_main(m, u4_noun_0, u4_noun_0, pol, das);
}
