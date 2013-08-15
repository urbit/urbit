/* mill/grip.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _grip_crib_in(): convert crib items to cage list.
*/
u4_noun
_grip_crib_in(u4_milr m,
              u4_noun wix)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(wix) ) {
    return u4_noun_0;
  } 
  else {
    u4_noun i_wix = u4_ch(wix);
    u4_noun t_wix = u4_ct(wix);
    u4_mark pi_wix = u4_ch(i_wix);
    u4_form qi_wix = u4_ct(i_wix);

    return u4_k_cell
      (lane, u4_k_trel(lane, u4_atom_name, pi_wix, _mill_grip(m, qi_wix)),
             _grip_crib_in(m, t_wix));
  }
}

/* _grip_pick_in()::
*/
static u4_noun
_grip_pick_in(u4_milr m,
              u4_noun bec)
{
  u4_lane lane  = m->lane;

  if ( u4_n_zero(bec) ) {
    return u4_noun_0;
  } 
  else {
    u4_form i_bec = u4_ch(bec);
    u4_noun t_bec = u4_ct(bec);

    return u4_k_cell
      (lane, u4_k_cell(lane, u4_atom_fing, i_bec),
             _grip_pick_in(m, t_bec));
  }
}

/* _grip_pick()::
*/
static u4_gene
_grip_pick(u4_milr m,
           u4_noun bec)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(bec) ) {
    return _mill_fail(m, "pick: empty");
  }
  else {
    u4_form hem = u4_ch(bec);

    return u4_k_trel
      (lane,
       u4_atom_trop,
       u4_k_trel
        (lane, 
         u4_atom_cast,
         u4_k_cell(lane, u4_atom_crad, u4_noun_0),
         _mill_grip(m, hem)),
       u4_k_trel
        (lane,
         u4_atom_gril,
         u4_k_cell(lane, u4_k_cell(lane, u4_atom_frag, u4_noun_2), u4_noun_0),
         _grip_pick_in(m, bec)));
  }
}

/* _mill_grip(): generate direct form.
*/
u4_gene
_mill_grip(u4_milr m,
           u4_form kel)
{
  u4_lane lane = m->lane;
  u4_noun p_kel, q_kel;

  if ( u4_n_eq(kel, u4_atom_blur) ) {
    return u4_k_cell(lane, u4_atom_crad, u4_noun_0);
  }
  else if ( u4_n_eq(kel, u4_atom_atom) ) {
    return u4_k_cell(lane, u4_atom_palt, u4_noun_0);
  }
  else if ( u4_n_eq(kel, u4_atom_flag) ) {
    return u4_k_trel
      (lane,
       u4_atom_nock,
       u4_noun_6,
       u4_k_qual
        (lane, 
         u4_atom_cage,
         u4_k_cell(lane, u4_atom_rock, u4_noun_0),
         u4_k_cell(lane, u4_atom_rock, u4_noun_0),
         u4_noun_0));
  }
  else if ( u4_n_eq(kel, u4_atom_twin) ) {
    return _mill_grip
      (m, u4_k_qual
        (lane, u4_atom_crib,
               u4_k_cell(lane, u4_noun_0, u4_atom_blur),
               u4_k_cell(lane, u4_noun_0, u4_atom_blur),
               u4_noun_0));
  }
  else if ( u4_b_p(kel, u4_atom_rock, &p_kel) ) {
    return u4_k_cell(lane, u4_atom_rock, p_kel);
  }
 
  else if ( u4_b_p(kel, u4_atom_crib, &p_kel) ) {
    return u4_k_cell
      (lane, u4_atom_cage, _grip_crib_in(m, p_kel));
  }
  else if ( u4_b_p(kel, u4_atom_dish, &p_kel) ) {
    return p_kel;
  }
  else if ( u4_b_p(kel, u4_atom_pick, &p_kel) ) {
    return _grip_pick(m, p_kel);
  }
  else if ( u4_b_p(kel, u4_atom_gate, &p_kel) ) {
    return u4_k_cell(lane, u4_atom_crad, u4_noun_0);
  }
  else if ( u4_b_pq(kel, u4_atom_grip, &p_kel, &q_kel) ) {
    return _mill_grip(m, p_kel);
  }
  else return u4_trip;
}
