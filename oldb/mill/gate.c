/* mill/gate.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _gate_arg(): generate argument for a form.
*/
static u4_noun
_gate_arg(u4_milr m,
          u4_form kel)
{
  u4_lane lane = m->lane;

  return u4_k_trel
    (lane, u4_atom_cast,
           u4_k_cell(lane, u4_atom_crad, u4_noun_0),
           _mill_lump(m, kel));
}

/* _gate_crib_in()::
*/
static u4_noun
_gate_crib_in(u4_milr m,
              u4_axis axe,
              u4_noun wix)
{
  u4_lane lane  = m->lane;
  u4_noun i_wix = u4_ch(wix);
  u4_noun t_wix = u4_ct(wix);
  u4_mark pi_wix = u4_ch(i_wix);
  u4_form qi_wix = u4_ct(i_wix);

  if ( u4_n_zero(t_wix) ) {
    return u4_k_cell
      (lane, u4_k_trel
               (lane, u4_atom_name,
                      pi_wix, 
                      u4_k_trel
                        (lane, u4_atom_mang,
                               _mill_gate(m, qi_wix), 
                                u4_k_cell(lane, u4_atom_frag, axe))),
              u4_noun_0);
  }
  else {
    u4_axis piq = u4_op_peg(lane, axe, u4_noun_2);
    u4_axis guz = u4_op_peg(lane, axe, u4_noun_3);

    return u4_k_cell
      (lane, u4_k_trel
               (lane, u4_atom_name,
                      pi_wix, 
                      u4_k_trel
                        (lane, u4_atom_mang,
                               _mill_gate(m, qi_wix), 
                                u4_k_cell(lane, u4_atom_frag, piq))),
              _gate_crib_in(m, guz, t_wix));
  }
}

/* _gate_crib()::
*/
static u4_gene
_gate_crib(u4_milr m,
           u4_noun wix)
{
  u4_lane lane = m->lane;

  return u4_k_quil
    (lane,
     u4_atom_trop,
     _gate_arg(m, u4_k_cell(lane, u4_atom_crib, wix)),
     u4_atom_lome,
     u4_atom_cage,
     _gate_crib_in(m, u4_noun_4, wix));
}

/* _gate_dish()::
*/
static u4_gene
_gate_dish(u4_milr m,
           u4_gene heb)
{
  u4_lane lane = m->lane;
  u4_noun far  = u4_k_cell(lane, u4_atom_frag, u4_noun_4);
  u4_wire rid  = u4_k_cell(lane, far, u4_noun_0);

  return u4_k_qual
    (lane, 
     u4_atom_trop,
     _gate_arg(m, u4_k_cell(lane, u4_atom_dish, heb)),
     u4_atom_lome,
     u4_k_trel
      (lane,
       u4_atom_feng,
       u4_k_trel(lane, u4_atom_like, rid, heb),
       far));
}

/* _gate_pick_in()::
*/
static u4_noun
_gate_pick_in(u4_milr m,
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
             _gate_pick_in(m, t_bec));
  }
}

/* _gate_pick()::
*/
static u4_gene
_gate_pick(u4_milr m,
           u4_noun bec)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(bec) ) {
    return _mill_fail(m, "pick: empty");
  }
  else {
    u4_form hem = u4_ch(bec);

    return u4_k_qual
      (lane,
       u4_atom_trop,
       _gate_arg(m, hem),
       u4_atom_lome,
       u4_k_trel
        (lane,
         u4_atom_gril,
         u4_k_cell(lane, u4_k_cell(lane, u4_atom_frag, u4_noun_4), u4_noun_0),
         _gate_pick_in(m, bec)));
  }
}

/* _gate_blur(): gate producing blur.
*/
static u4_gene
_gate_blur(u4_milr m)
{
  u4_lane lane = m->lane;

  return u4_k_qual
    (lane, u4_atom_trop,
           u4_k_cell(lane, u4_atom_crad, u4_noun_0),
           u4_atom_lome,
           u4_k_cell(lane, u4_atom_frag, u4_noun_4));
}

/* _gate_atom(): gate producing atom.
*/
static u4_gene
_gate_atom(u4_milr m)
{
  u4_lane lane = m->lane;
  u4_noun fog  = u4_k_cell(lane, u4_atom_frag, u4_noun_4);

  return u4_k_qual
    (lane, u4_atom_trop,
           u4_k_cell(lane, u4_atom_crad, u4_noun_0),
           u4_atom_lome,
           u4_k_trel
            (lane, 
             u4_atom_feng,
             u4_k_trel
              (lane, 
               u4_atom_like, 
               u4_k_cell(lane, fog, u4_noun_0),
               u4_k_cell(lane, u4_atom_palt, u4_noun_0)),
             fog));
}

/* _gate_flag(): gate producing flag.
*/
static u4_gene
_gate_flag(u4_milr m)
{
  u4_lane lane = m->lane;

  return u4_k_qual
    (lane, u4_atom_trop,
           u4_k_cell(lane, u4_atom_crad, u4_noun_0),
           u4_atom_lome,
           u4_k_trel
            (lane, 
             u4_atom_nock,
             u4_noun_6,
             u4_k_qual
               (lane, 
                u4_atom_cage,
                u4_k_cell(lane, u4_atom_rock, u4_noun_0),
                u4_k_cell(lane, u4_atom_frag, u4_noun_4),
                u4_noun_0)));
}

/* _gate_rock(): gate producing rock.
*/
static u4_gene
_gate_rock(u4_milr m,
           u4_noun p_kel)
{
  u4_lane lane = m->lane;
  u4_noun zed  = u4_k_cell(lane, u4_atom_frag, u4_noun_4);

  return u4_k_qual
    (lane, u4_atom_trop,
           u4_k_cell(lane, u4_atom_crad, p_kel),
           u4_atom_lome,
           u4_k_trel
            (lane, 
             u4_atom_feng,
             u4_k_trel
              (lane, 
               u4_atom_like, 
               u4_k_cell(lane, zed, u4_noun_0),
               u4_k_cell(lane, u4_atom_rock, p_kel)),
             zed));
}

/* _mill_gate(): gate from form.
*/
u4_gene
_mill_gate(u4_milr m,
           u4_form kel)
{
  u4_lane lane = m->lane;
  u4_noun p_kel, q_kel;

  if ( u4_n_eq(kel, u4_atom_blur) ) {
    return _gate_blur(m);
  }
  else if ( u4_n_eq(kel, u4_atom_atom) ) {
    return _gate_atom(m);
  }
  else if ( u4_n_eq(kel, u4_atom_flag) ) {
    return _gate_flag(m);
  }
  else if ( u4_n_eq(kel, u4_atom_twin) ) {
    return _mill_gate
      (m, u4_k_qual
        (lane, u4_atom_crib,
               u4_k_cell(lane, u4_noun_0, u4_atom_blur), 
               u4_k_cell(lane, u4_noun_0, u4_atom_blur), 
               u4_noun_0));
  }
  else if ( u4_b_p(kel, u4_atom_rock, &p_kel) ) {
    return _gate_rock(m, p_kel);
  }
  
  else if ( u4_b_p(kel, u4_atom_crib, &p_kel) ) {
    return _gate_crib(m, p_kel);
  }
  else if ( u4_b_p(kel, u4_atom_dish, &p_kel) ) {
    return _gate_dish(m, p_kel);
  }
  else if ( u4_b_p(kel, u4_atom_pick, &p_kel) ) {
    return _gate_pick(m, p_kel);
  }
  else if ( u4_b_p(kel, u4_atom_gate, &p_kel) ) {
    return p_kel;
  }
  else if ( u4_b_pq(kel, u4_atom_grip, &p_kel, &q_kel) ) {
    return _mill_gate(m, q_kel);
  }
  else {
    return u4_trip;
  }
}
