/* mill/test.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

#if 0
/* _mill_test_and(): analyze branch and.
*/
u4_wire
_mill_test_and(u4_milr m,
               u4_noun p_gen,
               u4_mold typ)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(p_gen) ) {
    return u4_k_cell
      (lane, u4_atom_blur,
             u4_k_cell(lane, u4_noun_1, u4_noun_0));
  }
  else {
    u4_gene ip_gen = u4_ch(p_gen);
    u4_noun tp_gen = u4_ct(p_gen);
    u4_wire vex    = _mill_test(m, ip_gen, typ);

    if ( u4_n_zero(tp_gen) ) {
      return vex;
    } else {
      u4_wire nuf = _mill_test_and(m, tp_gen, typ);

      return u4_k_cell(lane, _mill_both(m, u4_ch(vex), u4_ch(nuf)),
                             _mill_and(m, u4_ct(vex), u4_ct(nuf)));
    }
  }
}

/* _mill_test_or(): analyze branch or.
*/
u4_wire
_mill_test_or(u4_milr m,
              u4_noun p_gen,
              u4_mold typ)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(p_gen) ) {
    return u4_k_cell
      (lane, u4_atom_blot,
             u4_k_cell(lane, u4_noun_1, u4_noun_1));
  }
  else {
    u4_gene ip_gen = u4_ch(p_gen);
    u4_noun tp_gen = u4_ct(p_gen);
    u4_wire vex    = _mill_test(m, ip_gen, typ);

    if ( u4_n_zero(tp_gen) ) {
      return vex;
    } else {
      u4_wire nuf = _mill_test_or(m, tp_gen, typ);

      return u4_k_cell(lane, _mill_eith(m, u4_ch(vex), u4_ch(nuf)),
                             _mill_or(m, u4_ct(vex), u4_ct(nuf)));
    }
  }
}
#endif

/* _mill_test(): analyze branch test.
**
** Produce (p.mold q.nock).  p is the yes mold.  q is the test.
*/
u4_wire
_mill_test(u4_milr m,
           u4_gene gen,
           u4_mold typ)
{
  u4_lane lane = m->lane;
  u4_noun p_gen, q_gen;

  if ( u4_b_pq(gen, u4_atom_like, &p_gen, &q_gen) ) {
    u4_axis axe = u4_noun_1;
    u4_mold buv;
    u4_tape res = _mill_hunt(m, p_gen, typ, &axe, &buv);
    u4_mold hur = _mill_play(m, q_gen, typ);
    u4_plox zel = u4_k_cell(lane, u4_k_cell(lane, res, hur), u4_noun_0);
    u4_mold jac = _mill_edit(m, zel, u4_noun_0, u4_atom_blur);

    if ( _mill_orth(m, buv, hur) ) {
      if ( !u4_n_zero(m->rux) ) {
        u4_burp(lane, "orth: buv", _mill_dump(m, buv));
        u4_burp(lane, "orth: hur", _mill_dump(m, hur));
      }
      return u4_k_trel(lane, u4_atom_blot, u4_noun_1, u4_noun_1);
    }
    else if ( _mill_nest(m, buv, hur) ) {
      return u4_k_trel(lane, typ, u4_noun_1, u4_noun_0);
    }
    else {
      return u4_k_cell
        (lane, u4_k_trel(lane, u4_atom_fuse, jac, typ),
               _mill_fish(m, axe, hur));
    }
  }
 
#if 0
  else if ( u4_b_p(gen, u4_atom_boce, &p_gen) ) {
    return _mill_test_and(m, p_gen, typ);
  }
  else if ( u4_b_p(gen, u4_atom_dant, &p_gen) ) {
    return _mill_test_or(m, p_gen, typ);
  }
#endif

  else {
    u4_noun vob = _mill_open(m, gen);

    if ( u4_n_eq(vob, gen) ) {
      return u4_k_cell(lane, typ, _mill_bake(m, gen, typ));
    }
    else return _mill_test(m, vob, typ);
  }
}
