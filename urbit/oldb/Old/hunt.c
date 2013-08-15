/* mill/hunt.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_hunt(): analyze branch test.
**
** Produce (p.type q.form).  p is the gain on yes.  q is the test.
*/
u4_noun
_mill_hunt(u4_milr m,
           u4_type met,
           u4_gene nif)
{
  u4_lane lane = m->lane;
  u4_noun p_nif, q_nif;

  if ( u4_b_pq(nif, u4_atom_like, &p_nif, &q_nif) ) {
    u4_axis gam = _mill_hook(m, met, p_nif);
    u4_type lef = _mill_play(m, met, q_nif);
    u4_form maz = _mill_diff(m, gam, lef, met);

    if ( u4_n_eq(u4_noun_1, u4_ch(maz)) ) {
      return u4_k_cell(lane, u4_atom_blur, maz);
    }
    else {
      u4_type sar = _mill_flap(m, gam, lef);

      return u4_k_cell(lane, sar, maz);
    }
  }
  else if ( u4_b_pq(nif, u4_atom_and, &p_nif, &q_nif) ) {
    u4_loaf fot = _mill_hunt(m, met, p_nif);
    u4_type lep = u4_ch(fot);
    u4_form pid = u4_ct(fot);
    u4_type rew = _mill_both(m, lep, met);
    u4_loaf dof = _mill_hunt(m, rew, q_nif);
    u4_type sud = u4_ch(dof);
    u4_form ves = u4_ct(dof);

    return u4_k_cell(lane, _mill_both(m, lep, sud), 
                           _mill_and(m, pid, ves));
  }
  else if ( u4_b_pq(nif, u4_atom_or, &p_nif, &q_nif) ) {
    u4_noun miv = _mill_hunt(m, met, p_nif);
    u4_noun pas = _mill_hunt(m, met, q_nif);

    return u4_k_cell(lane, _mill_eith(m, u4_ch(miv), u4_ch(pas)),
                           _mill_or(m, u4_ct(miv), u4_ct(pas)));
  }
  else {
    u4_noun vob = _mill_open(m, nif);

    if ( u4_n_eq(vob, nif) ) {
      return u4_k_cell(lane, u4_atom_blur, _mill_make(m, met, nif));
    }
    else return _mill_hunt(m, met, vob);
  }
}
