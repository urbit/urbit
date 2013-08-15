/* mill/look.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _is_port()::
*/
static u4_t
_is_port(u4_knot sod, 
         u4_tick *tik, 
         u4_term *cox)
{
  if ( u4_n_atom(sod) ) {
    if ( tik ) *tik = u4_noun_0;
    if ( cox ) *cox = sod;
    return 1;
  }
  else if ( u4_b_pq(sod, u4_atom_port, tik, cox) ) {
    return 1;
  }
  else return 0;
}

/* _is_frag()::
*/
static u4_t
_is_frag(u4_knot sod,
         u4_axis *axe)
{
  return u4_b_p(sod, u4_atom_frag, axe);
}

/* _look_knot(): read a step on a rope.
*/
u4_loaf
_look_knot(u4_milr m,
           u4_knot vor,
           u4_mold typ)
{
  u4_lane lane = m->lane;
  u4_tick tik;
  u4_term cox;
  u4_axis axe;
  u4_loaf fod;

  if ( _is_port(vor, &tik, &cox) ) {
    fod = _mill_find(m, cox, u4_noun_0, typ);

    if ( u4_n_zero(fod) ) {
      u4_burp(lane, "mold", _mill_dump(m, typ));
      u4_burp(lane, "mark", u4_prep_textual(lane, cox));

      return _mill_fail(m, "look: not found");
    }
    else if ( u4_n_zero(tik) ) {
      return fod;
    }
    else {
      u4_mold lem = u4_ch(fod);
      u4_nock vil = u4_ct(fod);
      u4_noun p_vil;
      u4_mold ger;

      vor = u4_k_trel(lane, u4_atom_port, u4_op_dec(lane, tik), cox);

      if ( u4_b_p(vil, u4_noun_0, &axe) ) {
        ger = lem;
      }
      else if ( u4_b_pq(vil, u4_noun_3, &p_vil, 0) &&
                u4_b_p(p_vil, u4_noun_0, &axe) &&
                u4_b_pq(lem, u4_atom_hold, &ger, 0) )
      {
        axe = u4_op_peg(lane, axe, u4_noun_2);
        vil = u4_k_cell(lane, u4_noun_0, axe);
        ger = _mill_peek(m, u4_noun_2, u4_noun_0, ger);
      }
      else return u4_trip;

      fod = _look_knot(m, vor, ger);

      if ( u4_n_zero(fod) ) {
        u4_burp(lane, "mold", _mill_dump(m, typ));
        u4_burp(lane, "mark", u4_prep_textual(lane, cox));

        return _mill_fail(m, "look: no grip");
      }
      else {
        return u4_k_cell
          (lane, u4_ch(fod), _mill_comp(m, vil, u4_ct(fod)));
      }
    }
  }
  else if ( _is_frag(vor, &axe) ) {
    return u4_k_trel
      (lane, _mill_peek(m, axe, u4_noun_0, typ), 
             u4_noun_0, 
             axe);
  }
  else return u4_trip;
}

/* _mill_look(): read on a rope.
*/
u4_loaf
_mill_look(u4_milr m,
           u4_rope fes,
           u4_mold typ)
{
  u4_lane lane = m->lane;

  // u4_err(lane, "look: fes", fes);
  // u4_burp(lane, "look: typ", _mill_dump(m, typ));

  if ( u4_n_zero(fes) ) {
    return u4_k_trel(lane, typ, u4_noun_0, u4_noun_1);
  }
  else {
    u4_knot i_fes = u4_ch(fes);
    u4_rope t_fes = u4_ct(fes);
    u4_loaf fod   = _look_knot(m, i_fes, typ);
    u4_loaf gax   = _mill_look(m, t_fes, u4_ch(fod));

    return u4_k_cell
      (lane, u4_ch(gax), _mill_comp(m, u4_ct(fod), u4_ct(gax)));
  }
}


