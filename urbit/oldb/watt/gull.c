/* watt/gull.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* size:gull:plow
*/
u4_atom
_gull_size(u4_plow p,
           u4_book dab)
{
  u4_lane lan = p->lan;

  if ( u4_n_zero(dab) ) {
    return u4_noun_0;
  } else {
    u4_noun n_dab, l_dab, r_dab;

    u4_c_trel(dab, &n_dab, &l_dab, &r_dab);

    return u4_op_inc(lan, u4_op_add(lan, _gull_size(p, l_dab),
                                         _gull_size(p, r_dab)));
  }
}

/* fill:gull:plow
*/
u4_spec
_gull_fill(u4_plow p,
           u4_book dab,
           u4_menu sem)
{
  u4_lane lan = p->lan;

  return u4_tab_add_log(lan, sem, dab);
}

/* _ord_tag_simple():
**
**    Return 1 iff (tag_l tag_r) are in correct simple order.
**
**    Trip if (tag_l tag_r) are equal.
*/
static u4_t
_ord_tag_simple(u4_tag tag_l, u4_tag tag_r)
{
  u4_bit bit;

  if ( u4_bull == (bit = u4_op_ord(tag_l, tag_r)) ) {
    return u4_trip;
  }
  return u4_n_true(bit);
}

/* _ord_tag_tree():
**
**    Return 1 iff (hog_l hog_r) are in correct tree order (nub).
**
**    Stop iff (hog_l hog_r) are equal.
*/
static u4_t
_ord_tag_tree(u4_tag tag_l, u4_tag tag_r)
{
  u4_nub nub_l = u4_n_nub(tag_l);
  u4_nub nub_r = u4_n_nub(tag_r);

  if ( nub_l == nub_r ) {
    return _ord_tag_simple(tag_l, tag_r);
  }
  else return (nub_l < nub_r);
}

/* look:gull:plow
*/
  static u4_unit
  _gull_look_a(u4_plow p,
               u4_spec dab,
               u4_term cog,
               u4_axis axe)
  {
    u4_lane lan = p->lan;

    if ( u4_n_zero(dab) ) {
      return u4_nul;
    }
    else {
      u4_noun n_dab, l_dab, r_dab, pn_dab, qn_dab;

      u4_c_trel(dab, &n_dab, &l_dab, &r_dab);
      pn_dab = u4_ch(n_dab);
      qn_dab = u4_ct(n_dab);

      if ( u4_n_zero(l_dab) && u4_n_zero(r_dab) ) {
        if ( u4_n_eq(cog, pn_dab) ) {
          return u4_kt(lan, u4_nul, axe, qn_dab);
        }
        else return u4_nul;
      }
      else if ( u4_n_zero(l_dab) ) {
        if ( u4_n_eq(cog, pn_dab) ) {
          return u4_kt(lan, u4_nul, u4_op_peg(lan, axe, u4_noun_2), qn_dab);
        }
        else if ( _ord_tag_tree(cog, pn_dab) ) {
          return u4_nul;
        }
        else {
          return _gull_look_a(p, r_dab, cog, u4_op_peg(lan, axe, u4_noun_3));
        }
      }
      else if ( u4_n_zero(r_dab) ) {
        if ( u4_n_eq(cog, pn_dab) ) {
          return u4_kt(lan, u4_nul, u4_op_peg(lan, axe, u4_noun_2), qn_dab);
        }
        else if ( _ord_tag_tree(cog, pn_dab) ) {
          return _gull_look_a(p, l_dab, cog, u4_op_peg(lan, axe, u4_noun_3));
        }
        else {
          return u4_nul;
        }
      }
      else {
        if ( u4_n_eq(cog, pn_dab) ) {
          return u4_kt(lan, u4_nul, u4_op_peg(lan, axe, u4_noun_2), qn_dab);
        }
        else if ( _ord_tag_tree(cog, pn_dab) ) {
          return _gull_look_a(p, l_dab, cog, u4_op_peg(lan, axe, u4_noun_6));
        }
        else {
          return _gull_look_a(p, r_dab, cog, u4_op_peg(lan, axe, u4_noun_7));
        }
      }
    }
  }
u4_unit
_gull_look(u4_plow p,
           u4_spec dab,
           u4_term cog)
{
  return _gull_look_a(p, dab, cog, u4_axis_1);
}
