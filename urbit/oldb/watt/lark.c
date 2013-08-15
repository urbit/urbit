/* watt/lark.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* nuke:lark:rose:plow
*/
  static u4_tack
  _lark_nuke_port(u4_plow p,
                  u4_type sut,
                  u4_tack tac,
                  u4_rope rop,
                  u4_type feg,
                  u4_term gos,
                  u4_axis wam,
                  u4_unit yus,
                  u4_type gur)
  {
    u4_lane lan = p->lan;

    if ( u4_n_eq(u4_axis_1, wam) ) {
      if ( u4_n_zero(yus) ) {
        return u4_k_trel
          (lan, u4_atom_bran,
                gos,
                _lark_nuke(p, gur, tac, rop, feg));
      } else {
        return _lark_nuke(p, gur, tac, rop, feg);
      }
    }
    else {
      u4_axis tip = u4_op_tip(wam);
      u4_axis tap = u4_op_tap(lan, wam);

      if ( u4_n_eq(u4_axis_2, tip) ) {
        return u4_k_trel
          (lan, 
           u4_atom_pair,
           _lark_nuke_port(p, sut, tac, rop, feg, gos, tap, yus, gur),
           u4_nul);
      }
      else {
        return u4_k_trel
          (lan, 
           u4_atom_pair,
           u4_nul,
           _lark_nuke_port(p, sut, tac, rop, feg, gos, tap, yus, gur));
      }
    }
  }

  static u4_tack
  _lark_nuke_frag(u4_plow p,
                  u4_type sut,
                  u4_tack tac,
                  u4_rope rop,
                  u4_type feg,
                  u4_axis wam,
                  u4_type gur)
  {
    u4_lane lan = p->lan;

    if ( u4_n_eq(u4_axis_1, wam) ) {
      return _lark_nuke(p, gur, tac, rop, feg);
    }
    else {
      u4_axis tip = u4_op_tip(wam);
      u4_axis tap = u4_op_tap(lan, wam);

      if ( u4_n_eq(u4_axis_2, tip) ) {
        return u4_k_trel
          (lan, 
           u4_atom_pair,
           _lark_nuke_frag(p, sut, tac, rop, feg, tap, gur),
           u4_nul);
      }
      else {
        return u4_k_trel
          (lan, 
           u4_atom_pair,
           u4_nul,
           _lark_nuke_frag(p, sut, tac, rop, feg, tap, gur));
      }
    }
  }
u4_tack
_lark_nuke(u4_plow p,
           u4_type sut,
           u4_tack tac,
           u4_rope rop,
           u4_type feg)
{
  u4_lane lan = p->lan;

  if ( u4_n_zero(rop) ) {
    return u4_k_cell(lan, u4_atom_leaf, feg);
  }
  else {
    u4_cord i_rop = u4_ch(rop);
    u4_rope t_rop = u4_ct(rop);
    u4_noun pi_rop;

    if ( u4_n_atom(i_rop) ) {
      return _lark_nuke
        (p, sut, 
            tac, 
            u4_k_cell(lan, u4_k_cell(lan, u4_atom_pane, rop), t_rop),
            feg);
    }
    else if ( u4_b_p(i_rop, u4_atom_pane, &pi_rop) ) {
      u4_plan lar = _iris_find(p, sut, u4_nul, u4_axis_1, pi_rop);
      u4_axis p_lar = u4_ch(lar);
      u4_unit q_lar = u4_ch(u4_ct(lar));
      u4_type r_lar = u4_ct(u4_ct(lar));

      return _lark_nuke_port
        (p, sut, tac, t_rop, feg, pi_rop, p_lar, q_lar, r_lar);
    }
    else if ( u4_b_p(i_rop, u4_atom_frag, &pi_rop) ) {
      u4_type gur = _iris_peek(p, sut, u4_nul, u4_axis_1, pi_rop);

      return _lark_nuke_frag(p, sut, tac, t_rop, feg, pi_rop, gur); 
    }
    else return u4_trip;
  }
}

/* cram:lark:rose:plow
*/
u4_tack
_lark_cram(u4_plow p,
           u4_type sut,
           u4_tack tac,
           u4_tack les)
{
  u4_lane lan = p->lan;
  u4_noun p_tac, q_tac, p_les, q_les;

  if ( u4_n_zero(tac) ) {
    return les;
  }
  else if ( u4_n_zero(les) ) {
    return tac;
  }
  else if ( u4_b_pq(tac, u4_atom_bran, &p_tac, &q_tac) ) {
    if ( !u4_b_pq(les, u4_atom_bran, &p_les, &q_les) ||
         !u4_n_eq(p_tac, p_les) )
    {
      return _plow_fail(p, "cram crash");
    }
    else return u4_k_trel
      (lan, u4_atom_bran,
            p_tac,
            _lark_cram(p, sut, q_tac, q_les));
  }
  else if ( u4_b_pq(tac, u4_atom_pair, &p_tac, &q_tac) ) {
    if ( !u4_b_pq(les, u4_atom_pair, &p_les, &q_les) ) {
      return _plow_fail(p, "cram crash");
    }
    else return u4_k_trel
      (lan, u4_atom_pair,
            _lark_cram(p, sut, p_tac, p_les),
            _lark_cram(p, sut, q_tac, q_les));
  }
  else return _plow_fail(p, "cram crash");
}

/* feed:lark:rose:plow
*/
u4_tack
_lark_feed(u4_plow p,
           u4_type sut,
           u4_tack tac,
           u4_list mut)
{
  if ( u4_n_zero(mut) ) {
    return tac;
  }
  else {
    u4_noun i_mut  = u4_ch(mut);
    u4_list t_mut  = u4_ct(mut);
    u4_gene pi_mut = u4_ch(i_mut);
    u4_type qi_mut = u4_ct(i_mut);
    u4_rope rop    = _plow_rake(p, pi_mut);
    u4_tack bur    = _lark_nuke(p, sut, tac, rop, qi_mut);
    u4_tack tey    = _lark_cram(p, sut, tac, bur);

#if 0
    { 
      u4_err(p->lan, "tac", tac);
      u4_err(p->lan, "bur", bur);
      u4_err(p->lan, "tey", tey);
      printf("\n");
    }
#endif
    return _lark_feed(p, sut, tey, t_mut);
  }
}
