/* mill/make.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

  /** Forward declarations.
  **/
    u4_form _mill_make(u4_milr m, u4_type naf, u4_gene dug);


/* _make_book(): make a book.
*/
static u4_form
_make_book(u4_milr m, u4_type fez, u4_form q_dug)
{
  u4_lane lane = m->lane;

  if ( u4_n_cell(u4_ch(q_dug)) ) {
    return u4_k_cell(lane, _make_book(m, fez, u4_ch(q_dug)),
                           _make_book(m, fez, u4_ct(q_dug)));
  }
  else {
    return _mill_make(m, fez, u4_ct(q_dug));
  }
}

/* _make_mute(): construct a modification list.
*/
static u4_noun
_make_mute(u4_milr m, u4_type naf, u4_type buf, u4_form p_dug)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(p_dug) ) {
    return u4_noun_0;
  }
  else {
    u4_noun ip_dug = u4_ch(p_dug);
    u4_noun tp_dug = u4_ct(p_dug);
    u4_noun pip_dug = u4_ch(ip_dug);
    u4_noun qip_dug = u4_ct(ip_dug);

    return u4_k_cell
      (lane,
       u4_k_cell(lane, _mill_hook(m, buf, pip_dug),
                       _mill_make(m, naf, qip_dug)),
       _make_mute(m, naf, buf, tp_dug));
  }
}

/* _make_bend(): make for bend.
*/
u4_form
_make_bend(u4_milr m, u4_type naf, u4_noun p_dug, u4_noun q_dug)
{
  u4_lane lane  = m->lane;

  /* sop: formula for p.dug
  ** yuf: axis of formula
  ** fum: axis of call
  ** buf: type at axis
  ** ned: list (axis form) of changes
  */
  u4_form sop; 
  u4_t    t_call;
  u4_axis yuf;
  u4_axis fum;
  u4_type buf;
  u4_noun ned;
  u4_form raz;

  sop = _mill_make(m, naf, q_dug);

  /* Analyze bend formula.
  */
  {
    u4_noun foo, bar;

    if ( u4_b_p(sop, u4_noun_0, &yuf) ) {
      t_call = 0;
      buf = _mill_play(m, naf, q_dug);
    }
    else if ( u4_b_pq(sop, u4_noun_3, &foo, &bar) &&
              u4_b_p(foo, u4_noun_0, &yuf) &&
              u4_b_p(bar, u4_noun_0, &fum) )
    {
      t_call = 1;
      buf = _mill_hack(m, yuf, naf);
    }
    else {
      // u4_burp(lane, "make/a: sop", _mill_durf(m, sop));

      return _mill_fail(m, "make clog");
    }
  }

  /* Construct change list.
  */
  raz = _make_mute(m, naf, buf, p_dug);
  ned = _mill_hike(m, yuf, raz);

  if ( t_call ) {
    return u4_k_trel(lane, u4_noun_3, ned, u4_k_cell(lane, u4_noun_0, fum));
  }
  else return ned;
}

/* _make_main(): internal make.
*/
static u4_form
_make_main(u4_milr m,
           u4_type naf,
           u4_gene dug)
{
  u4_lane lane  = m->lane;
  u4_noun p_dug, q_dug, r_dug;

  if ( u4_b_fork(dug, &p_dug, &q_dug) ) {
    return _mill_cons
      (m,
       _mill_make(m, naf, p_dug),
       _mill_make(m, naf, q_dug));
  }

  else if ( u4_b_pq(dug, u4_atom_bend, &p_dug, &q_dug) ) {
    return _make_bend(m, naf, p_dug, q_dug);
  }

  else if ( u4_b_pq(dug, u4_atom_coat, &p_dug, &q_dug) ) {
    return _mill_make(m, naf, q_dug);
  }

  else if ( u4_b_pq(dug, u4_atom_cast, &p_dug, &q_dug) ) {
    return _mill_make(m, naf, q_dug);
  }

  else if ( u4_b_p(dug, u4_atom_dbug, &p_dug) ) {
    u4_form gaf;

    m->rux = u4_op_inc(lane, m->rux);
    gaf = _mill_make(m, naf, p_dug);
    m->rux = u4_op_dec(lane, m->rux);

    return gaf;
  }

  else if ( u4_b_pqr(dug, u4_atom_if, &p_dug, &q_dug, &r_dug) ) {
    u4_loaf ruf = _mill_hunt(m, naf, p_dug);
    u4_type sec = u4_ch(ruf);
    u4_form nak = u4_ct(ruf);

    if ( u4_n_eq(u4_noun_1, u4_ch(nak)) ) {
      if ( u4_n_eq(u4_noun_0, u4_ct(nak)) ) {
        return _mill_make(m, _mill_both(m, sec, naf), q_dug);
      } else {
        return _mill_make(m, naf, r_dug);
      }
    }
    else {
      return _mill_cond
        (m, nak,
            _mill_make(m, _mill_both(m, sec, naf), q_dug),
            _mill_make(m, naf, r_dug));
    }
  }

  else if ( u4_b_pq(dug, u4_atom_like, &p_dug, &q_dug) ) {
    u4_atom mev = _mill_hook(m, naf, p_dug);
    u4_type duc = _mill_play(m, naf, q_dug);

    return _mill_diff(m, mev, duc, naf);
  }

  else if ( u4_b_p(dug, u4_atom_limb, &p_dug) ) {
    return u4_k_cell(lane, u4_noun_0, p_dug);
  }

  else if ( u4_b_pq(dug, u4_atom_link, &p_dug, &q_dug)) {
    u4_type der = _mill_play(m, naf, p_dug);
    u4_form nad = _mill_make(m, naf, p_dug);

    return _mill_comp(m, nad, _mill_make(m, der, q_dug));
  }

  else if ( u4_b_p(dug, u4_atom_load, &p_dug) ) {
    u4_type fez = u4_k_trel(lane, u4_atom_cone, naf, p_dug);
    u4_noun baf = _make_book(m, fez, p_dug);

    return u4_k_cell
      (lane, u4_k_cell(lane, u4_noun_0, u4_noun_1),
             u4_k_cell(lane, u4_noun_1, baf));
  }

  else if (u4_b_pq(dug, u4_atom_look, &p_dug, &q_dug) ) {
    u4_loaf bun = _mill_look(m, naf, p_dug, q_dug);

    return u4_ct(bun);
  }

  else if ( u4_b_pq(dug, u4_atom_raw, &p_dug, &q_dug) ) {
    return u4_k_cell(lane, p_dug, _mill_make(m, naf, q_dug));
  }

  else if ( u4_b_p(dug, u4_atom_rock, &p_dug) ) {
    return u4_k_cell(lane, u4_noun_1, p_dug);
  }

  else if ( u4_b_pqr(dug, u4_atom_sure, &p_dug, &q_dug, &r_dug) ) {
    return _mill_make(m, naf, r_dug);
  }

  else return _mill_make(m, naf, _mill_open(m, dug));
}

/* _mill_make(): assemble formula.
*/
u4_form
_mill_make(u4_milr m,
           u4_type naf,
           u4_gene dug)
{
  u4_lane lane  = m->lane;
  u4_noun fid  = u4_k_cell(lane, naf, dug);
  u4_nopt raz  = u4_tab_get(fid, m->rof);

  if ( raz ) {
    return raz;
  }
  else {
    raz = _make_main(m, naf, dug);

    m->rof = u4_tab_add(lane, fid, raz, m->rof);
    return raz;
  }
}
