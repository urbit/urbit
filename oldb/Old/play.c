/* mill/play.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _flag()::
*/
static u4_type
_flag(u4_lane lane)
{
  return u4_k_trel
    (lane, u4_atom_fork, u4_k_cell(lane, u4_atom_cube, u4_noun_0),
                         u4_k_cell(lane, u4_atom_cube, u4_noun_1));
}

/* _bend_each(): 
*/
static u4_type
_bend_each(u4_milr m, u4_axis lub, u4_type naf, u4_type cav, u4_noun p_dug)
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
    {
      u4_axis nux = _mill_hook(m, cav, pip_dug);
      u4_axis loz = u4_op_peg(m->lane, lub, nux);
      u4_type vag = _mill_play(m, naf, qip_dug);

      return u4_k_cell
        (lane, u4_k_cell(lane, loz, vag), 
               _bend_each(m, lub, naf, cav, tp_dug));
    }
  }
}

/* _bend_molt(): compile change list [axis type]
*/
static u4_log
_bend_molt(u4_milr m, 
           u4_axis meg, 
           u4_type ger, 
           u4_type juk,
           u4_log  sol)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(sol) ) {
    return u4_noun_0;
  }
  else {
    u4_noun i_sol = u4_ch(sol);
    u4_noun t_sol = u4_ct(sol);
    u4_gene pi_sol = u4_ch(i_sol);
    u4_gene qi_sol = u4_ct(i_sol);
    {
      return u4_k_cell
        (lane, 
         u4_k_cell
          (lane, 
           u4_op_peg(lane, meg, _mill_hook(m, ger, pi_sol)),
           _mill_play(m, juk, qi_sol)
          ),
         _bend_molt(m, meg, ger, juk, t_sol)
        );
    }
  }
}

/* _mill_play_main(): internal of _mill_play().
*/
u4_type 
_mill_play_main(u4_milr m,
                u4_type naf,
                u4_gene dug)
{
  u4_lane lane = m->lane;
  u4_noun p_dug, q_dug, r_dug;

  if ( u4_b_fork(dug, &p_dug, &q_dug) ) {
    return u4_k_trel
      (lane,
       u4_atom_pair,
       _mill_play(m, naf, p_dug),
       _mill_play(m, naf, q_dug));
  }

  else if ( u4_b_pq(dug, u4_atom_bend, &p_dug, &q_dug) ) {
    u4_noun gus = _mill_bark(m, naf, q_dug);
    u4_axis lub = u4_ch(gus);
    u4_type cav = u4_ct(gus);

    /* Replay with dust.
    */
    {
      u4_log  guc = _bend_molt(m, lub, cav, naf, p_dug);
      u4_type yot = _mill_dust(m, guc, naf);

      return _mill_play(m, yot, q_dug);
    }
  }

  else if ( u4_b_pq(dug, u4_atom_cast, &p_dug, &q_dug) ) {
    return _mill_play(m, naf, p_dug);
  }

  else if ( u4_b_pq(dug, u4_atom_coat, &p_dug, &q_dug) ) {
    return u4_k_trel
      (lane, u4_atom_skin, p_dug, _mill_play(m, naf, q_dug));
  }

  else if ( u4_b_p(dug, u4_atom_dbug, &p_dug) ) {
    return _mill_play(m, naf, p_dug);
  }

  else if ( u4_b_pqr(dug, u4_atom_if, &p_dug, &q_dug, &r_dug) ) {
    u4_loaf ruf = _mill_hunt(m, naf, p_dug);
    u4_type sec = u4_ch(ruf);
    u4_form nak = u4_ct(ruf);
    
    if ( u4_n_eq(u4_noun_1, u4_ch(nak)) ) {
      if ( u4_n_eq(u4_noun_0, u4_ct(nak)) ) {
        return _mill_play(m, naf, q_dug);
      } else {
        return _mill_play(m, naf, r_dug);
      }
    }
    else {
      return _mill_eith
            (m, _mill_play(m, _mill_both(m, sec, naf), q_dug), 
                _mill_play(m, naf, r_dug));
    }
  }

  else if ( u4_b_pq(dug, u4_atom_like, &p_dug, &q_dug) ) {
    u4_atom mev = _mill_hook(m, naf, p_dug);
    u4_type duc = _mill_play(m, naf, q_dug);
    u4_form zur = _mill_diff(m, mev, duc, naf);

    if ( u4_n_eq(u4_noun_1, u4_ch(zur)) ) {
      return u4_k_cell(lane, u4_atom_cube, u4_ct(zur));
    }
    else return _flag(lane);
  }

  else if ( u4_b_p(dug, u4_atom_limb, &p_dug) ) {
    if ( u4_n_zero(p_dug) ) {
      return u4_atom_blot;
    }
    else {
      return _mill_hack(m, p_dug, naf);
    }
  }

  else if ( u4_b_pq(dug, u4_atom_link, &p_dug, &q_dug)) {
    return _mill_play(m, _mill_play(m, naf, p_dug), q_dug);
  }

  else if ( u4_b_p(dug, u4_atom_load, &p_dug) ) {
    return u4_k_trel(lane, u4_atom_cone, naf, p_dug);
  }

  else if (u4_b_pq(dug, u4_atom_look, &p_dug, &q_dug) ) {
    u4_loaf bun = _mill_look(m, naf, p_dug, q_dug);

    return u4_ch(bun);
  }

  else if ( u4_b_pq(dug, u4_atom_raw, &p_dug, &q_dug) ) {
    if ( u4_n_eq(u4_noun_3, p_dug) ) {
      return u4_atom_blur;
    }
    else if ( u4_n_eq(u4_noun_4, p_dug) || u4_n_eq(u4_noun_6, p_dug) ) {
      return u4_k_trel
        (lane, u4_atom_fork, 
               u4_k_cell(lane, u4_atom_cube, u4_noun_0),
               u4_k_cell(lane, u4_atom_cube, u4_noun_1));
    }
    else if ( u4_n_eq(u4_noun_5, p_dug) ) {
      return u4_atom_atom;
    }
    else return u4_exit;
  }

  else if ( u4_b_p(dug, u4_atom_rock, &p_dug) ) {
    return u4_k_cell(lane, u4_atom_cube, p_dug);
  }

  else if ( u4_b_pqr(dug, u4_atom_sure, &p_dug, &q_dug, &r_dug) ) {
    return _mill_play(m, naf, r_dug);
  }

  else return _mill_play(m, naf, _mill_open(m, dug));
}

/* _mill_play(): type inference, top level.
*/
u4_type 
_mill_play(u4_milr m,
           u4_type naf,
           u4_gene dug)
{
  u4_lane lane = m->lane;
  u4_noun fid  = u4_k_cell(lane, naf, dug);
  u4_nopt raz  = u4_tab_get(fid, m->niq);

  if ( raz ) {
    return raz;
  }
  else {
    raz = _mill_play_main(m, naf, dug);

    m->niq = u4_tab_add(lane, fid, raz, m->niq);
    return raz;
  }
}
