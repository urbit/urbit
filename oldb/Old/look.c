/* mill/look.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

  /** Forward declarations.
  **/
    static u4_noun
    _look_main(u4_milr, u4_bag, u4_log, u4_type, u4_atom, u4_term);


/* pget: pile get, with unit (0 p) | 0.
*/
static u4_noun
_pget(u4_lane lane,
      u4_noun a, 
      u4_noun b)
{
  if ( u4_n_cell(u4_ch(b)) ) {
    u4_noun c = _pget(lane, a, u4_ch(b));

    if ( u4_n_zero(c) ) {
      return _pget(lane, a, u4_ct(b));
    }
    else return c;
  }
  else return u4_n_eq(a, u4_ch(b)) ? u4_k_cell(lane, u4_noun_0, u4_ct(b)) 
                                   : u4_noun_0;
}

/* pax: pile axis.
*/
static u4_axis
_pax(u4_lane lane,
     u4_noun a,
     u4_noun b, 
     u4_noun c)
{
  if ( u4_n_cell(u4_ch(b)) ) {
    u4_noun d = u4_op_add(lane, c, c);
    u4_noun e = _pax(lane, a, u4_ch(b), d);

    if ( !u4_n_zero(e) ) {
      return e;
    }
    else return _pax(lane, a, u4_ct(b), u4_op_inc(lane, d));
  }
  else return u4_n_eq(a, u4_ch(b)) ? c : u4_noun_0;
}

/* lift: raise (type form) | 0.
*/
static u4_noun
_lift(u4_milr m,
      u4_axis yil,
      u4_noun deg)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(deg) ) {
    return u4_noun_0;
  }
  else {
    u4_type lud = u4_ch(deg);
    u4_form tev = u4_ct(deg);
    u4_noun p_tev, q_tev, pp_tev, pq_tev;

    if ( u4_b_p(tev, u4_noun_0, &p_tev) ) {
      tev = u4_k_cell
        (lane, u4_noun_0, u4_op_peg(lane, yil, p_tev));
    }
    else if ( u4_b_pq(tev, u4_noun_3, &p_tev, &q_tev) &&
              u4_b_p(p_tev, u4_noun_0, &pp_tev) &&
              u4_b_p(q_tev, u4_noun_0, &pq_tev) )
    {
      tev = u4_k_trel
        (lane, u4_noun_3, 
               u4_k_cell(lane, u4_noun_0, u4_op_peg(lane, yil, pp_tev)),
               u4_k_cell(lane, u4_noun_0, u4_op_peg(lane, yil, pq_tev)));

    }
    else {
      tev = _mill_comp(m, u4_k_cell(lane, u4_noun_0, yil), tev);
    }
    return u4_k_cell(lane, lud, tev);
  }
}

/* _mow(): strip at tip.
*/
static u4_log
_mow(u4_milr m,
     u4_bag  hed,
     u4_term nit,
     u4_log  dun)
{
  if ( u4_n_zero(dun) ) {
    return u4_noun_0;
  }
  else {
    u4_type i_dun = u4_ch(dun);
    u4_log  t_dun = u4_ct(dun);
    u4_noun gab = _look_main(m, hed, t_dun, i_dun, u4_noun_0, nit);

    if ( u4_n_zero(gab) ||
         !u4_n_eq(u4_noun_0, u4_cth(gab)) ||
         !u4_n_eq(u4_noun_1, u4_ctt(gab)) )
    {
      return u4_k_cell(m->lane, i_dun, _mow(m, hed, nit, t_dun));
    } 
    else {
      return u4_k_cell(m->lane, u4_ch(gab), _mow(m, hed, nit, t_dun));
    }
  }
}

/* _look_down(): look at a part.
*/
u4_noun
_look_down(u4_milr m,
           u4_bag  hed,
           u4_axis nar,
           u4_log  dun,
           u4_type gaf,
           u4_atom fol,
           u4_term nit)
{
  return _lift
    (m, nar, _look_main(m, hed, _mill_slip(m, nar, dun), gaf, fol, nit));
}

/* _look_cone(): look at a cone.
*/
static u4_noun
_look_cone(u4_milr m,
           u4_bag  hed,
           u4_log  dun,
           u4_type p_zog,
           u4_type q_zog,
           u4_atom fol,
           u4_term nit)
{
  u4_lane lane = m->lane;
  u4_noun zog = u4_k_trel(lane, u4_atom_cone, p_zog, q_zog);
  u4_noun suf = _pget(lane, nit, q_zog);

  if ( u4_n_zero(suf) ) {
    return _look_down(m, hed, u4_noun_2, dun, p_zog, fol, nit);
  }
  else {
    if ( !u4_n_zero(fol) ) {
      return _look_down
        (m, hed, u4_noun_2, dun, p_zog, u4_op_dec(lane, fol), nit);
    }
    else {
      u4_axis mar = _pax(lane, nit, q_zog, u4_noun_3);
      u4_gene fes = u4_ct(suf);
      u4_type rin = _mill_pack(m, dun, zog);
      u4_type zob = u4_k_trel(lane, u4_atom_post, rin, fes);
      u4_form bel = u4_k_trel
        (lane, 
         u4_noun_3, 
         u4_k_cell(lane, u4_noun_0, u4_noun_1), 
         u4_k_cell(lane, u4_noun_0, mar));

      return u4_k_cell(lane, zob, bel);
    }
  }
}

/* _look_skip(): skip a fork branch if true.
*/
static u4_t
_look_skip(u4_milr m,
           u4_bag  hed,
           u4_log  dun,
           u4_type zog)
{
  return 
    _mill_cull(m, zog, dun) 
    || (  (!u4_n_atom(zog) && u4_n_eq(u4_ch(zog), u4_atom_post))
       && (u4_bag_in(_mill_pull(m, dun, zog), hed)) );
}

/* _look_fork(): look at a fork.
*/
static u4_noun
_look_fork(u4_milr m,
           u4_bag  hed,
           u4_log  dun,
           u4_type p_zog,
           u4_type q_zog,
           u4_atom fol,
           u4_term nit)
{
  u4_lane lane = m->lane;

  if ( _look_skip(m, hed, dun, p_zog) ) {
    return _look_main(m, hed, dun, q_zog, fol, nit);
  }
  else if ( _look_skip(m, hed, dun, q_zog) ) {
    return _look_main(m, hed, dun, p_zog, fol, nit);
  }
  else {
    u4_noun bas = _look_main(m, hed, dun, p_zog, fol, nit);
    u4_noun pef = _look_main(m, hed, dun, q_zog, fol, nit);
 
    if ( u4_n_zero(bas) && u4_n_zero(pef) ) {
      return u4_noun_0;
    }
    else if ( u4_n_zero(bas) || u4_n_zero(pef) ) {
      u4_burp(lane, "p_zog", _mill_dump(m, p_zog));
      u4_burp(lane, "q_zog", _mill_dump(m, q_zog));
      u4_burp(lane, "p_pull", _mill_dump(m, _mill_pull(m, dun, p_zog)));
      u4_burp(lane, "q_pull", _mill_dump(m, _mill_pull(m, dun, q_zog)));
      u4_burp(lane, "nit", nit);

      return _mill_fail(m, "fork perforation");
    }
    else {
      u4_type p_bas = u4_ch(bas);
      u4_form q_bas = u4_ct(bas);
      u4_type p_pef = u4_ch(pef);
      u4_form q_pef = u4_ct(pef);

      if ( !u4_n_eq(q_bas, q_pef) ) {
        return _mill_fail(m, "fork subluxation");
      }
      else {
        return u4_k_cell(lane, _mill_eith(m, p_bas, p_pef), q_bas);
      }
    }
  }
}

/* _look_fuse(): look at a fuse.
*/
static u4_noun
_look_fuse(u4_milr m,
           u4_bag  hed,
           u4_log  dun,
           u4_type p_zog,
           u4_type q_zog,
           u4_atom fol,
           u4_term nit)
{
  u4_lane lane = m->lane;
  u4_noun pes  = u4_k_cell(lane, p_zog, dun);
  u4_noun fic  = _look_main(m, hed, dun, p_zog, fol, nit);
  u4_noun vol  = _look_main(m, hed, pes, q_zog, fol, nit);

  if ( u4_n_zero(fic) && u4_n_zero(vol) ) {
    return u4_noun_0;
  }
  else if ( !u4_n_zero(fic) && !u4_n_zero(vol) ) {
    if ( !u4_n_eq(u4_ct(fic), u4_ct(vol)) ) {
      return _mill_fail(m, "fuse conflict");
    }
    else return vol;
  }
  else if ( u4_n_zero(fic) ) {
    return vol;
  }
  else {
    u4_type p_fic = u4_ch(fic);
    u4_form q_fic = u4_ct(fic);

    if ( u4_n_eq(u4_noun_0, u4_ch(q_fic)) ) {
      u4_axis lax = u4_ct(q_fic);
      u4_type tuf = _mill_flap(m, lax, p_fic);
      u4_type vic = _mill_both(m, tuf, q_zog);
      u4_type res = _mill_hack(m, lax, vic);

      return u4_k_cell(lane, res, q_fic);
    }
    else return fic;
  }
}

/* _look_pair(): look at a pair.
*/
static u4_noun
_look_pair(u4_milr m,
           u4_bag  hed,
           u4_log  dun,
           u4_type p_zog,
           u4_type q_zog,
           u4_atom fol,
           u4_term nit)
{
  u4_noun muf = _look_down(m, hed, u4_noun_2, dun, p_zog, fol, nit);

  if ( !u4_n_zero(muf) ) {
    return muf;
  }
  else return _look_down(m, hed, u4_noun_3, dun, q_zog, fol, nit);
}

/* _look_skin(): look at a skin.
*/
static u4_noun
_look_skin(u4_milr m,
           u4_bag  hed,
           u4_log  dun,
           u4_term p_zog,
           u4_type q_zog,
           u4_atom fol,
           u4_term nit)
{
  u4_lane lane = m->lane;

  if ( u4_n_eq(nit, p_zog) ) {
    if ( !u4_n_zero(fol) ) {
      return _look_main(m, hed, dun, q_zog, u4_op_dec(lane, fol), nit);
    }
    else {
      u4_log  zaf = _mow(m, hed, nit, dun);
      u4_type wat = _mill_pull(m, zaf, q_zog);

      return u4_k_trel(lane, wat, u4_noun_0, u4_noun_1);
    }
  }
  return u4_noun_0;
}

/* _look_main(): internal look.  dun is background.  hed stops loops.
**
** Makes ((type form) | 0).
*/
static u4_noun
_look_main(u4_milr m,
           u4_bag  hed,
           u4_log  dun,
           u4_type zog,
           u4_atom fol,
           u4_term nit)
{
  u4_noun p_zog, q_zog;

  if (   u4_n_atom(zog)
      || u4_b_p(zog, u4_atom_cube, 0) 
      || u4_b_pq(zog, u4_atom_bone, &p_zog, &q_zog) )
  {
    return u4_noun_0;
  }
  else if ( u4_b_pq(zog, u4_atom_cone, &p_zog, &q_zog) ) {
    return _look_cone(m, hed, dun, p_zog, q_zog, fol, nit);
  }
  else if ( u4_b_pq(zog, u4_atom_fork, &p_zog, &q_zog) ) {
    return _look_fork(m, hed, dun, p_zog, q_zog, fol, nit);
  }
  else if ( u4_b_pq(zog, u4_atom_fuse, &p_zog, &q_zog) ) {
    return _look_fuse(m, hed, dun, p_zog, q_zog, fol, nit);
  }
  else if ( u4_b_pq(zog, u4_atom_pair, &p_zog, &q_zog) ) {
    return _look_pair(m, hed, dun, p_zog, q_zog, fol, nit);
  }
  else if ( u4_b_pq(zog, u4_atom_post, &p_zog, &q_zog) ) {
    u4_type cuv = _mill_pull(m, dun, zog);

    if ( u4_bag_in(cuv, hed) ) {
      return u4_noun_0;
    }
    else {
      hed = u4_bag_add(m->lane, cuv, hed);

      return _look_main(m, hed, dun, _mill_repo(m, p_zog, q_zog), fol, nit);
    }
  }
  else if ( u4_b_pq(zog, u4_atom_skin, &p_zog, &q_zog) ) {
    return _look_skin(m, hed, dun, p_zog, q_zog, fol, nit);
  }
  else {
    return _look_main(m, hed, dun, _mill_reap(m, zog), fol, nit);
  }
}

/* _look_fault(): path to text.
*/
static u4_sb
_look_fault(u4_lane lane,
            u4_noun nit,
            u4_cl   *buf)
{
  if ( !u4_n_atom(nit) ) {
    return u4_trip;
  }
  else {
    u4_sb sb_nit = u4_a_bin(nit, 3);

    if ( buf ) {
      u4_a_bytes(nit, (u4_xb *)buf, 0, sb_nit + 1);
    }
    return sb_nit;
  }
}

/* _mill_look(): read attribute.
**
** Makes (type form).
*/
u4_noun
_mill_look(u4_milr m,
           u4_type zog,
           u4_atom fol,
           u4_term nit)
{
  u4_noun dum = _look_main(m, u4_noun_0, u4_noun_0, zog, fol, nit);

  if ( u4_n_zero(dum) ) {
    u4_sb sb_nit = _look_fault(m->lane, nit, 0);
    u4_cl *cl_msg = alloca(10 + sb_nit);

    strcpy(cl_msg, "no port: ");
    _look_fault(m->lane, nit, (cl_msg + 9));

    u4_burp(m->lane, "zog", _mill_dump(m, zog));
    // return u4_trip;

    return _mill_fail(m, cl_msg);
  }
  return dum;
}
