/* mill/peek.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

  /** Forward declarations.
  **/
    static u4_noun
    _peek_main(u4_milr, u4_bag, u4_bag, u4_log, u4_type, u4_atom, u4_term);

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

/* _peek_down(): peek at a part.
*/
u4_noun
_peek_down(u4_milr m,
           u4_bag  gil,
           u4_bag  hed,
           u4_axis nar,
           u4_log  dun,
           u4_type zog,
           u4_atom fol,
           u4_term nit)
{
  u4_type feq = _mill_slip(m, nar, dun);
  u4_noun yoz = _peek_main(m, gil, hed, feq, zog, fol, nit);

  if ( u4_n_zero(yoz) ) {
    return u4_noun_0;
  }
  else {
    return _mill_comp(m, u4_k_cell(m->lane, u4_noun_0, nar), yoz);
  }
}

/* _peek_cone(): peek at a cone.
*/
static u4_noun
_peek_cone(u4_milr m,
           u4_bag  gil,
           u4_bag  hed,
           u4_log  dun,
           u4_type p_zog,
           u4_type q_zog,
           u4_atom fol,
           u4_term nit)
{
  u4_lane lane = m->lane;
  u4_noun suf = _pget(lane, nit, q_zog);

  if ( u4_n_zero(suf) ) {
    return _peek_down(m, gil, hed, u4_noun_2, dun, p_zog, fol, nit);
  }
  else {
    if ( !u4_n_zero(fol) ) {
      return _peek_down
        (m, gil, hed, u4_noun_2, dun, p_zog, u4_op_dec(lane, fol), nit);
    }
    else {
      u4_axis mar = _pax(lane, nit, q_zog, u4_noun_3);
#if 0
      u4_gene fes = u4_ct(suf);
      u4_type rin = _mill_pack(m, dun, zog);

      if ( !u4_n_zero(m->rux) ) {
        u4_burp(lane, "rin", _mill_dump(m, rin));
        u4_burp(lane, "fes", _mill_dung(m, fes));
      }
      if ( !_mill_safe_in(m, gil, rin, fes) ) {
        return _mill_fail(m, "cone abortion");
      }
      else 
#endif
      return u4_k_trel
        (lane, 
         u4_noun_3, 
         u4_k_cell(lane, u4_noun_0, u4_noun_1), 
         u4_k_cell(lane, u4_noun_0, mar));
    }
  }
}

/* _peek_skip(): skip a fork branch if true.
*/
static u4_t
_peek_skip(u4_milr m,
           u4_bag  hed,
           u4_log  dun,
           u4_type zog)
{
  return 
    _mill_cull(m, zog, dun) 
    || (  (!u4_n_atom(zog) && u4_n_eq(u4_ch(zog), u4_atom_post))
       && (u4_bag_in(_mill_pull(m, dun, zog), hed)) );
}

/* _peek_fork(): peek at a fork.
*/
static u4_noun
_peek_fork(u4_milr m,
           u4_bag  gil,
           u4_bag  hed,
           u4_log  dun,
           u4_type p_zog,
           u4_type q_zog,
           u4_atom fol,
           u4_term nit)
{
  if ( _peek_skip(m, hed, dun, p_zog) ) {
    return _peek_main(m, gil, hed, dun, q_zog, fol, nit);
  }
  else if ( _peek_skip(m, hed, dun, q_zog) ) {
    return _peek_main(m, gil, hed, dun, p_zog, fol, nit);
  }
  else {
    u4_noun bas = _peek_main(m, gil, hed, dun, p_zog, fol, nit);
    u4_noun pef = _peek_main(m, gil, hed, dun, q_zog, fol, nit);
 
    if ( u4_n_zero(bas) && u4_n_zero(pef) ) {
      return u4_noun_0;
    }
    else if ( u4_n_zero(bas) || u4_n_zero(pef) ) {
      u4_burp(m->lane, "p_zog", _mill_dump(m, p_zog));
      u4_burp(m->lane, "q_zog", _mill_dump(m, q_zog));
      u4_err(m->lane, "bas", bas);
      u4_err(m->lane, "pef", pef);

      return _mill_fail(m, "fork perforation");
    }
    else if ( !u4_n_eq(bas, pef) ) {
      return _mill_fail(m, "fork subluxation");
    }
    else return bas;
  }
}

/* _peek_fuse(): peek at a fuse.
*/
static u4_noun
_peek_fuse(u4_milr m,
           u4_bag  gil,
           u4_bag  hed,
           u4_log  dun,
           u4_type p_zog,
           u4_type q_zog,
           u4_atom fol,
           u4_term nit)
{
  u4_lane lane = m->lane;
  u4_noun pes  = u4_k_cell(lane, p_zog, dun);
  u4_noun fic  = _peek_main(m, gil, hed, dun, p_zog, fol, nit);
  u4_noun vol  = _peek_main(m, gil, hed, pes, q_zog, fol, nit);

  if ( u4_n_zero(fic) && u4_n_zero(vol) ) {
    return u4_noun_0;
  }
  else if ( u4_n_zero(fic) ) {
    return vol;
  }
  else if ( u4_n_zero(vol) ) {
    return fic;
  }
  else {
    if ( !u4_n_eq(fic, vol) ) {
      return _mill_fail(m, "fuse conflict");
    }
    else return vol;
  }
}

/* _peek_skin(): peek at a skin.
*/
static u4_noun
_peek_skin(u4_milr m,
           u4_bag  gil,
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
      return _peek_main(m, gil, hed, dun, q_zog, u4_op_dec(lane, fol), nit);
    }
    else {
      return u4_k_cell(lane, u4_noun_0, u4_noun_1);
    }
  }
  return u4_noun_0;
}

/* _peek_pair(): peek at a pair.
*/
static u4_noun
_peek_pair(u4_milr m,
           u4_bag  gil,
           u4_bag  hed,
           u4_log  dun,
           u4_type p_zog,
           u4_type q_zog,
           u4_atom fol,
           u4_term nit)
{
  u4_noun muf = _peek_down(m, gil, hed, u4_noun_2, dun, p_zog, fol, nit);

  if ( !u4_n_zero(muf) ) {
    return muf;
  }
  else return _peek_down(m, gil, hed, u4_noun_3, dun, q_zog, fol, nit);
}

/* _peek_main(): test look, internal.
**
** Produces ?(~ (form)).
*/
static u4_noun
_peek_main(u4_milr m,
           u4_bag  gil,
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
    return _peek_cone(m, gil, hed, dun, p_zog, q_zog, fol, nit);
  }
  else if ( u4_b_pq(zog, u4_atom_fork, &p_zog, &q_zog) ) {
    return _peek_fork(m, gil, hed, dun, p_zog, q_zog, fol, nit);
  }
  else if ( u4_b_pq(zog, u4_atom_fuse, &p_zog, &q_zog) ) {
    return _peek_fuse(m, gil, hed, dun, p_zog, q_zog, fol, nit);
  }
  else if ( u4_b_pq(zog, u4_atom_pair, &p_zog, &q_zog) ) {
    return _peek_pair(m, gil, hed, dun, p_zog, q_zog, fol, nit);
  }
  else if ( u4_b_pq(zog, u4_atom_post, &p_zog, &q_zog) ) {
    u4_type cuv = _mill_pull(m, dun, zog);

    if ( u4_bag_in(cuv, hed) ) {
      return u4_noun_0;
    }
    else {
      hed = u4_bag_add(m->lane, cuv, hed);

      return _peek_main
        (m, gil, hed, dun, _mill_repo(m, p_zog, q_zog), fol, nit);
    }
  }
  else if ( u4_b_pq(zog, u4_atom_skin, &p_zog, &q_zog) ) {
    return _peek_skin(m, gil, hed, dun, p_zog, q_zog, fol, nit);
  }
  else {
    return _peek_main(m, gil, hed, dun, _mill_reap(m, zog), fol, nit);
  }
}

/* _peek_fault(): path to text.
*/
static u4_sb
_peek_fault(u4_lane lane,
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

/* _mill_peek(): test look.
**
** Returns true or not at all.
*/
u4_t
_mill_peek(u4_milr m,
           u4_bag  gil,
           u4_type zog,
           u4_atom fol,
           u4_term nit)
{
  u4_noun rew = _peek_main(m, gil, u4_noun_0, u4_noun_0, zog, fol, nit);

  if ( u4_n_zero(rew) ) {
    u4_sb sb_nit = _peek_fault(m->lane, nit, 0);
    u4_cl *cl_msg = alloca(10 + sb_nit);

    strcpy(cl_msg, "no port: ");
    _peek_fault(m->lane, nit, (cl_msg + 9));

    // u4_burp(m->lane, "zog", _mill_dump(m, zog));
    _mill_fail(m, cl_msg);
    return 0;
  }
  else return 1;
}
