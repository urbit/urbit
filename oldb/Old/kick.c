/* mill/kick.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _kick_face(): return the face (rack or pair) of (typ), a type. 
/* _kick_snap(): adapt (faw), the change, to match (rit), the slot.
*/
static u4_type
_kick_snap(u4_milr m,
           u4_type faw,
           u4_type peq)
{
  if ( u4_b_pq(faw, u4_atom_pair, &p_faw, &q_faw) ) {
  }
}

/* _find_bump(): adapt (faw), the change, to match (peq), the slot.
*/
static u4_type
_find_bump(u4_milr m,
           u4_type peq,
           u4_type faw)
{
  u4_noun p_faw, q_faw, p_peq, q_peq;

  if ( u4_b_pq(faw, u4_atom_pair, &p_faw, &q_faw) ) {
    return u4_k_trel
      (m->lane,
       u4_atom_pair,
       _find_bump(m, _mill_hack(m, u4_noun_2, peq), p_faw),
       _find_bump(m, _mill_hack(m, u4_noun_3, peq), q_faw));
  }
  else if ( u4_b_pq(peq, u4_atom_fuse, &p_peq, &q_peq) ) {
    return _find_bump(m, q_peq, faw);
  }
  else if ( u4_b_pq(peq, u4_atom_skin, &p_peq, &q_peq) ) {
    if ( u4_b_pq(faw, u4_atom_skin, &p_faw, &q_faw) ) {
      if ( u4_n_eq(p_peq, p_faw) ) {
        return u4_k_trel
          (m->lane, p_peq, _find_bump(m, q_peq, q_faw));
      }
    }
    else return u4_k_trel(u4_atom_skin, p_peq, _find_bump(m, q_peq, faw));
  }
  else return faw;
}

/* _find_step(): resolve a path step.  Produce [0 | [type form]].
**
**    peq: type
**    mag: step
*/
static u4_loaf
_find_step(u4_milr m,
           u4_type peq,
           u4_type mag)
{
}

/* _find_base(): 
*/
static u4_loaf
_find_base(u4_milr m,
           u4_bag  gil,
           u4_type naf,
           u4_coat rov,
           u4_form vuz,
           u4_form peq)
{
}

/* _find_loop(): as _mill_find, internally.
**
**    vuz: path formula
**    peq: type at path
*/
static u4_loaf
_find_loop(u4_milr m,
           u4_bag  gil,
           u4_type naf,
           u4_path fes,
           u4_coat rov,
           u4_form vuz,
           u4_type peq)
{
  if ( u4_n_zero(fes) ) {
    u4_axis  
    if ( u4_b
    return _find_base(m, gil, naf, rov, vuz, peq);
  }
  else {
    u4_axis p_vuz;

    if ( !u4_b_p(vuz, u4_noun_0, &p_vuz) ) {
      return _mill_fail(m, "strange change");
    }
    else {
      u4_loaf soq = _find_step(m, peq, u4_ch(fes));

      if ( u4_n_zero(soq) ) {
        return u4_noun_0;
      }
      else {
        u4_type p_soq = u4_ch(soq);
        u4_form q_soq = u4_ct(soq);

        peq = p_soq;
        vuz = _mill_comp(m, vuz, q_soq);
      }
    }
  }
}

/* _mill_find(): seek in path.  Produce [type form].
**
**    gil: set of safe [type gene], or 0 - meaning no safety check.
**    naf: type at top
**    fes: seek path
**    rov: changes
*/
u4_loaf
_mill_find(u4_milr m,
           u4_bag  gil,
           u4_type naf,
           u4_path fes,
           u4_coat rov)
{
  u4_noun pam = _find_loop(m, gil, naf, fes, rov, u4_noun_1, naf);

  if ( u4_n_zero(pam) ) {
    /* XX: add fail diagnostic.
    */
    return _mill_fail(m, "no path");
  }
  else return pam;
}
