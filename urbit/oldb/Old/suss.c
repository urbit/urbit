/* mill/suss.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

  /** Forward declarations.
  **/
    static u4_t
    _bend_suss(u4_milr m, u4_bag gil, u4_noun lor, u4_type naf);

#if 0
  static u4_prep
  _prep_dust_in(u4_milr m,
                u4_noun lor)
  {
    if ( u4_n_zero(lor) ) {
      return u4_noun_0;
    }
    else {
      return 
        u4_k_cell
          (m->lane, 
           u4_k_qual(m->lane, 
                     u4_atom_glue,
                     u4_prep_decimal(m->lane, u4_chh(lor)),
                     _mill_dump(m, u4_cht(lor)),
                     u4_noun_0),
           _prep_dust_in(m, u4_ct(lor)));
    }
  }

  static u4_prep
  _prep_dust(u4_milr m,
             u4_noun lor)
  {
    return u4_k_cell(m->lane, u4_atom_glue, _prep_dust_in(m, lor));
  }
#endif
  

/* _cone_jack()::
*/
static u4_t 
_cone_jack(u4_milr m,
           u4_bag  gil,
           u4_type gox,
           u4_type naf,
           u4_noun wel)
{
  if ( u4_n_atom(u4_ch(wel)) ) {
    u4_gene har = u4_ct(wel);

    if ( !_mill_safe_in(m, gil, gox, har) ) {
      u4_burp(m->lane, "jack: gox", _mill_dump(m, gox));
      u4_burp(m->lane, "jack: har", _mill_dung(m, har));

      return 0;
    }
    else {
      u4_form duf = _mill_make(m, gox, har);
      u4_form vul = _mill_make(m, naf, har);

#if 0
      if ( !u4_n_eq(duf, vul) ) {
        u4_burp(m->lane, "jack: gox", _mill_dump(m, gox));
        u4_burp(m->lane, "jack: naf", _mill_dump(m, naf));
        u4_burp(m->lane, "jack: har", _mill_dung(m, har));

        u4_burp(m->lane, "jack: duf", _mill_durf(m, duf));
        u4_burp(m->lane, "jack: vul", _mill_durf(m, vul));
      }
#endif
      return u4_n_eq(duf, vul);
    }
  }
  else {
    return _cone_jack(m, gil, gox, naf, u4_ch(wel)) &&
           _cone_jack(m, gil, gox, naf, u4_ct(wel));
  }
}

/* _suss_cone()::
*/
static u4_t
_suss_cone(u4_milr m,
           u4_bag  gil,
           u4_log  gam,
           u4_type p_naf,
           u4_noun q_naf)
{
  if ( !_bend_suss(m, gil, gam, p_naf) ) {
    return 0;
  }
  else {
    u4_type fov = _mill_dust(m, gam, p_naf);

    if ( _mill_cong(m, p_naf, fov) ) {
      return 1;
    }
    else {
      u4_type gox = u4_k_trel(m->lane, u4_atom_cone, fov, q_naf);
      u4_type naf = u4_k_trel(m->lane, u4_atom_cone, p_naf, q_naf);

      // printf("jack poly\n");
      _mill_trap(m, "jack choke");

      if ( _cone_jack(m, gil, gox, naf, q_naf) ) {
        _mill_trap(m, 0);

        return 1;
      }
      else {
        _mill_trap(m, 0);
        return _mill_fail(m, "jack mismatch");
      }
    }
  }
}

/* _suss_fall():: suss through (naf).
*/
static u4_t
_suss_fall(u4_milr m,
           u4_bag  gil,
           u4_type gam,
           u4_type nog,
           u4_type naf)
{
  u4_noun p_naf, q_naf;

  if ( u4_n_eq(u4_atom_atom, naf) ||
       u4_n_eq(u4_atom_blot, naf) ||
       u4_n_eq(u4_atom_blur, naf) ||
       u4_b_p(naf, u4_atom_cube, &p_naf) )
  {
    return 1;
  }
  else if ( u4_b_pq(naf, u4_atom_bone, &p_naf, &q_naf) ) {
    return _bend_suss
      (m, gil,
          (u4_n_eq(u4_noun_2, u4_op_tip(p_naf)) ? gam : nog), 
          _mill_flap(m, u4_op_tap(m->lane, p_naf), q_naf)
      );
  }
  else if ( u4_b_pq(naf, u4_atom_cone, &p_naf, &q_naf) ) {
    if ( !u4_n_zero(nog) ) {
      return _mill_fail(m, "cone desecration");
    }
    else return _suss_cone(m, gil, gam, p_naf, q_naf);
  }
  else if ( u4_b_pq(naf, u4_atom_fork, &p_naf, &q_naf) ||
            u4_b_pq(naf, u4_atom_fuse, &p_naf, &q_naf) )
  {
    return _suss_fall(m, gil, gam, nog, p_naf) && 
           _suss_fall(m, gil, gam, nog, q_naf);
  }
  else if ( u4_b_pq(naf, u4_atom_pair, &p_naf, &q_naf) ) {
    return _bend_suss(m, gil, gam, p_naf) &&
           _bend_suss(m, gil, nog, q_naf);
  }
  else return _suss_fall(m, gil, gam, nog, _mill_reap(m, naf));
}

/* _bend_suss(): test correctness of dust.
**
** if suss is true, dust structure will be correct.  otherwise,
** either dust will crash or its results are unusable.
**
** crash-only; never returns false.
**
** lor: (list [axis type])
** naf: type
*/
static u4_t
_bend_suss(u4_milr m,
           u4_bag  gil,
           u4_noun lor,
           u4_type naf)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(lor) ) {
    return 1;
  }
  else {
    u4_noun zif = u4_noun_0;  // zif: *type: hits at axis 1
    u4_noun gam = u4_noun_0;  // gam: *(axis type): hits to the left
    u4_noun nog = u4_noun_0;  // nog: *(axis type): hits to the right

    _mill_trig(lane, lor, &zif, &gam, &nog);

    if ( !u4_n_zero(zif) ) {
      if ( !u4_n_zero(u4_ct(zif)) || !u4_n_zero(gam) || !u4_n_zero(nog) ) {
        return _mill_fail(m, "suss interference");
      }
      else return 1;
    }
    else return _suss_fall(m, gil, gam, nog, naf);
  }
}

/* _bend_molt(): examine and compile change list [axis type]
*/
static u4_log
_bend_molt(u4_milr m, 
           u4_bag  gil,
           u4_type cav, 
           u4_type naf,
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
      if ( !_mill_safe_in(m, gil, cav, pi_sol) ||
           !_mill_safe_in(m, gil, naf, qi_sol) )
      {
        /* Taking advantage of crash-only.
        */
        return u4_noun_0;
      }
      else {
        return u4_k_cell
          (lane, 
           u4_k_cell
            (lane, 
             _mill_hook(m, cav, pi_sol),
             _mill_play(m, naf, qi_sol)
            ),
           _bend_molt(m, gil, cav, naf, t_sol)
          );
      }
    }
  }
}

/* _bend_dig(): convert molt list for dust use [UGLY].
*/
static u4_log
_bend_dig(u4_milr m,
          u4_axis lub,
          u4_log  giz)
{
  if ( u4_n_zero(giz) ) {
    return u4_noun_0;
  } else {
    u4_noun p_giz = u4_ch(giz);
    u4_axis pp_giz = u4_ch(p_giz);
    u4_type qp_giz = u4_ct(p_giz);

    return u4_k_cell
        (m->lane, 
         u4_k_cell(m->lane, u4_op_peg(m->lane, lub, pp_giz), qp_giz), 
         _bend_dig(m, lub, u4_ct(giz)));
  }
}
              
/* _mill_suss(): test dust safety.
*/
u4_t
_mill_suss(u4_milr m, 
           u4_bag  gil,
           u4_type naf,
           u4_log  p_dug,
           u4_gene q_dug)
{
  if ( !_mill_safe_in(m, gil, naf, q_dug) ) {
    return 0;
  }
  else {
    u4_noun tul = _mill_bark(m, naf, q_dug);
    u4_axis lub = u4_ch(tul);
    u4_type cav = u4_ct(tul);
    u4_log  giz = _bend_molt(m, gil, cav, naf, p_dug);

    if ( !_bend_suss(m, gil, giz, cav) ) {
      return 0;
    }
    else {
      u4_log  lor = _bend_dig(m, lub, giz);
      u4_type dex = _mill_dust(m, lor, naf);

      return _mill_safe_in(m, gil, dex, q_dug);
    }
  }
}
