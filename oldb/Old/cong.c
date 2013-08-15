/* mill/cong.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

  /** Forward declarations.
  **/
    static u4_t _cong_main(u4_milr, u4_bag, u4_type, u4_type);
 
/* _cong_flat(): test atomicity, with background (raf).
*/
static u4_t
_cong_flat(u4_milr m,
           u4_bag  vit,
           u4_log  raf,
           u4_type gul)
{
  u4_noun p_gul, q_gul;

  if ( u4_n_eq(u4_atom_blot, gul) ) {
    return 1;
  }
  else if ( u4_n_eq(u4_atom_atom, gul) ) {
    return 1;
  }
  else if ( u4_n_eq(u4_atom_blur, gul) ) {
    return 0;
  }
  else if ( u4_b_pq(gul, u4_atom_bone, &p_gul, &q_gul) ) {
    return 0;
  }
  else if ( u4_b_p(gul, u4_atom_cube, &p_gul) ) {
    return u4_n_atom(p_gul);
  }
  else if ( u4_b_pq(gul, u4_atom_fork, &p_gul, &q_gul) ) {
    return ( (_mill_cull(m, p_gul, raf) || _cong_flat(m, vit, raf, p_gul)) &&
             (_mill_cull(m, q_gul, raf) || _cong_flat(m, vit, raf, q_gul)) );
  }
  else if ( u4_b_pq(gul, u4_atom_fuse, &p_gul, &q_gul) ) {
    return _cong_flat(m, vit, raf, p_gul) ||
           _cong_flat(m, vit, u4_k_cell(m->lane, p_gul, raf), q_gul);
  }
  else if ( u4_b_pq(gul, u4_atom_post, &p_gul, &q_gul) ) {
    u4_type laf = _mill_pull(m, raf, gul);

    if ( u4_bag_in(laf, vit) ) {
      return 1;
    }
    else {
      u4_type vaz;

      // printf("\n");
      // u4_burp(m->lane, "cf: type", _mill_dump(m, p_gul));
      // u4_burp(m->lane, "cf: gene", u4_pump_prep(m->lane, q_gul));

      vaz = _mill_repo(m, p_gul, q_gul);
      // u4_burp(m->lane, "cf: repo", _mill_dump(m, vaz));

      return _cong_flat(m, u4_bag_add(m->lane, laf, vit), raf, vaz);
    }
  }
  else return _cong_flat(m, vit, raf, _mill_reap(m, gul));
} 

/* _cong_deep(): test non-atomicity, with background (raf).
*/
static u4_t
_cong_deep(u4_milr m,
           u4_bag  vit,
           u4_log  raf,
           u4_type gul)
{
  u4_noun p_gul, q_gul;

  if ( u4_n_eq(u4_atom_blot, gul) ) {
    return 1;
  }
  else if ( u4_n_eq(u4_atom_atom, gul) ) {
    return 0;
  }
  else if ( u4_n_eq(u4_atom_blur, gul) ) {
    return 0;
  }
  else if ( u4_b_pq(gul, u4_atom_bone, &p_gul, &q_gul) ) {
    return 1;
  }
  else if ( u4_b_p(gul, u4_atom_cube, &p_gul) ) {
    return !u4_n_atom(p_gul);
  }
  else if ( u4_b_pq(gul, u4_atom_fork, &p_gul, &q_gul) ) {
    return ( (_mill_cull(m, p_gul, raf) || _cong_deep(m, vit, raf, p_gul)) &&
             (_mill_cull(m, q_gul, raf) || _cong_deep(m, vit, raf, q_gul)) );
  }
  else if ( u4_b_pq(gul, u4_atom_fuse, &p_gul, &q_gul) ) {
    return _cong_deep(m, vit, raf, p_gul) ||
           _cong_deep(m, vit, u4_k_cell(m->lane, p_gul, raf), q_gul);
  }
  else if ( u4_b_pq(gul, u4_atom_post, &p_gul, &q_gul) ) {
    u4_type laf = _mill_pull(m, raf, gul);

    if ( u4_bag_in(laf, vit) ) {
      return 1;
    }
    else {
      return _cong_deep
        (m, u4_bag_add(m->lane, laf, vit), raf, _mill_repo(m, p_gul, q_gul));
    }
  }
  else return _cong_deep(m, vit, raf, _mill_reap(m, gul));
} 

/* _cong_leaf(): constant test.
*/
static u4_t
_cong_leaf(u4_milr m,
           u4_bag  vit,
           u4_log  raf,
           u4_atom p_mis,
           u4_type gul)
{
  u4_noun p_gul, q_gul;

  if ( u4_n_eq(u4_atom_blot, gul) ) {
    return 1;
  }
  else if ( u4_n_eq(u4_atom_atom, gul) ) {
    return 0;
  }
  else if ( u4_n_eq(u4_atom_blur, gul) ) {
    return 0;
  }
  else if ( u4_b_pq(gul, u4_atom_bone, &p_gul, &q_gul) ) {
    return 0;
  }
  else if ( u4_b_p(gul, u4_atom_cube, &p_gul) ) {
    return u4_n_eq(p_mis, p_gul);
  }
  else if ( u4_b_pq(gul, u4_atom_fork, &p_gul, &q_gul) ) {
    return ( ( _mill_cull(m, p_gul, raf) || 
               _cong_leaf(m, vit, raf, p_mis, p_gul)
             ) &&
             ( _mill_cull(m, q_gul, raf) || 
               _cong_leaf(m, vit, raf, p_mis, q_gul)
             ) );
  }
  else if ( u4_b_pq(gul, u4_atom_fuse, &p_gul, &q_gul) ) {
    return _cong_leaf(m, vit, raf, p_mis, p_gul) ||
           _cong_leaf(m, vit, u4_k_cell(m->lane, p_gul, raf), p_mis, q_gul);
  }
  else if ( u4_b_pq(gul, u4_atom_post, &p_gul, &q_gul) ) {
    u4_type laf = _mill_pull(m, raf, gul);

    if ( u4_bag_in(laf, vit) ) {
      return 1;
    }
    else {
      vit = u4_bag_add(m->lane, laf, vit);
      return _cong_leaf(m, vit, raf, p_mis, _mill_repo(m, p_gul, q_gul));
    }
  }
  else return _cong_leaf(m, vit, raf, p_mis, _mill_reap(m, gul));
}

/* _cong_bone(): congruence to bone.
*/
static u4_t
_cong_bone(u4_milr m,
           u4_bag  zur,
           u4_axis p_mis,
           u4_type q_mis,
           u4_type gul)
{
  if ( u4_n_eq(u4_atom_blot, gul) ) {
    return 1;
  }
  else if ( u4_n_eq(u4_noun_1, p_mis) ) {
    return _cong_main(m, zur, q_mis, gul);
  }
  else {
    if ( !_cong_deep(m, u4_noun_0, u4_noun_0, gul) ) {
      return 0;
    }
    else {
      u4_axis wex = u4_op_tip(p_mis);
      u4_axis hir = u4_op_tap(m->lane, p_mis);
      u4_type fug = _mill_hack(m, wex, gul);

      return _cong_bone(m, zur, hir, q_mis, fug);
    }
  }
}

/* _cong_cone(): congruence to cone; fop is background.
*/
static u4_t
_cong_cone(u4_milr m,
           u4_bag  zur,
           u4_bag  vit,
           u4_type p_mis,
           u4_type q_mis,
           u4_type gul,
           u4_log  fop)
{
  u4_lane lane = m->lane;
  u4_noun p_gul, q_gul;

  if ( u4_n_eq(u4_atom_blot, gul) ) {
    return 1;
  }
  else if (  u4_n_atom(gul) || u4_b_pq(gul, u4_atom_bone, &p_gul, &q_gul) ) {
    return 0;
  }
  else if ( u4_b_pq(gul, u4_atom_cone, &p_gul, &q_gul) ) {
    u4_type nac = _mill_pull(m, _mill_slip(m, u4_noun_2, fop), p_gul);

    return (u4_n_eq(q_mis, q_gul) && _cong_main(m, zur, p_mis, nac));
  }
  else if ( u4_b_pq(gul, u4_atom_fork, &p_gul, &q_gul) ) {
    return
      ( (_mill_cull(m, p_gul, fop) || 
         _cong_cone(m, zur, vit, p_mis, q_mis, p_gul, fop)) &&
        (_mill_cull(m, q_gul, fop) ||
         _cong_cone(m, zur, vit, p_mis, q_mis, q_gul, fop)) );
  }
  else if ( u4_b_pq(gul, u4_atom_fuse, &p_gul, &q_gul) ) {
    u4_log dov = u4_k_cell(lane, p_gul, fop);

    return
      ( _cong_cone(m, zur, vit, p_mis, q_mis, p_gul, fop) ||
        _cong_cone(m, zur, vit, p_mis, q_mis, q_gul, dov) );
  }
  else if ( u4_b_pq(gul, u4_atom_post, &p_gul, &q_gul) ) {
    u4_type laf = _mill_pull(m, fop, gul);

    if ( u4_bag_in(laf, vit) ) {
      return 1;
    }
    else {
      vit = u4_bag_add(m->lane, laf, vit);
      return _cong_cone
        (m, zur, vit, p_mis, q_mis, _mill_repo(m, p_gul, q_gul), fop);
    }
  }
  else {
    return _cong_cone(m, zur, vit, p_mis, q_mis, _mill_reap(m, gul), fop);
  }
}

/* _cong_skin(): congruence to skin; fop is background.
*/
static u4_t
_cong_skin(u4_milr m,
           u4_bag  zur,
           u4_bag  vit,
           u4_term p_mis,
           u4_type q_mis,
           u4_type gul,
           u4_log  fop)
{
  u4_lane lane = m->lane;
  u4_noun p_gul, q_gul;

  if ( u4_n_eq(u4_atom_blot, gul) ) {
    return 1;
  }
  else if ( u4_n_atom(gul) || 
       u4_b_pq(gul, u4_atom_bone, &p_gul, &q_gul) ||
       u4_b_pq(gul, u4_atom_cone, &p_gul, &q_gul) )
  {
    return 0;
  }
  else if ( u4_b_pq(gul, u4_atom_fork, &p_gul, &q_gul) ) {
    return
      ( (_mill_cull(m, p_gul, fop) || 
         _cong_skin(m, zur, vit, p_mis, q_mis, p_gul, fop)) &&
        (_mill_cull(m, q_gul, fop) ||
         _cong_skin(m, zur, vit, p_mis, q_mis, q_gul, fop)) );
  }
  else if ( u4_b_pq(gul, u4_atom_fuse, &p_gul, &q_gul) ) {
    u4_log dov = u4_k_cell(lane, p_gul, fop);

    return
      ( _cong_skin(m, zur, vit, p_mis, q_mis, p_gul, fop) ||
        _cong_skin(m, zur, vit, p_mis, q_mis, q_gul, dov) );
  }
  else if ( u4_b_pq(gul, u4_atom_post, &p_gul, &q_gul) ) {
    u4_type laf = _mill_pull(m, fop, gul);

    if ( u4_bag_in(laf, vit) ) {
      return 1;
    }
    else {
      vit = u4_bag_add(m->lane, laf, vit);
      return _cong_skin
        (m, zur, vit, p_mis, q_mis, _mill_repo(m, p_gul, q_gul), fop);
    }
  }
  else if ( u4_b_pq(gul, u4_atom_skin, &p_gul, &q_gul) ) {
    u4_type ros = _mill_pull(m, fop, q_gul);

    return (u4_n_eq(p_mis, p_gul) && _cong_main(m, zur, q_mis, ros));
  }
  else {
    return _cong_skin(m, zur, vit, p_mis, q_mis, _mill_reap(m, gul), fop);
  }
}


/* _cong_forks(): lhs.
*/
static u4_log
_cong_forks(u4_milr m,
            u4_bag  lut,
            u4_type gav)
{
  u4_lane lane = m->lane;
  u4_noun p_gav, q_gav;

  if ( u4_n_atom(gav) || 
       u4_b_pq(gav,  u4_atom_bone, &p_gav, &q_gav) ||
       u4_b_pq(gav,  u4_atom_cone, &p_gav, &q_gav) ||
       u4_b_p(gav,   u4_atom_cube, &p_gav) ||
       u4_b_pq(gav,  u4_atom_fuse, &p_gav, &q_gav) )
  {
    if ( u4_n_eq(u4_atom_blot, gav) ) {
      return u4_trip;
    }
    return u4_k_cell(lane, gav, u4_noun_0);
  }
  else if ( u4_b_pq(gav, u4_atom_fork, &p_gav, &q_gav) ) {
    u4_log juk = _cong_forks(m, lut, p_gav);
    u4_log ryd = _cong_forks(m, lut, q_gav);

    return u4_log_cat(lane, juk, ryd);
  }
  else if ( u4_b_pq(gav, u4_atom_post, &p_gav, &q_gav) ) {
    if ( u4_bag_in(gav, lut) ) {
      return u4_noun_0;
    }
    else {
      lut = u4_bag_add(lane, gav, lut);

      return _cong_forks(m, lut, _mill_repo(m, p_gav, q_gav));
    }
  }
  else {
    return _cong_forks(m, lut, _mill_reap(m, gav));
  }
}


/* _cong_fork_row(): congruence for each.
*/
static u4_t
_cong_fork_row(u4_milr m,
               u4_bag  zur,
               u4_log  gal,
               u4_type i_hef)
{
  while ( !u4_n_zero(gal) ) {
    u4_type i_gal = u4_ch(gal);
    {
      if ( _cong_main(m, zur, i_gal, i_hef) ) {
        return 1;
      }
    }
    gal = u4_ct(gal);
  }
  return 0;
}

/* _cong_fork_square(): congruence across.
*/
static u4_t
_cong_fork_square(u4_milr m,
                  u4_bag zur,
                  u4_log gal,
                  u4_log hef)
{
  while ( !u4_n_zero(hef) ) {
    u4_type i_hef = u4_ch(hef);
    {
      if ( !_cong_fork_row(m, zur, gal, i_hef) ) {
        return 0;
      }
      hef = u4_ct(hef);
    }
  }
  return 1;
}

/* _cong_main(): congruence, with loop control (zur).
*/
static u4_t
_cong_main(u4_milr m,
           u4_bag  zur,
           u4_type mis,
           u4_type gul)
{
  u4_lane lane = m->lane;
  u4_noun p_mis, q_mis;

  if ( u4_n_eq(u4_atom_blot, gul) ) {
    return 1;
  }
  else if ( u4_n_eq(u4_atom_atom, mis) ) {
    return _cong_flat(m, u4_noun_0, u4_noun_0, gul);
  }
  else if ( u4_n_eq(u4_atom_blur, mis) ) {
    return 1;
  }
  else if ( u4_b_pq(mis, u4_atom_bone, &p_mis, &q_mis) ) {
    return _cong_bone(m, zur, p_mis, q_mis, gul);
  }
  else if ( u4_b_pq(mis, u4_atom_cone, &p_mis, &q_mis) ) {
    return _cong_cone(m, zur, u4_noun_0, p_mis, q_mis, gul, u4_noun_0);
  }
  else if ( u4_b_p(mis, u4_atom_cube, &p_mis) ) {
    if ( u4_n_cell(p_mis) ) {
      u4_noun mol = u4_k_cell(lane, u4_atom_cube, u4_ch(p_mis));
      u4_noun bim = u4_k_cell(lane, u4_atom_cube, u4_ct(p_mis));

      return _cong_deep(m, u4_noun_0, u4_noun_0, gul) &&
             _cong_main(m, zur, mol, _mill_hack(m, u4_noun_2, gul)) &&
             _cong_main(m, zur, bim, _mill_hack(m, u4_noun_3, gul));
    }
    else {
      return _cong_leaf(m, u4_noun_0, u4_noun_0, p_mis, gul);
    }
  }
  else if ( u4_b_pq(mis, u4_atom_fork, &p_mis, &q_mis) ) {
    u4_log vig = _cong_forks(m, u4_noun_0, mis);
    u4_log lec = _cong_forks(m, u4_noun_0, gul);

    /* Frankly and unabashedly conservative.  O(n^2), too.
    */
    return _cong_fork_square(m, zur, vig, lec);
  }
  else if ( u4_b_pq(mis, u4_atom_fuse, &p_mis, &q_mis) ) {
    return ( _cong_main(m, zur, p_mis, gul) &&
             _cong_main(m, zur, q_mis, gul) );
  }
#if 0
  else if ( u4_b_pq(mis, u4_atom_skin, &p_mis, &q_mis) ) {
    return _cong_skin(m, zur, u4_noun_0, p_mis, q_mis, gul, u4_noun_0);
  }
#endif
  else if ( u4_b_pq(mis, u4_atom_post, &p_mis, &q_mis) ) {
    u4_noun res  = u4_k_cell(lane, mis, gul);

    if ( u4_bag_in(res, zur) ) {
      /* Conservative search.
      */
      return 1;
    }
    else {
      zur = u4_bag_add(lane, res, zur);

      return _cong_main(m, zur, _mill_repo(m, p_mis, q_mis), gul);
    }
  }
  else {
    return _cong_main(m, zur, _mill_reap(m, mis), gul);
  }
}

/* _mill_cong(): test congruence.
*/
u4_t
_mill_cong(u4_milr m,
           u4_type mis,
           u4_type gul)
{
  if ( u4_n_eq(mis, gul) ) {
    return 1;
  }
  else if ( _mill_null(m, gul) ) {
    return 1;
  }
  else if ( _mill_null(m, mis) ) {
    return 0;
  }
  else {
    u4_noun qof = u4_k_cell(m->lane, mis, gul);
    u4_nopt zod = u4_tab_get(qof, m->vus);

    if ( zod != u4_bull ) {
      return u4_n_zero(zod);
    }
    else {
      u4_t t_zod = _cong_main(m, u4_noun_0, mis, gul);

      zod = (t_zod ? u4_noun_0 : u4_noun_1);

      m->vus = u4_tab_add(m->lane, qof, zod, m->vus);
      return t_zod;
    }
  }
}
