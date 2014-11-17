/* g/j.c
**
** This file is in the public domain.
*/
#include "all.h"

  /* _cj_count(): count and link dashboard entries.
  */
  static c3_w 
  _cj_count(u3j_core* par_u, u3j_core* dev_u)
  {
    c3_w len_l = 0;
    c3_w i_w;

    if ( dev_u ) {
      for ( i_w = 0; 0 != dev_u[i_w].cos_c; i_w++ ) {
        u3j_core* kid_u = &dev_u[i_w];

        kid_u->par_u = par_u;
        len_l += _cj_count(kid_u, kid_u->dev_u);
      }
    }
    return 1 + len_l;
  }
  /* _cj_install(): install dashboard entries.
  */
  static c3_w
  _cj_install(u3j_core* ray_u, c3_w jax_l, u3j_core* dev_u)
  {
    c3_w i_w;

    if ( dev_u ) {
      for ( i_w = 0; 0 != dev_u[i_w].cos_c; i_w++ ) {
        u3j_core* kid_u = &dev_u[i_w];

        kid_u->jax_l = jax_l;
        ray_u[jax_l++] = *kid_u;

        jax_l = _cj_install(ray_u, jax_l, kid_u->dev_u);
      }
    }
    return jax_l;
  }

/* _cj_axis(): axis from formula, or 0.  `fol` is RETAINED.
*/
static c3_l
_cj_axis(u3_noun fol)
{
  u3_noun p_fol, q_fol, r_fol;

  while ( _(u3du(fol)) && (10 == u3h(fol)) )
    { fol = u3t(u3t(fol)); }

  if ( !_(u3r_trel(fol, &p_fol, &q_fol, &r_fol)) ) {
    if ( !_(u3r_cell(fol, &p_fol, &q_fol)) ||
         (0 != p_fol) ||
         (!_(u3a_is_cat(q_fol))) )
    { 
      fprintf(stderr, "axis: bad a\r\n"); 
      return 0;
    }
    return q_fol;
  }
  else {
    if ( 9 != p_fol )
      { fprintf(stderr, "axis: bad b\r\n"); return 0; }
    if ( !_(u3a_is_cat(q_fol)) )
      { fprintf(stderr, "axis: bad c\r\n"); return 0; }
    if ( !_(u3du(r_fol)) || (0 != u3h(r_fol)) || (1 != u3t(r_fol)) )
      { fprintf(stderr, "axis: bad d\r\n"); return 0; }

    return q_fol;
  }
}

/* _cj_by_gut(): (~(get by a) b), unifying; RETAINS a, b, AND result.
*/
static u3_weak
_cj_by_gut(u3_noun a, u3_noun b)
{
  if ( u3_nul == a ) {
    return u3_none;
  }
  else {
    u3_noun l_a, n_a, r_a;
    u3_noun pn_a, qn_a;

    u3x_trel(a, &n_a, &l_a, &r_a);
    u3x_cell(n_a, &pn_a, &qn_a);
    {
      if ( (c3y == u3r_sing(b, pn_a)) ) {
        return qn_a;
      }
      else {
        if ( c3y == u3qc_gor(b, pn_a) ) {
          return _cj_by_gut(l_a, b);
        }
        else return _cj_by_gut(r_a, b);
      }
    }
  }
}

/* _cj_chum(): decode chum as string.
*/
static c3_c* 
_cj_chum(u3_noun chu)
{
  if ( _(u3ud(chu)) ) {
    return u3r_string(chu);
  } 
  else {
    u3_noun h_chu = u3h(chu);
    u3_noun t_chu = u3t(chu);
    
    if ( !_(u3a_is_cat(t_chu)) ) {
      return 0;
    } else {
      c3_c* h_chu_c = u3r_string(h_chu);
      c3_c  buf[33];

      memset(buf, 0, 33);
      snprintf(buf, 32, "%s%d", h_chu_c, t_chu);

      free(h_chu_c);
      return strdup(buf);
    }
  }
}

/* _cj_je_fsck: fsck:je, or none.
*/
static u3_noun
_cj_je_fsck(u3_noun clu)
{
  u3_noun     p_clu, q_clu, r_clu;
  u3_noun     huk;
  c3_c*       nam_c;
  c3_l        axe_l;

  if ( c3n == u3r_trel(clu, &p_clu, &q_clu, &r_clu) ) {
    u3z(clu); return u3_none;
  }
  if ( 0 == (nam_c = _cj_chum(p_clu)) ) {
    u3z(clu); return u3_none;
  }
  while ( _(u3du(q_clu)) && (10 == u3h(q_clu)) ) { 
    q_clu = u3t(u3t(q_clu));
  }
  if ( !_(u3du(q_clu)) ) {
    u3z(clu); free(nam_c); return u3_none;
  }

  if ( (1 == u3h(q_clu)) && (0 == u3t(q_clu)) ) {
    axe_l = 0;
  }
  else {
    if ( (0 != u3h(q_clu)) || !_(u3a_is_cat(axe_l = u3t(q_clu))) ) {
      u3z(clu); free(nam_c); return u3_none;
    }
  }

  {
    huk = 0;

    while ( _(u3du(r_clu)) ) {
      u3_noun ir_clu, tr_clu, pir_clu, qir_clu;

      if ( (c3n == u3r_cell(r_clu, &ir_clu, &tr_clu)) ||
           (c3n == u3r_cell(ir_clu, &pir_clu, &qir_clu)) ||
           (c3n == u3ud(pir_clu)) )
      {
        u3z(huk); u3z(clu); free(nam_c); return u3_none;
      }
      huk = u3kdb_put(huk, u3k(pir_clu), u3k(qir_clu));
      r_clu = tr_clu;
    }
  }
  u3z(clu);
  return u3nt(u3i_string(nam_c), axe_l, huk);
}

/* _cj_sham(): ++sham.
*/
static u3_atom
_cj_sham(u3_noun som)       //  XX wrong, does not match ++sham
{
  u3_atom jam = u3ke_jam(som);
  u3_noun sha = u3qe_shax(jam);
  u3_noun haf = u3qc_end(7, 1, sha);

  u3z(jam); u3z(sha);
  return haf;
}

/* _cj_cold_find_sys: search `sys` in dashboard.  RETAINS `bat` AND result.
*/
static u3_weak
_cj_cold_find_sys(u3_noun bat)
{
  u3a_road* rod_u = u3R;

  while ( 1 ) {
    u3_noun pro = _cj_by_gut(u3h(rod_u->jed.das), bat);

    if ( pro != u3_none ) {
      return pro;
    }
 
    if ( !rod_u->par_u ) break;
    rod_u = rod_u->par_u; 
  }
  return u3_none;
}

/* _cj_cold_mine(): in cold mode, declare a core.  RETAINS.
*/
static u3_weak
_cj_cold_mine(u3_noun cey, u3_noun cor)
{
  u3_noun         bat = u3h(cor);
  u3_noun         p_cey, q_cey, r_cey;

  u3r_trel(cey, &p_cey, &q_cey, &r_cey);
  {
    /* Calculate semantic identity (mop).
    */
    u3_noun mop;
    
    if ( 0 == q_cey ) {
      mop = u3nq(u3k(p_cey), 3, c3n, u3k(bat));
    }
    else {
      u3_weak rah = u3r_at(q_cey, cor);

      if ( (u3_none == rah) || !_(u3du(rah)) ) {
        fprintf(stderr, "fund: %s is bogus\r\n", u3r_string(p_cey));
        return u3_none;
      }
      else {
        u3_noun soh = _cj_cold_find_sys(u3h(rah));

        if ( u3_none == soh ) {
          fprintf(stderr, "fund: in %s, parent %x not found at %d\r\n", 
                          u3r_string(p_cey),
                          u3r_mug(u3h(rah)),
                          q_cey);
          return u3_none;
        }
        else {
          mop = u3nq(u3k(p_cey), u3k(q_cey), c3y, u3k(soh));
        }
      }
    }

    //  Assemble new core pattern.
    //
    {
      u3_noun soh = _cj_sham(u3k(mop));
      u3_noun hoe = u3kdb_get(u3k(u3t(u3R->jed.das)), u3k(soh));
      u3_noun cub = u3nc(u3_nul, u3k(r_cey));
      u3_noun sab;

      if ( u3_none == hoe ) {
        sab = u3nt(u3nc(u3k(bat), cub), u3_nul, u3_nul);
      }
      else {
        sab = u3kdb_put(u3k(u3t(hoe)), u3k(bat), cub);
        u3z(hoe);
      }
      {
        u3_noun sad, h_sad, t_sad;

        h_sad = u3kdb_put(u3k(u3h(u3R->jed.das)), u3k(bat), u3k(soh));
        t_sad = u3kdb_put(u3k(u3t(u3R->jed.das)), soh, u3nc(u3k(mop), sab));
        sad = u3nc(h_sad, t_sad);

        u3z(u3R->jed.das);
        u3R->jed.das = sad;
      }


      return mop;
    }
  }
}

/* _cj_warm_fend(): in warm state, return u3_none or calx.  RETAINS.
*/
u3_weak
_cj_warm_fend(u3_noun bat)
{
  u3a_road* rod_u = u3R;

  while ( 1 ) {
    u3_weak jaw = u3h_gut(rod_u->jed.har_p, bat);

    if ( u3_none != jaw ) {
      return jaw;
    }

    if ( rod_u->par_u ) {
      rod_u = rod_u->par_u;
    }
    else return u3_none;
  }
}

/* _cj_warm_hump(): generate axis-to-arm map.  RETAIN.
*/
static u3_noun
_cj_warm_hump(c3_l jax_l, u3_noun huc)
{
  u3_noun     hap = u3_nul;
  u3j_core* cop_u;

  /* Compute axes of all correctly declared arms.
  */
  if ( jax_l && (cop_u = &u3D.ray_u[jax_l])->arm_u ) {
    u3j_harm* jet_u;
    c3_l        i_l;

    for ( i_l = 0; (jet_u = &cop_u->arm_u[i_l])->fcs_c; i_l++ ) {
      c3_l axe_l = 0;

      if ( '.' == *(jet_u->fcs_c) ) {
        c3_d axe_d = 0;

        if ( (1 != sscanf(jet_u->fcs_c+1, "%llu", &axe_d)) ||
             axe_d >> 32ULL ||
             ((1 << 31) & (axe_l = (c3_w)axe_d)) ||
             (axe_l < 2) )
        {
          fprintf(stderr, "jets: activate: bad fcs %s\r\n", jet_u->fcs_c);
        }
      }
      else {
        u3_noun nam = u3i_string(jet_u->fcs_c);
        u3_noun fol = u3kdb_get(u3k(huc), nam);

        if ( u3_none == fol ) {
          fprintf(stderr, "jets: activate: bad fcs %s\r\n", jet_u->fcs_c);
        }
        else {
          axe_l = _cj_axis(fol);
          u3z(fol);
        }
      }
      if ( 0 != axe_l ) {
        hap = u3kdb_put(hap, axe_l, i_l);
      }
    }
  }
  return hap;
}

/* _cj_boil_mean(): in parent, declare a core.  RETAINS.
**
** XX bat is used only for printing, remove.
*/
static c3_l
_cj_boil_mean(c3_l par_l, u3_noun mop, u3_noun bat)
{
  u3j_core* par_u;
  u3j_core* dev_u;

  if ( 0 != par_l ) {
    par_u = &u3D.ray_u[par_l];
    dev_u = par_u->dev_u;
  }
    else {
    par_u = 0;
    dev_u = u3D.dev_u;
  }

  {
    c3_w i_l = 0;
    u3j_core* cop_u;

    while ( (cop_u = &dev_u[i_l])->cos_c ) {
      if ( _(u3r_sing_c(cop_u->cos_c, u3h(mop))) ) {
#if 0
        fprintf(stderr, "boil: bound jet %d/%s/%s/%x\r\n", 
                        cop_u->jax_l, 
                        cop_u->cos_c,
                        par_u ? par_u->cos_c : "~",
                        u3r_mug(bat));
#endif
        return cop_u->jax_l;
      }
      i_l++;
    }
  }
  return 0;
}

/* _cj_boil_mine(): in boiling state, declare a core.  RETAINS.
*/
static c3_l
_cj_boil_mine(u3_noun mop, u3_noun cor)
{
  u3_noun p_mop, q_mop, r_mop, hr_mop, tr_mop;

  u3x_trel(mop, &p_mop, &q_mop, &r_mop);
  u3x_cell(r_mop, &hr_mop, &tr_mop);
  {
    c3_l par_l;

    //  Calculate parent axis.
    //
    if ( c3y == hr_mop ) {
      u3_noun cax = _cj_warm_fend(u3h(u3r_at(q_mop, cor)));

      par_l = u3h(u3h(cax));
      u3z(cax);
    }
    else par_l = 0;

    return _cj_boil_mean(par_l, mop, u3h(cor));
  }
}

static c3_l _cj_warm_ream_at(u3_noun soh, u3_noun cag);

/* _cj_warm_ream_be(): install battery; RETAINS.
*/
static void
_cj_warm_ream_be(c3_l    jax_l,
                  u3_noun pax,
                  u3_noun bat,
                  u3_noun huc)
{
  u3h_put(u3R->jed.har_p,
            bat,
            u3nt(u3nt(jax_l, u3k(pax), _cj_warm_hump(jax_l, huc)),
                 u3_nul,
                 u3k(huc)));
}

/* _cj_warm_ream_is(): reream battery; RETAINS.
*/
static void
_cj_warm_ream_is(c3_l    jax_l, 
                  u3_noun pax,
                  u3_noun sab)
{
  if ( u3_nul != sab ) {
    u3_noun n_sab, l_sab, r_sab, pn_sab, qn_sab;

    u3x_trel(sab, &n_sab, &l_sab, &r_sab);
    u3x_cell(n_sab, &pn_sab, &qn_sab);

    _cj_warm_ream_be(jax_l, pax, pn_sab, u3t(qn_sab));
    _cj_warm_ream_is(jax_l, pax, l_sab);
    _cj_warm_ream_is(jax_l, pax, r_sab);
  }
}

/* _cj_warm_ream_un(): reream under `soh`; RETAINS.
*/
static c3_l
_cj_warm_ream_un(u3_noun soh)
{
  u3_noun cag = u3kdb_got(u3k(u3t(u3R->jed.das)), u3k(soh));
  u3_noun sab = u3t(cag);
  u3_noun cax;
  c3_l    jax_l;

  if ( u3_none != (cax = u3h_get(u3R->jed.har_p, u3h(u3h(sab)))) ) {
    jax_l = u3h(u3h(cax));
    u3z(cax);
  }
  else {
    jax_l = _cj_warm_ream_at(soh, cag);
  }
  u3z(cag);
  return jax_l;
}

/* _cj_warm_ream_at(): reream at `soh` and `cag`; RETAINS.
*/
static c3_l
_cj_warm_ream_at(u3_noun soh, u3_noun cag)
{
  u3_noun mop = u3h(cag);
  u3_noun sab = u3t(cag);
  u3_noun p_mop, q_mop, r_mop, hr_mop, tr_mop;

  u3x_trel(mop, &p_mop, &q_mop, &r_mop);
  u3x_cell(r_mop, &hr_mop, &tr_mop);
  {
    c3_l par_l, jax_l;

    if ( c3y == hr_mop ) {
      par_l = _cj_warm_ream_un(tr_mop);
    }
    else par_l = 0;

    jax_l = _cj_boil_mean(par_l, mop, 0);
   
    _cj_warm_ream_is(jax_l, q_mop, sab);
    return jax_l;
  }
} 

/* _cj_warm_ream_in(): reream in `taw`; RETAINS.
*/
static void
_cj_warm_ream_in(u3_noun taw)
{
  if ( u3_nul != taw ) {
    u3_noun n_taw, l_taw, r_taw, pn_taw, qn_taw;

    u3x_trel(taw, &n_taw, &l_taw, &r_taw);
    u3x_cell(n_taw, &pn_taw, &qn_taw);

    _cj_warm_ream_at(pn_taw, qn_taw);
    _cj_warm_ream_in(l_taw);
    _cj_warm_ream_in(r_taw);
  }
}

/* _cj_warm_ream(): reream warm from cold state.
*/
static void
_cj_warm_ream(void)
{
  c3_assert(u3R == &(u3H->rod_u));
  {
    _cj_warm_ream_in(u3t(u3R->jed.das));
  }
}

/* u3j_ream(): reream after restoring from checkpoint.
*/
void
u3j_ream(void)
{
  u3h_free(u3R->jed.har_p);
  u3R->jed.har_p = u3h_new();

  _cj_warm_ream();
}

/* _cj_warm_mine(): in warm mode, declare a core.  
*/
static void
_cj_warm_mine(u3_noun clu, u3_noun cor)
{
  u3_noun bat = u3h(cor);
  u3_noun cax;

  if ( !_(u3du(cor)) ) {
    u3z(clu);
  }
  else if ( u3_none != (cax = _cj_warm_fend(bat)) ) {
    u3z(cax); u3z(clu);
  }
  else {
    u3_noun cey = _cj_je_fsck(clu);

    // fprintf(stderr, "warm_mine %s\r\n", u3r_string(u3h(cey)));

    if ( u3_none != cey ) {
      u3_noun huc = u3t(u3t(cey));
      u3_noun pax = u3h(u3t(cey));
      u3_noun mop;

      if ( u3_none != (mop = _cj_cold_mine(cey, cor)) ) {
        c3_l jax_l = _cj_boil_mine(mop, cor);

        // fprintf(stderr, "warm: bat %x\r\n", u3r_mug(bat));
        u3h_put(u3R->jed.har_p, 
                  bat, 
                  u3nt(u3nt(jax_l, u3k(pax), _cj_warm_hump(jax_l, huc)),
                       u3_nul,
                       u3k(huc)));
        u3z(mop);
      }
      u3z(cey);
    }
  }
  u3z(cor);
}

/* u3j_boot(): initialize jet system.
*/
void
u3j_boot(void)
{
  c3_w jax_l;

  u3D.len_l =_cj_count(0, u3D.dev_u);
  u3D.all_l = (2 * u3D.len_l) + 1024;     //  horrid heuristic

  u3D.ray_u = (u3j_core*) malloc(u3D.all_l * sizeof(u3j_core));
  memset(u3D.ray_u, 0, (u3D.all_l * sizeof(u3j_core)));

  jax_l = _cj_install(u3D.ray_u, 1, u3D.dev_u);
  fprintf(stderr, "boot: installed %d jets\n", jax_l);
}

/* _cj_find(): search for jet, old school.  `bat` is RETAINED.
*/
c3_l
_cj_find(u3_noun bat)
{
  u3a_road* rod_u = u3R;

  while ( 1 ) {
    u3_weak jaw = u3h_gut(rod_u->jed.har_p, bat);

    if ( u3_none != jaw ) {
      u3_assure(u3a_is_cat(u3h(jaw)));

#if 0
      if ( rod_u != u3R ) {
        fprintf(stderr, "got: %x in %p/%p, %d\r\n", 
            bat, rod_u, rod_u->jed.har_p, jax);
      }
#endif
      return (c3_l)u3h(jaw);
    }
    if ( rod_u->par_u ) {
      rod_u = rod_u->par_u;
    }
    else return 0;
  }
}

/* u3j_find(): search for jet.  `bat` is RETAINED.
*/
c3_l
u3j_find(u3_noun bat)
{
  return _cj_find(bat);
}

/* _cj_soft(): kick softly by arm axis.
*/
static u3_noun
_cj_soft(u3_noun cor, u3_noun axe)
{
  u3_noun arm = u3x_at(axe, cor);

  return u3n_nock_on(cor, u3k(arm));
}

extern int SLAY;

/* _cj_kick_z(): try to kick by jet.  If no kick, produce u3_none.
**
** `cor` is RETAINED iff there is no kick, TRANSFERRED if one.  
** `axe` is RETAINED.
*/
static u3_weak
_cj_kick_z(u3_noun cor, u3j_core* cop_u, u3j_harm* ham_u, u3_atom axe)
{
  if ( 0 == ham_u->fun_f ) {
    return u3_none;
  }

  if ( !_(ham_u->liv) ) {
    return u3_none;
  }
  else {
    if ( _(ham_u->ice) ) {
      u3_weak pro = ham_u->fun_f(cor);

      if ( u3_none != pro ) {
        u3z(cor);
        return pro;
      }
    }
    else {
      u3_weak pro, ame;

      ham_u->ice = c3y;
      pro = ham_u->fun_f(u3k(cor));
      ham_u->ice = c3n;

      if ( u3_none == pro ) {
        u3z(cor);
        return pro;
      }
      ham_u->liv = c3n;
      ame = _cj_soft(cor, axe);
      ham_u->liv = c3y;

      if ( c3n == u3r_sing(ame, pro) ) {
        fprintf(stderr, "test: %s %s: mismatch: good %x, bad %x\r\n",
               cop_u->cos_c,
               (!strcmp(".2", ham_u->fcs_c)) ? "$" : ham_u->fcs_c,
               u3r_mug(ame), 
               u3r_mug(pro));
       
        c3_assert(0);
        return u3m_bail(c3__fail);
      }
      else {
#if 1
        fprintf(stderr, "test: %s %s\r\n",
               cop_u->cos_c,
               (!strcmp(".2", ham_u->fcs_c)) ? "$" : ham_u->fcs_c);
#endif
      }
    }
    return u3_none;
  }
}

/* _cj_hook_in(): execute hook from core, or fail.
*/
static u3_noun
_cj_hook_in(u3_noun     cor,
            const c3_c* tam_c,
            c3_o        jet_o)
{
  u3_noun bat = u3h(cor);

  if ( !_(u3du(cor)) ) { return u3m_bail(c3__fail); }
  {
    u3_weak cax = _cj_warm_fend(bat);

    if ( u3_none == cax ) { return u3m_bail(c3__fail); }
    {
      u3_noun p_cax, q_cax, r_cax;
      u3_noun jax, pax, ext, huc, hap;

      u3x_trel(cax, &p_cax, &q_cax, &r_cax);
      u3x_trel(p_cax, &jax, &pax, &hap);
      ext = q_cax;
      huc = r_cax;
      {
        c3_l        jax_l = jax;
        u3j_core* cop_u = &u3D.ray_u[jax_l];
        u3_noun     fol   = u3kdb_get(u3k(huc), u3i_string(tam_c));

        if ( u3_none == fol ) {
          //  The caller wants a deeper core.
          //
          if ( 0 == pax ) { return u3m_bail(c3__fail); }
          else {
            u3_noun inn = u3k(u3x_at(pax, cor));

            u3z(cax); u3z(cor);
            return _cj_hook_in(inn, tam_c, jet_o);
          }
        }
        else {
          u3_noun pro;
          c3_l    axe_l = _cj_axis(fol);
          c3_l    inx_l;
 
          if ( (0 == axe_l) || 
               (c3n == jet_o) ||
               (u3_none == (inx_l = u3kdb_get(u3k(hap), axe_l))) ||
               (u3_none == (pro = _cj_kick_z(cor, 
                                             cop_u, 
                                             &cop_u->arm_u[inx_l],
                                             axe_l))) )
          { 
            if ( 0 == axe_l ) {
              u3z(cax); 
              return u3n_nock_on(cor, fol);
            } else {
              //  Tricky: the above case would work here too, but would
              //  disable jet_o and create some infinite recursions.
              //
              u3z(cax); u3z(fol);
              return u3n_nock_on(cor, u3k(u3x_at(axe_l, cor)));
            }
          }
          else {
            u3z(cax); u3z(fol);
            return pro;
          }
        }
      }
    }
  }
}

/* u3j_soft(): execute soft hook.
*/
u3_noun
u3j_soft(u3_noun cor, 
           const c3_c* tam_c)
{
  u3_noun pro;

  pro = _cj_hook_in(cor, tam_c, c3n);
  return pro;
}

/* u3j_hook(): execute hook from core, or fail.
*/
u3_noun
u3j_hook(u3_noun     cor,
           const c3_c* tam_c)
{
  u3_noun pro;

  pro = _cj_hook_in(cor, tam_c, c3y);
  return pro;
}

/* u3j_kick(): new kick.
**
** `axe` is RETAINED by the caller; `cor` is RETAINED iff there 
** is no kick, TRANSFERRED if one.
*/
u3_weak
u3j_kick(u3_noun cor, u3_noun axe)
{
  if ( !_(u3du(cor)) ) { return u3_none; } 
  {
    u3_noun bat = u3h(cor);
    u3_weak cax = _cj_warm_fend(bat);

    if ( u3_none == cax ) { return u3_none; }
    {
      u3_noun hap = u3t(u3t(u3h(cax)));
      u3_noun inx = u3kdb_get(u3k(hap), u3k(axe));

      if ( u3_none == inx ) {
        u3z(cax); return u3_none;
      }
      else {
        c3_l jax_l      = u3h(u3h(cax));
        u3j_core* cop_u = &u3D.ray_u[jax_l];
        c3_l inx_l      = inx;
        u3j_harm* ham_u = &cop_u->arm_u[inx_l];
        u3_noun pro;

        u3z(cax);
        pro = _cj_kick_z(cor, cop_u, ham_u, axe);

        return pro;
      }
    }
  }
}

/* u3j_kink(): kick either by jet or by nock.
*/
u3_noun
u3j_kink(u3_noun cor,
           u3_noun axe)
{
  u3_weak pro = u3j_kick(cor, axe);

  if ( u3_none != pro ) {
    return pro;
  } else {
    return u3n_nock_on(cor, u3nq(9, axe, 0, 1));
  }
}

/* u3j_mine(): register core for jets.  Produce registered core.
*/
void
u3j_mine(u3_noun clu, u3_noun cor)
{
  _cj_warm_mine(clu, cor);
}

/* _cj_cold_reap_un: re-register clog map.  RETAIN but TRANSFER `sys`.
*/
static u3_noun 
_cj_cold_reap_un(u3_noun soh, u3_noun sab, u3_noun sys)
{
  if ( u3_nul == sab ) {
    return sys;
  } 
  else {
    u3_noun n_sab, l_sab, r_sab, pn_sab, qn_sab;

    u3x_trel(sab, &n_sab, &l_sab, &r_sab);
    u3x_cell(n_sab, &pn_sab, &qn_sab);
    {
      sys = _cj_cold_reap_un(soh, l_sab, sys);
      sys = _cj_cold_reap_un(soh, r_sab, sys);
      sys = u3kdb_put(sys, u3a_take(pn_sab), u3k(soh));

      return sys;
    }
  }
}

/* _cj_cold_reap_to: reap clog map.  RETAINS `sab`, TRANSFERS `bas`.
*/
static u3_noun
_cj_cold_reap_to(u3_noun sab, u3_noun bas)
{
  if ( u3_nul == sab ) {
    return bas;
  } 
  else {
    u3_noun n_sab, l_sab, r_sab, pn_sab, qn_sab;

    u3x_trel(sab, &n_sab, &l_sab, &r_sab);
    u3x_cell(n_sab, &pn_sab, &qn_sab);
    {
      bas = _cj_cold_reap_to(l_sab, bas);
      bas = _cj_cold_reap_to(r_sab, bas);
     
      //  If the battery is not junior, or if it has been
      //  already collected for the product, promote it.
      //
      if ( _(u3a_left(pn_sab)) ) {
        u3_noun bat = u3a_take(pn_sab);

        bas = u3kdb_put(bas, bat, u3a_take(qn_sab));
      }
      return bas;
    }
  }
}

/* _cj_cold_reap_at(): reap haw node.  RETAINS.
*/
static void
_cj_cold_reap_at(u3_noun soh, u3_noun cag)
{
  u3_noun sab = _cj_cold_reap_to(u3t(cag), u3_nul);

  if ( u3_nul != sab ) {
    u3_noun sys, haw, das;

    soh = u3a_take(soh);
    cag = u3nc(u3a_take(u3h(cag)), sab);

    sys = _cj_cold_reap_un(soh, sab, u3k(u3h(u3R->jed.das)));
    haw = u3kdb_put(u3k(u3t(u3R->jed.das)), soh, cag);

    das = u3nc(sys, haw);
    u3z(u3R->jed.das);

    u3R->jed.das = das;
  }
}

/* _cj_cold_reap_in(): reap in (junior) haw.  RETAINS.
*/
static void
_cj_cold_reap_in(u3_noun taw)
{
  if ( u3_nul != taw ) {
    u3_noun n_taw, l_taw, r_taw, pn_taw, qn_taw;

    u3x_trel(taw, &n_taw, &l_taw, &r_taw);
    u3x_cell(n_taw, &pn_taw, &qn_taw);

    _cj_cold_reap_at(pn_taw, qn_taw);
    _cj_cold_reap_in(l_taw);
    _cj_cold_reap_in(r_taw);
  }
}

/* _cj_warm_reap(): reap key and value from warm table.
*/
static void
_cj_warm_reap(u3_noun kev)
{
  u3_noun bat = u3h(kev);
  u3_noun cax = u3t(kev);

  if ( _(u3a_left(bat)) ) {
    u3_noun tab = u3a_take(bat);
    u3_noun xac = u3a_take(cax);
#if 0
    fprintf(stderr, "reap: bat %x (%d, %d), cax %x\r\n", 
                    u3r_mug(tab),
                    u3a_is_junior(u3R, bat), 
                    u3a_use(tab),
                    u3r_mug(xac));
#endif
    u3h_put(u3R->jed.har_p, tab, xac);
    u3z(tab);
  }
}

/* u3j_reap(): promote jet state.  RETAINS.
*/
void
u3j_reap(u3_noun das, u3p(u3h_root) har_p)
{
  _cj_cold_reap_in(u3t(das));
  u3h_walk(har_p, _cj_warm_reap);
}
