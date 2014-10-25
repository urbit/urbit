/* g/j.c
**
** This file is in the public domain.
*/
#include "all.h"

  /* _cj_count(): count and link dashboard entries.
  */
  static c3_w 
  _cj_count(u3_cs_core* par_u, u3_cs_core* dev_u)
  {
    c3_w len_l = 0;
    c3_w i_w;

    if ( dev_u ) {
      for ( i_w = 0; 0 != dev_u[i_w].cos_c; i_w++ ) {
        u3_cs_core* kid_u = &dev_u[i_w];

        kid_u->par_u = par_u;
        len_l += _cj_count(kid_u, kid_u->dev_u);
      }
    }
    return 1 + len_l;
  }
  /* _cj_install(): install dashboard entries.
  */
  static c3_w
  _cj_install(u3_cs_core* ray_u, c3_w jax_l, u3_cs_core* dev_u)
  {
    c3_w i_w;

    if ( dev_u ) {
      for ( i_w = 0; 0 != dev_u[i_w].cos_c; i_w++ ) {
        u3_cs_core* kid_u = &dev_u[i_w];

        kid_u->jax_l = jax_l;
        ray_u[jax_l++] = *kid_u;

        jax_l = _cj_install(ray_u, jax_l, kid_u->dev_u);
      }
    }
    return jax_l;
  }

  struct _cj_dash {
    u3_noun sys;
    u3_noun haw;
  };
  struct _cj_cope {
    u3_noun soh;
    u3_noun hud;
    u3_noun mop;
  };

/* _cj_axis(): axis from formula, or 0.  `fol` is RETAINED.
*/
static c3_l
_cj_axis(u3_noun fol)
{
  u3_noun p_fol, q_fol, r_fol;

  while ( u3_so(u3du(fol)) && (10 == u3h(fol)) )
    { fol = u3t(u3t(fol)); }

  if ( u3_ne(u3_cr_trel(fol, &p_fol, &q_fol, &r_fol)) ) {
    if ( u3_ne(u3_cr_cell(fol, &p_fol, &q_fol)) ||
         (0 != p_fol) ||
         (u3_ne(u3_co_is_cat(q_fol))) )
    { 
      fprintf(stderr, "axis: bad a\r\n"); 
      return 0;
    }
    return q_fol;
  }
  else {
    if ( 9 != p_fol )
      { fprintf(stderr, "axis: bad b\r\n"); return 0; }
    if ( u3_ne(u3_co_is_cat(q_fol)) )
      { fprintf(stderr, "axis: bad c\r\n"); return 0; }
    if ( u3_ne(u3du(r_fol)) || (0 != u3h(r_fol)) || (1 != u3t(r_fol)) )
      { fprintf(stderr, "axis: bad d\r\n"); return 0; }

    return q_fol;
  }
}

/* _cj_by_gut(): (~(get by a) b), unifying; RETAINS a, b, and result.
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

    u3_cx_trel(a, &n_a, &l_a, &r_a);
    u3_cx_cell(n_a, &pn_a, &qn_a);
    {
      if ( (u3_yes == u3_cr_sung(b, pn_a)) ) {
        return qn_a;
      }
      else {
        if ( u3_yes == u3_cqc_gor(b, pn_a) ) {
          return _cj_by_gut(l_a, b);
        }
        else return _cj_by_gut(r_a, b);
      }
    }
  }
}

/* _cj_insert(): append copy of core driver to jet table.  For dummies.
*/
static c3_l
_cj_insert(u3_cs_core* cop_u)
{
  c3_l jax_l = u3D.len_l;

  u3D.len_l += 1;
  c3_assert(u3D.len_l < u3D.all_l);

  memcpy(&u3D.ray_u[jax_l], cop_u, sizeof(u3_cs_core));
  cop_u->jax_l = jax_l;

  return jax_l;
}

/* _cj_x_dash(): export dash.  RETAIN.
*/
static void
_cj_x_dash(u3_noun das, struct _cj_dash* das_u)
{
  u3_cx_cell(das, &(das_u->sys), &(das_u->haw));

  u3k(das_u->sys); u3k(das_u->haw);
}

/* _cj_m_dash(): import dash. 
*/
static u3_noun
_cj_m_dash(struct _cj_dash* das_u)
{
  return u3nc(das_u->sys, das_u->haw);
}

/* _cj_f_dash(): release dash.
*/
static void
_cj_f_dash(struct _cj_dash* das_u)
{
  u3z(das_u->sys);
  u3z(das_u->haw);
}

/* _cj_x_cope(): export cope.  RETAIN.
*/
static void
_cj_x_cope(u3_noun coe, struct _cj_cope* coe_u)
{
  u3_cx_trel(coe, &(coe_u->soh), 
                  &(coe_u->hud),
                  &(coe_u->mop));

  u3k(coe_u->soh);
  u3k(coe_u->hud);
  u3k(coe_u->mop);
}

/* _cj_m_cope(): import cope.
*/
static u3_noun
_cj_m_cope(struct _cj_cope* coe_u)
{
  return u3nt(coe_u->soh, coe_u->hud, coe_u->mop);
}

/* _cj_f_cope(): release cope.
*/
static void
_cj_f_cope(struct _cj_cope* coe_u)
{
  u3z(coe_u->soh);
  u3z(coe_u->hud);
  u3z(coe_u->mop);
}

#if 0
/* _cj_je_fine(): fine:je.  RETAINS args.
*/
static c3_o
_cj_je_fine(struct _cj_dash* das_u, struct _cj_cope* coe_u, u3_noun cor)
{
  u3_noun p_mop, q_mop, r_mop, h_mop, t_mop;

  u3_cx_trel(coe_u->mop, &p_mop, &q_mop, &r_mop);
  u3_cx_cell(r_mop, &h_mop, &t_mop);
  {
    u3_weak rah = u3_cr_at(q_mop, cor);

    if ( u3_none == rah ) {
      return u3_no;
    }
    else {
      if ( u3_no == h_mop ) {
        return u3_cr_sing(t_mop, rah);
      }
      else {
        u3_noun eco;

        eco = u3_ckdb_got(u3k(das_u->haw), u3k(t_mop));
        {
          struct _cj_cope eco_u;
          c3_o           pro_o;

          _cj_x_cope(eco, &eco_u);
          pro_o = _cj_je_fine(das_u, &eco_u, rah);
          _cj_f_cope(&eco_u);

          u3z(eco);
          return pro_o;
        }
      }
    }
  }
}

/* _cj_je_fill: fill:je.  RETAINS args, PRODUCES result.
*/
static u3_noun 
_cj_je_fill(struct _cj_dash* das_u, u3_noun cor)
{
  u3_noun bus = _cj_je_find(das_u, cor);

  if ( u3_nul == bus ) {
    return u3_nul;
  } 
  else {
    u3_noun soh = u3t(bus);
    u3_noun coe;

    coe = u3_ckdb_got(u3k(das_u->haw), u3k(soh));
    {
      struct _cj_cope coe_u;
      c3_o            pro_o;

      _cj_x_cope(coe, &coe_u);
      pro_o = _cj_je_fine(das_u, &coe_u, cor);
      _cj_f_cope(&coe_u);
    }
    return u3nc(u3_nul, soh);
  }
}
#endif

/* _cj_chum(): decode chum as string.
*/
static c3_c* 
_cj_chum(u3_noun chu)
{
  if ( u3_so(u3ud(chu)) ) {
    return u3_cr_string(chu);
  } 
  else {
    u3_noun h_chu = u3h(chu);
    u3_noun t_chu = u3t(chu);
    
    if ( u3_ne(u3_co_is_cat(t_chu)) ) {
      return 0;
    } else {
      c3_c* h_chu_c = u3_cr_string(h_chu);
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

  if ( u3_no == u3_cr_trel(clu, &p_clu, &q_clu, &r_clu) ) {
    u3z(clu); return u3_none;
  }
  if ( 0 == (nam_c = _cj_chum(p_clu)) ) {
    u3z(clu); return u3_none;
  }
  while ( u3_so(u3du(q_clu)) && (10 == u3h(q_clu)) ) { 
    q_clu = u3t(u3t(q_clu));
  }
  if ( u3_ne(u3du(q_clu)) ) {
    u3z(clu); free(nam_c); return u3_none;
  }

  if ( (1 == u3h(q_clu)) && (0 == u3t(q_clu)) ) {
    axe_l = 0;
  }
  else {
    if ( (0 != u3h(q_clu)) || u3_ne(u3_co_is_cat(axe_l = u3t(q_clu))) ) {
      u3z(clu); free(nam_c); return u3_none;
    }
  }

  {
    huk = 0;

    while ( u3_so(u3du(r_clu)) ) {
      u3_noun ir_clu, tr_clu, pir_clu, qir_clu;

      if ( (u3_no == u3_cr_cell(r_clu, &ir_clu, &tr_clu)) ||
           (u3_no == u3_cr_cell(ir_clu, &pir_clu, &qir_clu)) ||
           (u3_no == u3ud(pir_clu)) )
      {
        u3z(huk); u3z(clu); free(nam_c); return u3_none;
      }
      huk = u3_ckdb_put(huk, u3k(pir_clu), u3k(qir_clu));
      r_clu = tr_clu;
    }
  }
  return u3nt(u3_ci_string(nam_c), axe_l, huk);
}

/* _cj_sham(): ++sham.
*/
static u3_atom
_cj_sham(u3_noun som)       //  XX wrong, does not match ++sham
{
  u3_atom jam = u3_cke_jam(som);
  u3_noun sha = u3_cqe_shax(jam);
  u3_noun haf = u3_cqc_end(7, 1, sha);

  u3z(jam); u3z(sha);
  return haf;
}

/* _cj_je_fuel: install battery and core pattern.
*/
static void
_cj_je_fuel(struct _cj_dash* das_u, u3_noun bat, u3_noun coe)
{
  struct _cj_cope coe_u;

  _cj_x_cope(coe, &coe_u);
  {
    das_u->sys = u3_ckdb_put(das_u->sys, u3k(bat), u3k(coe_u.soh));
    das_u->haw = u3_ckdb_put(das_u->haw, u3k(coe_u.soh), u3k(coe));
  }
  u3z(bat);
  u3z(coe);
  _cj_f_cope(&coe_u);
}

/* _cj_cold_find_bash(): in cold, find identity hash by battery.  
**
** RETAINS `bat` AND result.
*/
static u3_weak
_cj_cold_find_bash(u3_noun bat)
{
  u3_cs_road* rod_u = u3R;
  u3_weak     pro = u3_none;

  while ( 1 ) {
    if ( u3_ne(u3_co_is_senior(u3R, bat)) ) {
      struct _cj_dash das_u;

      _cj_x_dash(u3R->jed.das, &das_u);
      {
        pro = _cj_by_gut(das_u.sys, bat);
      }
      _cj_f_dash(&das_u);

      if ( pro != u3_none ) {
        break;
      }
    }
    if ( !u3R->par_u ) { break; } 
    else u3R = u3R->par_u;
  }
  u3R = rod_u;
  return pro;
}

/* _cj_cold_find_cope(): in cold, find core pattern by battery.  
**
** RETAINS `bat` AND result.
*/
static u3_weak
_cj_cold_find_cope(u3_noun bat)
{
  u3_cs_road* rod_u = u3R;
  u3_weak     pro = u3_none;

  while ( 1 ) {
    if ( u3_ne(u3_co_is_senior(u3R, bat)) ) {
      struct _cj_dash das_u;

      _cj_x_dash(u3R->jed.das, &das_u);
      {
        pro = _cj_by_gut(das_u.sys, bat);
        if ( u3_none != pro ) {
          pro = _cj_by_gut(das_u.haw, pro);
        }
      }
      _cj_f_dash(&das_u);

      if ( pro != u3_none ) {
        break;
      }
    }
    if ( !u3R->par_u ) { break; } 
    else u3R = u3R->par_u;
  }
  u3R = rod_u;
  return pro;
}

/* _cj_road(): find the proper road for a battery.  RETAIN.
*/
static u3_cs_road* 
_cj_road(u3_noun bat)
{
  u3_cs_road* rod_u = u3R;

  while ( rod_u->par_u && u3_so(u3_co_is_senior(rod_u, bat)) ) {
    rod_u = rod_u->par_u;
  }
  return rod_u;
}

/* _cj_cold_mine(): in cold mode, declare a core.  RETAIN.
*/
static c3_o
_cj_cold_mine(u3_noun cey, u3_noun cor)
{
  struct _cj_dash das_u;
  u3_noun         coe;
  u3_noun         bat = u3h(cor);
  u3_noun         p_cey, q_cey, r_cey;

  u3_cr_trel(cey, &p_cey, &q_cey, &r_cey);
  _cj_x_dash(u3R->jed.das, &das_u);
  {
    u3_noun mop, soh;

    /* Calculate semantic identity (mop).
    */
    {
      if ( 0 == q_cey ) {
        mop = u3nq(u3k(p_cey), 3, u3_no, u3k(bat));
      }
      else {
        u3_weak rah = u3_cr_at(q_cey, cor);

        if ( (u3_none == rah) || u3_ne(u3du(rah)) ) {
          fprintf(stderr, "fund: %s is bogus\r\n", u3_cr_string(p_cey));
          return u3_no;
        }
        else {
          u3_noun bah = _cj_cold_find_bash(u3h(rah));

          if ( u3_none == bah ) {
            fprintf(stderr, "fund: in %s, parent %x not found at %d\r\n", 
                            u3_cr_string(p_cey),
                            u3_cr_mug(u3h(rah)),
                            q_cey);
            abort();
            return u3_no;
          }
          else {
            mop = u3nq(u3k(p_cey), u3k(q_cey), u3_yes, u3k(bah));
          }
        }
      }

      //  Assemble new core pattern in this road.
      //
      {
        soh = _cj_sham(u3k(mop));
        coe = u3_ckdb_get(u3k(das_u.haw), u3k(soh));

        if ( u3_none != coe ) {
          fprintf(stderr, "fund: old %s\r\n", u3_cr_string(p_cey));
          {
            struct _cj_cope coe_u;

            _cj_x_cope(coe, &coe_u);
            coe_u.hud = u3_ckdb_put(coe_u.hud, u3k(bat), u3k(r_cey));
            u3z(coe);
            coe = _cj_m_cope(&coe_u);
          } 
        }
        else {
          // fprintf(stderr, "fund: new %s\r\n", u3_cr_string(p_cey));
          coe = u3nt(u3k(soh), 
                     u3nt(u3nc(u3k(bat), u3k(r_cey)), u3_nul, u3_nul),
                     u3k(mop)); 

        }
        das_u.haw = u3_ckdb_put(das_u.haw, u3k(soh), coe);
        das_u.sys = u3_ckdb_put(das_u.sys, u3k(bat), u3k(soh));
      }
      u3z(mop);
      u3z(soh);
    }
  }
  u3z(u3R->jed.das);
  u3R->jed.das = _cj_m_dash(&das_u);

  return u3_yes;
}

/* _cj_warm_find(): in warm state, return 0 or the battery index.  RETAINS.
*/
c3_l
_cj_warm_find(u3_noun bat)
{
  u3_cs_road* rod_u = u3R;

  while ( 1 ) {
    if ( u3_ne(u3_co_is_senior(rod_u, bat)) ) {
      u3_weak jaw = u3_ch_gut(rod_u->jed.har_u, bat);

      if ( u3_none != jaw ) {
        u3_assure(u3_co_is_cat(u3h(jaw)));

#if 0
        if ( rod_u != u3R ) {
          fprintf(stderr, "got: %x in %p/%p, %d\r\n", 
              bat, rod_u, rod_u->jed.har_u, jax);
        }
#endif
        return (c3_l)u3h(jaw);
      }
    }
    if ( rod_u->par_u ) {
      rod_u = rod_u->par_u;
    }
    else return 0;
  }
}

/* _cj_warm_fend(): in warm state, return u3_none or calx.  RETAINS.
*/
u3_weak
_cj_warm_fend(u3_noun bat)
{
  u3_cs_road* rod_u = u3R;

  while ( 1 ) {
    if ( u3_ne(u3_co_is_senior(rod_u, bat)) ) {
      u3_weak jaw = u3_ch_gut(rod_u->jed.har_u, bat);

      if ( u3_none != jaw ) {
        return jaw;
      }
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
  u3_cs_core* cop_u = &u3D.ray_u[jax_l];
  u3_noun hap = u3_nul;

  /* Compute axes of all correctly declared arms.
  */
  if ( cop_u->arm_u ) {
    u3_cs_harm* jet_u;
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
        u3_noun nam = u3_ci_string(jet_u->fcs_c);
        u3_noun fol = u3_ckdb_get(u3k(huc), nam);

        if ( u3_none == fol ) {
          fprintf(stderr, "jets: activate: bad fcs %s\r\n", jet_u->fcs_c);
        }
        axe_l = _cj_axis(fol);
      }
      if ( 0 != axe_l ) {
        hap = u3_ckdb_put(hap, axe_l, i_l);
      }
    }
  }
  return hap;
}

/* _cj_boil_mine(): in boiling state, declare a core.  RETAINS.
**  
*/
static c3_l
_cj_boil_mine(u3_noun cor)
{
  u3_noun         bat = u3h(cor);
  u3_weak         coe = _cj_cold_find_cope(bat);
  struct _cj_cope coe_u;
  c3_l            jax_l = 0;
 
  c3_assert(u3_none != coe);
  _cj_x_cope(coe, &coe_u);
  {
    u3_noun p_mop, q_mop, r_mop, hr_mop, tr_mop;

    u3_cx_trel(coe_u.mop, &p_mop, &q_mop, &r_mop);
    u3_cx_cell(r_mop, &hr_mop, &tr_mop);
    {
      u3_noun nam   = p_mop;
      u3_noun axe_l = q_mop;
      u3_noun par_l;
 
      //  Calculate parent axis.
      //
      if ( u3_yes == hr_mop ) {
        par_l = _cj_warm_find(u3h(u3_cr_at(axe_l, cor)));
        c3_assert(0 != par_l);
      }
      else par_l = 0;

      //  Link into parent.
      //
      {
        u3_cs_core* par_u = par_l ? &u3D.ray_u[par_l] : 0;
        u3_cs_core* dev_u = par_u ? par_u->dev_u : u3_Dash.dev_u;
        c3_w        i_l = 0;

        if ( dev_u ) {
          u3_cs_core* cop_u;

          while ( (cop_u = &dev_u[i_l])->cos_c ) {
            if ( u3_so(u3_cr_sing_c(cop_u->cos_c, nam)) ) {
              jax_l = cop_u->jax_l;
              u3D.ray_u[jax_l].axe_l = axe_l;
              u3D.ray_u[jax_l].par_u = par_u;
              c3_assert(0 != jax_l);
#if 1
              fprintf(stderr, "boil: bound jet %d/%s/%s/%x\r\n", 
                              cop_u->jax_l, 
                              cop_u->cos_c,
                              par_u ? par_u->cos_c : "~",
                              u3_cr_mug(bat));
#endif
              break;
            }
            i_l++;
          }
        }
        if ( 0 == jax_l ) {
          u3_cs_core fak_u;

          memset(&fak_u, 0, sizeof(u3_cs_core));
          fak_u.cos_c = u3_cr_string(nam);
          fak_u.par_u = par_u;
          fak_u.axe_l = axe_l;

          jax_l = _cj_insert(&fak_u);
#if 1
          fprintf(stderr, "boil: dummy jet %d/%s\r\n", jax_l, fak_u.cos_c);
#endif
        } 
      }
    }
  }
  _cj_f_cope(&coe_u);

  return jax_l;
}

/* _cj_warm_fresh_in(): refresh in `tys`; RETAINS.
*/
static void
_cj_warm_fresh_in(struct _cj_dash* das_u, u3_noun tys)
{
  if ( u3_nul == tys ) {
    u3_noun n_tys, l_tys, r_tys, pn_tys, qn_tys;

    u3_cx_trel(tys, &n_tys, &l_tys, &r_tys);
    u3_cx_cell(n_tys, &pn_tys, &qn_tys);

    _cj_warm_fresh_in(das_u, l_tys);
    _cj_warm_fresh_in(das_u, r_tys);
  }
}

/* _cj_warm_fresh(): refresh warm from cold state.
*/
static void
_cj_warm_fresh(void)
{
  struct _cj_dash das_u;

  _cj_x_dash(u3R->jed.das, &das_u); 
  {
    _cj_warm_boot_in(&das_u, das_u.sys);
  }
  _cj_f_dash(&das_u);
}
/* _cj_warm_mine(): in warm mode, declare a core.  
*/
static void
_cj_warm_mine(u3_noun clu, u3_noun cor)
{
  u3_noun bat = u3h(cor);

  if ( u3_ne(u3du(cor)) || (0 != _cj_warm_find(bat)) ) {
    u3z(clu);
  }
  else {
    u3_noun cey = _cj_je_fsck(clu);

    // fprintf(stderr, "warm_mine %s\r\n", u3_cr_string(u3h(cey)));

    if ( u3_none != cey ) {
      u3_cs_road* rod_u = u3R;

      u3R = _cj_road(bat);
      {
        u3_noun yec = u3_ca_take(cey);
        u3_noun huc = u3t(u3t(yec));
        u3_noun pax = u3h(u3t(yec));

        if ( u3_so(_cj_cold_mine(yec, cor)) ) {
          c3_l jax_l = _cj_boil_mine(cor);

          u3_ch_put(u3R->jed.har_u, 
                    bat, 
                    u3nq(jax_l, 
                         u3k(pax),
                         _cj_warm_hump(jax_l, u3k(huc)),
                         u3k(huc)));
        }
        u3z(yec);
      }
      u3R = rod_u;
    }
    u3z(cey);
  }
  u3z(cor);
}

/* u3_cj_boot(): initialize jet system.
*/
void
u3_cj_boot(void)
{
  c3_w jax_l;

  u3D.len_l =_cj_count(0, u3D.dev_u);
  u3D.all_l = (2 * u3D.len_l) + 1024;     //  horrid heuristic

  u3D.ray_u = (u3_cs_core*) malloc(u3D.all_l * sizeof(u3_cs_core));
  memset(u3D.ray_u, 0, (u3D.all_l * sizeof(u3_cs_core)));

  jax_l = _cj_install(u3D.ray_u, 1, u3D.dev_u);
  fprintf(stderr, "boot: installed %d jets\n", jax_l);
}

/* _cj_find(): search for jet, old school.  `bat` is RETAINED.
*/
c3_l
_cj_find(u3_noun bat)
{
  u3_cs_road* rod_u = u3R;

  while ( 1 ) {
    if ( u3_ne(u3_co_is_senior(rod_u, bat)) ) {
      u3_weak jaw = u3_ch_gut(rod_u->jed.har_u, bat);

      if ( u3_none != jaw ) {
        u3_assure(u3_co_is_cat(u3h(jaw)));

#if 0
        if ( rod_u != u3R ) {
          fprintf(stderr, "got: %x in %p/%p, %d\r\n", 
              bat, rod_u, rod_u->jed.har_u, jax);
        }
#endif
        return (c3_l)u3h(jaw);
      }
    }
    if ( rod_u->par_u ) {
      rod_u = rod_u->par_u;
    }
    else return 0;
  }
}

/* u3_cj_find(): search for jet.  `bat` is RETAINED.
*/
c3_l
u3_cj_find(u3_noun bat)
{
  return _cj_find(bat);
}

/* _cj_soft(): kick softly by arm axis.
*/
static u3_noun
_cj_soft(u3_noun cor, u3_noun axe)
{
  u3_noun arm = u3_cx_at(axe, cor);

  return u3_cn_nock_on(cor, u3k(arm));
}

/* _cj_kick_z(): try to kick by jet.  If no kick, produce u3_none.
**
** `cor` is RETAINED iff there is no kick, TRANSFERRED if one.  
** `axe` is RETAINED.
*/
static u3_weak
_cj_kick_z(u3_noun cor, u3_cs_core* cop_u, u3_cs_harm* ham_u, u3_atom axe)
{
  if ( 0 == ham_u->fun_f ) {
    return u3_none;
  }

  if ( u3_ne(ham_u->liv) ) {
    return u3_none;
  }
  else {
    if ( u3_so(ham_u->ice) ) {
      u3_weak pro = ham_u->fun_f(cor);

      if ( u3_none != pro ) {
        u3z(cor);
        return pro;
      }
    }
    else {
      u3_weak pro, ame;

      ham_u->ice = u3_yes;
      pro = ham_u->fun_f(u3k(cor));
      ham_u->ice = u3_no;

      if ( u3_none == pro ) {
        u3z(cor);
        return pro;
      }
      ham_u->liv = u3_no;
      ame = _cj_soft(cor, axe);
      ham_u->liv = u3_yes;

      if ( u3_no == u3_cr_sing(ame, pro) ) {
        fprintf(stderr, "test: %s %s: mismatch: good %x, bad %x\r\n",
               cop_u->cos_c,
               (!strcmp(".2", ham_u->fcs_c)) ? "$" : ham_u->fcs_c,
               u3_cr_mug(ame), 
               u3_cr_mug(pro));
       
        c3_assert(0);
        return u3_cm_bail(c3__fail);
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

  if ( u3_ne(u3du(cor)) ) { return u3_cm_bail(c3__fail); }
  {
    u3_weak cax = _cj_warm_fend(bat);

    if ( u3_none == cax ) { return u3_cm_bail(c3__fail); }
    {
      u3_noun jax, pax, huc, hap;

      u3_cx_qual(cax, &jax, &pax, &hap, &huc);
      {
        c3_l        jax_l = jax;
        u3_cs_core* cop_u = &u3D.ray_u[jax_l];
        u3_noun     fol   = u3_ckdb_get(u3k(huc), u3_ci_string(tam_c));

        if ( u3_none == fol ) {
          //  The caller wants a deeper core.
          //
          if ( 0 == pax ) { return u3_cm_bail(c3__fail); }
          else {
            u3_noun inn = u3k(u3_cx_at(pax, cor));

            u3z(cax); u3z(cor);
            return _cj_hook_in(inn, tam_c, jet_o);
          }
        }
        else {
          u3_noun pro;
          c3_l    axe_l = _cj_axis(fol);
          c3_l    inx_l;
 
          if ( (0 == axe_l) || 
               (u3_no == jet_o) ||
               (u3_none == (inx_l = u3_ckdb_get(u3k(hap), axe_l))) ||
               (u3_none == (pro = _cj_kick_z(cor, 
                                             cop_u, 
                                             &cop_u->arm_u[inx_l],
                                             axe_l))) )
          { 
            if ( 0 == axe_l ) {
              u3z(cax); 
              return u3_cn_nock_on(cor, fol);
            } else {
              //  Tricky: the above case would work here too, but would
              //  disable jet_o and create some infinite recursions.
              //
              u3z(cax); u3z(fol);
              return u3_cn_nock_on(cor, u3k(u3_cx_at(axe_l, cor)));
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

/* u3_cj_soft(): execute soft hook.
*/
u3_noun
u3_cj_soft(u3_noun cor, 
           const c3_c* tam_c)
{
  return _cj_hook_in(cor, tam_c, u3_no);
}

/* u3_cj_hook(): execute hook from core, or fail.
*/
u3_noun
u3_cj_hook(u3_noun     cor,
           const c3_c* tam_c)
{
  return _cj_hook_in(cor, tam_c, u3_yes);
}

/* u3_cj_kick(): new kick.
**
** `axe` is RETAINED by the caller; `cor` is RETAINED iff there 
** is no kick, TRANSFERRED if one.
*/
u3_weak
u3_cj_kick(u3_noun cor, u3_noun axe)
{
  if ( u3_ne(u3du(cor)) ) { return u3_none; } 
  {
    u3_noun bat = u3h(cor);
    u3_weak cax = _cj_warm_fend(bat);

    if ( u3_none == cax ) { return u3_none; }
    {
      u3_noun hap = u3h(u3t(u3t(cax)));
      u3_noun inx = u3_ckdb_get(u3k(hap), u3k(axe));

      if ( u3_none == inx ) {
        u3z(cax); return u3_none;
      }
      else {
        c3_l jax_l        = u3h(cax);
        u3_cs_core* cop_u = &u3D.ray_u[jax_l];
        c3_l inx_l        = inx;
        u3_cs_harm* ham_u = &cop_u->arm_u[inx_l];

        u3z(cax);
        return _cj_kick_z(cor, cop_u, ham_u, axe);
      }
    }
  }
}

/* u3_cj_kink(): kick either by jet or by nock.
*/
u3_noun
u3_cj_kink(u3_noun cor,
           u3_noun axe)
{
  u3_weak pro = u3_cj_kick(cor, axe);

  if ( u3_none != pro ) {
    return pro;
  } else {
    return u3_cn_nock_on(cor, u3nq(9, axe, 0, 1));
  }
}

/* u3_cj_clear(): clear jet table to re-register.
*/
void
u3_cj_clear(void)
{
  u3_ch_free(u3R->jed.har_u);
  u3R->jed.har_u = u3_ch_new();
}

/* u3_cj_mine(): register core for jets.  Produce registered core.
*/
void
u3_cj_mine(u3_noun clu, u3_noun cor)
{
  _cj_warm_mine(clu, cor);
}
