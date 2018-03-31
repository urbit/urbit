/* g/j.c
**
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

#if 0
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
#endif

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

/* _cj_find_cold(): search cold state for `bat`s registry.
 *                  RETAIN.
 */
static u3_weak
_cj_find_cold(u3_noun bat)
{
  u3a_road* rod_u = u3R;

  while ( 1 ) {
    u3_weak reg = u3h_gut(rod_u->jed.cod_p, bat);

    if ( u3_none != reg ) {
      return reg;
    }

    if ( rod_u->par_p ) {
      rod_u = u3to(u3_road, rod_u->par_p);
    }
    else return u3_none;
  }
}

/* _cj_find_warm(): search warm state for `loc`s activation.
 *                  RETAIN.
 */
static u3_weak
_cj_find_warm(u3_noun loc)
{
  u3a_road* rod_u = u3R;

  while ( 1 ) {
    u3_weak ank = u3h_gut(rod_u->jed.war_p, loc);

    if ( u3_none != ank ) {
      return ank;
    }

    if ( rod_u->par_p ) {
      rod_u = u3to(u3_road, rod_u->par_p);
    }
    else return u3_none;
  }
}

/* u3j_spot(): identify `cor`s location. RETAIN.
 */
u3_weak
u3j_spot(u3_noun cor)
{
  u3_weak reg = _cj_find_cold(u3h(cor));
  if ( u3_none == reg ) {
    return u3_none;
  }
  else {
    u3_noun rut = u3h(reg),
            pas = u3t(reg),
            rum = u3qdb_get(rut, u3t(cor));
    if ( u3_nul != rum ) {
      u3_noun loc = u3k(u3t(rum));
      u3z(rum); u3z(reg);
      return loc;
    }
    else {
      while ( u3_nul != pas ) {
        u3_noun pap = u3h(pas),
                axe = u3h(pap),
                lol = u3t(pap);
        u3_weak par = u3r_at(axe, cor),
                pel;
        if ( u3_none != par ) {
          pel = u3j_spot(par);
          if ( u3_none != pel ) {
            u3_noun nit = u3qdb_get(lol, pel);
            u3z(pel);
            if ( u3_nul != nit ) {
              u3_noun loc = u3k(u3t(nit));
              u3z(nit); u3z(reg);
              return loc;
            }
          }
        }
        pas = u3t(pas);
      }
      u3z(reg);
      return u3_none;
    }
  }
}

/* _cj_scan(): has this core been registered?
 */
static c3_o
_cj_scan(u3_noun cor)
{
  u3_weak loc = u3j_spot(cor);
  c3_o  reg_o = (u3_none == loc) ? c3n : c3y;
  u3z(loc);
  return reg_o;
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

        if ( (1 != sscanf(jet_u->fcs_c+1, "%" SCNu64, &axe_d)) ||
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

/* _cj_hot_mean(): in parent, declare a core.  RETAINS.
*/
static c3_l
_cj_hot_mean(c3_l par_l, u3_noun nam)
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
      if ( _(u3r_sing_c(cop_u->cos_c, nam)) ) {
#if 0
        fprintf(stderr, "hot: bound jet %d/%s/%s/\r\n", 
                        cop_u->jax_l, 
                        cop_u->cos_c,
                        par_u ? par_u->cos_c : "~");
#endif
        return cop_u->jax_l;
      }
      i_l++;
    }
  }
  return 0;
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

/* _cj_soft(): kick softly by arm axis.
*/
static u3_noun
_cj_soft(u3_noun cor, u3_noun axe)
{
  u3_noun arm = u3x_at(axe, cor);

  return u3n_nock_on(cor, u3k(arm));
}

  void
  find_error(u3_noun cor,
             u3_noun old,
             u3_noun new);

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
#ifdef U3_MEMORY_DEBUG
    c3_w cod_w;

    {
      char soc_c[6];

      memset(soc_c, 0, 6);
      strncpy(soc_c, cop_u->cos_c, 5);
      soc_c[5] = 0;
      cod_w = u3i_string(soc_c);

      cod_w = u3a_lush(cod_w);
    }
#endif

    if ( _(ham_u->ice) ) {
      u3_weak pro = ham_u->fun_f(cor);

#ifdef U3_MEMORY_DEBUG
      u3a_lop(cod_w);
#endif
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

#ifdef U3_MEMORY_DEBUG
      u3a_lop(cod_w);
#endif
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
        ham_u->liv = c3n;

        c3_assert(0);
        return u3m_bail(c3__fail);
      }
      else {

#if 0
        fprintf(stderr, "test: %s %s\r\n",
               cop_u->cos_c,
               (!strcmp(".2", ham_u->fcs_c)) ? "$" : ham_u->fcs_c);
#endif
        u3z(ame);
        return pro;
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
  u3_weak   loc, col;
  u3_noun   roc, tem, got, pat, nam, huc;

  if ( c3n == u3du(cor) ) {
    return u3m_bail(c3__fail);
  }

  loc = u3j_spot(cor);
  if ( u3_none == loc ) {
    return u3m_bail(c3__fail);
  }

  tem = u3i_string(tam_c);
  while ( 1 ) {
    u3x_trel(loc, &pat, &nam, &huc);
    got = u3qdb_get(huc, tem);
    if ( u3_nul != got ) {
      c3_l axe_l;
      u3_noun pro, fol;
      u3j_core* cop_u;

      u3z(tem);
      fol = u3k(u3t(got));
      u3z(got);
      axe_l = _cj_axis(fol);
      if ( 0 == axe_l ) {
        pro = u3n_nock_on(cor, fol);
      }
      else {
        c3_l jax_l, inx_l;
        u3_noun hap, act;

        u3z(fol);
        act   = _cj_find_warm(loc);
        jax_l = u3h(act);
        hap   = u3h(u3t(act));
        cop_u = &u3D.ray_u[jax_l];

        //  Tricky: the above case would work here too, but would
        //  disable jet_o and create some infinite recursions.
        //
        if ( (c3n == jet_o) ||
             (u3_none == (inx_l = u3kdb_get(u3k(hap), axe_l))) ||
             (u3_none == (pro = _cj_kick_z(cor,
                                           cop_u,
                                           &cop_u->arm_u[inx_l],
                                           axe_l))) ) {
          pro = u3n_nock_on(cor, u3k(u3x_at(axe_l, cor)));
        }
        u3z(act);
      }
      u3z(loc);
      return pro;
    }
    else if ( c3n == u3h(pat) ) {
      u3_noun dyn = u3t(pat),
              axe = u3h(dyn),
              pel = u3t(dyn);
      roc = u3k(u3r_at(axe, cor));
      u3z(cor);
      cor = roc;
      col = u3k(pel);
      u3z(loc);
      loc = col;
    }
    else {
      u3_noun sat = u3t(pat);
      if ( c3y == u3h(sat) ) {
        return u3m_bail(c3__fail);
      }
      else {
        col = u3k(u3t(sat));
        u3z(loc);
        loc = col;
        roc = u3k(u3t(cor));
        cor = roc;
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
  u3t_on(glu_o);
  u3_weak loc = u3j_spot(cor);
  if ( u3_none == loc ) {
    u3t_off(glu_o);
    return u3_none;
  }
  else {
    u3_weak act = _cj_find_warm(loc);
    u3z(loc);
    if ( u3_none == act ) {
      u3t_off(glu_o);
      return u3_none;
    }
    else {
      c3_l jax_l;
      u3_noun hap, bal, jit, inx;

      u3x_qual(act, &jax_l, &hap, &bal, &jit);

      if ( u3_none == (inx = u3kdb_get(u3k(hap), u3k(axe))) ) {
        u3t_off(glu_o); 
        {
          c3_o pof_o = __(u3C.wag_w & u3o_debug_cpu);

          if ( _(pof_o) ) {
            pof_o = u3t_come(bal);
          }
          u3z(act);
          if ( _(pof_o) ) {
            u3_noun pro = u3n_nock_on(cor, u3nq(9, u3k(axe), 0, 1));

            u3t_flee();
            return pro;
          }
          else {
            return u3_none;
          }
        }
      }
      else {
        u3j_core* cop_u = &u3D.ray_u[jax_l];
        c3_l      inx_l = inx;
        u3j_harm* ham_u = &cop_u->arm_u[inx_l];
        c3_o      pof_o = __(u3C.wag_w & u3o_debug_cpu);
        u3_noun   pro;

        if ( _(pof_o) ) {
          pof_o = u3t_come(bal);
        }
        u3z(act);
        u3t_off(glu_o);
        pro = _cj_kick_z(cor, cop_u, ham_u, axe);
 
        if ( u3_none == pro ) {
          if ( _(pof_o) ) {
            pro = u3n_nock_on(cor, u3nq(9, u3k(axe), 0, 1));

            u3t_flee();
            return pro;
          } 
          else return u3_none;
        }
        else {
          if ( _(pof_o) ) {
            u3t_flee();
          }
          return pro;
        }
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

/* _cj_jit(): generate arbitrary warm jet-associated data.  RETAIN.
*/
static u3_noun 
_cj_jit(c3_l jax_l, u3_noun bat)
{
  return u3_nul;
}

/* _cj_mine_par_old(): register hooks and parent location within existing
 *                     axis in ancestor list or u3_none. RETAIN.
 */
static u3_weak
_cj_mine_par_old(u3_noun lan, u3_noun axe, u3_noun pel, u3_noun loc)
{
  u3_noun par;
  if ( u3_nul == lan ) {
    return u3_none;
  }
  else if ( c3y == u3r_sing(axe, u3h(par = u3h(lan))) ) {
    u3_noun lol = u3qdb_put(u3t(par), pel, loc),
            rap = u3nc(u3k(axe), lol);
    return u3nc(rap, u3k(u3t(lan)));
  }
  else {
    u3_weak nex = _cj_mine_par_old(u3t(lan), axe, pel, loc);
    if ( u3_none == nex ) {
      return u3_none;
    }
    else {
      return u3nc(u3k(par), nex);
    }
  }
}

/* _cj_mine_par_new(): insert ancestor within lan at sorted index. RETAIN.
 */
static u3_noun
_cj_mine_par_new(u3_noun lan, u3_noun axe, u3_noun pel, u3_noun loc)
{
  u3_noun par;
  if ( (u3_nul == lan) || (c3y == u3qa_lth(axe, u3h(u3h(lan)))) ) {
    par = u3nc(u3k(axe), u3qdb_put(u3_nul, pel, loc));
    return u3nc(par, u3k(lan));
  }
  else {
    return u3nc(u3k(u3h(lan)),
                _cj_mine_par_new(u3t(lan), axe, pel, loc));
  }
}

/* _cj_mine_par(): register a location as an ancestor in a list of ancestors.
 *                 RETAIN.
 */
static u3_noun
_cj_mine_par(u3_noun lan, u3_noun axe, u3_noun pel, u3_noun loc)
{
  u3_weak old = _cj_mine_par_old(lan, axe, pel, loc);
  if ( u3_none != old ) {
    return old;
  }
  else {
    return _cj_mine_par_new(lan, axe, pel, loc);
  }
}

/* _cj_mine(): declare a core.  RETAIN.
*/
static void
_cj_mine(u3_noun cey, u3_noun cor)
{
  c3_l par_l, jax_l;
  u3_noun bat = u3h(cor),
          hap, reg, loc, bal, act, nam, axe, huc, ger;

  u3x_trel(cey, &nam, &axe, &huc);
  if ( 0 == axe ) {
    u3_noun pay = u3t(cor);
    reg   = _cj_find_cold(bat);
    loc   = u3nt(u3nt(c3y, c3y, u3k(pay)), u3k(nam), u3k(huc));
    if ( u3_none == reg ) {
      reg = u3nc(u3_nul, u3_nul);
    }
    ger   = u3nc(u3qdb_put(u3h(reg), pay, loc), u3k(u3t(reg)));
    u3z(reg);
    reg   = ger;
    bal   = u3nc(u3k(nam), u3_nul);
    par_l = 0;
  }
  else {
    u3_weak par, pel, pac;
    u3_noun pat;

    par = u3r_at(axe, cor);
    if ( u3_none == par || c3n == u3du(par) ) {
      fprintf(stderr, "fund: %s is bogus\r\n", u3r_string(nam));
      return;
    }
    pel = u3j_spot(par);
    if ( u3_none == pel ) {
      fprintf(stderr, "fund: in %s, parent %x not found at %d\r\n", 
                      u3r_string(nam),
                      u3r_mug(u3h(par)),
                      axe);
      return;
    }
    pac = _cj_find_warm(pel);
    c3_assert(u3_none != pac);
    par_l = u3h(pac);
    bal   = u3nc(u3k(nam), u3k(u3h(u3t(u3t(pac)))));
    u3z(pac);
    pat = ( ( 3 == axe ) && (c3y == u3h(u3h(pel))) )
        ? u3nt(c3y, c3n, u3k(pel))
        : u3nt(c3n, u3k(axe), u3k(pel));
    loc = u3nt(pat, u3k(nam), u3k(huc));
    reg = _cj_find_cold(bat);
    if ( u3_none == reg ) {
      reg = u3nc(u3_nul, u3_nul);
    }
    ger = u3nc(u3k(u3h(reg)),
               _cj_mine_par(u3t(reg), axe, pel, loc));
    u3z(pel); u3z(reg);
    reg = ger;
  }
  jax_l = _cj_hot_mean(par_l, nam);
#if 0
  u3m_p("new jet", bal);
  fprintf(stderr, "  bat %x, jax %d\r\n", u3r_mug(bat), jax_l);
#endif
  hap   = _cj_warm_hump(jax_l, huc);
  act   = u3nq(jax_l, hap, bal, _cj_jit(jax_l, bat));
  u3h_put(u3R->jed.cod_p, bat, reg);
  u3h_put(u3R->jed.war_p, loc, act);
  u3z(loc);
}

/* u3j_mine(): register core for jets.
*/
void
u3j_mine(u3_noun clu, u3_noun cor)
{
  u3t_on(glu_o);
  if ( (c3n == u3du(cor)) || (c3y == _cj_scan(cor)) ) {
    u3z(clu);
  }
  else {
    u3_noun cey = _cj_je_fsck(clu);

    if ( u3_none != cey ) {
      _cj_mine(cey, cor);
      u3z(cey);
    }
  }
  u3z(cor);
  u3t_off(glu_o);
}

/* _cj_warm_reap(): reap key and value from warm table.
*/
static void
_cj_warm_reap(u3_noun kev)
{
  u3_noun loc = u3a_take(u3h(kev));
  u3_noun act = u3a_take(u3t(kev));
  u3h_put(u3R->jed.war_p, loc, act);
  u3z(loc);
}

/* _cj_uni_jun(): merge junior map into senior map.
 *                sem is TRANSFERRED.
 *                jum is RETAINED.
 */
static u3_noun
_cj_uni_jun(u3_noun sem, u3_noun jum)
{
  if ( u3_nul == jum ) {
    return sem;
  }
  else {
    u3_noun n, l, r;
    u3x_trel(jum, &n, &l, &r);
    sem = _cj_uni_jun(sem, l);
    sem = _cj_uni_jun(sem, r);
    return u3kdb_put(sem, u3a_take(u3h(n)), u3a_take(u3t(n)));
  }
}

/* _cj_remarry(): merge parent lists.
 *                sel is TRANSFERRED.
 *                jul is RETAINED.
 */
static u3_noun
_cj_remarry(u3_noun sel, u3_noun jul)
{
  if ( u3_nul == sel ) {
    return u3a_take(jul);
  }
  else if ( u3_nul == jul ) {
    return sel;
  }
  else {
    u3_noun sap = u3h(sel),
            jup = u3h(jul),
            sax = u3h(sap),
            jux = u3h(jup);
    if ( c3y == u3r_sing(sax, jux) ) {
      u3_noun lol = _cj_uni_jun(u3k(u3t(sap)), u3t(jup)),
              par = u3nc(u3k(u3h(sap)), lol),
              nex = _cj_remarry(u3k(u3t(sel)), u3t(jul)),
              pro = u3nc(par, nex);
      u3z(sel);
      return pro;
    }
    else if ( c3y == u3qa_lth(sax, jux) ) {
      u3_noun nex = _cj_remarry(u3k(u3t(sel)), jul),
              pro = u3nc(u3k(sap), nex);
      u3z(sel);
      return pro;
    }
    else {
      return u3nc(u3a_take(jup), _cj_remarry(sel, u3t(jul)));
    }
  }
}

/* _cj_cold_reap(): reap cold dashboard entries.
 */
static void
_cj_cold_reap(u3_noun kev)
{
  u3_noun jur = u3t(kev);
  u3_noun bat = u3a_take(u3h(kev));
  u3_weak ser = _cj_find_cold(bat);
  u3_noun reg = ( u3_none == ser )
              ? u3a_take(jur)
              : u3nc(_cj_uni_jun(u3k(u3h(ser)), u3h(jur)),
                     _cj_remarry(u3k(u3t(ser)), u3t(jur)));
  u3h_put(u3R->jed.cod_p, bat, reg);
  u3z(ser); u3z(bat);
}

/* u3j_reap(): promote jet state.
*/
void
u3j_reap(u3p(u3h_root) cod_p, u3p(u3h_root) war_p)
{
  u3h_walk(cod_p, _cj_cold_reap);
  u3h_walk(war_p, _cj_warm_reap);
}

/* _cj_ream(): ream list of battery registry pairs. RETAIN.
 */
static void
_cj_ream(u3_noun all)
{
  c3_l par_l, jax_l;
  u3_noun i, j, k, rul, loc, bal, act, lop, kev, rut, hap,
          pat, reg, pol, rem, rec, bat, pel, nam, huc;
  u3_weak pac;

  for ( i = all, lop = u3_nul; i != u3_nul; i = u3t(i) ) {
    kev = u3h(i);
    bat = u3h(kev);
    reg = u3t(kev);
    rut = u3h(reg);

    // register roots
    rul   = u3qdb_tap(rut);
    for ( j = rul; j != u3_nul; j = u3t(j) ) {
      loc   = u3t(u3h(j));
      u3x_trel(loc, &pat, &nam, &huc);
      bal   = u3nc(u3k(nam), u3_nul);
      jax_l = _cj_hot_mean(0, nam);
      hap   = _cj_warm_hump(jax_l, huc);
      act   = u3nq(jax_l, hap, bal, _cj_jit(jax_l, bat));
#if 0
      u3m_p("old jet", bal);
      fprintf(stderr, "  bat %x, jax %d\r\n", u3r_mug(bat), jax_l);
#endif
      u3h_put(u3R->jed.war_p, loc, act);
    }
    u3z(rul);

    // put ancestors in lop (list [battery=^ parent=location this=location])
    for ( j = u3t(reg); j != u3_nul; j = u3t(j) ) {
      pol = lop;
      lop = u3qdb_tap(u3t(u3h(j)));
      for ( k = lop; u3_nul != k; k = u3t(k) ) {
        pol = u3nc(u3nc(u3k(bat), u3k(u3h(k))), pol);
      }
      u3z(lop);
      lop = pol;
    }
  }

  // ordering is random so we need to push onto rem when parent
  // isn't yet present in the warm state
  while ( u3_nul != lop ) {
    rem = u3_nul;
    for ( i = lop; u3_nul != i; i = u3t(i) ) {
      rec = u3h(i);
      u3x_trel(rec, &bat, &pel, &loc);
      pac = _cj_find_warm(pel);
      if ( u3_none == pac ) {
        rem = u3nc(u3k(rec), rem);
      }
      else {
        u3x_trel(loc, &pat, &nam, &huc);
        par_l = u3h(pac);
        jax_l = _cj_hot_mean(par_l, nam);
        bal   = u3nc(u3k(nam), u3k(u3h(u3t(u3t(pac)))));
        u3z(pac);

        act = u3nq(jax_l, 
                   _cj_warm_hump(jax_l, huc),
                   bal,
                   _cj_jit(jax_l, bat));
#if 0
        u3m_p("old jet", bal);
        fprintf(stderr, "  bat %x, jax %d\r\n", u3r_mug(bat), jax_l);
#endif
        u3h_put(u3R->jed.war_p, loc, act);
      }
      lop = u3t(lop);
    }
    u3z(lop);
    lop = rem;
  }
}

/* _cj_warm_tap(): tap war_p to rel
*/
static u3_noun rel;
static void
_cj_warm_tap(u3_noun kev)
{
  rel = u3nc(u3k(kev), u3k(rel));
}

/* u3j_ream(): rebuild warm state
*/
void
u3j_ream(void)
{
  u3h_free(u3R->jed.war_p);
  u3R->jed.war_p = u3h_new();
  c3_assert(u3R == &(u3H->rod_u));
  rel = u3_nul;
  u3h_walk(u3R->jed.cod_p, _cj_warm_tap);
  _cj_ream(rel);
  u3z(rel);
}

/*  XX FIXME: move to u3.md
|%
+=  location    $:  pattern=(each static dynamic)
                    name=term
                    hooks=(map term axis)
                ==
+=  static      (each payload=* parent=static)
+=  dynamic     [where=axis parent=location]
::
+=  registry    [roots=(map * location) parents=(list parent)]
+=  parent      (pair axis (map location location))
::
+=  activation  $:  hot-index=@ud
                    drivers=(map axis @ud)
                    label=path
                    jit=*
                ==
::
+=  cold        (map battery=^ registry)
+=  warm        (map location activation)
--
*/
