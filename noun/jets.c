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

u3_weak
u3j_spot(u3_noun cor)
{
  u3_weak reg = _cj_find_cold(u3h(cor));
  if ( u3_none == reg ) {
    return u3_none;
  }
  else {
    u3_noun rut, ban;
    u3x_cell(reg, &rut, &ban);
    u3_noun rum = u3qdb_get(rut, u3t(cor));
    if ( u3_nul != rum ) {
      u3_noun loc = u3nt(c3y, c3y, u3k(u3t(rum)));
      u3z(rum);
      return loc;
    }
    else {
      while ( u3_nul != ban ) {
        u3_weak pel, par;
        u3_noun axe, luk;
        u3x_cell(u3h(ban), &axe, &luk);
        par = u3r_at(axe, cor);
        if ( u3_none == par ) {
          return u3_none;
        }
        pel = u3j_spot(par);
        if ( u3_none != pel ) {
          u3_noun huc = u3qdb_get(luk, pel);
          if ( u3_nul == huc ) {
            u3z(pel);
          }
          else {
            u3_noun loc = ( (3 == axe) && (c3y == u3h(pel)) )
                        ? u3nc(c3y,
                            u3nq(c3n, u3k(nam), pel, u3k(u3t(huc))))
                        : u3nc(c3n,
                            u3nq(u3k(nam), u3k(axe), pel, u3k(u3t(huc))));
            u3z(huc);
            return loc;
          }
        }
        ban = u3t(ban);
      }
      return u3_none;
    }
  }
}

/* _cj_scan(): has this core been registered?
 */
static c3_o
_cj_scan(u3_noun cor)
{
  u3_weak reg = _cj_find_cold(u3h(cor));
  if ( u3_none == reg ) {
    return c3n;
  }
  else {
    u3_noun rut, ban;
    u3x_cell(reg, &rut, &ban);
    u3_noun rum = u3qdb_get(rut, u3t(cor));
    if ( u3_nul != rum ) {
      u3z(rum);
      return c3y;
    }
    else {
      while ( u3_nul != ban ) {
        u3_weak par;
        u3_noun axe, luk;
        u3x_cell(u3h(ban), &axe, &luk);
        par = u3r_at(axe, cor);
        if ( u3_none == par ) {
          return c3n;
        }
        else if ( c3y == _cj_scan(par) ) {
          return c3y;
        }
        else {
          ban = u3t(ban);
        }
      }
      return c3n;
    }
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
  if ( c3n == u3du(cor) ) {
    return u3m_bail(c3__fail);
  }
  else {
    u3_weak loc = u3j_spot(cor);

    if ( u3_none == loc ) {
      return u3m_bail(c3__fail);
    }
    else {
      u3j_core* cop_u;
      u3_noun tem = u3i_string(tam_c),
              got = u3_nul;
      while ( 1 ) {
        if ( c3y == u3h(loc) ) {
          u3_noun sat = u3t(loc),
                  las = u3t(u3t(sat)),
                  huc = u3t(las);
          got = u3qdb_get(huc, tem);
          if ( u3_nul != got ) {
            break;
          }
          else {
            if ( c3y == u3h(sat) ) {
              return u3m_bail(c3__fail);
            }
            else {
              u3_noun col = u3k(u3h(las));
              cor = u3t(cor);
              u3z(loc);
              loc = col;
            }
          }
        }
        else {
          u3_noun nam, axe, par, huc;
          u3x_qual(u3t(loc), &nam, &axe, &par, &huc);
          got = u3qdb_get(huc, tem);
          if ( u3_nul != got ) {
            break;
          }
          else {
            u3_noun col = u3k(par);
            u3z(loc);
            loc = col;
            cor = u3r_at(axe, cor);
            c3_assert(u3_none != cor);
          }
        }
      }
     
      u3z(tem);
      fol = u3k(u3t(got));
      u3z(got);
      axe_l = _cj_axis(fol);

      if ( 0 == axe_l ) {
        pro = u3n_nock_on(cor, fol);
      }
      else {
        c3_l axe_l, inx_l;
        u3_noun hap, act, pro, fol;

        u3z(fol);
        act  = _cj_find_warm(loc);
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

/* _cj_register_old(): register hooks and parent location within existing 
 *                     axis in ancestor list or u3_none. RETAIN.
 */
static u3_weak
_cj_register_old(u3_noun lan, u3_noun loc, u3_noun huc, u3_noun axe)
{
  u3_noun ank;
  if ( u3_nul == lan ) {
    return u3_none;
  }
  else if ( u3r_sing(axe, ank = u3h(lan)) ) {
    u3_noun hal = u3qdb_put(u3t(ank), loc, huc),
            kan = u3nc(axe, hal);
    return u3nc(kan, u3k(u3t(lan)));
  }
  else {
    u3_weak nex = _cj_register_old(u3t(lan), loc, huc, axe);
    if ( u3_none == nex ) {
      return u3_none;
    }
    else {
      return u3nc(u3k(ank), nex);
    }
  }
}

/* cj_register_new(): insert ancestor within lan at sorted index. RETAIN.
 */
static u3_noun
_cj_register_new(u3_noun lan, u3_noun loc, u3_noun huc, u3_noun axe)
{
  u3_noun ank;
  if ( (u3_nul == lan) || (c3y == u3qa_lth(axe, u3h(u3h(lan)))) ) {
    ank = u3nc(axe, u3qdb_put(u3_nul, loc, huc));
    return u3nc(ank, u3k(lan));
  }
  else {
    return u3nc(u3k(u3h(lan)),
                _cj_register_new(u3t(lan), loc, huc, axe));
  }
}

/* _cj_register(): register a location as an ancestor in a list of ancestors.
 *                 RETAIN.
 */
static u3_noun
_cj_register(u3_noun lan, u3_noun loc, u3_noun huc, u3_noun axe)
{
  u3_weak old = _cj_register_old(lan, loc, huc, axe);
  if ( u3_none != old ) {
    return old;
  }
  else {
    return _cj_register_new(lan, loc, huc, axe);
  }
}

/* _cj_mine(): declare a core.  RETAIN.
*/
static void
_cj_mine(u3_noun cey, u3_noun cor)
{
  c3_l jax_l, par_l;
  u3_weak par, pel;
  u3_noun pac, rut, ger, pay, ank, bal,
          nam, axe, huc, reg, loc, act;
  u3_noun bat = u3h(cor);

  u3x_trel(cey, &nam, &axe, &huc);

  if ( 0 == axe ) {
    pay   = u3t(cor);
    jax_l = 0;
    reg   = _cj_find_cold(bat);
    rut   = u3nt(u3k(nam), u3k(pay), u3k(huc));
    loc   = u3nt(c3y, c3y, rut);
    if ( u3_none == reg ) {
      reg = u3nc(u3_nul, u3_nul);
    }
    ger   = u3nc(u3qdb_put(u3h(reg), pay, rut), u3k(u3t(reg)));
    u3z(reg);
    reg   = ger;
    bal   = u3nc(u3k(nam), u3_nul);
    act   = u3nq(jax_l, u3_nul, bal, _cj_jit(jax_l, bat));
  }
  else {
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
    jax_l = _cj_hot_mean(par_l, nam);
    bal   = u3nc(u3k(nam), u3k(u3h(u3t(u3t(pac)))));
    u3z(pac);
    act = u3nq(jax_l, 
               _cj_warm_hump(jax_l, huc),
               bal,
               _cj_jit(jax_l, bat));
    loc = ( (3 == axe) && (c3y == u3h(pel)) )
        ? u3nt(c3y, c3n, u3nt(u3k(nam), pel, u3k(huc)))
        : u3nc(c3n, u3nq(u3k(nam), u3k(axe), pel, u3k(huc)));
    reg = _cj_find_cold(bat);
    if ( u3_none == reg ) {
      reg = u3nc(u3_nul, u3_nul);
    }
    ank = _cj_register(u3t(reg), loc, huc, axe);
    ger = u3nc(u3k(u3h(reg)), ank);
    u3z(reg);
    reg = ger;
  }
  u3h_put(u3R->jed.cod_p, bat, reg);
  u3h_put(u3R->jed.war_p, loc, act);
  u3z(loc);
#if 0
  u3m_p("new jet", bal);
  fprintf(stderr, "  bat %x, jax %d\r\n", u3r_mug(bat), jax_l);
#endif
}

/* u3j_mine(): register core for jets.
*/
void
u3j_mine(u3_noun clu, u3_noun cor)
{
  u3_noun bat = u3h(cor);
  u3_noun cax;

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
  u3h_put(u3R->jed.har_p, loc, act);
  u3z(loc);
}

/* 
|%
+=  battery     ^
+=  hooks       (map term axis)
+=  root        [name=term payload=* hooks]
+=  branch      [name=term parent=static hooks]
+=  dynamic     [name=term where=axis parent=location hooks]
+=  static      (each root branch)
+=  location    (each static dynamic)
+=  registry    [roots=(map * root) parents=(list ancestor)]
+=  ancestor    (pair axis (map location location))
::
+=  activation  $:  hot-index=@ud
                    drivers=(map axis @ud)
                    label=path
                    jit=*
                ==
::
+=  cold        (map battery registry)
+=  warm        (map location activation)
--
*/

/* _cj_merge_ancestors(): merge ancestor lists.
 *                        sel is TRANSFERRED.
 *                        jul is RETAINED.
 */
static u3_noun
_cj_merge_ancestors(u3_noun sel, u3_noun jul)
{
  u3_noun kel, kev, i, j, ank, les, axe, loc, huc;
  for ( i = jul; u3_nul != i; i = u3t(i) ) {
    ank = u3h(i);
    axe = u3a_take(u3h(ank));
    kel = u3qdb_tap(u3t(ank));
    for ( j = kel; u3_nul != j; j = u3t(j) ) {
      kev = u3h(j);
      loc = u3a_take(u3h(kev));
      huc = u3a_take(u3t(kev));
      les = _cj_register(sel, loc, huc, axe);
      u3z(sel); u3z(loc); u3z(huc);
      sel = les;
    }
    u3z(kel); u3z(ank); u3z(axe);
  }
  return sel;
}

/* _cj_merge_roots(): merge root maps of ancestor.
 *                    sor is TRANSFERRED.
 *                    jor is RETAINED.
 */
static u3_noun
_cj_merge_roots(u3_noun sor, u3_noun jor)
{
  u3_noun i, rut, kel = u3qdb_tap(jor);
  for ( i = kel; u3_nul != i; i = u3t(i) ) {
    rut = u3h(i);
    sor = u3kdb_put(sor, u3a_take(u3h(rut)), u3a_take(u3t(rut)));
  }
  u3z(kel);
  return sor;
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
              : u3nc(_cj_merge_roots(u3k(u3h(ser)), u3h(jur)),
                     _cj_merge_ancestors(u3k(u3t(ser)), u3t(jur)));
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

static c3_l _cj_warm_ream_at(u3_noun soh, u3_noun* lab, u3_noun cag);

/* _cj_warm_ream_be(): install battery; RETAINS.
*/
static void
_cj_warm_ream_be(c3_l    jax_l,
                 u3_noun soh,
                 u3_noun lab,
                 u3_noun mop,
                 u3_noun bat,
                 u3_noun cuz)
{
#if 0
  u3m_p("old jet", lab);
  fprintf(stderr, "  bat %x, soh %x, jax %d\r\n", 
      u3r_mug(bat), u3r_mug(soh), jax_l);
#endif

  u3h_put(u3R->jed.har_p,
            bat,
            u3nt(u3nq(jax_l, 
                      _cj_warm_hump(jax_l, u3t(cuz)), 
                      u3k(lab),
                      _cj_jit(jax_l, bat)),
                 u3nc(u3k(soh), u3k(mop)),
                 u3k(cuz)));
}

/* _cj_warm_ream_is(): reream battery; RETAINS.
*/
static void
_cj_warm_ream_is(c3_l    jax_l, 
                 u3_noun soh,
                 u3_noun lab,
                 u3_noun mop,
                 u3_noun sab)
{
  if ( u3_nul != sab ) {
    u3_noun n_sab, l_sab, r_sab, pn_sab, qn_sab;

    u3x_trel(sab, &n_sab, &l_sab, &r_sab);
    u3x_cell(n_sab, &pn_sab, &qn_sab);

    _cj_warm_ream_be(jax_l, soh, lab, mop, pn_sab, qn_sab);
    _cj_warm_ream_is(jax_l, soh, lab, mop, l_sab);
    _cj_warm_ream_is(jax_l, soh, lab, mop, r_sab);
  }
}

/* _cj_warm_ream_un(): reream under `soh`; RETAINS, transfers `*lab`.
*/
static c3_l
_cj_warm_ream_un(u3_noun soh, u3_noun* lab)
{
  u3_noun cag = u3kdb_got(u3k(u3R->jed.das), u3k(soh));
  u3_noun sab = u3t(cag);
  u3_noun cax;
  c3_l    jax_l;

  if ( u3_none != (cax = u3h_get(u3R->jed.har_p, u3h(u3h(sab)))) ) {
    jax_l = u3h(u3h(cax));
    *lab = u3k(u3h(u3t(u3t(u3h(cax)))));
    u3z(cax);
  }
  else {
    jax_l = _cj_warm_ream_at(soh, lab, cag);
  }
  u3z(cag);
  return jax_l;
}

/* _cj_warm_ream_at(): reream at `soh` and `cag`; RETAINS, transfers `*lab`.
*/
static c3_l
_cj_warm_ream_at(u3_noun soh, u3_noun* lab, u3_noun cag)
{
  u3_noun mop = u3h(cag);
  u3_noun sab = u3t(cag);
  u3_noun p_mop, q_mop, r_mop, hr_mop, tr_mop;

  u3x_trel(mop, &p_mop, &q_mop, &r_mop);
  u3x_cell(r_mop, &hr_mop, &tr_mop);
  {
    c3_l    par_l, jax_l;
    u3_noun pal = u3_nul;

    if ( c3y == hr_mop ) {
      par_l = _cj_warm_ream_un(tr_mop, &pal);
    }
    else {
      par_l = 0;
      pal = u3_nul;
    }
    *lab = u3nc(u3k(p_mop), pal);
    jax_l = _cj_hot_mean(par_l, u3h(mop));
  
    _cj_warm_ream_is(jax_l, soh, *lab, mop, sab);
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
    u3_noun lab;

    u3x_trel(taw, &n_taw, &l_taw, &r_taw);
    u3x_cell(n_taw, &pn_taw, &qn_taw);

    _cj_warm_ream_at(pn_taw, &lab, qn_taw);
    u3z(lab);

    _cj_warm_ream_in(l_taw);
    _cj_warm_ream_in(r_taw);
  }
}


/* _cj_warm_ream_all(): tap cod_p to rel
*/
static u3_noun rel;
static void
_cj_warm_ream_all(u3_noun kev)
{
  rel = u3nc(kev, u3k(rel));
}

/* u3j_ream(): rebuild warm state from cold state
*/
void
u3j_ream(void)
{
  u3_noun rut, lan, bat, reg, kev, rem, dol, lok;
  u3_weak pel;
  u3h_free(u3R->jed.war_p);
  u3R->jed.war_p = u3h_new();
  c3_assert(u3R == &(u3H->rod_u));
  rel = u3_nul;
  u3h_walk(u3R->jed.cod_p, _cj_warm_ream_all);

  for ( lok = rel, dol = u3_nul; lok != u3_nul; lok = u3t(lok) ) {
    kev = u3h(lok);
    bat = u3h(kev);
    reg = u3t(kev);
    rut = u3h(reg);

    // register roots
    rul   = u3qdb_tap(rut);
    jax_l = 0;
    for ( rep = rul; rep != u3_nul; rep = u3t(rep) ) {
      nut = u3h(rep);
      rin = u3t(nut);
      bal = u3nc(u3k(u3h(rin)), u3_nul);
      act = u3nq(jax_l, u3_nul, bal, _cj_jit(jax_l, bat));
      loc = u3nt(c3y, c3y, u3k(rin));
      u3h_put(u3R->jed.war_p, loc, act);
    }
    u3z(rul);

    // put ancestors in dol
    for ( lan = u3t(reg); lan != u3_nul; lan = u3t(lan) ) {
      ank = u3h(lan);
      axe = u3h(ank);
      kel = u3qdb_tap(u3t(ank));
      for ( pul = kel; pul != u3_nul; pul = u3t(pul) ) {
        dol = u3nc(u3nc(u3k(axe, u3k(u3h(pul)))), u3k(dol));
      }
      u3z(kel);
    }
  }
  u3z(rel);

  while ( u3_nul != dol ) {
    top = dol;
    rem = u3_nul;
    while ( u3_nul != dol ) {
      rec = u3h(dol);
      dol = u3t(dol);
      u3x_qual(rec, &axe, &pel, &nam, &huc);
      pac = _cj_find_warm(pel);
      if ( u3_none == pac ) {
        rem = u3nc(u3k(rec), u3k(rem));
      }
      else {
        par_l = u3h(pac);
        jax_l = _cj_hot_mean(par_l, nam);
        bal   = u3nc(u3k(nam), u3k(u3h(u3t(u3t(pac)))));
        u3z(pac);
        act = u3nq(jax_l, 
                   _cj_warm_hump(jax_l, huc),
                   bal,
                   _cj_jit(jax_l, bat));
        loc = ( (3 == axe) && (c3y == u3h(pel)) )
            ? u3nt(c3y, c3n, u3nt(u3k(nam), pel, u3k(huc)))
            : u3nc(c3n, u3nq(u3k(nam), u3k(axe), pel, u3k(huc)));
        reg = _cj_find_cold(bat);
      }
    }
    u3z(top);
    dol = rem;
  }

  w
  for ( top = dol, rem = u3_nul;
        dol != u3_nul;
         = dol = rem ) {
    rec = u3h(dol);

  }
}
