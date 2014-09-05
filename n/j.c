/* n/j.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u3_cj_boot(): initialize jet system.
*/
  /* _cj_count(): count and link dashboard entries.
  */
  static c3_w 
  _cj_count(u3_cs_core* par_u, u3_cs_core* dev_u)
  {
    c3_w len_l = 0;
    c3_w i_w;

    for ( i_w = 0; 0 != dev_u[len_l].cos_c; i_w++ ) {
      u3_cs_core* kid_u = &dev_u[i_w];

      kid_u->par_u = par_u;
      len_l = _cj_count(kid_u, kid_u->dev_u);
    }
    return 1 + len_l;
  }
  /* _cj_install(): install dashboard entries.
  */
  static c3_w
  _cj_install(u3_cs_core* ray_u, c3_w jax_l, u3_cs_core* dev_u)
  {
    c3_w i_w;

    for ( i_w = 0; 0 != dev_u[i_w].cos_c; i_w++ ) {
      u3_cs_core* kid_u = &dev_u[i_w];

      kid_u->jax_l = jax_l;
      ray_u[jax_l++] = *kid_u;

      jax_l = _cj_install(ray_u, jax_l, kid_u->dev_u);
    }
    return jax_l;
  }
/* u3_cj_boot(): initialize jet system.
*/
void
u3_cj_boot(void)
{
  u3D.len_l =_cj_count(0, u3D.dev_u);

  u3D.ray_u = (u3_cs_core*) malloc(u3D.len_l * sizeof(u3_cs_core));
  memset(u3D.ray_u, 0, (u3D.len_l * sizeof(u3_cs_core)));

  _cj_install(u3D.ray_u, 1, u3D.dev_u);
}

/* _cj_insert(): append copy of core driver to jet table.
*/
static c3_l
_cj_insert(u3_cs_core* cop_u)
{
  c3_l jax_l = u3D.len_l;

  u3D.len_l += 1;
  u3D.ray_u = realloc(u3D.ray_u, u3D.len_l * sizeof(u3_cs_core));

  memcpy(&u3D.ray_u[jax_l], cop_u, sizeof(u3_cs_core));
  cop_u->jax_l = jax_l;

  return jax_l;
}

/* u3_cj_find(): search for jet.  `cor` is RETAINED.
*/
c3_l
u3_cj_find(u3_noun bat)
{
  u3_weak jax = u3_ch_get(u3R->jed.har_u, bat);

  if ( u3_none == jax ) {
    return 0;
  } else {
    c3_assert(u3_co_is_cat(jax));

    return (c3_l)jax;
  }
}

/* _cj_kick_a(): try to kick by jet.  If no kick, produce u3_none.
**
** `cor` is RETAINED iff there is no kick, TRANSFERRED if one.
*/
static u3_weak
_cj_kick_a(u3_noun cor, u3_cs_hood* hud_u, c3_l axe_l)
{
  u3_cs_harm* ham_u;

  if ( axe_l >= hud_u->len_w ) {
    return u3_none;
  }
  if ( !(ham_u = hud_u->ray_u[axe_l]) ) {
    return u3_none;
  }
  if ( 0 == ham_u->fun_f ) {
    return u3_none;
  }

  // XX: support tot and ice flags, and validator
  //
  {
    u3_weak pro = ham_u->fun_f(cor);

    if ( u3_none != pro ) {
      return pro;
    }
  }
  return u3_none;
}

/* _cj_kick_b(): try to kick by jet.  If no kick, produce u3_none.
**
** `cor` is RETAINED iff there is no kick, TRANSFERRED if one.
*/
static u3_weak
_cj_kick_b(u3_noun cor, c3_l jax_l, c3_l axe_l)
{
  c3_l        mug_l = u3_cr_mug(u3h(cor));
  u3_cs_core* cop_u = &u3D.ray_u[jax_l];
  u3_cs_hood* hud_u = cop_u->hud_u;

  while ( 1 ) {
    if ( 0 == hud_u )                     { break; }
    if ( mug_l != hud_u->mug_l )          { hud_u = hud_u->nex_u; continue; }
    return _cj_kick_a(cor, hud_u, axe_l);
  }
  return u3_none;
}

/* u3_cj_hook(): execute hook from core, or fail.
*/
u3_noun
u3_cj_hook(u3_noun     cor,
           const c3_c* tam_c)
{
  u3_noun bat   = u3h(cor);
  c3_l    jax_l = u3_cj_find(bat);
 
  if ( 0 == jax_l ) { return 0; }
  else {
    u3_cs_core* cop_u = &u3D.ray_u[jax_l];
    u3_cs_hood* hud_u = cop_u->hud_u;
    c3_l        mug_l = u3_cr_mug(bat);

    while ( 1 ) {
      if ( 0 == hud_u )            { break; }
      if ( mug_l != hud_u->mug_l ) { hud_u = hud_u->nex_u; continue; }
      {
        u3_cs_hook* huk_u = hud_u->huk_u;

        while ( huk_u ) {
          if ( !strcmp(huk_u->nam_c, tam_c) ) {
            u3_noun pro = _cj_kick_a(cor, hud_u, huk_u->axe_l);

            if ( u3_none != pro ) {
              return pro;
            } 
            else return u3_cn_nock_on(cor, u3nq(9, huk_u->axe_l, 0, 1));
          }
          huk_u = huk_u->nex_u;
        }
      }
    }
  }
  u3z(cor);
  return u3_cm_bail(c3__fail);
}

/* u3_cj_kick(): try to kick by jet.  If no kick, produce u3_none.
**
** `axe` is RETAINED by the caller; `cor` is RETAINED iff there 
** is no kick, TRANSFERRED if one.
*/
u3_weak
u3_cj_kick(u3_noun cor, u3_noun axe)
{
  c3_l axe_l, jax_l;
 
  if ( u3_ne(u3_co_is_cat(axe)) ) { 
    return u3_none;
  } 
  axe_l = axe;

  if ( 0 == (jax_l = u3_cj_find(u3h(cor))) ) { 
    return u3_none;
  }
  return _cj_kick_b(cor, jax_l, axe_l);
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

/* _cj_axis(): axis from formula, or 0.  `fol` is RETAINED.
*/
static c3_l
_cj_axis(u3_noun fol)
{
  u3_noun p_fol, q_fol, r_fol;

  if ( u3_ne(u3_cr_trel(fol, &p_fol, &q_fol, &r_fol)) )
    { fprintf(stderr, "axis: bad a\r\n"); return 0; }
  if ( 9 != p_fol )
    { fprintf(stderr, "axis: bad b\r\n"); return 0; }
  if ( u3_ne(u3_co_is_cat(q_fol)) )
    { fprintf(stderr, "axis: bad c\r\n"); return 0; }
  if ( u3_ne(u3du(r_fol)) || (0 != u3h(r_fol)) || (1 != u3t(r_fol)) )
    { fprintf(stderr, "axis: bad d\r\n"); return 0; }

  return q_fol;
}

/* _cj_activate(): activate jets in `cop` for `hud`.
*/
static void
_cj_activate(u3_cs_core* cop_u, u3_cs_hood* hud_u)
{
  c3_l max_l = 0;

  /* Check for mismatched duplicates - very unlikely.
  */
  {
    u3_cs_hood* duh_u = cop_u->hud_u;

    while ( duh_u ) {
      if ( duh_u->mug_l == hud_u->mug_l ) {
        fprintf(stderr, "jets: mug collision!\r\n");
        return;
      }
      duh_u = duh_u->nex_u;
    }
  }

  /* Compute axes of all correctly declared jets.
  */
  {
    c3_l i_l = 0;

    while ( 1 ) {
      u3_cs_harm* jet_u = &cop_u->arm_u[i_l];

      if ( 0 == jet_u->fcs_c ) {
        break;
      } 
      else {
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
          u3_cs_hook* huk_u = hud_u->huk_u;

          while ( huk_u ) {
            if ( !strcmp(huk_u->nam_c, jet_u->fcs_c) ) {
              axe_l = huk_u->axe_l;
              break;
            }
            huk_u = huk_u->nex_u;
          }
        }
        max_l = c3_max(max_l, axe_l);
        jet_u->axe_l = axe_l;
      }
      i_l++;
    }
  }
  
  /* Allocate jet table for this battery.
  */
  {
    c3_w i_l;

    if ( !max_l ) {
      hud_u->len_w = 0;
    }
    else {
      hud_u->len_w = (max_l + 1);
      hud_u->ray_u = malloc(hud_u->len_w * (sizeof(u3_cs_harm *)));

      for ( i_l = 0; i_l < hud_u->len_w; i_l++ ) {
        hud_u->ray_u[i_l] = 0;
      }
    }
  }

  /* Fill jet table.
  */
  {
    c3_l i_l = 0;

    while ( 1 ) {
      u3_cs_harm* jet_u = &cop_u->arm_u[i_l];

      if ( jet_u->axe_l ) {
        hud_u->ray_u[jet_u->axe_l] = jet_u;
      }
    }
  }
 
  /* Link in new battery record.
  */
  {
    hud_u->nex_u = cop_u->hud_u;
    cop_u->hud_u = hud_u;
  }
}

/* u3_cj_mine(): register core for jets.
*/
u3_noun
u3_cj_mine(u3_noun clu,
           u3_noun cor)
{
  if ( u3_none != u3_ch_get(u3R->jed.har_u, u3h(cor)) ) {
    u3z(clu);
    return cor;
  }
  else {
    u3_noun     p_clu, q_clu, r_clu;
    u3_cs_hook* huk_u;
    u3_cs_hood* hud_u;
    c3_l        axe_l, par_l;
    u3_noun     nam;
    u3_noun     pab;

    if ( u3_no == u3_cr_trel(clu, &p_clu, &q_clu, &r_clu) )
      { fprintf(stderr, "mine: bad z\r\n"); u3z(clu); return cor; }
    if ( u3_ne(u3ud(nam = p_clu)) ) 
      { fprintf(stderr, "mine: bad a\r\n"); u3z(clu); return cor; }
    if ( u3_ne(u3du(q_clu)) )
      { fprintf(stderr, "mine: bad b\r\n"); u3z(clu); return cor; }
    if ( (0 != u3h(q_clu)) )
      { fprintf(stderr, "mine: bad c\r\n"); u3z(clu); return cor; }
    if ( u3_ne(u3_co_is_cat(axe_l = u3t(q_clu))) )
      { fprintf(stderr, "mine: bad d\r\n"); u3z(clu); return cor; }
    if ( (u3_none == (pab = u3_cr_at(axe_l, cor))) )
      { fprintf(stderr, "mine: bad e\r\n"); u3z(clu); return cor; }
    if ( (0 == (par_l = u3_cj_find(pab))) ) 
      { fprintf(stderr, "mine: bad f\r\n"); u3z(clu); return cor; }

    huk_u = 0;
    while ( 0 != r_clu ) {
      u3_noun ir_clu, tr_clu, pir_clu, qir_clu;
      u3_cs_hook* kuh_u;
      c3_l        kax_l;

      if ( u3_no == u3_cr_cell(clu, &ir_clu, &tr_clu) )
        { fprintf(stderr, "mine: bad g\r\n"); u3z(clu); return cor; }
      if ( u3_no == u3_cr_cell(ir_clu, &pir_clu, &qir_clu) )
        { fprintf(stderr, "mine: bad h\r\n"); u3z(clu); return cor; }
      if ( u3_ne(u3ud(pir_clu)) )
        { fprintf(stderr, "mine: bad i\r\n"); u3z(clu); return cor; }
      if ( 0 == (kax_l = _cj_axis(qir_clu)) ) 
        { fprintf(stderr, "mine: bad j\r\n"); u3z(clu); return cor; }

      kuh_u = malloc(sizeof(u3_cs_hook));
      kuh_u->nam_c = u3_cr_string(pir_clu);
      kuh_u->axe_l = kax_l;

      kuh_u->nex_u = huk_u;
      huk_u = kuh_u;
    }
    hud_u = malloc(sizeof(u3_cs_hood));
    hud_u->mug_l = u3_cr_mug(u3h(cor));
    hud_u->len_w = 0;
    hud_u->ray_u = 0;
    hud_u->huk_u = huk_u;
    hud_u->nex_u = 0;

    // Find the child, if possible, in the parent.  Otherwise, 
    // register it to avoid slow repeated search.
    //
    { 
      u3_cs_core* par_u = &u3D.ray_u[par_l];
      c3_l        jax_l = 0;
      c3_w        i_l = 0;

      while ( 1 ) {
        u3_cs_core* cop_u = &par_u->dev_u[i_l];

        if ( 0 == cop_u->cos_c ) { break; }
        if ( u3_so(u3_cr_sing_c(cop_u->cos_c, nam)) ) {
          jax_l = cop_u->jax_l;
          c3_assert(0 != jax_l);
          break;
        }
        i_l++;
      }
   
      if ( 0 == jax_l ) {
        u3_cs_core fak_u;

        memset(&fak_u, 0, sizeof(u3_cs_core));
        fak_u.cos_c = u3_cr_string(nam);
        fak_u.par_u = par_u;

        jax_l =_cj_insert(&fak_u);
      }
      u3_ch_put(u3R->jed.har_u, u3h(cor), jax_l);
      u3z(clu);

      _cj_activate(&u3D.ray_u[jax_l], hud_u);
      return cor;
    }
  }
}
