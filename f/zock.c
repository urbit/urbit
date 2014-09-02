/* f/zock.c
**
** This file is in the public domain.
*/
#include "f/meme.h"

/* u2_cj_boot(): initialize jet system.
*/
  /* _cj_count(): count and link dashboard entries.
  */
  static c3_w 
  _cj_count(u2_cs_core* par_u, u2_cs_core* dev_u)
  {
    c3_w len_l = 0;
    c3_w i_w;

    for ( i_w = 0; 0 != dev_u[len_l].cos_c; i_w++ ) {
      u2_cs_core* kid_u = &dev_u[i_w];

      kid_u->par_u = par_u;
      len_l = _cj_count(kid_u, kid_u->dev_u);
    }
    return 1 + len_l;
  }
  /* _cj_install(): install dashboard entries.
  */
  static c3_w
  _cj_install(u2_cs_core* ray_u, c3_w jax_l, u2_cs_core* dev_u)
  {
    c3_w i_w;

    for ( i_w = 0; 0 != dev_u[i_w].cos_c; i_w++ ) {
      u2_cs_core* kid_u = &dev_u[i_w];

      kid_u->jax_l = jax_l;
      ray_u[jax_l++] = *kid_u;

      jax_l = _cj_install(ray_u, jax_l, kid_u->dev_u);
    }
    return jax_l;
  }
/* u2_cj_boot(): initialize jet system.
*/
void
u2_cj_boot(void)
{
  u2D.len_l =_cj_count(0, u2D.dev_u);

  u2D.ray_u = (u2_cs_core*) malloc(u2D.len_l * sizeof(u2_cs_core));
  memset(u2D.ray_u, 0, (u2D.len_l * sizeof(u2_cs_core)));

  _cj_install(u2D.ray_u, 1, u2D.dev_u);
}

/* _cj_insert(): append copy of core driver to jet table.
*/
static c3_l
_cj_insert(u2_cs_core* cop_u)
{
  c3_l jax_l = u2D.len_l;

  u2D.len_l += 1;
  u2D.ray_u = realloc(u2D.ray_u, u2D.len_l * sizeof(u2_cs_core));

  memcpy(&u2D.ray_u[jax_l], cop_u, sizeof(u2_cs_core));
  cop_u->jax_l = jax_l;

  return jax_l;
}

/* u2_cj_find(): search for jet.  `cor` is RETAINED.
*/
c3_l
u2_cj_find(u2_noun bat)
{
  u2_weak jax = u2_ch_get(u2R->jed.har_u, bat);

  if ( u2_none == jax ) {
    return 0;
  } else {
    c3_assert(u2_co_is_cat(jax));

    return (c3_l)jax;
  }
}

/* _cj_kick_a(): try to kick by jet.  If no kick, produce u2_none.
**
** `cor` is RETAINED iff there is no kick, TRANSFERRED if one.
*/
static u2_weak
_cj_kick_a(u2_noun cor, u2_cs_hood* hud_u, c3_l axe_l)
{
  u2_cs_harm* ham_u;

  if ( axe_l >= hud_u->len_w ) {
    return u2_none;
  }
  if ( !(ham_u = hud_u->ray_u[axe_l]) ) {
    return u2_none;
  }
  if ( 0 == ham_u->fun_f ) {
    return u2_none;
  }

  // XX: support tot and ice flags, and validator
  //
  {
    u2_weak pro = ham_u->fun_f(cor);

    if ( u2_none != pro ) {
      return pro;
    }
  }
  return u2_none;
}

/* _cj_kick_b(): try to kick by jet.  If no kick, produce u2_none.
**
** `cor` is RETAINED iff there is no kick, TRANSFERRED if one.
*/
static u2_weak
_cj_kick_b(u2_noun cor, c3_l jax_l, c3_l axe_l)
{
  c3_l        mug_l = u2_cr_mug(u2h(cor));
  u2_cs_core* cop_u = &u2D.ray_u[jax_l];
  u2_cs_hood* hud_u = cop_u->hud_u;

  while ( 1 ) {
    if ( 0 == hud_u )                     { break; }
    if ( mug_l != hud_u->mug_l )          { hud_u = hud_u->nex_u; continue; }
    return _cj_kick_a(cor, hud_u, axe_l);
  }
  return u2_none;
}

/* u2_cj_hook(): execute hook from core, or fail.
*/
u2_noun
u2_cj_hook(u2_noun     cor,
           const c3_c* tam_c)
{
  u2_noun bat   = u2h(cor);
  c3_l    jax_l = u2_cj_find(bat);
 
  if ( 0 == jax_l ) { return 0; }
  else {
    u2_cs_core* cop_u = &u2D.ray_u[jax_l];
    u2_cs_hood* hud_u = cop_u->hud_u;
    c3_l        mug_l = u2_cr_mug(bat);

    while ( 1 ) {
      if ( 0 == hud_u )            { break; }
      if ( mug_l != hud_u->mug_l ) { hud_u = hud_u->nex_u; continue; }
      {
        u2_cs_hook* huk_u = hud_u->huk_u;

        while ( huk_u ) {
          if ( !strcmp(huk_u->nam_c, tam_c) ) {
            u2_noun pro = _cj_kick_a(cor, hud_u, huk_u->axe_l);

            if ( u2_none != pro ) {
              return pro;
            } 
            else return u2_cn_nock_on(cor, u2nq(9, huk_u->axe_l, 0, 1));
          }
          huk_u = huk_u->nex_u;
        }
      }
    }
  }
  u2z(cor);
  return u2_cm_bail(c3__fail);
}

/* u2_cj_kick(): try to kick by jet.  If no kick, produce u2_none.
**
** `axe` is RETAINED by the caller; `cor` is RETAINED iff there 
** is no kick, TRANSFERRED if one.
*/
u2_weak
u2_cj_kick(u2_noun cor, u2_noun axe)
{
  c3_l axe_l, jax_l;
 
  if ( u2_ne(u2_co_is_cat(axe)) ) { 
    return u2_none;
  } 
  axe_l = axe;

  if ( 0 == (jax_l = u2_cj_find(u2h(cor))) ) { 
    return u2_none;
  }
  return _cj_kick_b(cor, jax_l, axe_l);
}

/* u2_cj_kink(): kick either by jet or by nock.
*/
u2_noun
u2_cj_kink(u2_noun cor,
           u2_noun axe)
{
  u2_weak pro = u2_cj_kick(cor, axe);

  if ( u2_none != pro ) {
    return pro;
  } else {
    return u2_cn_nock_on(cor, u2nq(9, axe, 0, 1));
  }
}

/* _cj_axis(): axis from formula, or 0.  `fol` is RETAINED.
*/
static c3_l
_cj_axis(u2_noun fol)
{
  u2_noun p_fol, q_fol, r_fol;

  if ( u2_ne(u2_cr_trel(fol, &p_fol, &q_fol, &r_fol)) )
    { fprintf(stderr, "axis: bad a\r\n"); return 0; }
  if ( 9 != p_fol )
    { fprintf(stderr, "axis: bad b\r\n"); return 0; }
  if ( u2_ne(u2_co_is_cat(q_fol)) )
    { fprintf(stderr, "axis: bad c\r\n"); return 0; }
  if ( u2_ne(u2du(r_fol)) || (0 != u2h(r_fol)) || (1 != u2t(r_fol)) )
    { fprintf(stderr, "axis: bad d\r\n"); return 0; }

  return q_fol;
}

/* _cj_activate(): activate jets in `cop` for `hud`.
*/
static void
_cj_activate(u2_cs_core* cop_u, u2_cs_hood* hud_u)
{
  c3_l max_l = 0;

  /* Check for mismatched duplicates - very unlikely.
  */
  {
    u2_cs_hood* duh_u = cop_u->hud_u;

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
      u2_cs_harm* jet_u = &cop_u->arm_u[i_l];

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
          u2_cs_hook* huk_u = hud_u->huk_u;

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
      hud_u->ray_u = malloc(hud_u->len_w * (sizeof(u2_cs_harm *)));

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
      u2_cs_harm* jet_u = &cop_u->arm_u[i_l];

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

/* u2_cj_mine(): register core for jets.
*/
u2_noun
u2_cj_mine(u2_noun clu,
           u2_noun cor)
{
  if ( u2_none != u2_ch_get(u2R->jed.har_u, u2h(cor)) ) {
    u2z(clu);
    return cor;
  }
  else {
    u2_noun     p_clu, q_clu, r_clu;
    u2_cs_hook* huk_u;
    u2_cs_hood* hud_u;
    c3_l        axe_l, par_l;
    u2_noun     nam;
    u2_noun     pab;

    if ( u2_no == u2_cr_trel(clu, &p_clu, &q_clu, &r_clu) )
      { fprintf(stderr, "mine: bad z\r\n"); u2z(clu); return cor; }
    if ( u2_ne(u2ud(nam = p_clu)) ) 
      { fprintf(stderr, "mine: bad a\r\n"); u2z(clu); return cor; }
    if ( u2_ne(u2du(q_clu)) )
      { fprintf(stderr, "mine: bad b\r\n"); u2z(clu); return cor; }
    if ( (0 != u2h(q_clu)) )
      { fprintf(stderr, "mine: bad c\r\n"); u2z(clu); return cor; }
    if ( u2_ne(u2_co_is_cat(axe_l = u2t(q_clu))) )
      { fprintf(stderr, "mine: bad d\r\n"); u2z(clu); return cor; }
    if ( (u2_none == (pab = u2_cr_at(axe_l, cor))) )
      { fprintf(stderr, "mine: bad e\r\n"); u2z(clu); return cor; }
    if ( (0 == (par_l = u2_cj_find(pab))) ) 
      { fprintf(stderr, "mine: bad f\r\n"); u2z(clu); return cor; }

    huk_u = 0;
    while ( 0 != r_clu ) {
      u2_noun ir_clu, tr_clu, pir_clu, qir_clu;
      u2_cs_hook* kuh_u;
      c3_l        kax_l;

      if ( u2_no == u2_cr_cell(clu, &ir_clu, &tr_clu) )
        { fprintf(stderr, "mine: bad g\r\n"); u2z(clu); return cor; }
      if ( u2_no == u2_cr_cell(ir_clu, &pir_clu, &qir_clu) )
        { fprintf(stderr, "mine: bad h\r\n"); u2z(clu); return cor; }
      if ( u2_ne(u2ud(pir_clu)) )
        { fprintf(stderr, "mine: bad i\r\n"); u2z(clu); return cor; }
      if ( 0 == (kax_l = _cj_axis(qir_clu)) ) 
        { fprintf(stderr, "mine: bad j\r\n"); u2z(clu); return cor; }

      kuh_u = malloc(sizeof(u2_cs_hook));
      kuh_u->nam_c = u2_cr_string(pir_clu);
      kuh_u->axe_l = kax_l;

      kuh_u->nex_u = huk_u;
      huk_u = kuh_u;
    }
    hud_u = malloc(sizeof(u2_cs_hood));
    hud_u->mug_l = u2_cr_mug(u2h(cor));
    hud_u->len_w = 0;
    hud_u->ray_u = 0;
    hud_u->huk_u = huk_u;
    hud_u->nex_u = 0;

    // Find the child, if possible, in the parent.  Otherwise, 
    // register it to avoid slow repeated search.
    //
    { 
      u2_cs_core* par_u = &u2D.ray_u[par_l];
      c3_l        jax_l = 0;
      c3_w        i_l = 0;

      while ( 1 ) {
        u2_cs_core* cop_u = &par_u->dev_u[i_l];

        if ( 0 == cop_u->cos_c ) { break; }
        if ( u2_so(u2_cr_sing_c(cop_u->cos_c, nam)) ) {
          jax_l = cop_u->jax_l;
          c3_assert(0 != jax_l);
          break;
        }
        i_l++;
      }
   
      if ( 0 == jax_l ) {
        u2_cs_core fak_u;

        memset(&fak_u, 0, sizeof(u2_cs_core));
        fak_u.cos_c = u2_cr_string(nam);
        fak_u.par_u = par_u;

        jax_l =_cj_insert(&fak_u);
      }
      u2_ch_put(u2R->jed.har_u, u2h(cor), jax_l);
      u2z(clu);

      _cj_activate(&u2D.ray_u[jax_l], hud_u);
      return cor;
    }
  }
}
