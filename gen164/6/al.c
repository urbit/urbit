/* j/6/al.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* ~(. al gen)
*/
static u2_noun
_al_core(u2_wire wir_r,
         u2_noun van,
         u2_noun gen)
{
  u2_noun ter = u2_frag(u2_cv_con_3, van);
  u2_weak hoc = u2_ds_look(wir_r, ter, "al");

  if ( u2_none == hoc ) {
    return u2_cm_bail(c3__fail);
  }
  else {
    u2_noun gat = u2_nk_soft(wir_r, u2_rx(wir_r, ter), hoc);
    u2_noun cor = u2_rl_molt(wir_r, gat,
                                    u2_cv_sam, u2_rx(wir_r, gen),
                                    0);

    u2_rz(wir_r, hoc);
    u2_rz(wir_r, gat);

    return cor;
  }
}

/* ~(bunt al gen)
*/
u2_noun                                                         //  transfer
j2_mcy(Pt6, al, bunt)(u2_wire wir_r,
                      u2_noun van,                              //  retain
                      u2_noun gen)                              //  retain
{
  u2_noun cor = _al_core(wir_r, van, gen);
  u2_weak hoc = u2_ds_look(wir_r, cor, "bunt");

  if ( u2_none == hoc ) {
    return u2_cm_bail(c3__fail);
  } else {
    u2_noun pro = u2_nk_soft(wir_r, cor, hoc);

    u2_rz(wir_r, hoc);
    return pro;
  }
}

/* ~(whip al gen)
*/
u2_noun                                                         //  transfer
j2_mcy(Pt6, al, whip)(u2_wire wir_r,
                      u2_noun van,                              //  retain
                      u2_noun gen,                              //  retain
                      u2_noun axe)                              //
{
  u2_noun cor = _al_core(wir_r, van, gen);
  u2_weak hoc = u2_ds_look(wir_r, cor, "whip");

  if ( u2_none == hoc ) {
    return u2_cm_bail(c3__fail);
  } else {
    u2_noun gat = u2_nk_soft(wir_r, cor, hoc);
    u2_noun pro = u2_nk_mong(wir_r, gat, u2k(axe));

    u2_rz(wir_r, hoc);
    return pro;
  }
}
