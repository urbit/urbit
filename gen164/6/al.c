/* j/6/al.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* ~(. al gen)
*/
static u2_noun
_al_core(
         u2_noun van,
         u2_noun gen)
{
  u2_noun ter = u2_cr_at(u2_cv_con_3, van);
  u2_weak hoc = u2_cj_look(ter, "al");

  if ( u2_none == hoc ) {
    return u2_cm_bail(c3__fail);
  }
  else {
    u2_noun gat = u2_cn_nock_on(u2k(ter), u2k(hoc));
    u2_noun cor = u2_ci_molt(u2k(gat),
                             u2_cv_sam, u2k(gen),
                             0);
    u2z(hoc);
    u2z(gat);

    return cor;
  }
}

/* ~(bunt al gen)
*/
u2_noun                                                         //  transfer
j2_mcy(Pt6, al, bunt)(
                      u2_noun van,                              //  retain
                      u2_noun gen)                              //  retain
{
  u2_noun cor = _al_core(van, gen);
  u2_weak hoc = u2_cj_look(u2k(cor), "bunt");

  if ( u2_none == hoc ) {
    return u2_cm_bail(c3__fail);
  } else {
    u2_noun pro = u2_cn_nock_on(cor, u2k(hoc));

    u2z(hoc);
    return pro;
  }
}

/* ~(whip al gen)
*/
u2_noun                                                         //  transfer
j2_mcy(Pt6, al, whip)(
                      u2_noun van,                              //  retain
                      u2_noun gen,                              //  retain
                      u2_noun axe)                              //
{
  u2_noun cor = _al_core(van, gen);
  u2_weak hoc = u2_cj_look(u2k(cor), "whip");

  if ( u2_none == hoc ) {
    return u2_cm_bail(c3__fail);
  } else {
    u2_noun gat = u2_cn_nock_on(cor, u2k(hoc));
    u2_noun pro = u2_cn_slam_on(u2k(gat), u2k(axe));

    u2z(hoc);
    return pro;
  }
}
