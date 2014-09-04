/* j/6/al.c
**
** This file is in the public domain.
*/
#include "all.h"


/* ~(. al gen)
*/
static u2_noun
_al_core(u2_noun van,
         u2_noun gen)
{
  u2_noun ter = u2_cr_at(u2_cv_con_3, van);
  u2_noun gat = u2_cj_hook(u2k(ter), "al");

  return u2_ci_molt(gat, u2_cv_sam, u2k(gen), 0);
}

/* ~(bunt al gen)
*/
u2_noun
u2_cqfl_bunt(u2_noun van,
                      u2_noun gen)
{
  u2_noun cor = _al_core(van, gen);

  return u2_cj_hook(cor, "bunt");
}

/* ~(whip al gen)
*/
u2_noun
u2_cqfl_whip(u2_noun van,
                      u2_noun gen,
                      u2_noun axe)                              //
{
  u2_noun cor = _al_core(van, gen);
  u2_noun gat = u2_cj_hook(u2k(cor), "whip");

  return u2_cn_slam_on(gat, u2k(axe));
}
