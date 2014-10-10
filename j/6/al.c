/* j/6/al.c
**
** This file is in the public domain.
*/
#include "all.h"


/* ~(. al gen)
*/
static u3_noun
_al_core(u3_noun van,
         u3_noun gen)
{
  u3_noun ter = u3_cr_at(u3_cv_con_3, van);
  u3_noun gat = u3_cj_hook(u3k(ter), "al");

  return u3_ci_molt(gat, u3_cv_sam, u3k(gen), 0);
}

/* ~(bunt al gen)
*/
u3_noun
u3_cqfl_bunt(u3_noun van, u3_noun gen)
{
  u3_noun cor = _al_core(van, gen);

  return u3_cj_hook(cor, "bunt");
}

/* ~(whip al gen)
*/
u3_noun
u3_cqfl_whip(u3_noun van,
                      u3_noun gen,
                      u3_noun axe)                              //
{
  u3_noun cor = _al_core(van, gen);
  u3_noun gat = u3_cj_hook(u3k(cor), "whip");

  return u3_cn_slam_on(gat, u3k(axe));
}
