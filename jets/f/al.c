/* j/6/al.c
**
*/
#include "all.h"


/* ~(. al gen)
*/
static u3_noun
_al_core(u3_noun van,
         u3_noun gen)
{
  u3_noun ter = u3r_at(u3x_con_3, van);
  u3_noun gat = u3j_hook(u3k(ter), "al");

  return u3i_molt(gat, u3x_sam, u3k(gen), 0);
}

/* ~(bunt al gen)
*/
u3_noun
u3qfl_bunt(u3_noun van,
           u3_noun gen)
{
  u3_noun cor = _al_core(van, gen);

  return u3j_hook(cor, "bunt");
}

/* ~(whip al gen)
*/
u3_noun
u3qfl_whip(u3_noun van,
           u3_noun gen,
           u3_noun axe)                              //
{
  u3_noun cor = _al_core(van, gen);
  u3_noun gat = u3j_hook(u3k(cor), "whip");

  return u3n_slam_on(gat, u3k(axe));
}
