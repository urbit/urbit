/* f/hevn.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u2_hv_init(): initialize state.
*/
u2_ray
u2_hv_init(u2_ray wir_r)
{
  u2_ray hev_r = u2_rl_ralloc(wir_r, c3_wiseof(u2_loom_hevn));

  u2_hevx_be(hev_r, u2_pryr, god) = 0;
  u2_hevx_at(hev_r, lad) = 0;

  return hev_r;
}

/* u2_hv_mark(): mark heaven for gc.
*/
c3_w
u2_hv_mark(void)
{
  return u2_rl_gc_mark_noun(u2_Wire, u2_hevn_at(lad));
}
