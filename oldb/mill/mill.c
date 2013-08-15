/* mill/mill.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_mill(): 
**
**   Convert [gene mold] to [mold nock].
*/
u4_loaf
u4_mill(u4_lane lane,
        u4_lump nes,
        u4_lump zyl)
{
  struct _u4_mill milr;

  u4_mill_init(&milr, lane);

  printf(":");
  fflush(stdout);
  {
    u4_milr m   = &milr;
    u4_loaf lof = _mill_make(m, nes, zyl);

    printf(":\n");
    printf("prf: %d\n", m->prf);
    return lof;
  }
}
