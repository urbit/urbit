/* gen164/5/co_oxco.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"


/* functions
*/
  u2_weak
  j2_md(Pt5, coco, co, oxco)(u2_wire wir_r,
                             u2_noun cor)
  {
    u2_atom bas, gop;
    u2_noun dug;
    if ( u2_no == u2_mean(cor, u2_cv_sam_4, &bas,
                               u2_cv_sam_5, &gop,
                               u2_cv_sam_3, &dug,
                               0) )
      {
	return u2_bl_bail(wir_r, c3__exit);
      }

    return 0;
  }
/* structures
*/
  u2_ho_jet
  j2_mdj(Pt5, coco, co, oxco)[] = {
    { ".2", c3__lite, j2_md(Pt5, coco, co, oxco), u2_jet_dead, u2_none, u2_none },
    { }
  };
