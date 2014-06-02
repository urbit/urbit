/* gen164/5/co.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* declarations
*/
  extern u2_ho_jet j2_mdj(Pt5, coco, co, emco)[];
  extern u2_ho_jet j2_mdj(Pt5, coco, co, oxco)[];
  extern u2_ho_jet j2_mdj(Pt5, coco, co, roco)[];

  u2_ho_driver
  j2_mcd(Pt5, coco, co)[] = {
    { j2_sd(Pt5, coco, co, emco), j2_mdj(Pt5, coco, co, emco), 0, 0, u2_none },
    { j2_sd(Pt5, coco, co, oxco), j2_mdj(Pt5, coco, co, oxco), 0, 0, u2_none },
    { j2_sd(Pt5, coco, co, roco), j2_mdj(Pt5, coco, co, roco), 0, 0, u2_none },
    {}
  };

/* structures
*/
  u2_ho_driver
  j2_mbd(Pt5, coco)[] = {
    { j2_sc(Pt5, coco, co), 0, j2_mcd(Pt5, coco, co), 0, u2_none },
    {}
  };
