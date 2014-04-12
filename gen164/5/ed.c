/* gen164/5/ed.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* declarations
*/
  extern u2_ho_jet j2_mdj(Pt5, coed, ed, sign)[];
  extern u2_ho_jet j2_mdj(Pt5, coed, ed, puck)[];

  u2_ho_driver
  j2_mcd(Pt5, coed, ed)[] = {
    { j2_sd(Pt5, coed, ed, sign), j2_mdj(Pt5, coed, ed, sign), 0, 0, u2_none },
    { j2_sd(Pt5, coed, ed, puck), j2_mdj(Pt5, coed, ed, puck), 0, 0, u2_none },
    {}
  };

/* structures
*/
  u2_ho_driver
  j2_mbd(Pt5, coed)[] = {
    { j2_sc(Pt5, coed, ed), 0, j2_mcd(Pt5, coed, ed), 0, 0, u2_none },
    {}
  };
