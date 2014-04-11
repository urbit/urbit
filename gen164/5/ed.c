/* gen164/5/ed.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* declarations
*/
  extern u2_ho_jet j2_mcj(Pt5, ed, sign)[];

/* structures
*/
  u2_ho_driver
  j2_mbd(Pt5, ed)[] = {
    { j2_sc(Pt5, ed, sign), j2_mcj(Pt5, ed, sign), 0, 0, u2_none },
    {}
  };

  u2_ho_driver
  j2_db(Pt5, ed) =
    { j2_sb(Pt5, ed), 0, j2_mbd(Pt5, ed), 0, u2_none };
