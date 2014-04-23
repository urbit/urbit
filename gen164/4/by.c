/* j/4/by.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* declarations
*/
  extern u2_ho_jet j2_mcj(Pt4, by, gas)[];
  extern u2_ho_jet j2_mcj(Pt4, by, get)[];
  extern u2_ho_jet j2_mcj(Pt4, by, has)[];
  extern u2_ho_jet j2_mcj(Pt4, by, put)[];
  extern u2_ho_jet j2_mcj(Pt4, by, uni)[];

/* structures
*/
  u2_ho_driver
  j2_mbd(Pt4, by)[] = {
    { j2_sc(Pt4, by, gas), j2_mcj(Pt4, by, gas), 0, 0, u2_none },
    { j2_sc(Pt4, by, get), j2_mcj(Pt4, by, get), 0, 0, u2_none },
    { j2_sc(Pt4, by, has), j2_mcj(Pt4, by, has), 0, 0, u2_none },
    { j2_sc(Pt4, by, put), j2_mcj(Pt4, by, put), 0, 0, u2_none },
    { j2_sc(Pt4, by, uni), j2_mcj(Pt4, by, uni), 0, 0, u2_none },
    {}
  };

  u2_ho_driver
  j2_db(Pt4, by) =
    { j2_sb(Pt4, by), 0, j2_mbd(Pt4, by), 0, u2_none };
