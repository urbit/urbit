/* j/4/in.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* declarations
*/
  extern u2_ho_jet j2_mcj(Pt4, in, gas)[];
  extern u2_ho_jet j2_mcj(Pt4, in, has)[];
  extern u2_ho_jet j2_mcj(Pt4, in, put)[];
  extern u2_ho_jet j2_mcj(Pt4, in, tap)[];

/* structures
*/
  u2_ho_driver
  j2_mbd(Pt4, in)[] = {
    { j2_sc(Pt4, in, gas), j2_mcj(Pt4, in, gas), 0, 0, u2_none },
    { j2_sc(Pt4, in, has), j2_mcj(Pt4, in, has), 0, 0, u2_none },
    { j2_sc(Pt4, in, put), j2_mcj(Pt4, in, put), 0, 0, u2_none },
    { j2_sc(Pt4, in, tap), j2_mcj(Pt4, in, tap), 0, 0, u2_none },
    {}
  };

  u2_ho_jet
  j2_mbj(Pt4, in)[] = {
    { }
  };

  u2_ho_driver
  j2_db(Pt4, in) =
    { j2_sb(Pt4, in), j2_mbj(Pt4, in), j2_mbd(Pt4, in), 0, u2_none };
