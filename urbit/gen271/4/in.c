/* j/4/in.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* declarations
*/
  extern u2_ho_jet j2_mcj(Pit, in, gas)[];
  extern u2_ho_jet j2_mcj(Pit, in, has)[];
  extern u2_ho_jet j2_mcj(Pit, in, put)[];
  extern u2_ho_jet j2_mcj(Pit, in, tap)[];

/* structures
*/
  u2_ho_driver 
  j2_mbd(Pit, in)[] = {
    { j2_sc(Pit, in, gas), j2_mcj(Pit, in, gas), 0, 0, u2_none },
    { j2_sc(Pit, in, has), j2_mcj(Pit, in, has), 0, 0, u2_none },
    { j2_sc(Pit, in, put), j2_mcj(Pit, in, put), 0, 0, u2_none },
    { j2_sc(Pit, in, tap), j2_mcj(Pit, in, tap), 0, 0, u2_none },
    {}
  };

  u2_ho_jet 
  j2_mbj(Pit, in)[] = {
    { }
  };

  u2_ho_driver
  j2_db(Pit, in) = 
    { j2_sb(Pit, in), j2_mbj(Pit, in), j2_mbd(Pit, in), 0, u2_none };
