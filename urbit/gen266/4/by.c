/* j/4/by.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* declarations
*/
  extern u2_ho_jet j2_mcj(Pit, by, gas)[];
  extern u2_ho_jet j2_mcj(Pit, by, get)[];
  extern u2_ho_jet j2_mcj(Pit, by, has)[];
  extern u2_ho_jet j2_mcj(Pit, by, put)[];

/* structures
*/
  u2_ho_driver 
  j2_mbd(Pit, by)[] = {
    { j2_sc(Pit, by, gas), j2_mcj(Pit, by, gas), 0, 0, u2_none },
    { j2_sc(Pit, by, get), j2_mcj(Pit, by, get), 0, 0, u2_none },
    { j2_sc(Pit, by, has), j2_mcj(Pit, by, has), 0, 0, u2_none },
    { j2_sc(Pit, by, put), j2_mcj(Pit, by, put), 0, 0, u2_none },
    {}
  };

  u2_ho_driver
  j2_db(Pit, by) = 
    { j2_sb(Pit, by), 0, j2_mbd(Pit, by), 0, u2_none };
