/* j/play.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u3_zx_play_c():
*/
u3_fox
u3_zx_play_c(u3_z   z,
             u3_fox sut,
             u3_fox gen)
{
  u3_mote how;
  u3_rat  rat = u3_b_play(&z->l, sut, gen, &how); 

  if ( u3_none == rat ) {
    return u3_zc_tank(z, how);
  }
  else return rat;
}

/* u3_zx_play():
*/
u3_fox
u3_zx_play(u3_z   z,
           u3_fox cor)
{
  u3_fox ham = u3_zh(z, cor);
  u3_fox sam = u3_zh(z, ham);
  u3_fox con = u3_zt(z, ham);
  u3_fox sut = u3_zh(z, u3_zh(z, con));
  u3_fox gen = sam;

  return u3_zx_play_c(z, sut, gen);
}
