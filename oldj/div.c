/* j/div.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u3_zx_div_c():
*/
u3_fox
u3_zx_div_c(u3_z   z,
            u3_fox a,
            u3_fox b)
{
  if ( 0 == b ) {
    return u3_zc_tank(z, c3__exit);
  }
  else {
    mpz_t mp_a, mp_b;

    u3_lr_mp(z, mp_a, a);
    u3_lr_mp(z, mp_b, b);

    mpz_tdiv_q(mp_a, mp_a, mp_b);
    mpz_clear(mp_b);

    return u3_zc_mp(z, mp_a);
  }
}

/* u3_zx_div():
*/
u3_fox
u3_zx_div(u3_z   z,
          u3_fox cor)
{
  u3_fox sam = u3_zh(z, u3_zh(z, cor));
  u3_fox a, b;

  if ( (u3_no == u3_lr_cell(z, sam, &a, &b)) ||
       (u3_no == u3_lr_stud(z, a)) ||
       (u3_no == u3_lr_stud(z, b)) )
  {
    return u3_zc_tank(z, c3__punt);
  }
  else {
    return u3_zx_div_c(z, a, b);
  }
}
