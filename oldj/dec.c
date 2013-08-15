/* j/dec.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u3_zx_dec_c():
*/
u3_fox
u3_zx_dec_c(u3_z   z,
            u3_fox a)
{
  mpz_t mp_a;

  if ( 0 == a ) {
    return u3_zc_tank(z, c3__exit);
  }
  else {
    u3_lr_mp(z, mp_a, a);
    mpz_sub_ui(mp_a, mp_a, 1);

    return u3_zc_mp(z, mp_a);
  }
}

/* u3_zx_dec():
*/
u3_fox
u3_zx_dec(u3_z   z,
          u3_fox cor)
{
  u3_fox sam = u3_zh(z, u3_zh(z, cor));
  u3_fox a   = sam;

  if ( u3_no == u3_lr_stud(z, a) ) {
    return u3_zc_tank(z, c3__punt);
  }
  else return u3_zx_dec_c(z, a);
}

