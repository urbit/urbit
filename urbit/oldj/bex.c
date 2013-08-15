/* j/bex.c
**
** This file is in the public domain.
*/
#include "all.h"

/* u3_zx_bex_c():
*/
u3_fox
u3_zx_bex_c(u3_z   z,
            u3_fox a)
{
  mpz_t mp_a;

  if ( !u3_rat_is_cat(a) ) {
    return u3_zc_tank(z, c3__punt);
  }
  else {
    mpz_init_set_ui(mp_a, 1);
    mpz_mul_2exp(mp_a, mp_a, a);

    return u3_zc_mp(z, mp_a);
  }
}

/* u3_zx_bex():
*/
u3_fox
u3_zx_bex(u3_z   z,
          u3_fox cor)
{
  u3_fox sam = u3_zh(z, u3_zh(z, cor));
  u3_fox a   = sam;

  if ( u3_no == u3_lr_stud(z, a) ) {
    return u3_zc_tank(z, c3__punt);
  }
  else return u3_zx_bex_c(z, a);
}

