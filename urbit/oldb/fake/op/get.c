/* fake/op/get.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_op_get():
**
**   Produce the twig of (noun) at (twig), or bull.
**
**   Caller retains ownership of (noun).
**   Callee retains ownership of product.
*/
u4_nopt
u4_op_get(u4_twig twig,
          u4_noun noun)
{
  u4_st st = (u4_a_bin(twig, 0) - 1);
  u4_pt i;
  mpz_t mp_twig;

  u4_a_gmp(twig, mp_twig);

  for ( i=0; i < st; i++ ) {
    u4_st st_at = (st - (i + 1));

    if ( u4_n_atom(noun) ) {
      mpz_clear(mp_twig);

      return u4_bull;
    }
    else {
      if ( (mpz_tstbit(mp_twig, st_at) == 0) ) {
        noun = u4_ch(noun);
      } else {
        noun = u4_ct(noun);
      }
    }
  }
  mpz_clear(mp_twig);
  return noun;
}
