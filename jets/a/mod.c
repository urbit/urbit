/* j/1/mod.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qa_mod(u3_atom a,
           u3_atom b)
  {
#if 0
    if ( b == 3 && a == 2684227708 ) {
      printf("dword at 0x27ff84ff8 is %" PRIu64 "\r\n", *(c3_d *)0x27ff84ff8);
      *(c3_d *)0x27ff84ff8 = 25;
      printf("see, we modified it\r\n");
    }
#endif
    if ( 0 == b ) {
      return u3m_bail(c3__exit);
    } else {
      mpz_t a_mp, b_mp;

      u3r_mp(a_mp, a);
      u3r_mp(b_mp, b);

      mpz_tdiv_r(a_mp, a_mp, b_mp);
      mpz_clear(b_mp);

      return u3i_mp(a_mp);
    }
  }

  u3_noun
  u3wa_mod(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qa_mod(a, b);
    }
  }
