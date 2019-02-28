/* j/1/add.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qa_add(u3_atom a,
           u3_atom b)
  {
    if ( _(u3a_is_cat(a)) && _(u3a_is_cat(b)) ) {
      c3_w c = a + b;

      return u3i_words(1, &c);
    }
    else if ( 0 == a ) {
      return u3k(b);
    }
    else {
      mpz_t a_mp, b_mp;

      u3r_mp(a_mp, a);
      u3r_mp(b_mp, b);

      mpz_add(a_mp, a_mp, b_mp);
      mpz_clear(b_mp);

      return u3i_mp(a_mp);
    }
  }
  u3_noun
  u3wa_add(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b) && a != 0) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qa_add(a, b);
    }
  }

  u3_noun
  u3ka_add(u3_noun a,
           u3_noun b)
  {
    u3_noun c = u3qa_add(a, b);

    u3z(a); u3z(b);
    return c;
  }
