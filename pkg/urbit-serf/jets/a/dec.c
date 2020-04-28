/* j/1/dec.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qa_inc(u3_atom a)
  {
    return u3i_vint(u3k(a));
  }

  u3_noun
  u3qa_dec(u3_atom a)
  {
    if ( 0 == a ) {
      return u3m_error("decrement-underflow");
    }
    else {
      if ( _(u3a_is_cat(a)) ) {
        return a - 1;
      }
      else {
        mpz_t a_mp;

        u3r_mp(a_mp, a);
        mpz_sub_ui(a_mp, a_mp, 1);

        return u3i_mp(a_mp);
      }
    }
  }
  u3_noun
  u3wa_dec(u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3r_at(u3x_sam, cor))) ||
         (c3n == u3ud(a)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qa_dec(a);
    }
  }
