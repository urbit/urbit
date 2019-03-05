/* j/6/ut_peel.c
**
*/
#include "all.h"


/* logic
*/
  u3_noun
  _cqfu_peel(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun met)
  {
    if ( c3__gold == met ) {
      return u3nc(c3y, c3y);
    }
    else switch ( way ) {
      default: return u3m_bail(c3__fail);

      case c3__both: return u3nc(c3n, c3n);
      case c3__free: return u3nc(c3y, c3y);
      case c3__read: return u3nc(((met == c3__zinc) ? c3y : c3n), c3n);
      case c3__rite: return u3nc(((met == c3__iron) ? c3y : c3n), c3n);
    }
  }

/* boilerplate
*/
  u3_noun
  u3wfu_peel(u3_noun cor)
  {
    u3_noun sut, way, met, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &way,
                               u3x_sam_3, &met,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_peel(van, sut, way, met);
    }
  }

  u3_noun
  u3qfu_peel(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun met)
  {
    return _cqfu_peel(van, sut, way, met);
  }

