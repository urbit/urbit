/* j/3/mug.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3wc_mug(u3_noun cor)
  {
    u3_noun sam;

    if ( u3_none == (sam = u3r_at(u3x_sam, cor)) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3r_mug(sam);
    }
  }
