/* j/3/mug.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3wc_mug(u3_noun cor)
  {
    u3_noun sam;

    if ( u3_none == (sam = u3r_at(u3v_sam, cor)) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3r_mug(sam);
    }
  }
