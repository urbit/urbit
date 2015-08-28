/* j/6/slot.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qf_slot(u3_atom axe,
            u3_noun vax)
  {
    u3_noun fag = u3r_at(axe, u3t(vax));

    fprintf(stderr, "slot axe %d\r\n", axe);

    if ( u3_none == fag ) {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun typ = u3qfu_peek(
    }
    c3_w i_w, met_w = c3_min(u3r_met(3, axe), u3r_met(3, vax));

    if ( c3n == _slot_fiz(axe, vax) ) {
      return c3n;
    }
    for ( i_w = 0; i_w < met_w; i_w++ ) {
      c3_y axe_y = u3r_byte(i_w, axe);
      c3_y vax_y = u3r_byte(i_w, vax);

      if ( (axe_y >= 'A') && (axe_y <= 'Z') ) axe_y = 0;
      if ( (vax_y >= 'A') && (vax_y <= 'Z') ) vax_y = 0;

      if ( axe_y && vax_y && (axe_y != vax_y) ) {
        return c3n;
      }
    }
    return c3y;
  }

  u3_noun
  u3we_slot(u3_noun cor)
  {
    u3_noun axe, vax;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &axe, u3x_sam_3, &vax, 0)) ||
         (c3n == u3ud(axe)) ||
         (c3n == u3du(vax)) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_slot(axe, vax);
    }
  }
