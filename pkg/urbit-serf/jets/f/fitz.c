/* j/6/fitz.c
**
*/
#include "all.h"


/* functions
*/
  static u3_noun
  _fitz_fiz(u3_noun yaz,
            u3_noun wix)
  {
    c3_w yaz_w = u3r_met(3, yaz);
    c3_w wix_w = u3r_met(3, wix);
    c3_y yaz_y, wix_y;

    yaz_y = (0 == yaz_w) ? 0 : u3r_byte((yaz_w - 1), yaz);
    if ( (yaz_y < 'A') || (yaz_y > 'Z') ) yaz_y = 0;

    wix_y = (0 == wix_w) ? 0 : u3r_byte((wix_w - 1), wix);
    if ( (wix_y < 'A') || (wix_y > 'Z') ) wix_y = 0;

    if ( yaz_y && wix_y ) {
      if ( !wix_y || (wix_y > yaz_y) ) {
        return c3n;
      }
    }
    return c3y;
  }

  u3_noun
  u3qf_fitz(u3_noun yaz,
            u3_noun wix)
  {
    c3_w i_w, met_w = c3_min(u3r_met(3, yaz), u3r_met(3, wix));

    if ( c3n == _fitz_fiz(yaz, wix) ) {
      return c3n;
    }
    for ( i_w = 0; i_w < met_w; i_w++ ) {
      c3_y yaz_y = u3r_byte(i_w, yaz);
      c3_y wix_y = u3r_byte(i_w, wix);

      if ( (yaz_y >= 'A') && (yaz_y <= 'Z') ) yaz_y = 0;
      if ( (wix_y >= 'A') && (wix_y <= 'Z') ) wix_y = 0;

      if ( yaz_y && wix_y && (yaz_y != wix_y) ) {
        return c3n;
      }
    }
    return c3y;
  }

  u3_noun
  u3wf_fitz(u3_noun cor)
  {
    u3_noun yaz, wix;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &yaz, u3x_sam_3, &wix, 0)) ||
         (c3n == u3ud(yaz)) ||
         (c3n == u3ud(wix)) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_fitz(yaz, wix);
    }
  }
