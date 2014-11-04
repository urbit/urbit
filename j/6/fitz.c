/* j/6/fitz.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  static u3_bean
  _fitz_fiz(
            u3_noun yaz,
            u3_noun wix)
  {
    c3_w yaz_w = u3_cr_met(3, yaz);
    c3_w wix_w = u3_cr_met(3, wix);
    c3_y yaz_y, wix_y;

    yaz_y = (0 == yaz_w) ? 0 : u3_cr_byte((yaz_w - 1), yaz);
    if ( (yaz_y < 'A') || (yaz_y > 'Z') ) yaz_y = 0;

    wix_y = (0 == wix_w) ? 0 : u3_cr_byte((wix_w - 1), wix);
    if ( (wix_y < 'A') || (wix_y > 'Z') ) wix_y = 0;

    if ( yaz_y && wix_y ) {
      if ( !wix_y || (wix_y > yaz_y) ) {
        return u3_no;
      }
    }
    return u3_yes;
  }

  u3_noun
  u3_cqf_fitz(
                    u3_noun yaz,
                    u3_noun wix)
  {
    c3_w i_w, met_w = c3_min(u3_cr_met(3, yaz), u3_cr_met(3, wix));

    if ( u3_no == _fitz_fiz(yaz, wix) ) {
      return u3_no;
    }
    for ( i_w = 0; i_w < met_w; i_w++ ) {
      c3_y yaz_y = u3_cr_byte(i_w, yaz);
      c3_y wix_y = u3_cr_byte(i_w, wix);

      if ( (yaz_y >= 'A') && (yaz_y <= 'Z') ) yaz_y = 0;
      if ( (wix_y >= 'A') && (wix_y <= 'Z') ) wix_y = 0;

      if ( yaz_y && wix_y && (yaz_y != wix_y) ) {
        return u3_no;
      }
    }
    return u3_yes;
  }

  u3_noun
  u3_cwf_fitz(
                   u3_noun cor)
  {
    u3_noun yaz, wix;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &yaz, u3_cv_sam_3, &wix, 0)) ||
         (u3_no == u3ud(yaz)) ||
         (u3_no == u3ud(wix)) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return u3_cqf_fitz(yaz, wix);
    }
  }
