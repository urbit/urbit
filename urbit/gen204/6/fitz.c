/* j/6/fitz.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  static u2_flag 
  _fitz_fiz(u2_wire wir_r, 
            u2_noun yaz, 
            u2_noun wix)
  {
    c3_w yaz_w = u2_met(3, yaz);
    c3_w wix_w = u2_met(3, wix);
    c3_y yaz_y, wix_y;
    
    yaz_y = (0 == yaz_w) ? 0 : u2_byte((yaz_w - 1), yaz);
    if ( (yaz_y < 'A') || (yaz_y > 'Z') ) yaz_y = 0;

    wix_y = (0 == wix_w) ? 0 : u2_byte((wix_w - 1), wix);
    if ( (wix_y < 'A') || (wix_y > 'Z') ) wix_y = 0;

    if ( yaz_y && wix_y ) {
      if ( !wix_y || (wix_y > yaz_y) ) {
        return u2_no;
      }
    }
    return u2_yes;
  }

  u2_noun                                                         //  transfer
  j2_mby(Pt6, fitz)(u2_wire wir_r,
                    u2_noun yaz,                                  //  retain
                    u2_noun wix)                                  //  retain
  {
    c3_w i_w, met_w = c3_min(u2_met(3, yaz), u2_met(3, wix));

    if ( u2_no == _fitz_fiz(wir_r, yaz, wix) ) {
      return u2_no;
    }
    for ( i_w = 0; i_w < met_w; i_w++ ) {
      c3_y yaz_y = u2_byte(i_w, yaz);
      c3_y wix_y = u2_byte(i_w, wix);

      if ( (yaz_y >= 'A') && (yaz_y <= 'Z') ) yaz_y = 0;
      if ( (wix_y >= 'A') && (wix_y <= 'Z') ) wix_y = 0;

      if ( yaz_y && wix_y && (yaz_y != wix_y) ) {
        return u2_no;
      }
    }
    return u2_yes;
  }

  u2_noun                                                         //  transfer
  j2_mb(Pt6, fitz)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun yaz, wix;

    if ( (u2_no == u2_mean(cor, u2_cw_sam_2, &yaz, u2_cw_sam_3, &wix, 0)) ||
         (u2_no == u2ud(yaz)) ||
         (u2_no == u2ud(wix)) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt6, fitz)(wir_r, yaz, wix);
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt6, fitz)[] = {
    { ".3", c3__hevy, j2_mb(Pt6, fitz), Tier6_a, u2_none, u2_none },
    { }
  };
