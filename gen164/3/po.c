/* j/3/po.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

  //  good old linear search
  //
  static u2_noun
  _po_find(u2_noun buf, u2_noun a)
  {
    if ( u2_ne(u2_co_is_cat(a)) ) {
      return u2_nul;
    }
    else {
      c3_w i_w;
      c3_w a_w = a;

      for ( i_w = 0; i_w < 256; i_w++ ) {
        c3_y byt_y[3];
        c3_w but_w;

        u2_cr_bytes((i_w * 3), 3, byt_y, buf);
        but_w = (byt_y[0] | (byt_y[1] << 8) | (byt_y[2] << 16));

        if ( but_w == a_w ) {
          return u2nc(u2_nul, i_w);
        }
      }
      return u2_nul;
    }
  }

  u2_weak                                                         //  transfer
  j2_mc(Pt3, po, ins)(
                      u2_noun cor)                                //  retain
  {
    u2_noun x, a, buf;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &a, u2_cv_con_sam, &x, 0)) ||
         (u2_no == u2du(x)) ||
         (u2_no == u2ud(buf = u2h(x))) ||
         (u2_no == u2ud(a)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return _po_find(buf, a);
    }
  }
  u2_weak                                                         //  transfer
  j2_mc(Pt3, po, ind)(
                      u2_noun cor)                                //  retain
  {
    u2_noun x, a, buf;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &a, u2_cv_con_sam, &x, 0)) ||
         (u2_no == u2du(x)) ||
         (u2_no == u2ud(buf = u2t(x))) ||
         (u2_no == u2ud(a)) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      return _po_find(buf, a);
    }
  }

  u2_weak                                                         //  transfer
  j2_mc(Pt3, po, tos)(
                      u2_noun cor)                                //  retain
  {
    u2_noun x, a, buf;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &a, u2_cv_con_sam, &x, 0)) ||
         (u2_no == u2du(x)) ||
         (u2_no == u2ud(buf = u2h(x))) ||
         (u2_no == u2ud(a)) ||
         (a >= 256) )
    {
      return u2_cm_bail(c3__exit);
    }
    else {
      c3_y byt_y[3];

      u2_cr_bytes((a * 3), 3, byt_y, buf);
      return (byt_y[0] | (byt_y[1] << 8) | (byt_y[2] << 16));
    }
  }
  u2_weak                                                         //  transfer
  j2_mc(Pt3, po, tod)(
                      u2_noun cor)                                //  retain
  {
    u2_noun x, a, buf;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &a, u2_cv_con_sam, &x, 0)) ||
         (u2_no == u2du(x)) ||
         (u2_no == u2ud(buf = u2t(x))) ||
         (u2_no == u2ud(a)) ||
         (a >= 256) )
    {
      return u2_cm_bail(c3__exit);
    } else {
      c3_y byt_y[3];

      u2_cr_bytes((a * 3), 3, byt_y, buf);
      return (byt_y[0] | (byt_y[1] << 8) | (byt_y[2] << 16));
    }
  }
