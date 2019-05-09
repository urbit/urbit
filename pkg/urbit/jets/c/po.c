/* j/3/po.c
**
*/
#include "all.h"


  //  good old linear search
  //
  static u3_noun
  _po_find(u3_noun buf,
           u3_noun a)
  {
    if ( !_(u3a_is_cat(a)) ) {
      return u3_nul;
    }
    else {
      c3_w i_w;
      c3_w a_w = a;

      for ( i_w = 0; i_w < 256; i_w++ ) {
        c3_y byt_y[3];
        c3_w but_w;

        u3r_bytes((i_w * 3), 3, byt_y, buf);
        but_w = (byt_y[0] | (byt_y[1] << 8) | (byt_y[2] << 16));

        if ( but_w == a_w ) {
          return u3nc(u3_nul, i_w);
        }
      }
      return u3_nul;
    }
  }

  u3_noun
  u3wcp_ins(u3_noun cor)
  {
    u3_noun x, a, buf;

    if ( (c3n == u3r_mean(cor, u3x_sam, &a, u3x_con_sam, &x, 0)) ||
         (c3n == u3du(x)) ||
         (c3n == u3ud(buf = u3h(x))) ||
         (c3n == u3ud(a)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return _po_find(buf, a);
    }
  }
  u3_noun
  u3wcp_ind(u3_noun cor)
  {
    u3_noun x, a, buf;

    if ( (c3n == u3r_mean(cor, u3x_sam, &a, u3x_con_sam, &x, 0)) ||
         (c3n == u3du(x)) ||
         (c3n == u3ud(buf = u3t(x))) ||
         (c3n == u3ud(a)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return _po_find(buf, a);
    }
  }

  u3_noun
  u3wcp_tos(u3_noun cor)
  {
    u3_noun x, a, buf;

    if ( (c3n == u3r_mean(cor, u3x_sam, &a, u3x_con_sam, &x, 0)) ||
         (c3n == u3du(x)) ||
         (c3n == u3ud(buf = u3h(x))) ||
         (c3n == u3ud(a)) ||
         (a >= 256) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      c3_y byt_y[3];

      u3r_bytes((a * 3), 3, byt_y, buf);
      return (byt_y[0] | (byt_y[1] << 8) | (byt_y[2] << 16));
    }
  }
  u3_noun
  u3wcp_tod(u3_noun cor)
  {
    u3_noun x, a, buf;

    if ( (c3n == u3r_mean(cor, u3x_sam, &a, u3x_con_sam, &x, 0)) ||
         (c3n == u3du(x)) ||
         (c3n == u3ud(buf = u3t(x))) ||
         (c3n == u3ud(a)) ||
         (a >= 256) )
    {
      return u3m_bail(c3__exit);
    } else {
      c3_y byt_y[3];

      u3r_bytes((a * 3), 3, byt_y, buf);
      return (byt_y[0] | (byt_y[1] << 8) | (byt_y[2] << 16));
    }
  }
