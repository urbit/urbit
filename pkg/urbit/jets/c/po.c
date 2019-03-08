#include "all.h"

// _po_find: good old linear search
static u3_noun _po_find(u3_noun buf, u3_noun a) {
  if ( !_(u3a_is_cat(a)) ) {
    return u3_nul;
  }

  c3_w i_w;
  c3_w a_w = u3a_get_cat31(a);

  for ( i_w = 0; i_w < 256; i_w++ ) {
    c3_y byt_y[3];
    c3_w but_w;

    u3r_bytes((i_w * 3), 3, byt_y, buf);
    but_w = (byt_y[0] | (byt_y[1] << 8) | (byt_y[2] << 16));

    if ( but_w == a_w ) {
      return u3nc(u3_nul, UNSAFECAT(i_w));
    }
  }

  return u3_nul;
}

u3_noun u3wcp_ins(u3_noun cor) {
  u3_noun x, a, buf;

  if ( !_(u3r_mean(cor, u3x_sam, &a, u3x_con_sam, &x, 0)) ||
       !_(u3du(x)) ||
       !_(u3ud(buf = u3h(x))) ||
       !_(u3ud(a)) )
  {
    return u3m_bail(c3__exit);
  }

  return _po_find(buf, a);
}

u3_noun u3wcp_ind(u3_noun cor) {
  u3_noun x, a, buf;

  if ( !_(u3r_mean(cor, u3x_sam, &a, u3x_con_sam, &x, 0)) ||
       !_(u3du(x)) ||
       !_(u3ud(buf = u3t(x))) ||
       !_(u3ud(a)) )
  {
    return u3m_bail(c3__exit);
  }

  return _po_find(buf, a);
}

u3_noun u3wcp_tos(u3_noun cor) {
  u3_noun x, a, buf;

  c3_w a_w = u3a_get_cat31(a);

  if ( !_(u3r_mean(cor, u3x_sam, &a, u3x_con_sam, &x, 0)) ||
       !_(u3du(x)) ||
       !_(u3ud(buf = u3h(x))) ||
       !_(u3ud(a)) ||
       (a_w >= 256) )
  {
    return u3m_bail(c3__exit);
  }

  c3_y byt_y[3];

  u3r_bytes((a_w * 3), 3, byt_y, buf);
  return UNSAFECAT(byt_y[0] | (byt_y[1] << 8) | (byt_y[2] << 16));
}

u3_noun u3wcp_tod(u3_noun cor) {
  u3_noun x, a, buf;

  c3_w a_w = u3a_get_cat31(a);

  if ( !_(u3r_mean(cor, u3x_sam, &a, u3x_con_sam, &x, 0)) ||
       !_(u3du(x)) ||
       !_(u3ud(buf = u3t(x))) ||
       !_(u3ud(a)) ||
       (a_w >= 256) )
  {
    return u3m_bail(c3__exit);
  }

  c3_y byt_y[3];

  u3r_bytes((a_w * 3), 3, byt_y, buf);
  return UNSAFECAT(byt_y[0] | (byt_y[1] << 8) | (byt_y[2] << 16));
}
