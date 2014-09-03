/* gen164/5/ed_veri.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

#include <ed25519.h>

/* functions
*/
  static u2_noun
  _cqee_veri(u2_noun s, u2_noun m, u2_noun pk)
  {
    c3_y  sig_y[64];
    c3_y  pub_y[32];
    c3_w  ret;
    c3_y* mes_y;

    c3_w mesm_w = u2_cr_met(3, m);

    memset(sig_y, 0, 64);
    memset(pub_y, 0, 32);

    mes_y = c3_malloc(mesm_w);

    u2_cr_bytes(0, 64, sig_y, s);
    u2_cr_bytes(0, 32, pub_y, pk);
    u2_cr_bytes(0, mesm_w, mes_y, m);

    ret = ed25519_verify(sig_y, mes_y, mesm_w, pub_y) == 1 ? u2_yes : u2_no;
    free(mes_y);
    return ret;
  }

  u2_noun
  cwee_veri(u2_noun cor)
  {
    u2_noun a, b, c;
    if ( u2_no == u2_cr_mean(cor,
                          u2_cv_sam_2, &a, u2_cv_sam_6, &b,
                          u2_cv_sam_7, &c, 0) ) {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqee_veri(a, b, c);
    }
  }
