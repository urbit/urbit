/* gen164/5/ed_sign.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

#include <ed25519.h>

/* functions
*/
  u2_weak
  j2_mdc(Pt5, coed, ed, veri)(u2_wire wir_r,
                              u2_noun s,
                              u2_noun m,
                              u2_noun pk)
  {
    c3_y  sig_y[64];
    c3_y  pub_y[32];
    c3_w  ret;
    c3_y* mes_y;

    c3_w mesm_w = u2_met(3, m);

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
  u2_weak
  j2_md(Pt5, coed, ed, veri)(u2_wire wir_r,
                             u2_noun cor)
  {
    u2_noun a, b, c;
    if ( u2_no == u2_mean(cor,
                          u2_cv_sam_2, &a, u2_cv_sam_6, &b,
                          u2_cv_sam_7, &c, 0) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mdc(Pt5, coed, ed, veri)(wir_r, a, b, c);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mdj(Pt5, coed, ed, veri)[] = {
    { ".2", c3__lite, j2_md(Pt5, coed, ed, veri), Tier5, u2_none, u2_none },
    { }
  };
