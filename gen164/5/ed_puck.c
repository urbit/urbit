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
  j2_md(Pt5, coed, ed, puck)(u2_wire wir_r,
                             u2_noun cor)
  {
    c3_y pub_y[32];
    c3_y sec_y[64];
    c3_y sed_y[32];
    c3_w met_w;
    u2_noun a = u2_frag(u2_cv_sam, cor);

    if ( (u2_none == a) || (u2_no == u2_stud(a)) ) {
      return u2_bl_bail(wir_r, c3__exit);
    }

    met_w = u2_met(3, a);
    if ( met_w > 32 ) {
      return u2_bl_bail(wir_r, c3__exit);
    }

    memset(sed_y, 0, 32);
    u2_cr_bytes(0, met_w, sed_y, a);
    ed25519_create_keypair(pub_y, sec_y, sed_y);
    return u2_ci_bytes(32, pub_y);
  }
/* structures
*/
  u2_ho_jet
  j2_mdj(Pt5, coed, ed, puck)[] = {
    { ".2", c3__lite, j2_md(Pt5, coed, ed, puck),
      u2_jet_test|u2_jet_live, u2_none, u2_none },
    { }
  };
