#include "all.h"

u3_noun u3qc_rev(u3_atom boz, u3_atom len, u3_atom dat) {
  c3_w boz_w = u3a_get_cat31(boz);
  c3_w len_w = u3a_get_cat31(len);

  if ( (boz_w >= 32) ) {
    return u3m_bail(c3__fail);
  }

  dat = u3qc_end(boz, len, dat);

  c3_w met_w = u3r_met(boz_w, dat);

  return u3kc_lsh(boz, UNSAFECAT(len_w - met_w), u3kc_swp(boz, dat));
}

u3_noun u3wc_rev(u3_noun cor) {
  u3_noun boz, len, dat;

  if ( !_(u3r_mean(cor, u3x_sam_2, &boz,
                        u3x_sam_6, &len,
                        u3x_sam_7, &dat, 0)) ||
       !_(u3ud(boz)) ||
       !_(u3ud(len)) ||
       !_(u3ud(dat)) )
  {
    return u3m_bail(c3__exit);
  }

  return u3qc_rev(boz, len, dat);
}

u3_noun u3kc_rev(u3_atom boz, u3_atom len, u3_atom dat) {
  u3_noun res = u3qc_rev(boz, len, dat);
  u3z(boz); u3z(len); u3z(dat);
  return res;
}
