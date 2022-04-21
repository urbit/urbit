#include "all.h"

u3_noun
u3qdu_peak(u3_noun k, u3_noun l)
{
  u3_atom h = u3r_mug(k);
  u3_atom i = u3r_mug(l);

  u3_atom a = u3qc_mix(h, i);

  u3z(h);
  u3z(i);

  // XX worth investigating performance of this at some point
  //
  // http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2

  u3_atom u = u3qc_xeb(a);
  u3_atom v = u3qc_bex(u);

  u3_atom pro = u3qc_rsh(0, 1, v);

  u3z(a);
  u3z(u);
  u3z(v);

  return pro;
}

u3_noun
u3wdu_peak(u3_noun cor)
{
  u3_noun a, b;

  if (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0))
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_peak(a, b);
  }
}

