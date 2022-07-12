#include "all.h"

u3_noun
u3qdu_peak(u3_noun k, u3_noun l)
{
  u3_atom h = u3r_mug(k);
  u3_atom i = u3r_mug(l);

  c3_w a = h ^ i;

  // see
  //
  // http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2

  a |= a >> 1;
  a |= a >> 2;
  a |= a >> 4;
  a |= a >> 8;
  a |= a >> 16;
  a ^= a >> 1;

  return a;
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

