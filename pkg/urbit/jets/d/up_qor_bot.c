#include "all.h"

u3_noun
u3qdu_qor_bot(u3_noun a)
{
  if (u3_nul == a) {
    return u3_nul;
  }
  else
  {
    u3_noun na, ta, ma;
    u3x_trel(a, &na, &ta, &ma);

    u3_noun kna, pna, vna;
    u3x_trel(na, &kna, &pna, &vna);

    return u3nc(u3_nul,
             u3nq(u3k(kna), u3k(pna), u3k(vna), u3qdu_qor_sec(ta, ma)));
  }
}

u3_noun
u3wdu_qor_bot(u3_noun cor)
{
  u3_noun a;

  if (c3n == u3r_mean(cor, u3x_sam, &a, 0))
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qor_bot(a);
  }
}

