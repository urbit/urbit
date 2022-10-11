#include "all.h"

u3_noun
u3qdu_bot(u3_noun a)
{
  if (u3_nul == a) {
    return u3_nul;
  }

  u3_atom hoc = u3h(a);

  if (c3n == u3ud(hoc)) {
    return u3m_bail(c3__exit);
  }
  else switch ( hoc ) {
    default:
      return u3m_bail(c3__exit);

    case c3__tip: {
      u3_noun ka, pa, va;
      u3x_trel(u3t(a), &ka, &pa, &va);

      return u3nc(u3_nul,
              u3nq(u3k(ka), u3k(pa), u3k(u3h(va)), u3qdu_cut(a)));
    }

    case c3__bin: {
      u3_noun ka, pa, ta;
      u3x_trel(u3t(a), &ka, &pa, &ta);

      return u3nc(u3_nul,
              u3nq(u3k(ka), u3k(pa), u3k(u3h(u3h(ta))), u3qdu_cut(a)));
    }
  }
}

u3_noun
u3wdu_bot(u3_noun cor)
{
  u3_noun a;

  if (c3n == u3r_mean(cor, u3x_sam, &a, 0)) {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_bot(a);
  }
}


