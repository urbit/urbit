#include "all.h"

u3_noun
u3qe_qat_bot(u3_noun a)
{
  if (u3_nul == a) {
    return u3_nul;
  }

  u3_noun hoc = u3h(a);

  if ( c3n == u3ud(hoc) )  {
    return u3m_bail(c3__exit);
  }
  else switch ( hoc ) {
    default:
      return u3m_bail(c3__exit);

    case c3__tip: {
      u3_noun ka, pa, va;
      u3x_trel(u3t(a), &ka, &pa, &va);

      return u3nc(u3_nul, u3nq(u3k(ka), u3k(pa), u3k(va), u3_nul));
    }

    case c3__bin: {
      u3_noun ka, pa, va, ta;
      u3_noun ma, la, ra;

      u3x_qual(u3t(a), &ka, &pa, &va, &ta);
      u3x_trel(ta, &ma, &la, &ra);

      u3_noun pro = u3nq(u3k(ka), u3k(pa), u3k(va),
                      u3qe_qat_fuse(ma, la, ra));

      return pro;
    }
  }
}

u3_noun
u3we_qat_bot(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qe_qat_bot(a);
  }
}


