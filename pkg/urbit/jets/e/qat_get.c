#include "all.h"

u3_noun
u3qe_qat_get(u3_noun a, u3_noun k)
{
  if (u3_nul == a) {
    return u3_nul;
  }

  u3_noun hoc = u3h(a);

  if (c3n == u3ud(hoc)) {
    return u3m_bail(c3__exit);
  }
  else switch ( hoc ) {
    default:
      return u3m_bail(c3__exit);

    case c3__tip: {
      u3_noun ka, pa, va;
      u3x_trel(u3t(a), &ka, &pa, &va);

      if (c3n == u3r_sing(k, ka)) {
        return u3_nul;
      }
      else {
        return u3nt(u3_nul, u3k(pa), u3k(va));
      }
    }

    case c3__bin: {
      u3_noun ka, pa, va;
      u3_noun ta, ma, la, ra;
      u3x_qual(u3t(a), &ka, &pa, &va, &ta);
      u3x_trel(ta, &ma, &la, &ra);

      if (c3y == u3qe_qat_feud(ma, k, ka)) {
        return u3_nul;
      }
      else if (c3y == u3r_sing(k, ka)) {
        return u3nt(u3_nul, u3k(pa), u3k(va));
      }
      else if (c3y == u3qe_qat_zero(ma, k)) {
        return u3qe_qat_get(la, k);
      }
      else {
        return u3qe_qat_get(ra, k);
      }
    }
  }
}

u3_noun
u3we_qat_get(u3_noun cor)
{
  u3_noun a, k;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &k, 0)) ||
       (c3n == u3ud(k)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qe_qat_get(a, k);
  }
}


