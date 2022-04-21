#include "all.h"

u3_noun
u3qdu_cut(u3_noun a)
{
  if (u3_nul == a) {
    return u3_nul;
  }

  u3_noun lal = u3h(a);

  if (c3n == u3ud(lal)) {
    return u3m_bail(c3__exit);
  }
  else switch ( lal ) {
    default:
      return u3m_bail(c3__exit);

    case c3__tip: {
      u3_noun tva = u3t(u3t(u3t(u3t(a))));
      u3_noun hol = u3qdu_qor_bot(tva);

      u3_noun pro = (u3_nul == hol) ? u3_nul : u3nc(c3__tip, u3k(u3t(hol)));

      u3z(hol);
      return pro;
    }

    case c3__bin: {
      u3_noun ka, pa, va, ta;
      u3_noun ma, la, ra;

      u3x_qual(a, &ka, &pa, &va, &ta);
      u3x_trel(ta, &ma, &la, &ra);

      u3_noun hol = u3qdu_qor_bot(u3t(va));

      if (u3_nul == hol) {
        return u3qdu_fuse(ma, la, ra);
      }
      else {
        u3_noun kh, ph, vh;
        u3x_trel(u3t(hol), &kh, &ph, &vh);

        u3_noun pre = u3qdu_fuse(ma, la, ra);
        u3_noun pro = u3qdu_qat_raw(pre, kh, ph, vh);

        u3z(hol);
        u3z(pre);

        return pro;
      }
    }
  }
}

u3_noun
u3wdu_cut(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_cut(a);
  }
}


