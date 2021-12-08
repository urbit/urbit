#include "all.h"

u3_noun
u3qdu_qat_del(u3_noun a, u3_noun k)
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
      return (c3y == u3r_sing(k, u3h(u3t(a)))) ? u3_nul : u3k(a);
    }

    case c3__bin: {
      u3_noun ka, pa, va, ma;
      u3_noun ta, la, ra;
      u3x_qual(u3t(a), &ka, &pa, &va, &ta);
      u3x_trel(ta, &ma, &la, &ra);

      if (c3y == u3qdu_gone(k, ka, ma)) {
        return u3k(a);
      }
      else if (c3y == u3r_sing(k, ka)) {
        return u3qdu_fuse(ma, la, ra);
      }
      else if (c3y == u3qdu_zero(k, ma)) {
        u3_noun ul = u3qdu_qat_del(la, k);
        u3_noun pro = u3qdu_funk(ka, pa, va, ma, ul, ra);

        u3z(ul);

        return pro;
      }
      else {
        u3_noun ur = u3qdu_qat_del(ra, k);
        u3_noun pro = u3qdu_wane(ka, pa, va, ma, la, ur);

        u3z(ur);

        return pro;
      }
    }
  }
}

u3_noun
u3wdu_qat_del(u3_noun cor)
{
  u3_noun a, k;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &k, 0)) ||
       (c3n == u3ud(k)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qat_del(a, k);
  }
}


