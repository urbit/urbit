#include "all.h"

u3_noun
u3qdu_qor_get(u3_noun a, u3_noun k)
{
  if (u3_nul == a) {
    return u3_nul;
  }

  u3_noun tor = u3qdu_qor_see(a);

  if (u3_nul == tor) {
    return u3_nul;
  }

  u3_noun hot = u3h(tor);

  if (c3n == u3ud(hot)) {
    return u3m_bail(c3__exit);
  }
  else switch ( hot ) {
    default:
      return u3m_bail(c3__exit);

    case c3__sing: {
      u3_noun n_tor = u3t(tor);

      u3_noun kn_tor, pn_tor, vn_tor;
      u3x_trel(n_tor, &kn_tor, &pn_tor, &vn_tor);

      if (c3n == u3r_sing(k, kn_tor)) {
        u3z(tor);
        return u3_nul;
      }
      else {
        u3_noun pro = u3nt(u3_nul, u3k(pn_tor), u3k(vn_tor));

        u3z(tor);

        return pro;
      }
    }

    case c3__play: {
      u3_noun l_tor, r_tor;
      u3x_cell(u3t(tor), &l_tor, &r_tor);

      u3_noun lop = u3qdu_qor_top(l_tor);

      if ( (c3y == u3r_sing(k, lop)) || (c3y == u3qc_gor(k, lop)) ) {
        u3_noun pro = u3qdu_qor_get(l_tor, k);

        u3z(tor);
        u3z(lop);

        return pro;
      }
      else {
        u3_noun pro = u3qdu_qor_get(r_tor, k);

        u3z(tor);
        u3z(lop);

        return pro;
      }

    }
  }
}

u3_noun
u3wdu_qor_get(u3_noun cor)
{
  u3_noun a, k;

  if (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &k, 0))
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qor_get(a, k);
  }
}



