#include "all.h"

u3_noun
u3qdu_get(u3_noun a, u3_noun k)
{
  if (u3_nul == a) {
    return u3_nul;
  }

  u3_atom hoc = u3h(a);

  if (c3n == u3ud(hoc)) {
    return u3m_bail(c3__exit);
  }
  else switch ( hoc) {
    default:
      return u3m_bail(c3__exit);

    case c3__tip: {
      u3_noun ka, pa, va;
      u3x_trel(u3t(a), &ka, &pa, &va);

      u3_noun vva, tva;
      u3x_cell(va, &vva, &tva);

      u3_atom mk  = u3r_mug(k);
      u3_atom mka = u3r_mug(ka);

      if (c3n == u3r_sing(mk, mka)) {
        return u3_nul;
      }
      else if (c3y == u3r_sing(k, ka)) {
        return u3nt(u3_nul, u3k(pa), u3k(vva));
      }
      else {
        return u3qdu_qor_get(tva, k);
      }
    }

    case c3__bin: {
      u3_noun ka, pa, va, ta;
      u3_noun vva, tva;
      u3_noun ma, la, ra;

      u3x_qual(u3t(a), &ka, &pa, &va, &ta);
      u3x_cell(va, &vva, &tva);
      u3x_trel(ta, &ma, &la, &ra);

      u3_noun mk  = u3r_mug(k);
      u3_noun mka = u3r_mug(ka);

      if (c3y == u3qdu_feud(ma, k, ka)) {
        return u3_nul;
      }
      else if (c3y == u3r_sing(mk, mka)) {
        if (c3y == u3r_sing(k, ka)) {
          return u3nt(u3_nul, u3k(pa), u3k(vva));
        }
        else {
          return u3qdu_qor_get(tva, k);
        }
      }
      else if (c3y == u3qdu_zero(ma, k)) {
        return u3qdu_get(la, k);
      }
      else {
        return u3qdu_get(ra, k);
      }
    }
  }
}

u3_noun
u3wdu_get(u3_noun cor)
{
  u3_noun a, k;

  if (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &k, 0))
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_get(a, k);
  }
}


