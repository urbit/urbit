#include "all.h"

u3_noun
u3qdu_qat_raw(u3_noun a, u3_noun k, u3_noun p, u3_noun v)
{
  if (u3_nul == a) {
    return u3nq(c3__tip, u3k(k), u3k(p), u3k(v));
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

      if (c3y == u3qdu_lex(p, k, pa, ka)) {
        return u3qdu_qat_tie(k, p, v, ka, a, u3_nul);
      }
      else
      {
        u3_noun rec = u3nq(c3__tip, u3k(k), u3k(p), u3k(v));
        u3_noun pro = u3qdu_qat_tie(ka, pa, va, k, rec, u3_nul);

        u3z(rec);

        return pro;
      }
    }

    case c3__bin: {
      u3_noun ka, pa, va;
      u3_noun ta, ma, la, ra;
      u3x_qual(u3t(a), &ka, &pa, &va, &ta);
      u3x_trel(ta, &ma, &la, &ra);

      if (c3y == u3qdu_gone(k, ka, ma)) {
        if (c3y == u3qdu_lex(p, k, pa, ka)) {
          return u3qdu_qat_tie(k, p, v, ka, a, u3_nul);
        }
        else {
          u3_noun rec = u3nq(c3__tip, u3k(k), u3k(p), u3k(v));
          u3_noun rev = u3qdu_fuse(ma, la, ra);
          u3_noun pro = u3qdu_qat_tie(ka, pa, va, k, rec, rev);

          u3z(rec);
          u3z(rev);

          return pro;
        }
      }
      else if (c3y == u3qdu_lex(p, k, pa, ka)) {
        if (c3y == u3qdu_zero(ka, ma)) {
          u3_noun rev = u3qdu_qat_raw(la, ka, pa, va);
          return u3nq(c3__bin, u3k(k), u3k(p),
                     u3nq(u3k(v), u3k(ma), rev, u3k(ra)));
        }
        else
        {
          u3_noun rev = u3qdu_qat_raw(ra, ka, pa, va);
          return u3nq(c3__bin, u3k(k), u3k(p),
                     u3nq(u3k(v), u3k(ma), u3k(la), rev));
        }
      }
      else if (c3y == u3qdu_zero(k, ma)) {
        u3_noun rev = u3qdu_qat_raw(la, k, p, v);
        return u3nq(c3__bin, u3k(ka), u3k(pa),
                  u3nq(u3k(va), u3k(ma), rev, u3k(ra)));
      }
      else
      {
        u3_noun rev = u3qdu_qat_raw(ra, k, p, v);
        return u3nq(c3__bin, u3k(ka), u3k(pa),
                  u3nq(u3k(va), u3k(ma), u3k(la), rev));
      }
    }
  }
}

u3_noun
u3wdu_qat_raw(u3_noun cor)
{
  u3_noun a, k, p, v;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_6, &k, u3x_sam_14, &p,
                        u3x_sam_15, &v, 0)) ||
       (c3n == u3ud(k)) ||
       (c3n == u3ud(p)) ||
       (c3n == u3du(v)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qat_raw(a, k, p, v);
  }
}


