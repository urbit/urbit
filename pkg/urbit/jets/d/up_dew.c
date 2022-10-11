#include "all.h"

u3_noun
u3qdu_dew(u3_noun a, u3_noun k)
{
  u3_noun ded = u3qdu_qat_dew(a, k);

  if (u3_nul == ded) {
    return u3_nul;
  }
  else {
    u3_noun pv, qv, rv, sv;
    u3_noun vrv, trv;

    u3x_qual(u3t(ded), &pv, &qv, &rv, &sv);
    u3x_cell(rv, &vrv, &trv);

    if (c3y == u3r_sing(k, pv)) {
      u3_noun low = u3qdu_qor_bot(trv);

      if (u3_nul == low) {
        u3_noun pro = u3nq(u3_nul, u3k(qv), u3k(vrv), u3k(sv));

        u3z(ded);

        return pro;
      }
      else {
        u3_noun kl, pl, vl;
        u3x_trel(u3t(low), &kl, &pl, &vl);

        u3_noun pro = u3nq(u3_nul, u3k(qv), u3k(vrv),
                        u3qdu_qat_raw(sv, kl, pl, vl));

        u3z(ded);
        u3z(low);

        return pro;
      }
    }
    else {
      u3_noun low = u3qdu_qor_dew(trv, k);

      if (u3_nul == low) {
        u3z(ded);

        return u3_nul;
      }
      else {
        u3_noun pl, ql, rl;
        u3x_trel(u3t(low), &pl, &ql, &rl);

        u3_noun buc = u3nc(u3k(vrv), u3k(rl));
        u3_noun pro = u3nq(u3_nul, u3k(pl), u3k(ql),
                        u3qdu_qat_raw(sv, pv, qv, buc));

        u3z(ded);
        u3z(low);
        u3z(buc);

        return pro;
      }
    }
  }
}

u3_noun
u3wdu_dew(u3_noun cor)
{
  u3_noun a, k;

  if (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &k, 0)) {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_dew(a, k);
  }
}

