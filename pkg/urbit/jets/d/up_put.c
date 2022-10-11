#include "all.h"

u3_noun
u3qdu_put(u3_noun a, u3_noun k, u3_atom p, u3_noun v)
{
  u3_noun ded = u3qdu_qat_dew(a, k);

  if (u3_nul == ded) {
    u3_noun buc = u3nc(u3k(v), u3_nul);
    u3_noun pro = u3qdu_qat_raw(a, k, p, buc);

    u3z(buc);
    return pro;
  }
  else {
    u3_noun pv, qv, rv, sv;
    u3_noun vrv, trv;

    u3x_qual(u3t(ded), &pv, &qv, &rv, &sv);
    u3x_cell(rv, &vrv, &trv);

    if (k == pv) {
      u3_noun sun = u3qdu_qor_sink(trv, k, p, v);

      u3_noun ps, qs, rs;
      u3x_trel(sun, &ps, &qs, &rs);

      u3_noun pro = u3qdu_qat_raw(sv, ps, qs, rs);

      u3z(ded);
      u3z(sun);

      return pro;
    }
    else if ( (c3y == u3qa_lth(qv, p)) ||
              ( (c3y == u3r_sing(p, qv)) && (c3y == u3qc_gor(pv, k)) ) ) {

      u3_noun buc = u3nc(u3k(vrv), u3qdu_qor_put(trv, k, p, v));
      u3_noun pro = u3qdu_qat_raw(sv, pv, qv, buc);

      u3z(ded);
      u3z(buc);

      return pro;
    }
    else if (c3y == u3qdu_qor_has(trv, k)) {
      u3_noun dud = u3qdu_qor_del(trv, k);
      u3_noun buc = u3nc(u3k(v), u3qdu_qor_put(dud, pv, qv, vrv));
      u3_noun pro = u3qdu_qat_raw(sv, k, p, buc);

      u3z(ded);
      u3z(dud);
      u3z(buc);

      return pro;
    }
    else {
      u3_noun buc = u3nc(u3k(v), u3qdu_qor_put(trv, pv, qv, vrv));
      u3_noun pro = u3qdu_qat_raw(sv, k, p, buc);

      u3z(ded);
      u3z(buc);

      return pro;
    }
  }
}

u3_noun
u3wdu_put(u3_noun cor)
{
  u3_noun a, k, p, v;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_6, &k, u3x_sam_14, &p,
                          u3x_sam_15, &v, 0)) ||
       (c3n == u3ud(p)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_put(a, k, p, v);
  }
}


