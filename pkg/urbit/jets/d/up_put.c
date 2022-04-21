#include "all.h"

static u3_noun
_xfer_raw_bee(u3_noun pri, u3_noun bee)
{
  u3_noun pb, qb, rb;
  u3x_trel(bee, &pb, &qb, &rb);

  u3_noun pro = u3qdu_qat_raw(pri, pb, qb, rb);

  u3z(bee);

  return pro;
}

u3_noun
u3qdu_put(u3_noun a, u3_noun k, u3_atom p, u3_noun v)
{
  u3_noun ped = u3qdu_qat_dew(a, k);

  if (u3_nul == ped) {
    u3_noun val = u3nc(u3k(v), u3_nul);
    u3_noun pro = u3qdu_qat_raw(a, k, p, val);

    u3z(val);
    return pro;
  }
  else {
    u3_noun l, q, buc, pri;
    u3x_qual(u3t(ped), &l, &q, &buc, &pri);

    u3_atom mk = u3r_mug(k);
    u3_atom ml = u3r_mug(l);

    if (mk == ml) {
      u3_noun bee = u3qdu_qor_sink(u3t(buc), k, p, v);
      u3_noun pro = _xfer_raw_bee(pri, bee);

      u3z(ped);
      return pro;
    }
    else if ( (c3y == u3qa_lth(q, p)) ||
              ( (c3y == u3r_sing(p, q)) && (c3y == u3qc_gor(l, k)) ) ) {
      u3_noun bee = u3nq(u3k(l), u3k(q), u3k(u3h(buc)),
                      u3qdu_qor_put(u3t(buc), k, p, v));

      u3_noun pro = _xfer_raw_bee(pri, bee);

      u3z(ped);
      return pro;
    }
    else {
      if (c3y == u3qdu_qor_has(u3t(buc), k)) {
        u3_noun pre = u3qdu_qor_del(u3t(buc), k);
        u3_noun bee = u3qdu_qor_put(pre, l, q, u3h(buc));

        u3z(pre);

        u3_noun pro = _xfer_raw_bee(pri, bee);

        u3z(ped);
        return pro;
      }
      else {
        u3_noun bee = u3qdu_qor_put(u3t(buc), l, q, u3h(buc));
        u3_noun pro = _xfer_raw_bee(pri, bee);

        u3z(ped);
        return pro;
      }
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


