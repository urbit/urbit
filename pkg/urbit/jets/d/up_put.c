#include "all.h"

static u3_noun
_help(u3_noun pb, u3_noun buc, u3_noun k, u3_noun p, u3_noun v)
{
  u3_noun k_buc, v_buc, t_buc;
  u3x_trel(buc, &k_buc, &v_buc, &t_buc);

  if (c3y == u3r_sing(k, k_buc)) {
    return u3qdu_qat_make(k, p, v, t_buc);
  }
  else if ( (c3y == u3qa_lth(pb, p)) ||
            ((c3y == u3r_sing(p, pb)) && (c3y == u3qc_gor(k_buc, k))) )
  {
    return u3nq(u3k(pb), u3k(k_buc), u3k(v_buc),
             u3qdu_qor_put(t_buc, k, p, v));
  }
  else if (c3y == u3qdu_qor_has(t_buc, k)) {
    u3_noun pre = u3qdu_qor_del(t_buc, k);
    u3_noun pro = u3qdu_qor_put(pre, k_buc, pb, v_buc);

    u3z(pre);

    return u3nq(u3k(p), u3k(k), u3k(v), pro);
  }
  else {
    return u3nq(u3k(p), u3k(k), u3k(v),
             u3qdu_qor_put(t_buc, k_buc, pb, v_buc));
  }
}

u3_noun
u3qdu_put(u3_noun a, u3_noun k, u3_noun p, u3_noun v)
{
  u3_atom h = u3r_mug(k);
  u3_noun ped = u3qdu_qat_pet(a, h);

  if (u3_nul == ped) {
    u3_noun pre = u3nt(u3k(k), u3k(v), u3_nul);
    u3_noun pro = u3qdu_qat_raw(a, h, p, pre);

    u3z(pre);

    return pro;
  }
  else {
    u3_noun pp, qp, rp;
    u3x_trel(u3t(ped), &pp, &qp, &rp);

    u3_noun bee = _help(pp, qp, k, p, v);

    u3_noun pb, qb;
    u3x_cell(bee, &pb, &qb);

    u3_noun pro = u3qdu_qat_raw(rp, h, pb, qb);

    u3z(bee);
    u3z(ped);

    return pro;
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


