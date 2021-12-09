#include "all.h"

static u3_noun
_help(u3_noun bp, u3_noun bb, u3_noun k, u3_noun p)
{
  u3_noun k_bb, v_bb, t_bb;
  u3x_trel(bb, &k_bb, &v_bb, &t_bb);

  if (c3y == u3r_sing(k, k_bb)) {
    return u3nc(u3nt(u3_nul, u3k(bp), u3k(v_bb)),
                  u3qdu_qat_make(k, p, v_bb, t_bb));
  }
  else {
    u3_noun val = u3qdu_qor_get(t_bb, k);

    if (u3_nul == val) {
      return u3nt(u3_nul, u3k(bp), u3k(bb));
    }
    else {
      u3_noun pv, qv;
      u3x_cell(u3t(val), &pv, &qv);

      u3_noun pre = u3qdu_qor_put(t_bb, k, p, qv);
      return u3nt(val, u3k(bp), u3nt(u3k(k_bb), u3k(v_bb), pre));
    }
  }
}

u3_noun
u3qdu_qat_see(u3_noun a, u3_noun k, u3_noun p)
{
  u3_noun h = u3r_mug(k);

  if (u3_nul == a) {
    return u3nc(u3_nul, u3_nul);
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

      if (c3y == u3r_sing(h, ka)) {
        u3_noun val = _help(pa, va, k, p);

        u3_noun pv, qv, rv;
        u3x_trel(val, &pv, &qv, &rv);

        u3_noun pro = u3nc(u3k(pv), u3nq(c3__tip, u3k(h), u3k(qv), u3k(rv)));

        u3z(val);

        return pro;
      }
      else {
        return u3nc(u3_nul, u3k(a));
      }
    }

    case c3__bin: {
      u3_noun ka, pa, va;
      u3_noun ta, ma, la, ra;
      u3x_qual(u3t(a), &ka, &pa, &va, &ta);
      u3x_trel(ta, &ma, &la, &ra);

      if (c3y == u3qdu_gone(h, ka, ma)) {
        return u3nc(u3_nul, u3k(a));
      }
      else if (c3y == u3r_sing(h, ka)) {
        u3_noun val = _help(pa, va, k, p);

        u3_noun pv, qv, rv;
        u3x_trel(val, &pv, &qv, &rv);

        if (c3y == u3qdu_zero(h, ma)) {
          u3_noun pre = u3qdu_qat_raw(la, h, qv, rv);
          u3_noun pro = u3nc(u3k(pv), u3qdu_fuse(ma, pre, ra));

          u3z(val);
          u3z(pre);

          return pro;
        }
        else {
          u3_noun pre = u3qdu_qat_raw(ra, h, qv, rv);
          u3_noun pro = u3nc(u3k(pv), u3qdu_fuse(ma, la, pre));

          u3z(val);
          u3z(pre);

          return pro;
        }
      }
      else if (c3y == u3qdu_zero(h, ma)) {
        u3_noun val = u3qdu_qat_see(la, k, p);

        u3_noun pv, qv;
        u3x_cell(val, &pv, &qv);

        u3_noun pro = u3nc(u3k(pv),
                        u3nq(c3__bin, u3k(ka), u3k(pa),
                          u3nq(u3k(va), u3k(ma), u3k(qv), u3k(ra))));

        u3z(val);

        return pro;
      }
      else {
        u3_noun val = u3qdu_qat_see(ra, k, p);

        u3_noun pv, qv;
        u3x_cell(val, &pv, &qv);

        u3_noun pro = u3nc(u3k(pv),
                        u3nq(c3__bin, u3k(ka), u3k(pa),
                          u3nq(u3k(va), u3k(ma), u3k(la), u3k(qv))));

        u3z(val);

        return pro;
      }
    }
  }
}

u3_noun
u3wdu_qat_see(u3_noun cor)
{
  u3_noun a, k, p;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_6, &k, u3x_sam_7 &p, 0)) ||
       (c3n == u3ud(p)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qat_see(a, k, p);
  }
}


