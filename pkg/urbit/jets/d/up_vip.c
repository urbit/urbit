#include "all.h"

u3_noun
u3qdu_vip(u3_noun a, u3_noun k, u3_atom p, u3_noun v)
{
  if (u3_nul == a) {
    return u3nc(u3_nul,
             u3nq(c3__tip, u3k(k), u3k(p), u3nc(u3k(v), u3_nul)));
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

      if (c3y == u3r_sing(mk, mka)) {
        if (c3y == u3r_sing(k, ka)) {
          return u3nc(u3nt(u3_nul, u3k(pa), u3k(vva)),
                   u3nc(c3__tip, u3qdu_qor_sink(tva, k, p, v)));
        }
        else {
          u3_noun got = u3qdu_qor_get(tva, k);
          u3_noun buc = u3nc(u3k(vva), u3qdu_qor_put(tva, k, p, v));
          u3_noun pro = u3nc(u3k(got),
                          u3nq(c3__tip, u3k(ka), u3k(pa), u3k(buc)));

          u3z(got);
          u3z(buc);
          return pro;
        }
      }
      else {
        u3_noun lef = u3nq(c3__tip, u3k(k), u3k(p), u3nc(u3k(v), u3_nul));
        u3_noun pro = u3nc(u3_nul, u3qdu_rule(ka, pa, va, lef, u3_nul));

        u3z(lef);
        return pro;
      }
    }

    case c3__bin: {
      u3_noun ka, pa, va, ta, ma, la, ra;
      u3x_qual(u3t(a), &ka, &pa, &va, &ta);
      u3x_trel(ta, &ma, &la, &ra);

      if (c3y == u3qdu_feud(ma, k, ka)) {
        u3_noun lef = u3nq(c3__tip, u3k(k), u3k(p), u3nc(u3k(v), u3_nul));
        u3_noun rye = u3qdu_fuse(ma, la, ra);

        u3_noun pro = u3nc(u3_nul, u3qdu_rule(ka, pa, va, lef, rye));

        u3z(lef);
        u3z(rye);

        return pro;
      }

      u3_noun mk  = u3r_mug(k);
      u3_noun mka = u3r_mug(ka);

      if (c3y == u3r_sing(mk, mka)) {

        u3_noun vva, tva;
        u3x_cell(va, &vva, &tva);

        if (c3y == u3r_sing(k, ka)) {
          u3_noun val = u3qdu_qor_sink(tva, k, p, v);

          u3_noun pv, qv, rv;
          u3x_trel(val, &pv, &qv, &rv);

          if (c3y == u3qdu_zero(ma, pv)) {
            u3_noun lef = u3qdu_qat_raw(la, pv, qv, rv);
            u3_noun pro = u3nc(u3nt(u3_nul, u3k(pa), u3k(vva)),
                            u3qdu_fuse(ma, lef, ra));

            u3z(val);
            u3z(lef);

            return pro;
          }
          else {
            u3_noun rye = u3qdu_qat_raw(ra, pv, qv, rv);
            u3_noun pro = u3nc(u3nt(u3_nul, u3k(pa), u3k(vva)),
                            u3qdu_fuse(ma, la, rye));

            u3z(val);
            u3z(rye);

            return pro;
          }
        }
        else {
          u3_noun got = u3qdu_qor_get(tva, k);
          u3_noun buc = u3nc(u3k(vva), u3qdu_qor_put(tva, k, p, v));
          u3_noun pro = u3nc(u3k(got),
                          u3nq(c3__bin, u3k(ka), u3k(pa),
                            u3nq(u3k(buc), u3k(ma), u3k(la), u3k(ra))));

          u3z(got);
          u3z(buc);

          return pro;
        }

      }
      else if (c3y == u3qdu_zero(ma, k)) {
        u3_noun val = u3qdu_vip(la, k, p, v);
        u3_noun pro = u3nc(u3k(u3h(val)),
                        u3nq(c3__bin, u3k(ka), u3k(pa),
                         u3nq(u3k(va), u3k(ma), u3k(u3t(val)), u3k(ra))));

        u3z(val);

        return pro;
      }
      else {
        u3_noun val = u3qdu_vip(ra, k, p, v);
        u3_noun pro = u3nc(u3k(u3h(val)),
                        u3nq(c3__bin, u3k(ka), u3k(pa),
                          u3nq(u3k(va), u3k(ma), u3k(la), u3k(u3t(val)))));

        u3z(val);

        return pro;
      }
    }
  }
}

u3_noun
u3wdu_vip(u3_noun cor)
{
  u3_noun a, k, p, v;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_6, &k, u3x_sam_14, &p,
                          u3x_sam_15, &v, 0)) ||
       (c3n == u3ud(p)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_vip(a, k, p, v);
  }
}


