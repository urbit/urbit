#include "all.h"

static u3_noun
_stir(u3_noun k, u3_atom p, u3_noun l, u3_atom q, u3_noun buc)
{
  u3_noun vb, tb;
  u3x_cell(buc, &vb, &tb);

  if (c3y == u3r_sing(k, l)) {
    if (u3_nul == tb) {
      return u3nc(u3nt(u3_nul, u3k(q), u3k(vb)),
               u3qdu_qor_sink(tb, l, p, vb));
    }
    else {
      return u3nc(u3nt(u3_nul, u3k(q), u3k(vb)),
               u3qdu_qor_sink(tb, k, p, vb));
    }
  }
  else {
    u3_noun val = u3qdu_qor_get(tb, k);

    if (u3_nul == val) {
      return u3nq(u3_nul, u3k(l), u3k(q), u3k(buc));
    }
    else {
      u3_noun pro = u3nq(u3k(val), u3k(l), u3k(q),
                      u3nc(u3k(vb),
                        u3qdu_qor_put(tb, k, p, u3t(u3t(val)))));

      u3z(val);
      return pro;
    }
  }
}

u3_noun
u3qdu_see(u3_noun a, u3_noun k, u3_noun p)
{
  if (u3_nul == a) {
    return u3nc(u3_nul, u3_nul);
  }

  u3_atom hoc = u3h(a);

  if (c3n == u3ud(hoc)) {
    return u3m_bail(c3__exit);
  }
  else switch ( hoc ) {
    default:
      return u3m_bail(c3__exit);

    case c3__tip: {
      u3_noun ka, pa, va;
      u3x_trel(u3t(a), &ka, &pa, &va);

      u3_atom mk  = u3r_mug(k);
      u3_atom mka = u3r_mug(ka);

      if (mk != mka) {
        return u3nc(u3_nul, u3k(a));
      }
      else {
        u3_noun mud = _stir(k, p, ka, pa, va);

        u3_noun pm, qm, rm, sm;
        u3x_qual(mud, &pm, &qm, &rm, &sm);

        u3_noun pro = u3nc(u3k(pm), u3nq(c3__tip, u3k(qm), u3k(rm), u3k(sm)));

        u3z(mud);

        return pro;
      }
    }

    case c3__bin: {
      u3_noun ka, pa, va, ta, ma, la, ra;
      u3x_qual(u3t(a), &ka, &pa, &va, &ta);
      u3x_trel(ta, &ma, &la, &ra);

      if (c3y == u3qdu_feud(ma, k, ka)) {
        return u3nc(u3_nul, u3k(a));
      }

      u3_atom mk  = u3r_mug(k);
      u3_atom mka = u3r_mug(ka);

      if (mk == mka) {
        u3_noun mud = _stir(k, p, ka, pa, va);
        u3_noun pm, qm, rm, sm;
        u3x_qual(mud, &pm, &qm, &rm, &sm);

        if (c3y == u3qdu_zero(ma, k)) {
          u3_noun lef = u3qdu_qat_raw(la, qm, rm, sm);
          u3_noun pro = u3nc(u3k(pm), u3qdu_fuse(ma, lef, ra));

          u3z(mud);
          u3z(lef);

          return pro;
        }
        else {
          u3_noun rye = u3qdu_qat_raw(ra, qm, rm, sm);
          u3_noun pro = u3nc(u3k(pm), u3qdu_fuse(ma, la, rye));

          u3z(mud);
          u3z(rye);

          return pro;
        }
      }
      else if (c3y == u3qdu_zero(ma, k)) {
        u3_noun val = u3qdu_see(la, k, p);

        u3_noun pv, qv;
        u3x_cell(val, &pv, &qv);

        u3_noun pro = u3nc(u3k(pv),
                        u3nq(c3__bin, u3k(ka), u3k(pa),
                          u3nq(u3k(va), u3k(ma), u3k(qv), u3k(ra))));

        u3z(val);
        return pro;
      }
      else {
        u3_noun val = u3qdu_see(ra, k, p);

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
u3wdu_see(u3_noun cor)
{
  u3_noun a, k, p;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_6, &k,
                          u3x_sam_7, &p, 0)) ||
       (c3n == u3ud(p)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_see(a, k, p);
  }
}


