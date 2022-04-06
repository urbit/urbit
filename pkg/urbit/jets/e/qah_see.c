#include "all.h"

u3_noun
u3qe_qah_see(u3_noun a, u3_noun k, u3_noun p)
{
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

      if (c3n == u3r_sing(k, ka)) {
        return u3nc(u3_nul, u3_nul);
      }
      else {
        return u3nc(u3nt(u3_nul, u3k(pa), u3k(va)),
                 u3nq(c3__tip, u3k(k), u3k(p), u3k(va)));
      }
    }

    case c3__bin: {
      u3_noun ka, pa, va;
      u3_noun ta, ma, la, ra;
      u3x_qual(u3t(a), &ka, &pa, &va, &ta);
      u3x_trel(ta, &ma, &la, &ra);

      if (c3y == u3qe_qah_feud(ma, k, ka)) {
        return u3nc(u3_nul, u3k(a));
      }
      else if (c3y == u3r_sing(k, ka)) {
        if (c3y == u3qe_qah_zero(ma, k)) {
          return u3nc(u3nt(u3_nul, u3k(pa), u3k(va)),
                   u3qe_qah_fuse(ma, u3qe_qah_raw(la, k, p, va), ra));
        }
        else {
          return u3nc(u3nt(u3_nul, u3k(pa), u3k(va)),
                   u3qe_qah_fuse(ma, la, u3qe_qah_raw(ra, k, p, va)));
        }
      }
      else if (c3y == u3qe_qah_zero(ma, k)) {
        u3_noun val = u3qe_qah_see(la, k, p);

        u3_noun pv, qv;
        u3x_cell(val, &pv, &qv);

        u3_noun pro = u3nc(u3k(pv),
                        u3nq(c3__bin, u3k(ka), u3k(pa),
                          u3nq(u3k(va), u3k(ma), u3k(qv), u3k(ra))));

        u3z(val);

        return pro;
      }
      else {
        u3_noun val = u3qe_qah_see(ra, k, p);

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
u3we_qah_see(u3_noun cor)
{
  u3_noun a, k, p;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_6, &k, u3x_sam_7 &p, 0)) ||
       (c3n == u3ud(p)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qe_qah_see(a, k, p);
  }
}


