#include "all.h"

static u3_noun
_omit(u3_noun b, u3_noun k)
{
  if (u3_nul == b) {
    return u3nc(u3_nul, u3_nul);
  }

  u3_noun hob = u3h(b);

  if (c3n == u3ud(hob)) {
    return u3m_bail(c3__exit);
  }
  else switch ( hob ) {
    default:
      return u3m_bail(c3__exit);

    case c3__tip: {
      u3_noun kb, pb, vb;
      u3x_trel(u3t(b), &kb, &pb, &vb);

      u3_noun mk  = u3r_mug(k);
      u3_noun mkb = u3r_mug(kb);

      return (c3n == u3r_sing(mk, mkb))
           ? u3nc(u3_nul, u3k(b))
           : u3nc(u3nq(u3_nul, u3k(kb), u3k(pb), u3k(vb)), u3_nul);
    }

    case c3__bin: {
      u3_noun kb, pb, vb;
      u3_noun tb, mb, lb, rb;
      u3x_qual(u3t(b), &kb, &pb, &vb, &tb);
      u3x_trel(tb, &mb, &lb, &rb);

      if (c3y == u3qdu_feud(mb, k, kb)) {
        return u3nc(u3_nul, u3k(b));
      }

      u3_atom mk  = u3r_mug(k);
      u3_atom mkb = u3r_mug(kb);

      if (c3y == u3r_sing(mk, mkb)) {
        return u3nc(u3nq(u3_nul, u3k(kb), u3k(pb), u3k(vb)),
                 u3qdu_fuse(mb, lb, rb));
      }
      else if (c3y == u3qdu_zero(mb, k)) {
        u3_noun med = _omit(lb, k);
        u3_noun pro = u3nc(u3k(u3h(med)),
                        u3qdu_funk(kb, pb, vb, mb, u3t(med), rb));

        u3z(med);
        return pro;
      }
      else {
          u3_noun med = _omit(rb, k);
          u3_noun pro = u3nc(u3k(u3h(med)),
                          u3qdu_wane(kb, pb, vb, mb, lb, u3t(med)));

          u3z(med);
          return pro;
      }
    }
  }
}

u3_noun
u3qdu_qat_dew(u3_noun a, u3_noun k)
{
  u3_noun med = _omit(a, k);

  u3_noun pm, qm;
  u3x_cell(med, &pm, &qm);

  if (u3_nul == pm) {
    u3z(med);
    return u3_nul;
  }
  else {
    u3_noun vel = u3t(pm);

    u3_noun pv, qv, rv;
    u3x_trel(vel, &pv, &qv, &rv);

    u3_noun pro = u3nc(u3_nul, u3nq(u3k(pv), u3k(qv), u3k(rv), u3k(qm)));

    u3z(med);
    return pro;
  }
}

u3_noun
u3wdu_qat_dew(u3_noun cor)
{
  u3_noun a, k;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &k, 0)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qat_dew(a, k);
  }
}

