#include "all.h"

static u3_noun
_help(u3_noun k, u3_noun p, u3_noun buc)
{
  u3_noun k_buc, v_buc, t_buc;
  u3x_trel(buc, &k_buc, &v_buc, &t_buc);

  u3_noun pre = u3nq(u3_nul, u3k(k_buc), u3k(p), u3k(v_buc));
  u3_noun boc = u3qdu_qor_bot(t_buc);

  if (u3_nul == boc) {
    return u3nc(pre, u3_nul);
  }
  else {
    u3_noun pb, qb;
    u3x_cell(u3t(boc), &pb, &qb);
    u3_noun kpb, ppb, vpb;
    u3x_trel(pb, &kpb, &ppb, &vpb);

    u3_noun pro = u3k(u3nc(pre, u3nq(u3_nul, u3k(k), u3k(ppb),
                    u3nt(u3k(kpb), u3k(vpb), u3k(qb)))));

    u3z(boc);

    return pro;
  }
}

u3_noun
u3qdu_qat_jib(u3_noun a)
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

      u3_noun bee = _help(ka, pa, va);

      u3_noun p_bee, q_bee;
      u3x_cell(bee, &p_bee, &q_bee);

      if (u3_nul == q_bee) {
        u3_noun pro = u3nc(u3k(p_bee), u3_nul);

        u3z(bee);
        return pro;
      }
      else
      {
        u3_noun pro = u3nc(u3k(p_bee), u3nc(c3__tip, u3k(u3t(q_bee))));

        u3z(bee);
        return pro;
      }
    }

    case c3__bin: {
      u3_noun ka, pa, va;
      u3_noun ta, ma, la, ra;
      u3x_qual(u3t(a), &ka, &pa, &va, &ta);
      u3x_trel(ta, &ma, &la, &ra);

      u3_noun bee = _help(ka, pa, va);

      u3_noun p_bee, q_bee;
      u3x_cell(bee, &p_bee, &q_bee);

      if (u3_nul == q_bee) {
        u3_noun pro = u3nc(u3k(p_bee), u3qdu_fuse(ma, la, ra));

        u3z(bee);
        return pro;
      }

      u3_noun pt, qt, rt;
      u3x_trel(u3t(q_bee), &pt, &qt, &rt);

      if (c3n == u3r_sing(ka, pt)) {
        u3_noun fus = u3qdu_fuse(ma, la, ra);
        u3_noun pro = u3nc(u3k(p_bee), u3qdu_qat_put(fus, pt, qt, rt));

        u3z(fus);
        u3z(bee);

        return pro;
      }
      else if (c3y == u3qa_lte(qt, pa)) {
        u3_noun pro = u3nc(u3k(p_bee), u3nq(c3__bin, u3k(ka), u3k(qt),
                           u3nq(u3k(rt), u3k(ma), u3k(la), u3k(ra))));

        u3z(bee);
        return pro;
      }
      else
      {
        u3_noun fus = u3qdu_fuse(ma, la, ra);
        u3_noun pro = u3nc(u3k(p_bee), u3qdu_qat_raw(fus, ka, qt, rt));

        u3z(fus);
        u3z(bee);

        return pro;
      }
    }
  }
}

u3_noun
u3wdu_qat_jib(u3_noun cor)
{
  u3_noun a;

  if ( (c3n == u3r_mean(cor, u3x_sam, &a, 0)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qat_jib(a);
  }
}


