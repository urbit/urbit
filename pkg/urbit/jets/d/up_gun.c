#include "all.h"

u3_noun
u3qdu_gun(u3_noun a, u3_noun k, u3_noun p, u3_noun v)
{
  u3_noun big = u3qdu_qat_gun(a, k, p, v);

  u3_noun pb, qb;
  u3x_cell(big, &pb, &qb);

  if (u3_nul == pb) {
    u3_noun pro = u3nc(u3_nul, u3k(qb));

    u3z(big);
    return pro;
  }
  else {
    u3_noun ppb, bb;
    u3x_cell(u3t(pb), &ppb, &bb);

    u3_noun k_bb, v_bb, t_bb;
    u3x_trel(bb, &k_bb, &v_bb, &t_bb);

    if (c3y == u3r_sing(k, k_bb)) {
      u3_noun pro = u3nc(u3nt(u3_nul, u3k(ppb), u3k(v_bb)), u3k(qb));

      u3z(big);

      return pro;
    }
    else {
      u3_noun pro = u3nc(u3qdu_qor_get(t_bb, k), u3k(qb));

      u3z(big);

      return pro;
    }
  }
}

u3_noun
u3wdu_gun(u3_noun cor)
{
  u3_noun a, k, p, v;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_6, &k, u3x_sam_14, &p,
                          u3x_sam_15, &v, 0)) ||
       (c3n == u3ud(p)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_gun(a, k, p, v);
  }
}


