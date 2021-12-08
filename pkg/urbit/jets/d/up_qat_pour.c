#include "all.h"

u3_noun
u3qdu_qat_pour(u3_noun a)
{
  u3_noun bot = u3qdu_qor_bot(a);

  if (u3_nul == bot) {
    return u3_nul;
  }
  else {
    u3_noun u = u3t(bot);

    u3_noun pu, qu;
    u3x_cell(u, &pu, &qu);

    u3_atom k_pu, p_pu, v_pu;
    u3x_trel(pu, &k_pu, &p_pu, &v_pu);

    u3_noun pro = u3nc(u3_nul, u3nq(u3k(p_pu), u3k(k_pu), u3k(v_pu), u3k(qu)));

    u3z(bot);

    return pro;
  }
}

u3_noun
u3wdu_qat_pour(u3_noun cor)
{
  u3_noun a;

  if (c3n == u3r_mean(cor, u3x_sam, &a, 0))
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qat_pour(a);
  }
}


