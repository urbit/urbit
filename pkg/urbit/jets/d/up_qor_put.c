#include "all.h"

u3_noun
u3qdu_qor_put(u3_noun a, u3_noun k, u3_noun p, u3_noun v)
{
  if (u3_nul == a) {
    return u3nt(u3nt(u3k(k), u3k(p), u3k(v)), u3_nul, u3k(k));
  }

  u3_noun n_a, t_a, m_a;
  u3_noun k_n_a, p_n_a, v_n_a;
  u3x_trel(a, &n_a, &t_a, &m_a);
  u3x_trel(n_a, &k_n_a, &p_n_a, &v_n_a);

  if (u3_nul == t_a) {
    if (c3y == u3r_sing(k, m_a)) {
      return u3nt(u3nt(u3k(k), u3k(p), u3k(v)), u3_nul, u3k(k));
    }
    else if (c3y == u3qc_gor(k, m_a)) {
      u3_noun fel = u3nt(u3nt(u3k(k), u3k(p), u3k(v)), u3_nul, u3k(k));
      u3_noun fer = u3nt(u3k(n_a), u3_nul, u3k(k_n_a));

      u3_noun pro = u3qdu_qor_toy(fel, fer);

      u3z(fel);
      u3z(fer);

      return pro;
    }
    else {
      u3_noun fel = u3nt(u3nt(u3k(k_n_a), u3k(p_n_a), u3k(v_n_a)),
                         u3_nul,
                         u3k(k_n_a));
      u3_noun fer = u3nt(u3nt(u3k(k), u3k(p), u3k(v)), u3_nul, u3k(k));
      u3_noun pro = u3qdu_qor_toy(fel, fer);

      u3z(fel);
      u3z(fer);

      return pro;
    }
  }

  u3_noun hot = u3h(t_a);

  if (c3n == u3ud(hot)) {
    return u3m_bail(c3__exit);
  }
  else switch ( hot ) {
    default:
      return u3m_bail(c3__exit);

    case c3__rlos: {
      u3_noun b = u3t(u3t(t_a));

      u3_noun n_b, l_b, m_b, r_b;
      u3x_qual(b, &n_b, &l_b, &m_b, &r_b);

      if ( (c3y == u3r_sing(k, m_b)) || (c3y == u3qc_gor(k, m_b)) ) {
        u3_noun fel = u3nt(u3k(n_a), u3k(l_b), u3k(m_b));
        u3_noun fer = u3nt(u3k(n_b), u3k(r_b), u3k(m_a));
        u3_noun pre = u3qdu_qor_put(fel, k, p, v);
        u3_noun pro = u3qdu_qor_toy(pre, fer);

        u3z(pre);
        u3z(fel);
        u3z(fer);

        return pro;
      } else {
        u3_noun fel = u3nt(u3k(n_b), u3k(r_b), u3k(m_a));
        u3_noun fer = u3nt(u3k(n_a), u3k(l_b), u3k(m_b));
        u3_noun pre = u3qdu_qor_put(fel, k, p, v);
        u3_noun pro = u3qdu_qor_toy(fer, pre);

        u3z(pre);
        u3z(fel);
        u3z(fer);

        return pro;
      }
    }

    case c3__llos: {
      u3_noun b = u3t(u3t(t_a));

      u3_noun n_b, l_b, m_b, r_b;
      u3x_qual(b, &n_b, &l_b, &m_b, &r_b);

      if ( (c3y == u3r_sing(k, m_b)) || (c3y == u3qc_gor(k, m_b)) ) {
        u3_noun fel = u3nt(u3k(n_b), u3k(l_b), u3k(m_b));
        u3_noun fer = u3nt(u3k(n_a), u3k(r_b), u3k(m_a));
        u3_noun pre = u3qdu_qor_put(fel, k, p, v);

        u3_noun pro = u3qdu_qor_toy(pre, fer);

        u3z(pre);
        u3z(fel);
        u3z(fer);

        return pro;
      } else {
        u3_noun fel = u3nt(u3k(n_a), u3k(r_b), u3k(m_a));
        u3_noun fer = u3nt(u3k(n_b), u3k(l_b), u3k(m_b));
        u3_noun pre = u3qdu_qor_put(fel, k, p, v);
        u3_noun pro = u3qdu_qor_toy(fer, pre);

        u3z(pre);
        u3z(fel);
        u3z(fer);

        return pro;
      }
    }
  }
}

u3_noun
u3wdu_qor_put(u3_noun cor)
{
  u3_noun a, k, p, v;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_6, &k,
                             u3x_sam_14, &p, u3x_sam_15, &v, 0)) ||
       (c3n == u3ud(p)) )
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qor_put(a, k, p, v);
  }
}


