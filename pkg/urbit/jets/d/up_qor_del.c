#include "all.h"

u3_noun
u3qdu_qor_del(u3_noun a, u3_noun k)
{
  if (u3_nul == a) {
    return u3_nul;
  }

  u3_noun n_a, t_a, m_a;
  u3x_trel(a, &n_a, &t_a, &m_a);

  if (u3_nul == t_a) {
    u3_noun kn_a = u3h(n_a);

    if (c3y == u3r_sing(k, kn_a)) {
      return u3_nul;
    }
    else {
      return u3nt(u3k(n_a), u3_nul, u3k(kn_a));
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
        u3_noun lel = u3nt(u3k(n_a), u3k(l_b), u3k(m_b));
        u3_noun rel = u3nt(u3k(n_b), u3k(r_b), u3k(m_a));

        u3_noun lod = u3qdu_qor_del(lel, k);
        u3_noun pro = u3qdu_toy(lod, rel);

        u3z(lod);
        u3z(lel);
        u3z(rel);

        return pro;
      }
      else {
        u3_noun lel = u3nt(u3k(n_a), u3k(l_b), u3k(m_b));
        u3_noun rel = u3nt(u3k(n_b), u3k(r_b), u3k(m_a));

        u3_noun rod = u3qdu_qor_del(rel, k);
        u3_noun pro = u3qdu_toy(lel, rod);

        u3z(rod);
        u3z(lel);
        u3z(rel);

        return pro;
      }
    }

    case c3__llos: {
      u3_noun b = u3t(u3t(t_a));

      u3_noun n_b, l_b, m_b, r_b;
      u3x_qual(b, &n_b, &l_b, &m_b, &r_b);

      if ( (c3y == u3r_sing(k, m_b)) || (c3y == u3qc_gor(k, m_b)) ) {
        u3_noun lel = u3nt(u3k(n_b), u3k(l_b), u3k(m_b));
        u3_noun rel = u3nt(u3k(n_a), u3k(r_b), u3k(m_a));

        u3_noun lod = u3qdu_qor_del(lel, k);
        u3_noun pro = u3qdu_toy(lod, rel);

        u3z(lod);
        u3z(lel);
        u3z(rel);

        return pro;
      }
      else {
        u3_noun lel = u3nt(u3k(n_b), u3k(l_b), u3k(m_b));
        u3_noun rel = u3nt(u3k(n_a), u3k(r_b), u3k(m_a));

        u3_noun rod = u3qdu_qor_del(rel, k);
        u3_noun pro = u3qdu_toy(lel, rod);

        u3z(rod);
        u3z(lel);
        u3z(rel);

        return pro;
      }
    }
  }
}

u3_noun
u3wdu_qor_del(u3_noun cor)
{
  u3_noun a, k;

  if (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &k, 0))
  {
    return u3m_bail(c3__exit);
  } else {
    return u3qdu_qor_del(a, k);
  }
}



