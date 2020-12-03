/* j/4/by_uni.c
 */
#include "all.h"

u3_noun
u3qdb_uni(u3_noun a, u3_noun b)
{
  if ( u3_nul == b ) {
    return u3k(a);
  }
  else if ( u3_nul == a ) {
    return u3k(b);
  }
  else {
    u3_noun n_a, l_a, r_a;
    u3_noun n_b, l_b, r_b;
    u3_noun p_n_a, q_n_a;
    u3_noun p_n_b, q_n_b;
    u3x_trel(a, &n_a, &l_a, &r_a);
    u3x_trel(b, &n_b, &l_b, &r_b);
    u3x_cell(n_a, &p_n_a, &q_n_a);
    u3x_cell(n_b, &p_n_b, &q_n_b);

    if ( c3y == u3r_sing(p_n_a, p_n_b) ) {
      return u3nt(u3k(n_b),
                  u3qdb_uni(l_a, l_b),
                  u3qdb_uni(r_a, r_b));
    }
    else if ( c3y == u3qc_mor(p_n_a, p_n_b) ) {
      u3_noun new_a, old_b;

      if ( c3y == u3qc_gor(p_n_b, p_n_a) ) {
        u3_noun new_b  = u3nt(u3k(n_b), u3k(l_b), u3_nul);
        u3_noun new_la = u3qdb_uni(l_a, new_b);
        u3z(new_b);

        new_a = u3nt(u3k(n_a), new_la, u3k(r_a));
        old_b = r_b;
      }
      else {
        u3_noun new_b  = u3nt(u3k(n_b), u3_nul, u3k(r_b));
        u3_noun new_ra = u3qdb_uni(r_a, new_b);
        u3z(new_b);

        new_a = u3nt(u3k(n_a), u3k(l_a), new_ra);
        old_b = l_b;
      }

      {
        u3_noun pro = u3qdb_uni(new_a, old_b);
        u3z(new_a);
        return pro;
      }
    }
    else {
      u3_noun old_a, new_b;

      if ( c3y == u3qc_gor(p_n_a, p_n_b) ) {
        u3_noun new_a  = u3nt(u3k(n_a), u3k(l_a), u3_nul);
        u3_noun new_lb = u3qdb_uni(new_a, l_b);
        u3z(new_a);

        new_b = u3nt(u3k(n_b), new_lb, u3k(r_b));
        old_a = r_a;
      }
      else {
        u3_noun new_a  = u3nt(u3k(n_a), u3_nul, u3k(r_a));
        u3_noun new_rb = u3qdb_uni(new_a, r_b);
        u3z(new_a);

        new_b = u3nt(u3k(n_b), u3k(l_b), new_rb);
        old_a = l_a;
      }

      {
        u3_noun pro = u3qdb_uni(old_a, new_b);
        u3z(new_b);
        return pro;
      }
    }
  }
}

u3_noun
u3wdb_uni(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);
  return u3qdb_uni(a, b);
}

u3_noun
u3kdb_uni(u3_noun a, u3_noun b)
{
  u3_noun pro = u3qdb_uni(a, b);
  u3z(a); u3z(b);
  return pro;
}
