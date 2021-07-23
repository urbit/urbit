/* j/4/by_int.c
**
*/
#include "all.h"

u3_noun
u3qdb_int(u3_noun a, u3_noun b)
{
  if (  (u3_nul == a)
     || (u3_nul == b) )
  {
    return u3_nul;
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

    if ( c3y == u3qc_mor(p_n_a, p_n_b) ) {
      if ( c3y == u3r_sing(p_n_b, p_n_a) ) {
        return u3nt(u3k(n_b), u3qdb_int(l_a, l_b), u3qdb_int(r_a, r_b));
      }
      else if ( c3y == u3qc_gor(p_n_b, p_n_a) ) {
        u3_noun new_l_b = u3nt(u3k(n_b), u3k(l_b), u3_nul);
        u3_noun   new_a = u3qdb_int(l_a, new_l_b);

        u3z(new_l_b);
        return u3kdb_uni(new_a, u3qdb_int(a, r_b));
      }
      else {
        u3_noun new_r_b = u3nt(u3k(n_b), u3_nul, u3k(r_b));
        u3_noun   new_a = u3qdb_int(r_a, new_r_b);

        u3z(new_r_b);
        return u3kdb_uni(new_a, u3qdb_int(a, l_b));
      }
    }
    else if ( c3y == u3r_sing(p_n_a, p_n_b) ) {
      return u3nt(u3k(n_b), u3qdb_int(l_a, l_b), u3qdb_int(r_a, r_b));
    }
    else if ( c3y == u3qc_gor(p_n_a, p_n_b) ) {
      u3_noun new_l_a = u3nt(u3k(n_a), u3k(l_a), u3_nul);
      u3_noun   new_a = u3qdb_int(new_l_a, l_b);

      u3z(new_l_a);
      return u3kdb_uni(new_a, u3qdb_int(r_a, b));
    }
    else {
      u3_noun new_r_a = u3nt(u3k(n_a), u3_nul, u3k(r_a));
      u3_noun   new_a = u3qdb_int(new_r_a, r_b);

      u3z(new_r_a);
      return u3kdb_uni(new_a, u3qdb_int(l_a, b));
    }
  }
}

u3_noun
u3wdb_int(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0);
  return u3qdb_int(a, b);
}
