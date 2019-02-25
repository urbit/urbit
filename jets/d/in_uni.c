/* j/4/in_uni.c
**
*/
#include "all.h"

/* internal functions
*/

static u3_noun
_in_uni(u3_noun a, u3_noun b)
{
  u3_noun n_a, l_a, r_a,
          n_b, l_b, r_b,
          neb, sub, naw, pro;
  if ( u3_nul == a ) {
    return u3k(b);
  }
  else if ( u3_nul == b ) {
    return u3k(a);
  }
  else if ( (c3n == u3r_trel(a, &n_a, &l_a, &r_a))
         || (c3n == u3r_trel(b, &n_b, &l_b, &r_b)) )
  {
    return u3m_bail(c3__exit);
  }
  else if ( c3n == u3qc_mor(n_a, n_b) ) {
    if ( c3y == u3r_sing(n_a, n_b) ) {
      return u3nt(u3k(n_b), _in_uni(l_a, l_b), _in_uni(r_a, r_b));
    }
    else if ( c3y == u3qc_gor(n_a, n_b) ) {
      naw = u3nt(u3k(n_a), u3k(l_a), u3_nul);
      sub = _in_uni(naw, l_b);
      neb = u3nt(u3k(n_b), sub, u3k(r_b));
      pro = _in_uni(r_a, neb);
      u3z(naw); u3z(neb);
      return pro;
    }
    else {
      naw = u3nt(u3k(n_a), u3_nul, u3k(r_a));
      sub = _in_uni(naw, r_b);
      neb = u3nt(u3k(n_b), u3k(l_b),  sub);
      pro = _in_uni(l_a, neb);
      u3z(naw); u3z(neb);
      return pro;
    }
  }
  else if ( c3y == u3r_sing(n_b, n_a) ) {
    return u3nt(u3k(n_b), _in_uni(l_a, l_b), _in_uni(r_a, r_b));
  }
  else if ( c3y == u3qc_gor(n_b, n_a) ) {
    neb = u3nt(u3k(n_b), u3k(l_b), u3_nul);
    sub = _in_uni(l_a, neb);
    naw = u3nt(u3k(n_a), sub, u3k(r_a));
    pro = _in_uni(naw, r_b);
    u3z(neb); u3z(naw);
    return pro;
  }
  else {
    neb = u3nt(u3k(n_b), u3_nul, u3k(r_b));
    sub = _in_uni(r_a, neb);
    naw = u3nt(u3k(n_a), u3k(l_a), sub);
    pro = _in_uni(naw, l_b);
    u3z(neb); u3z(naw);
    return pro;
  }
}


/* functions
*/
  u3_noun
  u3kdi_uni(u3_noun a,
            u3_noun b)
  {
    u3_noun c = u3qdi_uni(a, b);

    u3z(a);
    u3z(b);
    return c;
  }

  u3_noun
  u3qdi_uni(u3_noun a,
            u3_noun b)
  {
    if ( c3y == u3r_sing(a, b) ) {
      return u3k(a);
    }
    else {
      return _in_uni(a, b);
    }
  }
  u3_noun
  u3wdi_uni(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam, &b, u3x_con_sam, &a, 0) ) {
      return u3m_bail(c3__exit);
    } else {
      return u3qdi_uni(a, b);
    }
  }

