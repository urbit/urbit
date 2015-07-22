/* j/e/rd.c
**
*/
#include "all.h"
#include <math.h>

#define SINGNAN 0x7fc00000

union sing {
  float d;
  c3_w c;
};

/* functions
*/
  static inline c3_t
  _nan_test(float a)
  {
    return !(a == a);
  }

  static inline float
  _nan_unify(float a)
  {
    if (_nan_test(a))
    {
      *(c3_w*)(&a) = SINGNAN;
    }
    return a;
  }

/* add
*/
  u3_noun
  u3qef_add(u3_atom a, u3_atom b)
  {
    union sing c, d, e;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);
    e.d = c.d + d.d;
    e.d = _nan_unify(e.d);

    return u3i_words(1, &e.c);
  }

  u3_noun
  u3wef_add(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qef_add(a, b);
    }
  }

/* sub
*/
  u3_noun
  u3qef_sub(u3_atom a, u3_atom b)
  {
    union sing c, d, e;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);
    e.d = c.d - d.d;
    e.d = _nan_unify(e.d);

    return u3i_words(1, &e.c);
  }

  u3_noun
  u3wef_sub(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qef_sub(a, b);
    }
  }

/* mul
*/
  u3_noun
  u3qef_mul(u3_atom a, u3_atom b)
  {
    union sing c, d, e;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);
    e.d = c.d * d.d;
    e.d = _nan_unify(e.d);

    return u3i_words(1, &e.c);
  }

  u3_noun
  u3wef_mul(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qef_mul(a, b);
    }
  }

/* div
*/
  u3_noun
  u3qef_div(u3_atom a, u3_atom b)
  {
    union sing c, d, e;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);
    e.d = c.d / d.d;
    e.d = _nan_unify(e.d);

    return u3i_words(1, &e.c);
  }

  u3_noun
  u3wef_div(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qef_div(a, b);
    }
  }

/* sqt
*/
  u3_noun
  u3qef_sqt(u3_atom a)
  {
    union sing b, c;
    b.c = u3r_word(0, a);
    c.d = sqrt(b.d);
    c.d = _nan_unify(c.d);

    return u3i_words(1, &c.c);
  }

  u3_noun
  u3wef_sqt(u3_noun cor)
  {
    u3_noun a;

    if ( c3n == (a = u3r_at(u3x_sam, cor)) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qef_sqt(a);
    }
  }

/* lth
*/
  u3_noun
  u3qef_lth(u3_atom a, u3_atom b)
  {
    union sing c, d;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);

    if (_nan_test(c.d) || _nan_test(d.d))
    {
      return u3_nul;
    }
    else {
      return u3nc(u3_nul, __(c.d < d.d));
    }
  }

  u3_noun
  u3wef_lth(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qef_lth(a, b);
    }
  }

/* lte
*/
  u3_noun
  u3qef_lte(u3_atom a, u3_atom b)
  {
    union sing c, d;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);

    if (_nan_test(c.d) || _nan_test(d.d))
    {
      return u3_nul;
    }
    else {
      return u3nc(u3_nul, __(c.d <= d.d));
    }
  }

  u3_noun
  u3wef_lte(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qef_lte(a, b);
    }
  }

/* equ
*/
  u3_noun
  u3qef_equ(u3_atom a, u3_atom b)
  {
    union sing c, d;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);

    if (_nan_test(c.d) || _nan_test(d.d))
    {
      return u3_nul;
    }
    else {
      return u3nc(u3_nul, __(c.d == d.d));
    }
  }

  u3_noun
  u3wef_equ(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qef_equ(a, b);
    }
  }

/* gte
*/
  u3_noun
  u3qef_gte(u3_atom a, u3_atom b)
  {
    union sing c, d;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);

    if (_nan_test(c.d) || _nan_test(d.d))
    {
      return u3_nul;
    }
    else {
      return u3nc(u3_nul, __(c.d >= d.d));
    }
  }

  u3_noun
  u3wef_gte(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qef_gte(a, b);
    }
  }

/* gth
*/
  u3_noun
  u3qef_gth(u3_atom a, u3_atom b)
  {
    union sing c, d;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);

    if (_nan_test(c.d) || _nan_test(d.d))
    {
      return u3_nul;
    }
    else {
      return u3nc(u3_nul, __(c.d > d.d));
    }
  }

  u3_noun
  u3wef_gth(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qef_gth(a, b);
    }
  }
