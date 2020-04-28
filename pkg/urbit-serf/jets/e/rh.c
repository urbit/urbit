/* j/e/rh.c
**
*/
#include "all.h"
#include <softfloat.h>

#define HALFNAN 0x7e00

  union half {
    float16_t h;
    c3_s c;
  };

/* functions
*/
  static inline c3_t
  _nan_test(float16_t a)
  {
    return !f16_eq(a, a);
  }

  static inline float16_t
  _nan_unify(float16_t a)
  {
    if ( _nan_test(a) )
    {
      *(c3_s*)(&a) = HALFNAN;
    }
    return a;
  }

  static inline void
  _set_rounding(c3_w a)
  {
    switch ( a )
    {
    default:
      u3m_bail(c3__fail);
      break;
    case c3__n:
      softfloat_roundingMode = softfloat_round_near_even;
      break;
    case c3__z:
      softfloat_roundingMode = softfloat_round_minMag;
      break;
    case c3__u:
      softfloat_roundingMode = softfloat_round_max;
      break;
    case c3__d:
      softfloat_roundingMode = softfloat_round_min;
      break;
    }
  }

/* add
*/
  u3_noun
  u3qes_add(u3_atom a,
            u3_atom b,
            u3_atom r)
  {
    union half c, d, e;
    _set_rounding(r);
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);
    e.h = _nan_unify(f16_add(c.h, d.h));

    return e.c;
  }

  u3_noun
  u3wes_add(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qes_add(a, b, u3x_at(30, cor));
    }
  }

/* sub
*/
  u3_noun
  u3qes_sub(u3_atom a,
            u3_atom b,
            u3_atom r)
  {
    union half c, d, e;
    _set_rounding(r);
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);
    e.h = _nan_unify(f16_sub(c.h, d.h));

    return e.c;
  }

  u3_noun
  u3wes_sub(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qes_sub(a, b, u3x_at(30, cor));
    }
  }

/* mul
*/
  u3_noun
  u3qes_mul(u3_atom a,
            u3_atom b,
            u3_atom r)
  {
    union half c, d, e;
    _set_rounding(r);
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);
    e.h = _nan_unify(f16_mul(c.h, d.h));

    return e.c;
  }

  u3_noun
  u3wes_mul(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qes_mul(a, b, u3x_at(30, cor));
    }
  }

/* div
*/
  u3_noun
  u3qes_div(u3_atom a,
            u3_atom b,
            u3_atom r)
  {
    union half c, d, e;
    _set_rounding(r);
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);
    e.h = _nan_unify(f16_div(c.h, d.h));

    return e.c;
  }

  u3_noun
  u3wes_div(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qes_div(a, b, u3x_at(30, cor));
    }
  }

/* sqt
*/
  u3_noun
  u3qes_sqt(u3_atom a,
            u3_atom r)
  {
    union half c, d;
    _set_rounding(r);
    c.c = u3r_word(0, a);
    d.h = _nan_unify(f16_sqrt(c.h));

    return d.c;
  }

  u3_noun
  u3wes_sqt(u3_noun cor)
  {
    u3_noun a;

    if ( c3n == (a = u3r_at(u3x_sam, cor)) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qes_sqt(a, u3x_at(30, cor));
    }
  }

/* fma
*/
  u3_noun
  u3qes_fma(u3_atom a,
            u3_atom b,
            u3_atom c,
            u3_atom r)
  {
    union half d, e, f, g;
    _set_rounding(r);
    d.c = u3r_word(0, a);
    e.c = u3r_word(0, b);
    f.c = u3r_word(0, c);
    g.h = _nan_unify(f16_mulAdd(d.h, e.h, f.h));

    return g.c;
  }

  u3_noun
  u3wes_fma(u3_noun cor)
  {
    u3_noun a, b, c;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_6, &b, u3x_sam_7, &c, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) ||
         c3n == u3ud(c) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qes_fma(a, b, c, u3x_at(30, cor));
    }
  }

/* lth
*/
  u3_noun
  u3qes_lth(u3_atom a,
            u3_atom b)
  {
    union half c, d;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);

    return __(f16_lt(c.h, d.h));
  }

  u3_noun
  u3wes_lth(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qes_lth(a, b);
    }
  }

/* lte
*/
  u3_noun
  u3qes_lte(u3_atom a,
            u3_atom b)
  {
    union half c, d;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);

    return __(f16_le(c.h, d.h));
  }

  u3_noun
  u3wes_lte(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qes_lte(a, b);
    }
  }

/* equ
*/
  u3_noun
  u3qes_equ(u3_atom a,
            u3_atom b)
  {
    union half c, d;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);

    return __(f16_eq(c.h, d.h));
  }

  u3_noun
  u3wes_equ(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qes_equ(a, b);
    }
  }

/* gte
*/
  u3_noun
  u3qes_gte(u3_atom a,
            u3_atom b)
  {
    union half c, d;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);

    return __(f16_le(d.h, c.h));
  }

  u3_noun
  u3wes_gte(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qes_gte(a, b);
    }
  }

/* gth
*/
  u3_noun
  u3qes_gth(u3_atom a,
            u3_atom b)
  {
    union half c, d;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);

    return __(f16_lt(d.h, c.h));
  }

  u3_noun
  u3wes_gth(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qes_gth(a, b);
    }
  }
