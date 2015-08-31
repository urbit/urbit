/* j/e/rs.c
**
*/
#include "all.h"
#include "softfloat.h"

#define SINGNAN 0x7fc00000

  union sing {
    float32_t s;
    c3_w c;
  };

/* functions
*/
  static inline c3_t
  _nan_test(float32_t a)
  {
    return !f32_eq(a, a);
  }

  static inline float32_t
  _nan_unify(float32_t a)
  {
    if ( _nan_test(a) )
    {
      *(c3_w*)(&a) = SINGNAN;
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
  u3qet_add(u3_atom a,
            u3_atom b,
            u3_atom r)
  {
    union sing c, d, e;
    _set_rounding(r);
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);
    e.s = _nan_unify(f32_add(c.s, d.s));

    return u3i_words(1, &e.c);
  }

  u3_noun
  u3wet_add(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qet_add(a, b, u3x_at(30, cor));
    }
  }

/* sub
*/
  u3_noun
  u3qet_sub(u3_atom a,
            u3_atom b,
            u3_atom r)
  {
    union sing c, d, e;
    _set_rounding(r);
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);
    e.s = _nan_unify(f32_sub(c.s, d.s));

    return u3i_words(1, &e.c);
  }

  u3_noun
  u3wet_sub(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qet_sub(a, b, u3x_at(30, cor));
    }
  }

/* mul
*/
  u3_noun
  u3qet_mul(u3_atom a,
            u3_atom b,
            u3_atom r)
  {
    union sing c, d, e;
    _set_rounding(r);
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);
    e.s = _nan_unify(f32_mul(c.s, d.s));

    return u3i_words(1, &e.c);
  }

  u3_noun
  u3wet_mul(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qet_mul(a, b, u3x_at(30, cor));
    }
  }

/* div
*/
  u3_noun
  u3qet_div(u3_atom a, 
            u3_atom b, 
            u3_atom r)
  {
    union sing c, d, e;
    _set_rounding(r);
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);
    e.s = _nan_unify(f32_div(c.s, d.s));

    return u3i_words(1, &e.c);
  }

  u3_noun
  u3wet_div(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qet_div(a, b, u3x_at(30, cor));
    }
  }

/* sqt
*/
  u3_noun
  u3qet_sqt(u3_atom a, 
            u3_atom r)
  {
    union sing c, d;
    _set_rounding(r);
    c.c = u3r_word(0, a);
    d.s = _nan_unify(f32_sqrt(c.s));

    return u3i_words(1, &d.c);
  }

  u3_noun
  u3wet_sqt(u3_noun cor)
  {
    u3_noun a;

    if ( c3n == (a = u3r_at(u3x_sam, cor)) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qet_sqt(a, u3x_at(30, cor));
    }
  }

/* fma
*/
  u3_noun
  u3qet_fma(u3_atom a,
            u3_atom b,
            u3_atom c,
            u3_atom r)
  {
    union sing d, e, f, g;
    _set_rounding(r);
    d.c = u3r_word(0, a);
    e.c = u3r_word(0, b);
    f.c = u3r_word(0, c);
    g.s = _nan_unify(f32_mulAdd(d.s, e.s, f.s));

    return u3i_words(1, &g.c);
  }

  u3_noun
  u3wet_fma(u3_noun cor)
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
      return u3qet_fma(a, b, c, u3x_at(30, cor));
    }
  }

/* lth
*/
  u3_noun
  u3qet_lth(u3_atom a,
            u3_atom b)
  {
    union sing c, d;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);

    return __(f32_lt(c.s, d.s));
  }

  u3_noun
  u3wet_lth(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qet_lth(a, b);
    }
  }

/* lte
*/
  u3_noun
  u3qet_lte(u3_atom a,
            u3_atom b)
  {
    union sing c, d;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);

    return __(f32_le(c.s, d.s));
  }

  u3_noun
  u3wet_lte(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qet_lte(a, b);
    }
  }

/* equ
*/
  u3_noun
  u3qet_equ(u3_atom a,
            u3_atom b)
  {
    union sing c, d;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);

    return __(f32_eq(c.s, d.s));
  }

  u3_noun
  u3wet_equ(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qet_equ(a, b);
    }
  }

/* gte
*/
  u3_noun
  u3qet_gte(u3_atom a,
            u3_atom b)
  {
    union sing c, d;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);

    return __(f32_le(d.s, c.s));
  }

  u3_noun
  u3wet_gte(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qet_gte(a, b);
    }
  }

/* gth
*/
  u3_noun
  u3qet_gth(u3_atom a,
            u3_atom b)
  {
    union sing c, d;
    c.c = u3r_word(0, a);
    d.c = u3r_word(0, b);

    return __(f32_lt(d.s, c.s));
  }

  u3_noun
  u3wet_gth(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qet_gth(a, b);
    }
  }
