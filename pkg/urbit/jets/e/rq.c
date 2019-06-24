/* j/e/rq.c
**
*/
#include "all.h"
#include <softfloat.h>

#define QUADNAN 0x7fff800000000000

  union quad {
    float128_t* q;
    c3_w* c;
  };

/* functions
*/
  static inline c3_t
  _nan_test(float128_t* a)
  {
    return !f128M_eq(a, a);
  }

  static inline void
  _nan_unify(float128_t* a)
  {
    if ( _nan_test(a) )
    {
      *( (c3_d*)a)    = 0;
      *(((c3_d*)a)+1) = QUADNAN;
    }
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
  u3qeq_add(u3_atom a,
            u3_atom b,
            u3_atom r)
  {
    union quad c, d, e;
    _set_rounding(r);
    c.c = alloca(16);
    d.c = alloca(16);
    e.c = alloca(16);

    u3r_words(0, 4, c.c, a);
    u3r_words(0, 4, d.c, b);
    f128M_add(c.q, d.q, e.q);
    _nan_unify(e.q);

    u3_atom f = u3i_words(4, e.c);
    return f;
  }

  u3_noun
  u3weq_add(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qeq_add(a, b, u3x_at(30, cor));
    }
  }

/* sub
*/
  u3_noun
  u3qeq_sub(u3_atom a,
            u3_atom b,
            u3_atom r)
  {
    union quad c, d, e;
    _set_rounding(r);
    c.c = alloca(16);
    d.c = alloca(16);
    e.c = alloca(16);

    u3r_words(0, 4, c.c, a);
    u3r_words(0, 4, d.c, b);
    f128M_sub(c.q, d.q, e.q);
    _nan_unify(e.q);

    u3_atom f = u3i_words(4, e.c);
    return f;
  }

  u3_noun
  u3weq_sub(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qeq_sub(a, b, u3x_at(30, cor));
    }
  }

/* mul
*/
  u3_noun
  u3qeq_mul(u3_atom a,
            u3_atom b,
            u3_atom r)
  {
    union quad c, d, e;
    _set_rounding(r);
    c.c = alloca(16);
    d.c = alloca(16);
    e.c = alloca(16);

    u3r_words(0, 4, c.c, a);
    u3r_words(0, 4, d.c, b);
    f128M_mul(c.q, d.q, e.q);
    _nan_unify(e.q);

    u3_atom f = u3i_words(4, e.c);
    return f;
  }

  u3_noun
  u3weq_mul(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qeq_mul(a, b, u3x_at(30, cor));
    }
  }

/* div
*/
  u3_noun
  u3qeq_div(u3_atom a,
            u3_atom b,
            u3_atom r)
  {
    union quad c, d, e;
    _set_rounding(r);
    c.c = alloca(16);
    d.c = alloca(16);
    e.c = alloca(16);

    u3r_words(0, 4, c.c, a);
    u3r_words(0, 4, d.c, b);
    f128M_div(c.q, d.q, e.q);
    _nan_unify(e.q);

    u3_atom f = u3i_words(4, e.c);
    return f;
  }

  u3_noun
  u3weq_div(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qeq_div(a, b, u3x_at(30, cor));
    }
  }

/* sqt
*/
  u3_noun
  u3qeq_sqt(u3_atom a,
            u3_atom r)
  {
    union quad c, d;
    _set_rounding(r);
    c.c = alloca(16);
    d.c = alloca(16);

    u3r_words(0, 4, c.c, a);
    f128M_sqrt(c.q, d.q);
    _nan_unify(d.q);

    u3_atom e = u3i_words(4, d.c);
    return e;
  }

  u3_noun
  u3weq_sqt(u3_noun cor)
  {
    u3_noun a;

    if ( c3n == (a = u3r_at(u3x_sam, cor)) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qeq_sqt(a, u3x_at(30, cor));
    }
  }

/* fma
*/
  u3_noun
  u3qeq_fma(u3_atom a,
            u3_atom b,
            u3_atom c,
            u3_atom r)
  {
    union quad d, e, f, g;
    _set_rounding(r);
    d.c = alloca(16);
    e.c = alloca(16);
    f.c = alloca(16);
    g.c = alloca(16);

    u3r_words(0, 4, d.c, a);
    u3r_words(0, 4, e.c, b);
    u3r_words(0, 4, f.c, c);
    f128M_mulAdd(d.q, e.q, f.q, g.q);
    _nan_unify(g.q);

    u3_atom h = u3i_words(4, g.c);
    return h;
  }

  u3_noun
  u3weq_fma(u3_noun cor)
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
      return u3qeq_fma(a, b, c, u3x_at(30, cor));
    }
  }

/* lth
*/
  u3_noun
  u3qeq_lth(u3_atom a,
            u3_atom b)
  {
    union quad c, d;
    c.c = alloca(16);
    d.c = alloca(16);

    u3r_words(0, 4, c.c, a);
    u3r_words(0, 4, d.c, b);
    c3_o e = __(f128M_lt(c.q, d.q));

    return e;
  }

  u3_noun
  u3weq_lth(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qeq_lth(a, b);
    }
  }

/* lte
*/
  u3_noun
  u3qeq_lte(u3_atom a,
            u3_atom b)
  {
    union quad c, d;
    c.c = alloca(16);
    d.c = alloca(16);

    u3r_words(0, 4, c.c, a);
    u3r_words(0, 4, d.c, b);
    c3_o e = __(f128M_le(c.q, d.q));

    return e;
  }

  u3_noun
  u3weq_lte(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qeq_lte(a, b);
    }
  }

/* equ
*/
  u3_noun
  u3qeq_equ(u3_atom a,
            u3_atom b)
  {
    union quad c, d;
    c.c = alloca(16);
    d.c = alloca(16);

    u3r_words(0, 4, c.c, a);
    u3r_words(0, 4, d.c, b);
    c3_o e = __(f128M_eq(c.q, d.q));

    return e;
  }

  u3_noun
  u3weq_equ(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qeq_equ(a, b);
    }
  }

/* gte
*/
  u3_noun
  u3qeq_gte(u3_atom a,
            u3_atom b)
  {
    union quad c, d;
    c.c = alloca(16);
    d.c = alloca(16);

    u3r_words(0, 4, c.c, a);
    u3r_words(0, 4, d.c, b);
    c3_o e = __(f128M_le(d.q, c.q));

    return e;
  }

  u3_noun
  u3weq_gte(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qeq_gte(a, b);
    }
  }

/* gth
*/
  u3_noun
  u3qeq_gth(u3_atom a,
            u3_atom b)
  {
    union quad c, d;
    c.c = alloca(16);
    d.c = alloca(16);

    u3r_words(0, 4, c.c, a);
    u3r_words(0, 4, d.c, b);
    c3_o e = __(f128M_lt(d.q, c.q));

    return e;
  }

  u3_noun
  u3weq_gth(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qeq_gth(a, b);
    }
  }
