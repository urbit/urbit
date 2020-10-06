/* jets/e/lvd.c
**
*/
#include "all.h"
#include <softfloat.h>
/*#include <stdio.h>*/
/*#include <atlas.h>*/

#define DOUBNAN 0x7ff8000000000000

  union trip {
    float64_t d;    //struct containing v, uint_64
    c3_d c;         //uint_64
    double b;       //float_64
  };

/* functions
*/
  static inline c3_t
  _nan_test(float64_t a)
  {
    return !f64_eq(a, a);
  }

  static inline float64_t
  _nan_unify(float64_t a)
  {
    if ( _nan_test(a) )
    {
      *(c3_d*)(&a) = DOUBNAN;
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

/* zeros in @lvd
*/
  u3_noun
  u3qelvd_zeros(u3_atom u)  /* @ud */
  {
    c3_d n_u = u3r_chub(0,u);
    c3_d* w_  = (c3_d*)u3a_malloc((n_u+1)*sizeof(float64_t));

    uint i;
    union trip c;
    c.c = 0x0000000000000000;
    w_[n_u] = n_u;
    for ( i = 0; i < n_u; i++ ) {
      w_[i] = (c3_d)c.d.v;
    }
    u3_noun w = u3i_chubs(n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welvd_zeros(u3_noun cor)
  {
    u3_noun a;

    if ( c3n == (a = u3r_at(u3x_sam, cor)) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelvd_zeros(a);
    }
  }

/* ones in @lvd
*/
  u3_noun
  u3qelvd_ones(u3_atom u)  /* @ud */
  {
    c3_d n_u = u3r_chub(0,u);
    c3_d* w_  = (c3_d*)u3a_malloc((n_u+1)*sizeof(float64_t));

    uint i;
    union trip c;
    c.c = 0x3FF0000000000000;
    w_[n_u] = n_u;
    for ( i = 0; i < n_u; i++ ) {
      w_[i] = (c3_d)c.d.v;
    }
    u3_noun w = u3i_chubs(n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welvd_ones(u3_noun cor)
  {
    u3_noun a;

    if ( c3n == (a = u3r_at(u3x_sam, cor)) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelvd_ones(a);
    }
  }

/* getter from @lvd, 1-indexed
*/
  u3_noun
  u3qelvd_get(u3_atom u,  /* @lvd */
              u3_atom i)  /* @ud */
  {
    c3_d n_u = u3r_met(3,u)/8;  // n_u is the vector length
    c3_d i_u = u3r_chub(0, i);
    if ((i_u < 1) || (i_u > n_u))
    {
      return u3m_bail(c3__exit);
    }

    c3_d* w_  = (c3_d*)u3a_malloc(1*sizeof(float64_t));
    w_[0] = u3r_chub(i_u-1, u);
    u3_atom w = u3i_chubs(1, w_);
    return w;
  }

  u3_noun
  u3welvd_get(u3_noun cor)
  {
    u3_noun u, i;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_3, &i, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelvd_get(u, i);
    }
  }

/* setter for @lvd, 1-indexed
*/
  u3_noun
  u3qelvd_set(u3_atom u,  /* @lvd */
              u3_atom i,  /* @ud */
              u3_atom a)  /* @rd */
  {
    c3_d n_u = u3r_met(3,u)/8;  // n_u is the vector length
    c3_d i_u = u3r_chub(0, i);
    if ((i_u < 1) || (i_u > n_u))
    {
      return u3m_bail(c3__exit);
    }

    c3_d* w_  = (c3_d*)u3a_malloc((n_u+1)*sizeof(float64_t));
    w_[n_u] = n_u;
    for ( i = 0; i < n_u; i++ ) {
      if ( i == i_u )
      {
        w_[i] = u3r_chub(0, a);
      }
      else
      {
        w_[i] = u3r_chub(i, u);
      }
    }
    u3_noun w = u3i_chubs(n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welvd_set(u3_noun cor)
  {
    u3_noun u, i, a;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_6, &i, u3x_sam_7, &a, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelvd_set(u, i, a);
    }
  }

/* add lvd + scalar
*/
  u3_noun
  u3qelvd_adds(u3_atom u,  /* @lvd */
               u3_atom a,  /* @rd */
               u3_atom r)
  {
    c3_d n_u = u3r_met(3,u)/8;  // n_u is the vector length
    c3_d* w_  = (c3_d*)u3a_malloc((n_u+1)*sizeof(float64_t));

    uint i;
    union trip c, d;
    _set_rounding(r);
    d.c = u3r_chub(0, a);
    w_[n_u] = n_u;
    for ( i = 0; i < n_u; i++ ) {
      c.c = u3r_chub(i, u);
      w_[i] = (c3_d)_nan_unify(f64_add(c.d,d.d)).v;
    }
    u3_noun w = u3i_chubs(n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welvd_adds(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelvd_adds(a, b, u3x_at(30, cor));
    }
  }

/* subtract lvd + scalar
*/
  u3_noun
  u3qelvd_subs(u3_atom u,  /* @lvd */
               u3_atom a,  /* @rd */
               u3_atom r)
  {
    c3_d n_u = u3r_met(3,u)/8;  // n_u is the vector length
    c3_d* w_  = (c3_d*)u3a_malloc((n_u+1)*sizeof(float64_t));

    uint i;
    union trip c, d;
    _set_rounding(r);
    d.c = u3r_chub(0, a);
    w_[n_u] = n_u;
    for ( i = 0; i < n_u; i++ ) {
      c.c = u3r_chub(i, u);
      w_[i] = (c3_d)_nan_unify(f64_sub(c.d,d.d)).v;
    }
    u3_noun w = u3i_chubs(n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welvd_subs(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelvd_subs(a, b, u3x_at(30, cor));
    }
  }

/* multiply lvd x scalar
*/
  u3_noun
  u3qelvd_muls(u3_atom u,  /* @lvd */
               u3_atom a,  /* @rd */
               u3_atom r)
  {
    c3_d n_u = u3r_met(3,u)/8;  // n_u is the vector length
    c3_d* w_  = (c3_d*)u3a_malloc((n_u+1)*sizeof(float64_t));

    uint i;
    union trip c, d;
    _set_rounding(r);
    d.c = u3r_chub(0, a);
    w_[n_u] = n_u;
    for ( i = 0; i < n_u; i++ ) {
      c.c = u3r_chub(i, u);
      w_[i] = (c3_d)_nan_unify(f64_mul(c.d,d.d)).v;
    }
    u3_noun w = u3i_chubs(n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welvd_muls(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelvd_muls(a, b, u3x_at(30, cor));
    }
  }


/* divide lvd / scalar
*/
  u3_noun
  u3qelvd_divs(u3_atom u,  /* @lvd */
               u3_atom a,  /* @rd */
               u3_atom r)
  {
    c3_d n_u = u3r_met(3,u)/8;  // n_u is the vector length
    c3_d* w_  = (c3_d*)u3a_malloc((n_u+1)*sizeof(float64_t));

    uint i;
    union trip c, d;
    _set_rounding(r);
    d.c = u3r_chub(0, a);
    w_[n_u] = n_u;
    for ( i = 0; i < n_u; i++ ) {
      c.c = u3r_chub(i, u);
      w_[i] = (c3_d)_nan_unify(f64_div(c.d,d.d)).v;
    }
    u3_noun w = u3i_chubs(n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welvd_divs(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelvd_divs(a, b, u3x_at(30, cor));
    }
  }

/* add lvd + lvd
*/
  u3_noun
  u3qelvd_addv(u3_atom u,  /* @lvd */
               u3_atom v,  /* @lvd */
               u3_atom r)
  {
    c3_d n_u = u3r_met(3,u)/8;  // n_u is the vector length
    c3_d n_v = u3r_met(3,v)/8;
    if (n_u != n_v) {
      return u3m_bail(c3__exit);
    }
    c3_d* w_ = (c3_d*)u3a_malloc((n_u+1)*sizeof(float64_t));

    uint i;
    union trip c, d;
    _set_rounding(r);
    w_[n_u] = n_u;
    for ( i = 0; i < n_u; i++ ) {
      c.c = u3r_chub(i, u);
      d.c = u3r_chub(i, v);

      w_[i] = (c3_d)_nan_unify(f64_add(c.d,d.d)).v;
    }
    u3_noun w = u3i_chubs(n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welvd_addv(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelvd_addv(a, b, u3x_at(30, cor));
    }
  }

  /* subtract lvd - lvd
  */
    u3_noun
    u3qelvd_subv(u3_atom u,  /* @lvd */
                 u3_atom v,  /* @lvd */
                 u3_atom r)
    {
      c3_d n_u = u3r_met(3,u)/8;  // n_u is the vector length
      c3_d n_v = u3r_met(3,v)/8;
      if (n_u != n_v) {
        return u3m_bail(c3__exit);
      }
      c3_d* w_ = (c3_d*)u3a_malloc((n_u+1)*sizeof(float64_t));

      uint i;
      union trip c, d;
      _set_rounding(r);
      w_[n_u] = n_u;
      for ( i = 0; i < n_u; i++ ) {
        c.c = u3r_chub(i, u);
        d.c = u3r_chub(i, v);

        w_[i] = (c3_d)_nan_unify(f64_sub(c.d,d.d)).v;
      }
      u3_noun w = u3i_chubs(n_u+1, w_);
      u3a_free(w_);

      return w;
    }

    u3_noun
    u3welvd_subv(u3_noun cor)
    {
      u3_noun a, b;

      if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
           c3n == u3ud(a) ||
           c3n == u3ud(b) )
      {
        return u3m_bail(c3__exit);
      }
      else {
        return u3qelvd_subv(a, b, u3x_at(30, cor));
      }
    }

  /* multiply lvd x lvd
  */
    u3_noun
    u3qelvd_mulv(u3_atom u,  /* @lvd */
                 u3_atom v,  /* @lvd */
                 u3_atom r)
    {
      c3_d n_u = u3r_met(3,u)/8;  // n_u is the vector length
      c3_d n_v = u3r_met(3,v)/8;
      if (n_u != n_v) {
        return u3m_bail(c3__exit);
      }
      c3_d* w_ = (c3_d*)u3a_malloc((n_u+1)*sizeof(float64_t));

      uint i;
      union trip c, d;
      _set_rounding(r);
      w_[n_u] = n_u;
      for ( i = 0; i < n_u; i++ ) {
        c.c = u3r_chub(i, u);
        d.c = u3r_chub(i, v);

        w_[i] = (c3_d)_nan_unify(f64_mul(c.d,d.d)).v;
      }
      u3_noun w = u3i_chubs(n_u+1, w_);
      u3a_free(w_);

      return w;
    }

    u3_noun
    u3welvd_mulv(u3_noun cor)
    {
      u3_noun a, b;

      if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
           c3n == u3ud(a) ||
           c3n == u3ud(b) )
      {
        return u3m_bail(c3__exit);
      }
      else {
        return u3qelvd_mulv(a, b, u3x_at(30, cor));
      }
    }

  /* divide lvd by lvd
  */
    u3_noun
    u3qelvd_divv(u3_atom u,  /* @lvd */
                 u3_atom v,  /* @lvd */
                 u3_atom r)
    {
      c3_d n_u = u3r_met(3,u)/8;  // n_u is the vector length
      c3_d n_v = u3r_met(3,v)/8;
      if (n_u != n_v) {
        return u3m_bail(c3__exit);
      }
      c3_d* w_ = (c3_d*)u3a_malloc((n_u+1)*sizeof(float64_t));

      uint i;
      union trip c, d;
      _set_rounding(r);
      w_[n_u] = n_u;
      for ( i = 0; i < n_u; i++ ) {
        c.c = u3r_chub(i, u);
        d.c = u3r_chub(i, v);

        w_[i] = (c3_d)_nan_unify(f64_div(c.d,d.d)).v;
      }
      u3_noun w = u3i_chubs(n_u+1, w_);
      u3a_free(w_);

      return w;
    }

    u3_noun
    u3welvd_divv(u3_noun cor)
    {
      u3_noun a, b;

      if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
           c3n == u3ud(a) ||
           c3n == u3ud(b) )
      {
        return u3m_bail(c3__exit);
      }
      else {
        return u3qelvd_divv(a, b, u3x_at(30, cor));
      }
    }
