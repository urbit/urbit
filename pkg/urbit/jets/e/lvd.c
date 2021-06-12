/* jets/e/lvd.c
**
::
::::  Vector type in double-precision floating-point @rd
  ::
::  Conventions:
::
::  m,n,p   always dimensions
::  i,j,k   always indices (also ii,jj,kk for coder-spec not user-spec)
::  a,b,c   always lists
::  u,v,w   always vector/matrix atoms
::  s,t     always real/floats
::  `1` suffix indicates 1-indexed value (default 0-indexed as conventional C)
::  `_` suffix indicates a C array unpacking of an @lv atom
*/
#include "all.h"
#include <softfloat.h>
#include <stdio.h>
/*#include <atlas.h>*/

#define DOUBNAN 0x7ff8000000000000

  union trip {
    float64_t s;    //struct containing v, uint_64
    c3_d c;         //uint_64
    float b;        //float_64, compiler-native, useful for debugging printfs
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
  _set_rounding(c3_d a)
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

    uint32_t i;
    union trip c, d;
    _set_rounding(r);
    w_[n_u] = 1;
    for ( i = 0; i < n_u; i++ ) {
      c.c = u3r_chub(i, u);
      d.c = u3r_chub(i, v);

      w_[i] = (c3_d)_nan_unify(f64_add(c.s,d.s)).v;
    }
    u3_noun w = u3i_chubs(n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welvd_addv(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a,
                              u3x_sam_3, &b, 0) ||
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

    uint32_t i;
    union trip c, d;
    _set_rounding(r);
    w_[n_u] = 1;
    for ( i = 0; i < n_u; i++ ) {
      c.c = u3r_chub(i, u);
      d.c = u3r_chub(i, v);

      w_[i] = (c3_d)_nan_unify(f64_sub(c.s,d.s)).v;
    }
    u3_noun w = u3i_chubs(n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welvd_subv(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a,
                              u3x_sam_3, &b, 0) ||
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

    uint32_t i;
    union trip c, d;
    _set_rounding(r);
    w_[n_u] = 1;
    for ( i = 0; i < n_u; i++ ) {
      c.c = u3r_chub(i, u);
      d.c = u3r_chub(i, v);

      w_[i] = (c3_d)_nan_unify(f64_mul(c.s,d.s)).v;
    }
    u3_noun w = u3i_chubs(n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welvd_mulv(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a,
                              u3x_sam_3, &b, 0) ||
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

    uint32_t i;
    union trip c, d;
    _set_rounding(r);
    w_[n_u] = 1;
    for ( i = 0; i < n_u; i++ ) {
      c.c = u3r_chub(i, u);
      d.c = u3r_chub(i, v);

      w_[i] = (c3_d)_nan_unify(f64_div(c.s,d.s)).v;
    }
    u3_noun w = u3i_chubs(n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welvd_divv(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a,
                              u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelvd_divv(a, b, u3x_at(30, cor));
    }
  }

/* inner product of lvd + lvd
*/
  u3_noun
  u3qelvd_inner(u3_atom u,  /* @lvd */
                u3_atom v,  /* @lvd */
                u3_atom r)
  {
    c3_d n_u = u3r_met(3,u)/8;  // n_u is the vector length
    c3_d n_v = u3r_met(3,v)/8;
    if (n_u != n_v) {
      return u3m_bail(c3__exit);
    }

    uint32_t i;
    union trip c, d, e, t;
    _set_rounding(r);
    t.c = 0;
    for ( i = 0; i < n_u; i++ ) {
      c.c = u3r_chub(i, u);
      d.c = u3r_chub(i, v);

      e.s = _nan_unify(f64_mul(c.s,d.s));
      t.s = _nan_unify(f64_add(t.s,e.s));
    }

    return u3i_chubs(1, &t.c);
  }

  u3_noun
  u3welvd_inner(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a,
                              u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelvd_inner(a, b, u3x_at(30, cor));
    }
  }
