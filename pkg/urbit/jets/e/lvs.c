/* jets/e/lvs.c
**
::
::::  Vector type in single-precision floating-point @rs
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

#define SINGNAN 0x7fc00000

  union trip {
    float32_t s;    //struct containing v, uint_32
    c3_w c;         //uint_32
    float b;        //float_32, compiler-native, useful for debugging printfs
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

/* add lvs + lvs
*/
  u3_noun
  u3qelvs_addv(u3_atom u,  /* @lvs */
               u3_atom v,  /* @lvs */
               u3_atom r)
  {
    c3_w n_u = u3r_met(3,u)/4;  // n_u is the vector length
    c3_w n_v = u3r_met(3,v)/4;
    if (n_u != n_v) {
      return u3m_bail(c3__exit);
    }
    c3_w* w_ = (c3_w*)u3a_malloc((n_u+1)*sizeof(float32_t));

    uint32_t i;
    union trip c, d;
    _set_rounding(r);
    w_[n_u] = 1;
    for ( i = 0; i < n_u; i++ ) {
      c.c = u3r_word(i, u);
      d.c = u3r_word(i, v);

      w_[i] = (c3_w)_nan_unify(f32_add(c.s,d.s)).v;
    }
    u3_noun w = u3i_words(n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welvs_addv(u3_noun cor)
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
      return u3qelvs_addv(a, b, u3x_at(30, cor));
    }
  }

/* subtract lvs - lvs
*/
  u3_noun
  u3qelvs_subv(u3_atom u,  /* @lvs */
               u3_atom v,  /* @lvs */
               u3_atom r)
  {
    c3_w n_u = u3r_met(3,u)/4;  // n_u is the vector length
    c3_w n_v = u3r_met(3,v)/4;
    if (n_u != n_v) {
      return u3m_bail(c3__exit);
    }
    c3_w* w_ = (c3_w*)u3a_malloc((n_u+1)*sizeof(float32_t));

    uint32_t i;
    union trip c, d;
    _set_rounding(r);
    w_[n_u] = 1;
    for ( i = 0; i < n_u; i++ ) {
      c.c = u3r_word(i, u);
      d.c = u3r_word(i, v);

      w_[i] = (c3_w)_nan_unify(f32_sub(c.s,d.s)).v;
    }
    u3_noun w = u3i_words(n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welvs_subv(u3_noun cor)
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
      return u3qelvs_subv(a, b, u3x_at(30, cor));
    }
  }

/* multiply lvs x lvs
*/
  u3_noun
  u3qelvs_mulv(u3_atom u,  /* @lvs */
               u3_atom v,  /* @lvs */
               u3_atom r)
  {
    c3_w n_u = u3r_met(3,u)/4;  // n_u is the vector length
    c3_w n_v = u3r_met(3,v)/4;
    if (n_u != n_v) {
      return u3m_bail(c3__exit);
    }
    c3_w* w_ = (c3_w*)u3a_malloc((n_u+1)*sizeof(float32_t));

    uint32_t i;
    union trip c, d;
    _set_rounding(r);
    w_[n_u] = 1;
    for ( i = 0; i < n_u; i++ ) {
      c.c = u3r_word(i, u);
      d.c = u3r_word(i, v);

      w_[i] = (c3_w)_nan_unify(f32_mul(c.s,d.s)).v;
    }
    u3_noun w = u3i_words(n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welvs_mulv(u3_noun cor)
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
      return u3qelvs_mulv(a, b, u3x_at(30, cor));
    }
  }

/* divide lvs by lvs
*/
  u3_noun
  u3qelvs_divv(u3_atom u,  /* @lvs */
               u3_atom v,  /* @lvs */
               u3_atom r)
  {
    c3_w n_u = u3r_met(3,u)/4;  // n_u is the vector length
    c3_w n_v = u3r_met(3,v)/4;
    if (n_u != n_v) {
      return u3m_bail(c3__exit);
    }
    c3_w* w_ = (c3_w*)u3a_malloc((n_u+1)*sizeof(float32_t));

    uint32_t i;
    union trip c, d;
    _set_rounding(r);
    w_[n_u] = 1;
    for ( i = 0; i < n_u; i++ ) {
      c.c = u3r_word(i, u);
      d.c = u3r_word(i, v);

      w_[i] = (c3_w)_nan_unify(f32_div(c.s,d.s)).v;
    }
    u3_noun w = u3i_words(n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welvs_divv(u3_noun cor)
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
      return u3qelvs_divv(a, b, u3x_at(30, cor));
    }
  }

/* inner product of lvs + lvs
*/
  u3_noun
  u3qelvs_inner(u3_atom u,  /* @lvs */
                u3_atom v,  /* @lvs */
                u3_atom r)
  {
    c3_w n_u = u3r_met(3,u)/4;  // n_u is the vector length
    c3_w n_v = u3r_met(3,v)/4;
    if (n_u != n_v) {
      return u3m_bail(c3__exit);
    }

    uint32_t i;
    union trip c, d, e, t;
    _set_rounding(r);
    t.c = 0;
    for ( i = 0; i < n_u; i++ ) {
      c.c = u3r_word(i, u);
      d.c = u3r_word(i, v);

      e.s = _nan_unify(f32_mul(c.s,d.s));
      t.s = _nan_unify(f32_add(t.s,e.s));
    }

    return u3i_words(1, &t.c);
  }

  u3_noun
  u3welvs_inner(u3_noun cor)
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
      return u3qelvs_inner(a, b, u3x_at(30, cor));
    }
  }
