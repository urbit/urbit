/* jets/e/lms.c
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
::  `_` suffix indicates a C array unpacking of an @lv atom
*/
#include "all.h"
#include <softfloat.h>
#include <stdio.h>
#include <math.h>
/*#include <atlas.h>*/

#define max(a,b) \
  ({ __typeof__ (a) _a = (a); \
      __typeof__ (b) _b = (b); \
    _a > _b ? _a : _b; })
#define min(a,b) \
  ({ __typeof__ (a) _a = (a); \
      __typeof__ (b) _b = (b); \
    _a < _b ? _a : _b; })

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

  struct
  dims
  {
    c3_w rows, cols;
  };

  struct dims
  shape(u3_atom p)  /* @ud */
  {
    c3_w pp;
    u3r_word(pp, p);
    c3_w mn = u3r_met(3, p)/4 - 1;  // total elements
    c3_w d = u3r_word(mn+1, p);     // dimensionality, for now presumptively 2
    c3_w m = u3r_word(mn, p);       // number of rows

    struct dims shp;
    shp.rows = m;
    shp.cols = (c3_w)(mn / m);              // number of columns

    return shp;
  }

  void
  print_matrix(c3_w* u,
               uint32_t m_u,
               uint32_t n_u)
  {
    c3_w ii, jj;
    union trip a;
    fprintf(stderr, "[%u x %u] matrix:\n\r", m_u, n_u);
    for ( ii = 1; ii <= m_u; ii++ )
    {
      fprintf(stderr, "[ ");
      for ( jj = 1; jj <= n_u; jj++ )
      {
        a.c = u[(ii-1)*n_u+jj-1];
        fprintf(stderr, "%f ", a.b);
      }
      fprintf(stderr, " ]\n\r");
    }
  }

/* zeros in @lms
*/
  u3_noun
  u3qelms_zeros(u3_atom m,  /* @ud */
                u3_atom n)  /* @ud */
  {
    c3_w m_u = u3r_word(0, m);
    c3_w n_u = u3r_word(0, n);
    c3_w* w_  = (c3_w*)u3a_malloc((m_u*n_u+2)*sizeof(float32_t));

    uint32_t i;
    union trip c;
    c.c = 0x00000000;
    w_[m_u*n_u+1] = 2;
    w_[m_u*n_u] = m_u;
    for ( i = 1; i <= m_u*n_u; i++ ) {
      w_[i-1] = (c3_w)c.s.v;
    }
    u3_noun w = u3i_words(m_u*n_u+2, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_zeros(u3_noun cor)
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
      u3_noun c = u3qelms_zeros(a, b);
      return c;
    }
  }

/* ones in @lms
*/
  u3_noun
  u3qelms_ones(u3_atom m,  /* @ud */
               u3_atom n)  /* @ud */
  {
    c3_w m_u = u3r_word(0, m);
    c3_w n_u = u3r_word(0, n);

    c3_w* w_  = (c3_w*)u3a_malloc((m_u*n_u+2)*sizeof(float32_t));

    uint32_t i;
    union trip c;
    c.c = 0x3F800000;
    w_[m_u*n_u+1] = 2;
    w_[m_u*n_u] = m_u;
    for ( i = 1; i <= m_u*n_u; i++ ) {
      w_[i-1] = (c3_w)c.s.v;
    }
    u3_noun w = u3i_words(m_u*n_u+2, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_ones(u3_noun cor)
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
      return u3qelms_ones(a, b);
    }
  }

/* identity in @lms
*/
  u3_noun
  u3qelms_id(u3_atom m)  /* @ud */
  {
    c3_w m_u = u3r_word(0, m);

    u3_noun w = u3qelms_zeros(m, m);

    c3_w* w_  = (c3_w*)u3a_malloc((m_u*m_u+2)*sizeof(float32_t));
    u3r_words(0, m_u*m_u+2, w_, w);

    uint32_t i;
    union trip c;
    c.c = 0x3F800000;
    for ( i = 1; i <= m_u; i++ ) {
      w_[(i-1)*m_u+(i-1)] = (c3_w)c.s.v;
    }
    w = u3i_words(m_u*m_u+2, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_id(u3_noun cor)
  {
    u3_noun a;

    if ( c3n == u3r_mean(cor, u3x_sam, &a, 0) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_id(a);
    }
  }

  /* getter from @lms, 1-indexed
  */
    u3_noun
    u3qelms_get(u3_atom u,  /* @lms */
                u3_atom i,  /* @ud */
                u3_atom j)  /* @ud */
    {
      c3_w i_ = u3r_word(0, i);   // 1-indexed
      c3_w j_ = u3r_word(0, j);   // 1-indexed

      struct dims mn = shape(u);
      c3_w m_u = mn.rows;
      c3_w n_u = mn.cols;

      if ((i_ < 1) || (i_ > m_u) || (j_ < 1) || (j_ > n_u))
      {
        return u3m_bail(c3__exit);
      }

      c3_w offset = (i_-1)*n_u+j_-1;
      c3_w w_ = u3r_word(offset, u);
      u3_atom w = u3i_word(w_);

      return w;
    }

    u3_noun
    u3welms_get(u3_noun cor)
    {
      u3_noun a, i, j;

      if ( c3n == u3r_mean(cor, u3x_sam_2, &a,
                                u3x_sam_6, &i,
                                u3x_sam_7, &j, 0) ||
           c3n == u3ud(a) ||
           c3n == u3ud(i) ||
           c3n == u3ud(j) )
      {
        return u3m_bail(c3__exit);
      }
      else {
        return u3qelms_get(a, i, j);
      }
    }

  /* setter for @lms, 1-indexed
  */
    u3_noun
    u3qelms_set(u3_atom u,  /* @lms */
                u3_atom i,  /* @ud */
                u3_atom j,  /* @ud */
                u3_atom a)  /* @rs */
    {
      c3_w i_ = u3r_word(0, i);
      c3_w j_ = u3r_word(0, j);

      struct dims mn = shape(u);
      c3_w m_u = mn.rows;
      c3_w n_u = mn.cols;

      if ((i_ < 1) || (i_ > m_u) || (j_ < 1) || (j_ > n_u))
      {
        return u3m_bail(c3__exit);
      }

      c3_w* w_  = (c3_w*)u3a_malloc((m_u*m_u+2)*sizeof(float32_t));
      u3r_words(0, m_u*n_u+2, w_, u);

      uint32_t ii;
      w_[m_u*n_u+1] = 2;
      w_[m_u*n_u] = m_u;
      c3_w offset = (i_-1)*n_u+(j_-1);
      w_[offset] = u3r_word(0, a);

      u3_atom w = u3i_words(m_u*n_u+2, w_);
      u3a_free(w_);

      return w;
    }

    u3_noun
    u3welms_set(u3_noun cor)
    {
      u3_noun u, i, j, a;

      if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                                u3x_sam_6, &i,
                                u3x_sam_14, &j,
                                u3x_sam_15, &a, 0) ||
           c3n == u3ud(u) ||
           c3n == u3ud(i) ||
           c3n == u3ud(j) ||
           c3n == u3ud(a) )
      {
        return u3m_bail(c3__exit);
      }
      else {
        return u3qelms_set(u, i, j, a);
      }
    }

/* getter from @lms, 1-indexed
*/
  u3_noun
  u3qelms_getc(u3_atom u,  /* @lms */
               u3_atom j)  /* @ud */
  {
    c3_w j_    = u3r_word(0, j);  // 1-indexed

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;

    c3_w m_v   = m_u;  // n_v is the vector length

    if ((j_ < 1) || (j_ > n_u))
    {
      return u3m_bail(c3__exit);
    }

    c3_w* w_  = (c3_w*)u3a_malloc((m_v+1)*sizeof(float32_t));

    uint32_t ii;
    c3_w offset;
    union trip d;
    w_[m_v] = 1;
    for ( ii = 1; ii < m_u+1; ii++ ) {
      offset = ((ii-1)*n_u)+j_-1;
      d.c = u3r_word(0, u3qelms_get(u, ii, j_));
      w_[ii-1] = d.s.v;
    }
    u3_noun w = u3i_words(m_v+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_getc(u3_noun cor)
  {
    u3_noun a, i;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a,
                              u3x_sam_3, &i, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(i) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_getc(a, i);
    }
  }

/* setter for @lms, 1-indexed
*/
  u3_noun
  u3qelms_setc(u3_atom u,  /* @lms */
               u3_atom j,  /* @ud */
               u3_atom v)  /* @lvs */
  {
    c3_w j_    = u3r_word(0, j);

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;

    c3_w m_v = u3r_met(3,v)/4;  // m_v is the vector length

    if ((j_ < 1) || (j_ > n_u) || !(m_v == m_u))
    {
      return u3m_bail(c3__exit);
    }

    c3_w* w_  = (c3_w*)u3a_malloc((m_u*n_u+2)*sizeof(float32_t));
    u3r_words(0, m_u*n_u+2, w_, u);
    w_[m_u*n_u+1] = 2;
    w_[m_u*n_u] = m_u;

    uint32_t ii;
    c3_w offset;
    union trip d;
    for ( ii = 1; ii < m_u+1; ii++ ) {
      d.c = u3r_word(0, u3qelvs_get(v, ii));
      offset = ((ii-1)*n_u)+j_-1;
      w_[offset] = d.s.v;
    }
    u3_noun w = u3i_words(m_u*n_u+2, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_setc(u3_noun cor)
  {
    u3_noun u, i, a;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_6, &i,
                              u3x_sam_7, &a, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_setc(u, i, a);
    }
  }

/* getter from @lms, 1-indexed
*/
  u3_noun
  u3qelms_getr(u3_atom u,  /* @lms */
               u3_atom i)  /* @ud */
  {
    c3_w i_    = u3r_word(0, i);  // 1-indexed

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;

    if ((i_ < 1) || (i_ > m_u))
    {
      return u3m_bail(c3__exit);
    }

    c3_w* v_  = (c3_w*)u3a_malloc((n_u+1)*sizeof(float32_t));
    u3r_words((i_-1)*n_u, n_u, v_, u);
    v_[n_u] = 1;

    u3_atom v = u3i_words(n_u+1, v_);  /* @lvd */
    u3a_free(v_);
    return v;
  }

  u3_noun
  u3welms_getr(u3_noun cor)
  {
    u3_noun u, i;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_3, &i, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_getr(u, i);
    }
  }

/* setter for @lms, 1-indexed
*/
  u3_noun
  u3qelms_setr(u3_atom u,  /* @lms */
               u3_atom i,  /* @ud */
               u3_atom v)  /* @lvs */
  {
    c3_w i_    = u3r_word(0, i);  // 1-indexed

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;
    c3_w n_v = u3r_met(3, v)/4;  // n_v is the vector length

    if ((i_ < 1) || (i_ > m_u) || !(n_v == n_u))
    {
      return u3m_bail(c3__exit);
    }

    c3_w* w_  = (c3_w*)u3a_malloc((m_u*n_u+2)*sizeof(float32_t));
    u3r_words(0, m_u*n_u+2, w_, u);
    u3r_words(0, n_v, &(w_[(i_-1)*n_u]), v);

    u3_noun w = u3i_words(m_u*n_u+2, w_);   /* @lvd */
    u3a_free(w_);
    return w;
  }

  u3_noun
  u3welms_setr(u3_noun cor)
  {
    u3_noun u, i, v;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_6, &i,
                              u3x_sam_7, &v, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) ||
         c3n == u3ud(v) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_setr(u, i, v);
    }
  }

/* swap two columns
*/
  u3_noun
  u3qelms_swapc(u3_atom u,  /* @lms */
                u3_atom i,  /* @ud */
                u3_atom j)  /* @ud */
  {
    //  XXX could be done faster as an in-place swap
    u3_noun ui = u3qelms_getc(u, i);
    u3_noun uj = u3qelms_getc(u, j);
    u3_noun ut = u3qelms_setc(u, i, uj);
    u = u3qelms_setc(ut, j, ui);

    return u;
  }

  u3_noun
  u3welms_swapc(u3_noun cor)
  {
    u3_noun u, i, j;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_6, &i,
                              u3x_sam_7, &j, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) ||
         c3n == u3ud(j) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_swapc(u, i, j);
    }
  }

/* swap two rows
*/
  u3_noun
  u3qelms_swapr(u3_atom u,  /* @lms */
                u3_atom i,  /* @ud */
                u3_atom j)  /* @ud */
  {
    //  XXX could be done faster as an in-place swap
    u3_noun ui = u3qelms_getr(u, i);
    u3_noun uj = u3qelms_getr(u, j);
    u3_noun ut = u3qelms_setr(u, i, uj);
    u = u3qelms_setr(ut, j, ui);

    return u;
  }

  u3_noun
  u3welms_swapr(u3_noun cor)
  {
    u3_noun u, i, j;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_6, &i,
                              u3x_sam_7, &j, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) ||
         c3n == u3ud(j) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_swapr(u, i, j);
    }
  }

/* transpose
*/
  u3_noun
  u3qelms_trans(u3_atom u)  /* @lms */
  {
    c3_w mnt_u = u3r_met(3,u)/4;  // mnt_u is the matrix size (total)

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;

    uint32_t i;
    u3_noun w = u3qelms_zeros(n_u, m_u);
    for ( i = 1; i <= m_u; i++ )
    {
      w = u3qelms_setc(w, i, u3qelms_getr(u, i));
    }

    return w;
  }

  u3_noun
  u3welms_trans(u3_noun cor)
  {
    u3_noun u;

    if ( c3n == (u = u3r_at(u3x_sam, cor)) ||
         c3n == u3ud(u) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_trans(u);
    }
  }

/* add lms + lvs at col
*/
  u3_noun
  u3qelms_addvc(u3_atom u,  /* @lms */
                u3_atom i,  /* @ud */
                u3_atom a,  /* @lvs */
                u3_atom r)
  {
    return u3qelms_setc(u, i, u3qelvs_addv(u3qelms_getc(u, i), a, r));
  }

  u3_noun
  u3welms_addvc(u3_noun cor)
  {
    u3_noun u, i, a;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_6, &i,
                              u3x_sam_7, &a, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_addvc(u, i, a, u3x_at(30, cor));
    }
  }

/* sub lms + lvs at col
*/
  u3_noun
  u3qelms_subvc(u3_atom u,  /* @lms */
                u3_atom i,  /* @ud */
                u3_atom a,  /* @lvs */
                u3_atom r)
  {
    return u3qelms_setc(u, i, u3qelvs_subv(u3qelms_getc(u, i), a, r));
  }

  u3_noun
  u3welms_subvc(u3_noun cor)
  {
    u3_noun u, i, a;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_6, &i,
                              u3x_sam_7, &a, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_subvc(u, i, a, u3x_at(30, cor));
    }
  }

/* mul lms + lvs at col
*/
  u3_noun
  u3qelms_mulvc(u3_atom u,  /* @lms */
                u3_atom i,  /* @ud */
                u3_atom a,  /* @lvs */
                u3_atom r)
  {
    return u3qelms_setc(u, i, u3qelvs_mulv(u3qelms_getc(u, i), a, r));
  }

  u3_noun
  u3welms_mulvc(u3_noun cor)
  {
    u3_noun u, i, a;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_6, &i,
                              u3x_sam_7, &a, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_mulvc(u, i, a, u3x_at(30, cor));
    }
  }

/* div lms + lvs at col
*/
  u3_noun
  u3qelms_divvc(u3_atom u,  /* @lms */
                u3_atom i,  /* @ud */
                u3_atom a,  /* @lvs */
                u3_atom r)
  {
    return u3qelms_setc(u, i, u3qelvs_divv(u3qelms_getc(u, i), a, r));
  }

  u3_noun
  u3welms_divvc(u3_noun cor)
  {
    u3_noun u, i, a;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_6, &i,
                              u3x_sam_7, &a, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_divvc(u, i, a, u3x_at(30, cor));
    }
  }

/* add lms + lvs at row
*/
  u3_noun
  u3qelms_addvr(u3_atom u,  /* @lms */
                u3_atom i,  /* @ud */
                u3_atom a,  /* @lvs */
                u3_atom r)
  {
    return u3qelms_setr(u, i, u3qelvs_addv(u3qelms_getr(u, i), a, r));
  }

  u3_noun
  u3welms_addvr(u3_noun cor)
  {
    u3_noun u, i, a;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_6, &i,
                              u3x_sam_7, &a, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_addvr(u, i, a, u3x_at(30, cor));
    }
  }

/* sub lms + lvs at row
*/
  u3_noun
  u3qelms_subvr(u3_atom u,  /* @lms */
                u3_atom i,  /* @ud */
                u3_atom a,  /* @lvs */
                u3_atom r)
  {
    return u3qelms_setr(u, i, u3qelvs_subv(u3qelms_getr(u, i), a, r));
  }

  u3_noun
  u3welms_subvr(u3_noun cor)
  {
    u3_noun u, i, a;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_6, &i,
                              u3x_sam_7, &a, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_subvr(u, i, a, u3x_at(30, cor));
    }
  }

/* mul lms + lvs at col
*/
  u3_noun
  u3qelms_mulvr(u3_atom u,  /* @lms */
                u3_atom i,  /* @ud */
                u3_atom a,  /* @lvs */
                u3_atom r)
  {
    return u3qelms_setr(u, i, u3qelvs_mulv(u3qelms_getr(u, i), a, r));
  }

  u3_noun
  u3welms_mulvr(u3_noun cor)
  {
    u3_noun u, i, a;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_6, &i,
                              u3x_sam_7, &a, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_mulvr(u, i, a, u3x_at(30, cor));
    }
  }

/* div lms + lvs at row
*/
  u3_noun
  u3qelms_divvr(u3_atom u,  /* @lms */
                u3_atom i,  /* @ud */
                u3_atom a,  /* @lvs */
                u3_atom r)
  {
    return u3qelms_setr(u, i, u3qelvs_divv(u3qelms_getr(u, i), a, r));
  }

  u3_noun
  u3welms_divvr(u3_noun cor)
  {
    u3_noun u, i, a;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_6, &i,
                              u3x_sam_7, &a, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) ||
         c3n == u3ud(a) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_divvr(u, i, a, u3x_at(30, cor));
    }
  }

/* add lms + lms
*/
  u3_noun
  u3qelms_addm(u3_atom u,  /* @lms */
               u3_atom v,  /* @lms */
               u3_atom r)
  {
    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;
    mn = shape(v);
    c3_w m_v = mn.rows;
    c3_w n_v = mn.cols;

    if ( (m_u != m_v) || (n_u != n_v) )
    {
      return u3m_bail(c3__exit);
    }

    uint32_t ii;
    uint32_t k;
    //u3_atom w = u;
    u3_atom ru, rv, rr;
    for ( ii = 1; ii <= m_u; ii++ )
    {
      ru = u3qelms_getr(u, ii);
      rv = u3qelms_getr(v, ii);
      rr = u3qelvs_addv(ru, rv, r);
      u = u3qelms_setr(u, ii, rr);
    }

    u3z(ru); u3z(rv); u3z(rr);
    return u;
  }

  u3_noun
  u3welms_addm(u3_noun cor)
  {
    u3_noun u, v;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_3, &v, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(v) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_addm(u, v, u3x_at(30, cor));
    }
  }

/* sub lms + lms
*/
  u3_noun
  u3qelms_subm(u3_atom u,  /* @lms */
               u3_atom v,  /* @lms */
               u3_atom r)
  {
    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;
    mn = shape(v);
    c3_w m_v = mn.rows;
    c3_w n_v = mn.cols;

    if ( (m_u != m_v) || (n_u != n_v) )
    {
      return u3m_bail(c3__exit);
    }

    uint32_t ii;
    u3_atom w = u;
    for ( ii = 1; ii <= m_u; ii++ )
    {
      w = u3qelms_setr(w, ii, u3qelvs_subv(u3qelms_getr(u, ii), u3qelms_getr(v, ii), r));
    }

    return w;
  }

  u3_noun
  u3welms_subm(u3_noun cor)
  {
    u3_noun u, v;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_3, &v, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(v) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_subm(u, v, u3x_at(30, cor));
    }
  }

/* mul lms + lms
*/
  u3_noun
  u3qelms_mulm(u3_atom u,  /* @lms */
               u3_atom v,  /* @lms */
               u3_atom r)
  {
    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;
    mn = shape(v);
    c3_w m_v = mn.rows;
    c3_w n_v = mn.cols;

    if ( (m_u != m_v) || (n_u != n_v) )
    {
      return u3m_bail(c3__exit);
    }

    uint32_t ii;
    u3_atom w = u;
    for ( ii = 1; ii <= m_u; ii++ )
    {
      w = u3qelms_setr(w, ii, u3qelvs_mulv(u3qelms_getr(u, ii), u3qelms_getr(v, ii), r));
    }

    return w;
  }

  u3_noun
  u3welms_mulm(u3_noun cor)
  {
    u3_noun u, v;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_3, &v, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(v) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_mulm(u, v, u3x_at(30, cor));
    }
  }

/* div lms + lms
*/
  u3_noun
  u3qelms_divm(u3_atom u,  /* @lms */
               u3_atom v,  /* @lms */
               u3_atom r)
  {
    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;
    mn = shape(v);
    c3_w m_v = mn.rows;
    c3_w n_v = mn.cols;

    if ( (m_u != m_v) || (n_u != n_v) )
    {
      return u3m_bail(c3__exit);
    }

    uint32_t ii;
    u3_atom w = u;
    for ( ii = 1; ii <= m_u; ii++ )
    {
      w = u3qelms_setr(w, ii, u3qelvs_divv(u3qelms_getr(u, ii), u3qelms_getr(v, ii), r));
    }

    return w;
  }

  u3_noun
  u3welms_divm(u3_noun cor)
  {
    u3_noun u, v;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_3, &v, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(v) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_divm(u, v, u3x_at(30, cor));
    }
  }

/* matrix multiply lms + lms
   XXX this one would benefit from ATLAS/BLAS
*/
  u3_noun
  u3qelms_mmul(u3_atom u,  /* @lms */
               u3_atom v,  /* @lms */
               u3_atom r)
  {
    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;
    mn = shape(v);
    c3_w m_v = mn.rows;
    c3_w n_v = mn.cols;

    if ((n_u != m_v))
    {
      return u3m_bail(c3__exit);
    }

    uint32_t ii, jj;
    u3_atom w = u3qelms_zeros(m_u, n_v);
    u3_atom t;
    // XXX not strictly speaking the most efficient way but very clean code
    for ( ii = 1; ii <= m_u; ii++ )
    {
      for ( jj = 1; jj <= n_v; jj++ )
      {
        t = u3qelvs_inner(u3qelms_getr(u, ii), u3qelms_getc(v, jj), r);
        w = u3qelms_set(w, ii, jj, t);
      }
    }

    return w;
  }

  u3_noun
  u3welms_mmul(u3_noun cor)
  {
    u3_noun u, v;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_3, &v, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(v) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_mmul(u, v, u3x_at(30, cor));
    }
  }

/* matrix exponentiation (A**N, not e(A))
*/
  u3_noun
  u3qelms_mpow(u3_atom u,  /* @lms */
               u3_atom i,  /* @ud */
               u3_atom r)
  {
    c3_w i_, ii;
    i_ = u3r_word(0, i);

    u3_atom w = u;
    for ( ii = 1; ii <= i_; ii++ )
    {
      w = u3qelms_mmul(u, w, r);
    }

    return w;
  }

  u3_noun
  u3welms_mpow(u3_noun cor)
  {
    u3_noun u, i;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_3, &i, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_mpow(u, i, u3x_at(30, cor));
    }
  }

/* matrix minor
*/
  u3_noun
  u3qelms_minor(u3_atom u,  /* @lms */
                u3_atom i,  /* @ud */
                u3_atom j)  /* @ud */
  {
    c3_w i_ = u3r_word(0, i);
    c3_w j_ = u3r_word(0, j);

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;

    if ((i_ < 1) || (i_ > m_u) || (j_ < 1) || (j_ > n_u))
    {
      return u3m_bail(c3__exit);
    }

    c3_w m_w, n_w;
    m_w = m_u - 1;
    n_w = n_u - 1;
    c3_w* w_  = (c3_w*)u3a_malloc((m_w*n_w+2)*sizeof(float32_t));
    w_[m_w*n_w+1] = 2;
    w_[m_w*n_w] = m_w;
    uint32_t ii, jj, mi, mj;
    i_ = m_u - i_ + 1;
    j_ = n_u - j_ + 1;
    for ( ii = 0; ii < m_u; ii++ )
    {
      mi = ii > (i_-1) ? ii - 1 : ii;
      for ( jj = 0; jj < n_u; jj++ )
      {
        mj = jj > (j_-1) ? jj - 1 : jj;
        if ( (ii == (i_-1)) || (jj == (j_-1)) ) continue;
        w_[mi*n_w+mj] = u3r_word(ii*n_u+jj, u);
      }
    }
    u3_noun w = u3i_words(m_w*n_w+2, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_minor(u3_noun cor)
  {
    u3_noun u, i, j;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_6, &i,
                              u3x_sam_7, &j, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(i) ||
         c3n == u3ud(j) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_minor(u, i, j);
    }
  }

/* submatrix
*/
  u3_noun
  u3qelms_submatrix(u3_atom u,   /* @lms */
                    u3_atom ia,  /* @ud */
                    u3_atom ib,  /* @ud */
                    u3_atom ja,  /* @ud */
                    u3_atom jb)  /* @ud */
  {
    c3_w ia_ = u3r_word(0, ia);
    c3_w ib_ = u3r_word(0, ib);
    c3_w ja_ = u3r_word(0, ja);
    c3_w jb_ = u3r_word(0, jb);

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;

    if ((ia_ < 1) || (ib_ > m_u) || (ja_ < 1) || (jb_ > n_u) || (ia_ > ib_) || (ja_ > jb_))
    {
      return u3m_bail(c3__exit);
    }

    c3_w* u_  = (c3_w*)u3a_malloc((m_u*n_u+2)*sizeof(float32_t));
    u3r_words(0, m_u*n_u+2, u_, u);

    c3_w m_w, n_w;
    m_w = (ib_ - ia_) + 1;
    n_w = (jb_ - ja_) + 1;
    c3_w* w_  = (c3_w*)u3a_malloc((m_w*n_w+2)*sizeof(float32_t));
    w_[m_w*n_w+1] = 2;
    w_[m_w*n_w] = m_w;
    uint32_t ii, jj, mi, mj;
    for ( ii = ia_, mi = 1; ii <= ib_; ii++, mi++ )
    {
      for ( jj = ja_, mj = 1; jj <= jb_; jj++, mj++ )
      {
        w_[(mi-1)*n_w+mj-1] = u_[(ii-1)*n_u+jj-1];
      }
    }
    u3_noun w = u3i_words(m_w*n_w+2, w_);
    u3a_free(u_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_submatrix(u3_noun cor)
  {
    u3_noun u, ia, ib, ja, jb;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u,
                              u3x_sam_12, &ia,
                              u3x_sam_13, &ib,
                              u3x_sam_14, &ja,
                              u3x_sam_15, &jb, 0) ||
         c3n == u3ud(u) ||
         c3n == u3ud(ia) ||
         c3n == u3ud(ib) ||
         c3n == u3ud(ja) ||
         c3n == u3ud(jb) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_submatrix(u, ia, ib, ja, jb);
    }
  }

/* augment matrix
*/
  u3_noun
  u3qelms_augment(u3_atom u)  /* @lms */
  {
    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;
    c3_w m_w = m_u;
    c3_w n_w = 2*n_u;

    u3_atom v = u3qelms_id(m_u);

    c3_w* w_  = (c3_w*)u3a_malloc((m_w*n_w+2)*sizeof(float32_t));
    w_[m_w*n_w+1] = 2;
    w_[m_w*n_w] = m_w;
    uint32_t ii, jj;
    c3_w offset,  // offset within source matrices
         offset_; // offset within target matrix
    for ( ii = 1; ii <= m_u; ii++ )
    {
      for ( jj = 1; jj <= n_u; jj++ )
      {
        offset  = (ii-1)*n_u+jj-1;  // offset within source matrices
        offset_ = (ii-1)*n_w+jj-1;  // offset within target matrix
        w_[offset_]     = u3r_word(offset, u);
        w_[offset_+n_u] = u3r_word(offset, v);
      }
    }
    u3_noun w = u3i_words(m_w*n_w+2, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_augment(u3_noun cor)
  {
    u3_noun u;

    if ( c3n == (u = u3r_at(u3x_sam, cor)) ||
         c3n == u3ud(u) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_augment(u);
    }
  }

/* invert matrix
   XXX this should become a wrapper not an implementation
   https://www.codesansar.com/numerical-methods/matrix-inverse-using-gauss-jordan-cpp-output.htm TODO
*/
  u3_noun
  u3qelms_invert(u3_atom u)  /* @lms */
  {
    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;

    if ( m_u != n_u )
    {
      return u3m_bail(c3__exit);
    }

    u3_atom w = u3qelms_augment(u);
    mn = shape(w);
    c3_w m_w = mn.rows;
    c3_w n_w = mn.cols;

    c3_w* w_  = (c3_w*)u3a_malloc((m_u*n_w+2)*sizeof(float32_t));
    u3r_words(0, m_w*n_w+2, w_, w);

    uint32_t ii, jj, kk, i;   // misc indices
    uint32_t lead = 1;        // index of current leading diagonal
    c3_w offset, final_offset, offset_r, offset_s;  // misc array offsets
    c3_w* t_  = (c3_w*)u3a_malloc((n_w)*sizeof(float32_t));  // temp storage
    union trip a, b, c, ratio;
    for ( ii = 1; ii <= m_u; ii++ )
    {
      if ( lead > n_u )  // because not working through augmented solution cols
      {
        break;
      }

      //  Identify leading row of column.
      i = ii;
      while (w_[(i-1)*n_w+lead-1] == 0.0)
      {
        i++;
        if ( i > m_w )
        {
          i = ii;
          lead++;
          if ( lead > n_w )
          {
            break;
          }
        }
      }

      //  Swap rows.
      if ( i != ii )
      {
        offset_r = (i-1)*n_w;
        offset_s = (ii-1)*n_w;
        memcpy(t_, &(w_[offset_r]), (n_w)*sizeof(float32_t));
        memcpy(&(w_[offset_r]), &(w_[offset_s]), (n_w)*sizeof(float32_t));
        memcpy(&(w_[offset_s]), t_, (n_w)*sizeof(float32_t));
      }

      //  Multiply by ratio so 1.0 is on diagonal.
      offset = (ii-1)*n_w+lead-1;
      a.c = w_[offset];
      ratio.b = 1.0 / a.b;
      for ( jj = 1; jj <= n_w; jj++ )
      {
        offset = (ii-1)*n_w+jj-1;
        a.c = w_[offset];
        b.b = ratio.b * a.b;
        w_[offset] = b.c;
      }
      //  Subtract row i's leading value times the row ii from row i.
      for ( i = 1; i <= m_w; i++ )
      {
        if ( i != ii )
        {
          offset = (i-1)*n_w+lead-1;
          ratio.c = w_[offset];
          for ( jj = 1; jj <= n_w; jj++ )
          {
            offset = (ii-1)*n_w+jj-1;
            a.c = w_[offset];
            final_offset = (i-1)*n_w+jj-1;
            b.c = w_[final_offset];
            c.b = b.b - a.b * ratio.b;
            w_[final_offset] = c.c;
          }
        }
      }
      lead++;
    }

    w = u3i_words(m_w*n_w+2, w_);
    u3_noun w__ = u3qelms_submatrix(w, 1, m_u, n_u+1, n_w);
    u3a_free(w_);
    u3a_free(t_);

    return w__;
  }

  u3_noun
  u3welms_invert(u3_noun cor)
  {
    u3_noun u;

    if ( c3n == (u = u3r_at(u3x_sam, cor)) ||
         c3n == u3ud(u) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_invert(u);
    }
  }
