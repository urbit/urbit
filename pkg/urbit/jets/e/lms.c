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

  struct dims {
    c3_w rows, cols;
  };

  struct dims
  shape(u3_atom p)  /* @ud */
  {
    c3_w m = u3r_word(0, p);
    c3_w mn = u3r_met(3, p)/4;

    struct dims shp;
    shp.rows = m;
    shp.cols = mn / m;

    return shp;
  }

/* zeros in @lms
*/
  u3_noun
  u3qelms_zeros(u3_atom m,  /* @ud */
                u3_atom n)  /* @ud */
  {
    c3_w m_ = u3r_word(0,m);
    c3_w n_ = u3r_word(0,n);
    c3_w* w_  = (c3_w*)u3a_malloc((m_*n_+1)*sizeof(float32_t));

    uint32_t i;
    union trip c;
    c.c = 0x00000000;
    w_[m_*n_] = m_;
    for ( i = 0; i < n_*m_; i++ ) {
      w_[i] = (c3_w)c.s.v;
    }
    u3_noun w = u3i_words(m_*n_+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_zeros(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_zeros(a, b);
    }
  }

/* ones in @lms
*/
  u3_noun
  u3qelms_ones(u3_atom m,  /* @ud */
               u3_atom n)  /* @ud */
  {
    c3_w m_ = u3r_word(0,m);
    c3_w n_ = u3r_word(0,n);
    c3_w* w_  = (c3_w*)u3a_malloc((m_*n_+1)*sizeof(float32_t));

    uint32_t i;
    union trip c;
    c.c = 0x3F800000;
    w_[m_*n_] = m_;
    for ( i = 0; i < n_*m_; i++ ) {
      w_[i] = (c3_w)c.s.v;
    }
    u3_noun w = u3i_words(m_*n_+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_ones(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
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
  u3qelms_id(u3_atom m,  /* @ud */
             u3_atom n)  /* @ud */
  {
    c3_w m_ = u3r_word(0,m);
    c3_w n_ = u3r_word(0,n);
    c3_w* w_  = (c3_w*)u3a_malloc((m_*n_+1)*sizeof(float32_t));

    uint32_t i;
    union trip c, d;
    c.c = 0x00000000;
    d.c = 0x3F800000;
    w_[m_*n_] = m_;
    for ( i = 0; i < n_*m_; i++ ) {
      if ( i % (n_+1) == 0 )
      {
        w_[i] = (c3_w)d.s.v;
      }
      else
      {
        w_[i] = (c3_w)c.s.v;
      }
    }
    u3_noun w = u3i_words(m_*n_+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_id(u3_noun cor)
  {
    u3_noun a, b;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(b) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_id(a, b);
    }
  }

  /* getter from @lms, 1-indexed
  */
    u3_noun
    u3qelms_get(u3_atom u,  /* @lms */
                u3_atom i,  /* @ud */
                u3_atom j)  /* @ud */
    {
      c3_w mnt_ = u3r_met(3,u)/4;  // mnt_ is the vector length
      c3_w i_ = u3r_word(0, i);
      c3_w j_ = u3r_word(0, j);

      union trip p_;
      p_.c = u3r_word(mnt_, u);
      u3_atom p__ = u3i_words(1,&(p_.c));

      struct dims mn = shape(u);
      c3_w m_u = mn.rows;
      c3_w n_u = mn.cols;

      if ((i_ < 1) || (i_ > m_u) || (j_ < 1) || (j_ > n_u))
      {
        return u3m_bail(c3__exit);
      }

      c3_w* w_  = (c3_w*)u3a_malloc(1*sizeof(float32_t));
      w_[0] = u3r_word((m_u*n_u)-((i_-1)*n_u+j_), u);
      u3_atom w = u3i_words(1, w_);
      return w;
    }

    u3_noun
    u3welms_get(u3_noun cor)
    {
      u3_noun a, i, j;

      if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_6, &i, u3x_sam_7, &j, 0) ||
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
      c3_w mnt_ = u3r_met(3,u)/4;  // mnt_ is the vector length
      c3_w i_ = u3r_word(0, i);
      c3_w j_ = u3r_word(0, j);

      union trip p_;
      p_.c = u3r_word(mnt_, u);
      u3_atom p__ = u3i_words(1,&(p_.c));

      struct dims mn = shape(u);
      c3_w m_u = mn.rows;
      c3_w n_u = mn.cols;

      if ((i_ < 1) || (i_ > m_u) || (j_ < 1) || (j_ > n_u))
      {
        return u3m_bail(c3__exit);
      }

      c3_w* w_  = (c3_w*)u3a_malloc((m_u*n_u+1)*sizeof(float32_t));

      uint32_t ii;
      w_[m_u*n_u] = p_.c;
      for ( ii = 0; ii < m_u*n_u; ii++ ) {
        if ( ii == ((m_u*n_u)-((i_-1)*n_u+j_)) )
        {
          w_[ii] = u3r_word(0, a);
        }
        else
        {
          w_[ii] = u3r_word(ii, u);
        }
      }
      u3_noun w = u3i_words(m_u*n_u+1, w_);
      u3a_free(w_);

      return w;
    }

    u3_noun
    u3welms_set(u3_noun cor)
    {
      u3_noun u, i, j, a;

      if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_6, &i, u3x_sam_14, &j, u3x_sam_15, &a, 0) ||
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
    c3_w mnt_u = u3r_met(3,u)/4;  // mnt_u is the matrix size (total)
    c3_w j_    = u3r_word(0, j);  // 1-indexed

    union trip p_u;
    p_u.c = u3r_word(mnt_u, u);
    u3_atom p__ = u3i_words(1,&(p_u.c));

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;
    c3_w n_v   = n_u;  // n_v is the vector length

    if ((j_ < 1) || (j_ > m_u))
    {
      return u3m_bail(c3__exit);
    }

    c3_w* w_  = (c3_w*)u3a_malloc((n_v+1)*sizeof(float32_t));

    c3_w ii, jj;
    w_[n_v] = n_v;
    ii = 1;
    for ( jj = 0; jj < m_u*n_u; jj++ ) {
      if ( ((m_u*n_u) - jj) % n_u == j_ % n_u )  // %n_u is b/c 1-indexed
      {
        w_[ii-1] = u3qelms_get(u, ii, j_);
        ii++;  if ( ii > n_v ) { break; }  // underflow check (0-indexed at 4294967295)
      }
    }
    u3_noun w = u3i_words(n_v+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_getc(u3_noun cor)
  {
    u3_noun a, i;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &i, 0) ||
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
    c3_w mnt_u = u3r_met(3,u)/4;  // mnt_u is the matrix size (total)
    c3_w n_v   = u3r_met(3,v)/4;  // n_v is the vector length
    c3_w j_    = u3r_word(0, j);
    // TODO: alternatively just use set to do this across the array

    union trip p_u;
    p_u.c = u3r_word(mnt_u, u);
    u3_atom p__ = u3i_words(1,&(p_u.c));

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;

    if ((j_ < 1) || (j_ > m_u) || !(n_v == n_u))
    {
      return u3m_bail(c3__exit);
    }

    c3_w* w_  = (c3_w*)u3a_malloc((m_u*n_u+1)*sizeof(float32_t));

    uint32_t ii, jj;
    w_[m_u*n_u] = p_u.c;
    ii = n_v;
    for ( jj = 0; jj < m_u*n_u; jj++ ) {
      if ( ((m_u*n_u) - jj) % n_u == j_ % n_u )  // %n_u is b/c 1-indexed
      {
        w_[jj] = u3qelvs_get(v, ii);
        ii--;
      }
      else
      {
        w_[jj] = u3r_word(jj, u);
      }
    }
    u3_noun w = u3i_words(m_u*n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_setc(u3_noun cor)
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
      return u3qelms_setc(u, i, a);
    }
  }

/* getter from @lms, 1-indexed
*/
  u3_noun
  u3qelms_getr(u3_atom u,  /* @lms */
               u3_atom i)  /* @ud */
  {
    c3_w mnt_u = u3r_met(3,u)/4;  // mnt_u is the matrix size (total)
    c3_w i_    = u3r_word(0, i);  // 1-indexed

    union trip p_u;
    p_u.c = u3r_word(mnt_u, u);
    u3_atom p__ = u3i_words(1,&(p_u.c));

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;
    c3_w m_v   = m_u;  // m_v is the vector length

    if ((i_ < 1) || (i_ > m_u))
    {
      return u3m_bail(c3__exit);
    }

    c3_w* w_  = (c3_w*)u3a_malloc((m_v+1)*sizeof(float32_t));

    c3_w ii, jj;
    w_[m_v] = m_v;
    jj = 1;
    for ( ii = 0; ii < m_u*n_u; ii++ ) {
      if ( (((m_u*n_u) - i_*n_u) <= ii) && (ii < ((m_u*n_u) - (i_-1)*n_u)) )
      {
        w_[jj-1] = u3qelms_get(u, i_, jj);
        jj++;  if ( jj > m_v ) { break; }  // underflow check (0-indexed at 4294967295)
      }
    }
    u3_noun w = u3i_words(m_v+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_getr(u3_noun cor)
  {
    u3_noun a, i;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &i, 0) ||
         c3n == u3ud(a) ||
         c3n == u3ud(i) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qelms_getr(a, i);
    }
  }

/* setter for @lms, 1-indexed
*/
  u3_noun
  u3qelms_setr(u3_atom u,  /* @lms */
               u3_atom i,  /* @ud */
               u3_atom v)  /* @lvs */
  {
    c3_w mnt_u = u3r_met(3,u)/4;  // mnt_u is the matrix size (total)
    c3_w n_v   = u3r_met(3,v)/4;  // n_v is the vector length
    c3_w i_    = u3r_word(0, i);

    union trip p_u;
    p_u.c = u3r_word(mnt_u, u);
    u3_atom p__ = u3i_words(1,&(p_u.c));

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;

    if ((i_ < 1) || (i_ > m_u) || !(n_v == n_u))
    {
      return u3m_bail(c3__exit);
    }

    c3_w* w_  = (c3_w*)u3a_malloc((m_u*n_u+1)*sizeof(float32_t));

    uint32_t ii, jj;
    w_[m_u*n_u] = p_u.c;
    jj = n_v;
    for ( ii = 0; ii < m_u*n_u; ii++ ) {
      if ( (((m_u*n_u) - i_*n_u) <= ii) && (ii < ((m_u*n_u) - (i_-1)*n_u)) )
      {
        w_[ii] = (double)u3qelvs_get(v, jj);
        jj--;
      }
      else
      {
        w_[ii] = (double)u3r_word(ii, u);
      }
    }
    u3_noun w = u3i_words(m_u*n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_setr(u3_noun cor)
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
      return u3qelms_setr(u, i, a);
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

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_6, &i, u3x_sam_7, &j, 0) ||
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

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_6, &i, u3x_sam_7, &j, 0) ||
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

    union trip p_u;
    p_u.c = u3r_word(mnt_u, u);
    u3_atom p__ = u3i_words(1,&(p_u.c));

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;

    uint32_t i;
    u3_noun w = u3qelms_zeros(n_u, m_u);
    for ( i = 0; i < m_u; i++ )
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

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_6, &i, u3x_sam_7, &a, 0) ||
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

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_6, &i, u3x_sam_7, &a, 0) ||
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

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_6, &i, u3x_sam_7, &a, 0) ||
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

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_6, &i, u3x_sam_7, &a, 0) ||
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

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_6, &i, u3x_sam_7, &a, 0) ||
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

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_6, &i, u3x_sam_7, &a, 0) ||
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

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_6, &i, u3x_sam_7, &a, 0) ||
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

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_6, &i, u3x_sam_7, &a, 0) ||
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
    c3_w mnt_u = u3r_met(3,u)/4;  // mnt_u is the matrix size (total)
    c3_w mnt_v = u3r_met(3,v)/4;  // mnt_v is the matrix size (total)

    union trip p_u, p_v;
    p_u.c = u3r_word(mnt_u, u);
    p_v.c = u3r_word(mnt_v, v);
    u3_atom p__u = u3i_words(1,&(p_u.c));
    u3_atom p__v = u3i_words(1,&(p_v.c));

    if ( (mnt_u != mnt_v) || (p__u != p__v) )
    {
      return u3m_bail(c3__exit);
    }

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;

    c3_w* w_  = (c3_w*)u3a_malloc((m_u*n_u+1)*sizeof(float32_t));

    union trip c, d;
    c3_w ii, jj, index;
    w_[m_u*n_u] = p_u.c;
    for ( ii = 0; ii < n_u; ii++ )
    {
      for ( jj = 0; jj < m_u; jj++ )
      {
        index = ii*n_u+jj;
        d.c = u3r_word(index, u);
        c.c = u3r_word(index, v);
        w_[index] = (c3_w)_nan_unify(f32_add(c.s,d.s)).v;
      }
    }
    u3_noun w = u3i_words(m_u*n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_addm(u3_noun cor)
  {
    u3_noun u, v;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_3, &v, 0) ||
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
    c3_w mnt_u = u3r_met(3,u)/4;  // mnt_u is the matrix size (total)
    c3_w mnt_v = u3r_met(3,v)/4;  // mnt_v is the matrix size (total)

    union trip p_u, p_v;
    p_u.c = u3r_word(mnt_u, u);
    p_v.c = u3r_word(mnt_v, v);
    u3_atom p__u = u3i_words(1,&(p_u.c));
    u3_atom p__v = u3i_words(1,&(p_v.c));

    if ( (mnt_u != mnt_v) || (p__u != p__v) )
    {
      return u3m_bail(c3__exit);
    }

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;

    c3_w* w_  = (c3_w*)u3a_malloc((m_u*n_u+1)*sizeof(float32_t));

    union trip c, d;
    c3_w ii, jj, index;
    w_[m_u*n_u] = p_u.c;
    for ( ii = 0; ii < n_u; ii++ )
    {
      for ( jj = 0; jj < m_u; jj++ )
      {
        index = ii*n_u+jj;
        d.c = u3r_word(index, u);
        c.c = u3r_word(index, v);
        w_[index] = (c3_w)_nan_unify(f32_sub(c.s,d.s)).v;
      }
    }
    u3_noun w = u3i_words(m_u*n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_subm(u3_noun cor)
  {
    u3_noun u, v;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_3, &v, 0) ||
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
    c3_w mnt_u = u3r_met(3,u)/4;  // mnt_u is the matrix size (total)
    c3_w mnt_v = u3r_met(3,v)/4;  // mnt_v is the matrix size (total)

    union trip p_u, p_v;
    p_u.c = u3r_word(mnt_u, u);
    p_v.c = u3r_word(mnt_v, v);
    u3_atom p__u = u3i_words(1,&(p_u.c));
    u3_atom p__v = u3i_words(1,&(p_v.c));

    if ( (mnt_u != mnt_v) || (p__u != p__v) )
    {
      return u3m_bail(c3__exit);
    }

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;

    c3_w* w_  = (c3_w*)u3a_malloc((m_u*n_u+1)*sizeof(float32_t));

    union trip c, d;
    c3_w ii, jj, index;
    w_[m_u*n_u] = p_u.c;
    for ( ii = 0; ii < n_u; ii++ )
    {
      for ( jj = 0; jj < m_u; jj++ )
      {
        index = ii*n_u+jj;
        d.c = u3r_word(index, u);
        c.c = u3r_word(index, v);
        w_[index] = (c3_w)_nan_unify(f32_mul(c.s,d.s)).v;
      }
    }
    u3_noun w = u3i_words(m_u*n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_mulm(u3_noun cor)
  {
    u3_noun u, v;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_3, &v, 0) ||
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
    c3_w mnt_u = u3r_met(3,u)/4;  // mnt_u is the matrix size (total)
    c3_w mnt_v = u3r_met(3,v)/4;  // mnt_v is the matrix size (total)

    union trip p_u, p_v;
    p_u.c = u3r_word(mnt_u, u);
    p_v.c = u3r_word(mnt_v, v);
    u3_atom p__u = u3i_words(1,&(p_u.c));
    u3_atom p__v = u3i_words(1,&(p_v.c));

    if ( (mnt_u != mnt_v) || (p__u != p__v) )
    {
      return u3m_bail(c3__exit);
    }

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;

    c3_w* w_  = (c3_w*)u3a_malloc((m_u*n_u+1)*sizeof(float32_t));

    union trip c, d;
    c3_w ii, jj, index;
    w_[m_u*n_u] = p_u.c;
    for ( ii = 0; ii < n_u; ii++ )
    {
      for ( jj = 0; jj < m_u; jj++ )
      {
        index = ii*n_u+jj;
        d.c = u3r_word(index, u);
        c.c = u3r_word(index, v);
        w_[index] = (c3_w)_nan_unify(f32_div(c.s,d.s)).v;
      }
    }
    u3_noun w = u3i_words(m_u*n_u+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_divm(u3_noun cor)
  {
    u3_noun u, v;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_3, &v, 0) ||
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
    c3_w mnt_u = u3r_met(3,u)/4;  // mnt_u is the matrix size (total)
    c3_w mnt_v = u3r_met(3,v)/4;  // mnt_v is the matrix size (total)

    union trip p_u, p_v;
    p_u.c = u3r_word(mnt_u, u);
    p_v.c = u3r_word(mnt_v, v);
    u3_atom p__u = u3i_words(1,&(p_u.c));
    u3_atom p__v = u3i_words(1,&(p_v.c));

    if ( (mnt_u != mnt_v) || (p__u != p__v) )
    {
      return u3m_bail(c3__exit);
    }

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;

    mn = shape(v);
    c3_w m_v = mn.rows;
    c3_w n_v = mn.cols;

    if (n_u != m_v )
    {
      return u3m_bail(c3__exit);
    }

    c3_w* w_  = (c3_w*)u3a_malloc((m_u*n_u+1)*sizeof(float32_t));

    union trip c, d, sum;
    c3_w ii, jj, kk;
    w_[m_u*n_v] = p_u.c;
    for ( ii = 0; ii < m_u; ii++ )
    {
      for ( jj = 0; jj < n_v; jj++ )
      {
        sum.c = 0x00000000;
        for ( kk = 0; kk < m_v; kk++ )
        {
          d.c = u3r_word(ii*n_u+kk, u);
          c.c = u3r_word(kk*n_v+jj, v);
          sum.c = (c3_w)_nan_unify(f32_add(sum.s,f32_mul(c.s,d.s))).v;
        }
        w_[ii*n_u+jj] = sum.c;
      }
    }
    u3_noun w = u3i_words(m_u*n_v+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_mmul(u3_noun cor)
  {
    u3_noun u, v;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_3, &v, 0) ||
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
    for ( ii = 0; ii < i_; ii++ )
    {
      w = u3qelms_mmul(u, w, r);
    }

    return w;
  }

  u3_noun
  u3welms_mpow(u3_noun cor)
  {
    u3_noun u, i;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_3, &i, 0) ||
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
    c3_w mnt_ = u3r_met(3,u)/4;  // mnt_ is the vector length
    c3_w i_ = u3r_word(0, i);
    c3_w j_ = u3r_word(0, j);

    union trip p_;
    p_.c = u3r_word(mnt_, u);
    u3_atom p__ = u3i_words(1,&(p_.c));

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
    c3_w* w_  = (c3_w*)u3a_malloc((m_w*n_w+1)*sizeof(float32_t));
    w_[m_w*n_w] = m_w;
    c3_w ii, jj, mi, mj;
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
    u3_noun w = u3i_words(m_w*n_w+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_minor(u3_noun cor)
  {
    u3_noun u, i, j;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_6, &i, u3x_sam_7, &j, 0) ||
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
  u3qelms_submatrix(u3_atom u,  /* @lms */
                    u3_atom ia,  /* @ud */
                    u3_atom ib,  /* @ud */
                    u3_atom ja,  /* @ud */
                    u3_atom jb)  /* @ud */
  {
    c3_w mnt_ = u3r_met(3,u)/4;  // mnt_ is the vector length
    c3_w ia_ = u3r_word(0, ia);
    c3_w ib_ = u3r_word(0, ib);
    c3_w ja_ = u3r_word(0, ja);
    c3_w jb_ = u3r_word(0, jb);
    //fprintf(stderr, "sub:  %u %u %u %u\n", ia_, ib_, ja_, jb_);

    union trip p_;
    p_.c = u3r_word(mnt_, u);
    u3_atom p__ = u3i_words(1,&(p_.c));

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;
    //fprintf(stderr, "sub:  %u %u %u %u %u %u\n", ia_, ib_, ja_, jb_, m_u, n_u);

    if ((ia_ < 1) || (ib_ > m_u) || (ja_ < 1) || (jb_ > n_u) || (ia_ > ib_) || (ja > jb_))
    {
      return u3m_bail(c3__exit);
    }

    //  Flip the order around so it feels natural.
    //  This isn't formally necessary, but it's a lot easier to think about.
    c3_w* u__  = (c3_w*)u3a_malloc((m_u*n_u+1)*sizeof(float32_t));
    u3r_words(0, m_u*n_u+1, u__, u);
    c3_w* u_  = (c3_w*)u3a_malloc((m_u*n_u+1)*sizeof(float32_t));
    u_[m_u*n_u] = u__[m_u*n_u];
    c3_w iii;
    for ( iii = 0; iii < m_u*n_u; iii++ )
    {
      u_[m_u*n_u-1-iii] = u__[iii];
    }
    u3a_free(u__);

    c3_w m_w, n_w;
    m_w = ib_ - ia_ + 1;
    n_w = jb_ - ja_ + 1;
    c3_w* w_  = (c3_w*)u3a_malloc((m_w*n_w+1)*sizeof(float32_t));
    w_[m_w*n_w] = m_w;
    c3_w ii, jj, mi, mj;
    for ( ii = ia_-1, mi = 0; ii <= ib_-1; ii++, mi++ )
    {
      for ( jj = ja_-1, mj = 0; jj <= jb_-1; jj++, mj++ )
      {
        w_[m_w*n_w-(mi*n_w+mj)-1] = u_[ii*n_u+jj];
      }
    }
    u3_noun w = u3i_words(m_w*n_w+1, w_);
    u3a_free(w_);

    return w;
  }

  u3_noun
  u3welms_submatrix(u3_noun cor)
  {
    u3_noun u, ia, ib, ja, jb;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &u, u3x_sam_6, &ia, u3x_sam_14, &ib, u3x_sam_30, &ja, u3x_sam_31, &jb, 0) ||
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
    c3_w mnt_ = u3r_met(3,u)/4;  // mnt_ is the vector length

    union trip p_;
    p_.c = u3r_word(mnt_, u);
    u3_atom p__ = u3i_words(1,&(p_.c));

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;
    c3_w m_w = m_u;
    c3_w n_w = 2*n_u;

    u3_atom v = u3qelms_id(m_u, n_u);

    c3_w* w_  = (c3_w*)u3a_malloc((m_w*n_w+1)*sizeof(float32_t));
    w_[m_w*n_w] = m_w;
    c3_w ii, jj;
    for ( ii = 0; ii < m_u; ii++ )
    {
      for ( jj = 0; jj < n_u; jj++ )
      {
        w_[ii*n_w+n_u+jj] = u3r_word(ii*n_u+jj, u);
      }
      for ( jj = 0; jj < n_u; jj++ )
      {
        w_[ii*n_w+jj] = u3r_word(ii*n_u+jj, v);
      }
    }
    u3_noun w = u3i_words(m_w*n_w+1, w_);
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
*/
  u3_noun
  u3qelms_invert(u3_atom u)  /* @lms */
  {
    c3_w mnt_ = u3r_met(3,u)/4;  // mnt_ is the vector length

    union trip p_;
    p_.c = u3r_word(mnt_, u);
    u3_atom p__ = u3i_words(1,&(p_.c));

    struct dims mn = shape(u);
    c3_w m_u = mn.rows;
    c3_w n_u = mn.cols;

    if ( m_u != n_u )
    {
      return u3m_bail(c3__exit);
    }

    u3_atom v = u3qelms_augment(u);
    c3_w m_v, n_v;
    m_v = m_u;
    n_v = 2*n_u;

    //  Flip the order around so it feels natural.
    //  This isn't formally necessary, but it's a lot easier to think about.
    c3_w* w__  = (c3_w*)u3a_malloc((m_u*n_v+1)*sizeof(float32_t));
    u3r_words(0, m_u*n_v+1, w__, v);
    c3_w* w_  = (c3_w*)u3a_malloc((m_u*n_v+1)*sizeof(float32_t));
    //w_[m_u*n_v] = w__[m_u*n_v];
    c3_w ii, jj, kk, max_row;
    for ( ii = 0; ii < m_u*n_v; ii++ )
    {
      w_[m_u*n_v-1-ii] = w__[ii];
    }
    //fprintf(stderr,"%u %u => %u %u\n",m_u,n_v,p___,w_[m_u*n_v]);
    //u3a_free(w__);

    union trip el, max_el, t, c, d, e;
    c3_w iii, jjj;
    for ( ii = 0; ii < m_u; ii++ )
    {
      //  Locate maximum value in column for pivot.
      max_el.c = abs(w_[ii*n_v+ii]);
      max_row = ii;
      //fprintf(stderr, "%u %f %u\n", ii, max_el.b, max_row);
      for ( kk = ii+1; kk < m_u; kk++ )
      {
        el.c = abs(w_[kk*n_v+ii]);
        if ( el.b > max_el.b ) {
          max_el.c = abs(w_[kk*n_v+ii]);
          max_row = kk;
        }
      }
      //  Swap max row with current row.
      for ( jj = ii; jj < n_v; jj++ )
      {
        t.c = w_[max_row*n_v+jj];
        w_[max_row*n_v+jj] = w_[ii*n_v+jj];
        w_[ii*n_v+jj] = t.c;
      }
      //  Divide current row across by leading value.
      c.c = w_[ii*n_v+ii];  // leading value, leftmost value of current row
      if ( c.b == 1.0 )  continue;
      for ( jj = ii; jj < n_v; jj++ )  // divide across current row
      {
        d.c = w_[ii*n_v+jj];
        w_[ii*n_v+jj] = (c3_w)_nan_unify(f32_div(d.s, c.s)).v;
      }
      //  Set all rows below this to zero in current column.
      for ( kk = ii+1; kk < m_u; kk++ )
      {
        d.c = w_[kk*n_v+ii];  // reference row first element
        for ( jj = ii; jj < n_v; jj++ )
        {
          c.c = w_[ii*n_v+jj];  // reference row element
          e.c = w_[kk*n_v+jj];  // current row element
          el.c = f32_sub(e.s, f32_mul(d.s,c.s)).v;
          w_[kk*n_v+jj] = (c3_w)_nan_unify(f32_sub(e.s, f32_mul(d.s,c.s))).v;
        }
      }
    }
    // then replace up
    //  rowj = rowj - rowi*elemij
    for ( ii = m_u-1; (ii >= 0) && (ii < m_u); ii-- )  // watch underflow
    {
      for ( kk = ii-1; (kk >= 0) && (kk < m_u); kk-- )  // watch underflow
      {
        d.c = w_[kk*n_v+ii];
        for ( jj = kk; jj < n_v; jj++ )
        {
          c.c = w_[ii*n_v+jj];  // reference row element
          e.c = w_[kk*n_v+jj];  // current row element
          el.c = f32_sub(e.s, f32_mul(d.s,c.s)).v;
          w_[kk*n_v+jj] = (c3_w)_nan_unify(f32_sub(e.s, f32_mul(d.s,c.s))).v;
        }
      }
    }
    /*fprintf(stderr, "***\n");
    for ( ii = 0; ii < m_u; ii++ )
    {
      for ( jj = 0; jj < n_v; jj++ )
      {
        c.c = w_[ii*n_v+jj];
        fprintf(stderr,"%f ",c.b);
      }
      fprintf(stderr,"\n");
    }*/
    w_[m_u*n_v] = m_u;
    //c3_w asdf = u3qelms_cantor(m_u,n_u);
    //fprintf(stderr, "cantor:  %u %u -> %u v. %u\n", m_u, n_v, w_[m_u*n_v], asdf);
    //  Reverse back out, as we flipped the order up above to ease the math.
    //c3_w* w  = (c3_w*)u3a_malloc((m_u*n_v+1)*sizeof(float32_t));
    //u3r_words(0, m_u*n_v+1, w__, v);
    for ( ii = 0; ii < m_u*n_v; ii++ )
    {
      w__[m_u*n_v-1-ii] = w_[ii];
    }
    /*
    fprintf(stderr, "---\n");
    for ( ii = 0; ii < m_u; ii++ )
    {
      for ( jj = 0; jj < n_v; jj++ )
      {
        c.c = w__[ii*n_v+jj];
        fprintf(stderr,"%f ",c.b);
      }
      fprintf(stderr,"\n");
    }*/

    u3_noun w = u3i_words(m_u*n_v+1, w__);
    //fprintf(stderr, "***\n");
    u3_noun w___ = u3qelms_submatrix(w, 1, m_u, n_u+1, n_v);
    //fprintf(stderr, "***\n");
    u3a_free(w__);
    u3a_free(w_);

    return w___;
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
