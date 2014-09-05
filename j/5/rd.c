/* j/5/aes.c
**
** This file is in the public domain.
*/
#include "all.h"


union doub {
  double d;
  c3_d c;
};


/* functions
*/
/* sun
*/
  u3_noun
  u3_cqer_sun(u3_atom a)
  {
    union doub b;
    b.d = (double) u3_cr_chub(0, a);

    return u3_ci_chubs(1, &b.c);
  }

  u3_noun
  u3_cwer_sun(u3_noun cor)
  {
    u3_noun a;

    if (u3_no == u3_cr_mean(cor, u3_cv_sam, &a, 0)
        || u3_no == u3ud(a)) {
      return u3_cm_bail(c3__exit);
    }
    else {
      return u3_cqer_sun(a);
    }
  }

/* mul
*/
  u3_noun
  u3_cqer_mul(u3_atom a, u3_atom b)
  {
    union doub c, d, e;
    c.c = u3_cr_chub(0, a);
    d.c = u3_cr_chub(0, b);
    e.d = c.d * d.d;

    return u3_ci_chubs(1, &e.c);
  }

  u3_noun
  u3_cwer_mul(u3_noun cor)
  {
    u3_noun a, b;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0) ||
         u3_no == u3ud(a) ||
         u3_no == u3ud(b) )
    {
      return u3_cm_bail(c3__exit);
    }
    else {
      return u3_cqer_mul(a, b);
    }
  }

/* div
*/
  u3_noun
  u3_cqer_div(u3_atom a, u3_atom b)
  {
    union doub c, d, e;
    c.c = u3_cr_chub(0, a);
    d.c = u3_cr_chub(0, b);
    e.d = c.d / d.d;

    return u3_ci_chubs(1, &e.c);
  }

  u3_noun
  u3_cwer_div(u3_noun cor)
  {
    u3_noun a, b;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0) ||
         u3_no == u3ud(a) ||
         u3_no == u3ud(b) )
    {
      return u3_cm_bail(c3__exit);
    }
    else {
      return u3_cqer_div(a, b);
    }
  }

/* add
*/
  u3_noun
  u3_cqer_add(u3_atom a, u3_atom b)
  {
    union doub c, d, e;
    c.c = u3_cr_chub(0, a);
    d.c = u3_cr_chub(0, b);
    e.d = c.d + d.d;

    return u3_ci_chubs(1, &e.c);
  }

  u3_noun
  u3_cwer_add(u3_noun cor)
  {
    u3_noun a, b;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0) ||
         u3_no == u3ud(a) ||
         u3_no == u3ud(b) )
    {
      return u3_cm_bail(c3__exit);
    }
    else {
      return u3_cqer_add(a, b);
    }
  }

/* sub
*/
  u3_noun
  u3_cqer_sub(u3_atom a, u3_atom b)
  {
    union doub c, d, e;
    c.c = u3_cr_chub(0, a);
    d.c = u3_cr_chub(0, b);
    e.d = c.d - d.d;

    return u3_ci_chubs(1, &e.c);
  }

  u3_noun
  u3_cwer_sub(u3_noun cor)
  {
    u3_noun a, b;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0) ||
         u3_no == u3ud(a) ||
         u3_no == u3ud(b) )
    {
      return u3_cm_bail(c3__exit);
    }
    else {
      return u3_cqer_sub(a, b);
    }
  }

/* lte
*/
  u3_noun
  u3_cqer_lte(u3_atom a, u3_atom b)
  {
    union doub c, d;
    c.c = u3_cr_chub(0, a);
    d.c = u3_cr_chub(0, b);

    return u3_say(c.d <= d.d);
  }

  u3_noun
  u3_cwer_lte(u3_noun cor)
  {
    u3_noun a, b;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0) ||
         u3_no == u3ud(a) ||
         u3_no == u3ud(b) )
    {
      return u3_cm_bail(c3__exit);
    }
    else {
      return u3_cqer_lte(a, b);
    }
  }

/* lth
*/
  u3_noun
  u3_cqer_lth(u3_atom a, u3_atom b)
  {
    union doub c, d;
    c.c = u3_cr_chub(0, a);
    d.c = u3_cr_chub(0, b);

    return u3_say(c.d < d.d);
  }

  u3_noun
  u3_cwer_lth(u3_noun cor)
  {
    u3_noun a, b;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0) ||
         u3_no == u3ud(a) ||
         u3_no == u3ud(b) )
    {
      return u3_cm_bail(c3__exit);
    }
    else {
      return u3_cqer_lth(a, b);
    }
  }

/* gte
*/
  u3_noun
  u3_cqer_gte(u3_atom a, u3_atom b)
  {
    union doub c, d;
    c.c = u3_cr_chub(0, a);
    d.c = u3_cr_chub(0, b);

    return u3_say(c.d >= d.d);
  }

  u3_noun
  u3_cwer_gte(u3_noun cor)
  {
    u3_noun a, b;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0) ||
         u3_no == u3ud(a) ||
         u3_no == u3ud(b) )
    {
      return u3_cm_bail(c3__exit);
    }
    else {
      return u3_cqer_gte(a, b);
    }
  }

/* gth
*/
  u3_noun
  u3_cqer_gth(u3_atom a, u3_atom b)
  {
    union doub c, d;
    c.c = u3_cr_chub(0, a);
    d.c = u3_cr_chub(0, b);

    return u3_say(c.d > d.d);
  }

  u3_noun
  u3_cwer_gth(u3_noun cor)
  {
    u3_noun a, b;

    if ( u3_no == u3_cr_mean(cor, u3_cv_sam_2, &a, u3_cv_sam_3, &b, 0) ||
         u3_no == u3ud(a) ||
         u3_no == u3ud(b) )
    {
      return u3_cm_bail(c3__exit);
    }
    else {
      return u3_cqer_gth(a, b);
    }
  }
