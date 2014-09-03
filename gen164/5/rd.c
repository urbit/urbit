/* j/5/aes.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

union doub {
  double d;
  c3_d c;
};


/* functions
*/
/* sun
*/
  u2_weak
  j2_mcd(Pt5, rd, sun)(
                        u2_atom a)
  {
    union doub b;
    b.d = (double) u2_cr_chub(0, a);

    return u2_ci_chubs(1, &b.c);
  }

  u2_weak
  j2_mc(Pt5, rd, sun)(
                       u2_noun cor)
  {
    u2_noun a;

    if (u2_no == u2_cr_mean(cor, u2_cv_sam, &a, 0)
        || u2_no == u2ud(a)) {
      return u2_cm_bail(c3__exit);
    }
    else {
      return j2_mcd(Pt5, rd, sun)(a);
    }
  }

/* mul
*/
  u2_weak
  j2_mcd(Pt5, rd, mul)(
                        u2_atom a,
                        u2_atom b)
  {
    union doub c, d, e;
    c.c = u2_cr_chub(0, a);
    d.c = u2_cr_chub(0, b);
    e.d = c.d * d.d;

    return u2_ci_chubs(1, &e.c);
  }

  u2_weak
  j2_mc(Pt5, rd, mul)(
                       u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ||
         u2_no == u2ud(a) ||
         u2_no == u2ud(b) )
    {
      return u2_cm_bail(c3__exit);
    }
    else {
      return j2_mcd(Pt5, rd, mul)(a, b);
    }
  }

/* div
*/
  u2_weak
  j2_mcd(Pt5, rd, div)(
                        u2_atom a,
                        u2_atom b)
  {
    union doub c, d, e;
    c.c = u2_cr_chub(0, a);
    d.c = u2_cr_chub(0, b);
    e.d = c.d / d.d;

    return u2_ci_chubs(1, &e.c);
  }

  u2_weak
  j2_mc(Pt5, rd, div)(
                       u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ||
         u2_no == u2ud(a) ||
         u2_no == u2ud(b) )
    {
      return u2_cm_bail(c3__exit);
    }
    else {
      return j2_mcd(Pt5, rd, div)(a, b);
    }
  }

/* add
*/
  u2_weak
  j2_mcd(Pt5, rd, add)(
                        u2_atom a,
                        u2_atom b)
  {
    union doub c, d, e;
    c.c = u2_cr_chub(0, a);
    d.c = u2_cr_chub(0, b);
    e.d = c.d + d.d;

    return u2_ci_chubs(1, &e.c);
  }

  u2_weak
  j2_mc(Pt5, rd, add)(
                       u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ||
         u2_no == u2ud(a) ||
         u2_no == u2ud(b) )
    {
      return u2_cm_bail(c3__exit);
    }
    else {
      return j2_mcd(Pt5, rd, add)(a, b);
    }
  }

/* sub
*/
  u2_weak
  j2_mcd(Pt5, rd, sub)(
                        u2_atom a,
                        u2_atom b)
  {
    union doub c, d, e;
    c.c = u2_cr_chub(0, a);
    d.c = u2_cr_chub(0, b);
    e.d = c.d - d.d;

    return u2_ci_chubs(1, &e.c);
  }

  u2_weak
  j2_mc(Pt5, rd, sub)(
                       u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ||
         u2_no == u2ud(a) ||
         u2_no == u2ud(b) )
    {
      return u2_cm_bail(c3__exit);
    }
    else {
      return j2_mcd(Pt5, rd, sub)(a, b);
    }
  }

/* lte
*/
  u2_weak
  j2_mcd(Pt5, rd, lte)(
                        u2_atom a,
                        u2_atom b)
  {
    union doub c, d;
    c.c = u2_cr_chub(0, a);
    d.c = u2_cr_chub(0, b);

    return u2_say(c.d <= d.d);
  }

  u2_weak
  j2_mc(Pt5, rd, lte)(
                       u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ||
         u2_no == u2ud(a) ||
         u2_no == u2ud(b) )
    {
      return u2_cm_bail(c3__exit);
    }
    else {
      return j2_mcd(Pt5, rd, lte)(a, b);
    }
  }

/* lth
*/
  u2_weak
  j2_mcd(Pt5, rd, lth)(
                        u2_atom a,
                        u2_atom b)
  {
    union doub c, d;
    c.c = u2_cr_chub(0, a);
    d.c = u2_cr_chub(0, b);

    return u2_say(c.d < d.d);
  }

  u2_weak
  j2_mc(Pt5, rd, lth)(
                       u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ||
         u2_no == u2ud(a) ||
         u2_no == u2ud(b) )
    {
      return u2_cm_bail(c3__exit);
    }
    else {
      return j2_mcd(Pt5, rd, lth)(a, b);
    }
  }

/* gte
*/
  u2_weak
  j2_mcd(Pt5, rd, gte)(
                        u2_atom a,
                        u2_atom b)
  {
    union doub c, d;
    c.c = u2_cr_chub(0, a);
    d.c = u2_cr_chub(0, b);

    return u2_say(c.d >= d.d);
  }

  u2_weak
  j2_mc(Pt5, rd, gte)(
                       u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ||
         u2_no == u2ud(a) ||
         u2_no == u2ud(b) )
    {
      return u2_cm_bail(c3__exit);
    }
    else {
      return j2_mcd(Pt5, rd, gte)(a, b);
    }
  }

/* gth
*/
  u2_weak
  j2_mcd(Pt5, rd, gth)(
                        u2_atom a,
                        u2_atom b)
  {
    union doub c, d;
    c.c = u2_cr_chub(0, a);
    d.c = u2_cr_chub(0, b);

    return u2_say(c.d > d.d);
  }

  u2_weak
  j2_mc(Pt5, rd, gth)(
                       u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ||
         u2_no == u2ud(a) ||
         u2_no == u2ud(b) )
    {
      return u2_cm_bail(c3__exit);
    }
    else {
      return j2_mcd(Pt5, rd, gth)(a, b);
    }
  }
