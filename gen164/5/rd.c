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

/* declarations
*/
  extern u2_ho_jet j2_mcj(Pt5, rd, sun)[];
  extern u2_ho_jet j2_mcj(Pt5, rd, mul)[];
  extern u2_ho_jet j2_mcj(Pt5, rd, div)[];
  extern u2_ho_jet j2_mcj(Pt5, rd, add)[];
  extern u2_ho_jet j2_mcj(Pt5, rd, sub)[];
  extern u2_ho_jet j2_mcj(Pt5, rd, lte)[];
  extern u2_ho_jet j2_mcj(Pt5, rd, lth)[];
  extern u2_ho_jet j2_mcj(Pt5, rd, gte)[];
  extern u2_ho_jet j2_mcj(Pt5, rd, gth)[];

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

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt5, rd, sun)[] = {
    { ".2", c3__lite, j2_mc(Pt5, rd, sun), Tier5_test, u2_none, u2_none },
    { }
  };
  u2_ho_jet
  j2_mcj(Pt5, rd, mul)[] = {
    { ".2", c3__lite, j2_mc(Pt5, rd, mul), Tier5_test, u2_none, u2_none },
    { }
  };
  u2_ho_jet
  j2_mcj(Pt5, rd, div)[] = {
    { ".2", c3__lite, j2_mc(Pt5, rd, div), Tier5_test, u2_none, u2_none },
    { }
  };
  u2_ho_jet
  j2_mcj(Pt5, rd, add)[] = {
    { ".2", c3__lite, j2_mc(Pt5, rd, add), Tier5_test, u2_none, u2_none },
    { }
  };
  u2_ho_jet
  j2_mcj(Pt5, rd, sub)[] = {
    { ".2", c3__lite, j2_mc(Pt5, rd, sub), Tier5_test, u2_none, u2_none },
    { }
  };
  u2_ho_jet
  j2_mcj(Pt5, rd, lte)[] = {
    { ".2", c3__lite, j2_mc(Pt5, rd, lte), Tier5_test, u2_none, u2_none },
    { }
  };
  u2_ho_jet
  j2_mcj(Pt5, rd, lth)[] = {
    { ".2", c3__lite, j2_mc(Pt5, rd, lth), Tier5_test, u2_none, u2_none },
    { }
  };
  u2_ho_jet
  j2_mcj(Pt5, rd, gte)[] = {
    { ".2", c3__lite, j2_mc(Pt5, rd, gte), Tier5_test, u2_none, u2_none },
    { }
  };
  u2_ho_jet
  j2_mcj(Pt5, rd, gth)[] = {
    { ".2", c3__lite, j2_mc(Pt5, rd, gth), Tier5_test, u2_none, u2_none },
    { }
  };

  u2_ho_driver
  j2_mbd(Pt5, rd)[] = {
    { j2_sc(Pt5, rd, sun), j2_mcj(Pt5, rd, sun), 0, 0, u2_none },
    { j2_sc(Pt5, rd, mul), j2_mcj(Pt5, rd, mul), 0, 0, u2_none },
    { j2_sc(Pt5, rd, div), j2_mcj(Pt5, rd, div), 0, 0, u2_none },
    { j2_sc(Pt5, rd, add), j2_mcj(Pt5, rd, add), 0, 0, u2_none },
    { j2_sc(Pt5, rd, sub), j2_mcj(Pt5, rd, sub), 0, 0, u2_none },
    { j2_sc(Pt5, rd, lte), j2_mcj(Pt5, rd, lte), 0, 0, u2_none },
    { j2_sc(Pt5, rd, lth), j2_mcj(Pt5, rd, lth), 0, 0, u2_none },
    { j2_sc(Pt5, rd, gte), j2_mcj(Pt5, rd, gte), 0, 0, u2_none },
    { j2_sc(Pt5, rd, gth), j2_mcj(Pt5, rd, gth), 0, 0, u2_none },
    { }
  };

  u2_ho_driver
  j2_db(Pt5, rd) =
    { j2_sb(Pt5, rd), 0, j2_mbd(Pt5, rd), 0, u2_none };
