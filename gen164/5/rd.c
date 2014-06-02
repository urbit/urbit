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
  extern u2_ho_jet j2_mcj(Pt5, rd, mul)[];

/* functions
*/
  u2_weak
  j2_mcd(Pt5, rd, mul)(u2_wire wir_r,
                        u2_atom a,
                        u2_atom b)
  {
    union doub c, d, e;
    c.c = u2_chub(0, a);
    d.c = u2_chub(0, b);
    e.d = c.d * d.d;
      
    return u2_ci_chubs(1, &e.c);
  }

  u2_weak
  j2_mc(Pt5, rd, mul)(u2_wire wir_r,
                       u2_noun cor)
  {
    u2_noun a, b;

    if ( u2_no == u2_mean(cor, u2_cv_sam_2, &a, u2_cv_sam_3, &b, 0) ||
         u2_no == u2_stud(a) ||
         u2_no == u2_stud(b) )
    {
      return u2_bl_bail(wir_r, c3__exit);
    }
    else {
      return j2_mcd(Pt5, rd, mul)(wir_r, a, b);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt5, rd, mul)[] = {
    { ".2", c3__lite, j2_mc(Pt5, rd, mul), Tier5_test, u2_none, u2_none },
    { }
  };

  u2_ho_driver
  j2_mbd(Pt5, rd)[] = {
    { j2_sc(Pt5, rd, mul), j2_mcj(Pt5, rd, mul), 0, 0, u2_none },
    { }
  };

  u2_ho_driver
  j2_db(Pt5, rd) =
    { j2_sb(Pt5, rd), 0, j2_mbd(Pt5, rd), 0, u2_none };
