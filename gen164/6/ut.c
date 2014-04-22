/* j/6/ut.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* debugging hacks
*/
#if 0
  static void
  _dump_wall(u2_wire     wir_r,
             const c3_c* cap_c,
             u2_noun     wal)
  {
    if ( cap_c ) printf("%s:\n", cap_c);

    while ( u2_nul != wal ) {
      u2_noun h_wal = u2_h(wal);

      while ( u2_nul != h_wal ) {
        c3_assert(u2_h(h_wal) < 128);

        putchar(u2_h(h_wal));
        h_wal = u2_t(h_wal);
      }
      putchar(10);
      wal = u2_t(wal);
    }
  }
#endif

  //  duck: create a duck core for mean.
  //
  u2_noun                                                         //  produce
  j2_mcy(Pt6, ut, duck)(u2_wire     wir_r,
                        u2_noun     van,                          //  retain
                        u2_noun     typ)                          //  retain
  {
    u2_noun von = u2_bn_molt(wir_r, van, u2_cv_sam, typ, 0);
    u2_noun ret = u2_bn_hook(wir_r, von, "dune");

    u2_rz(wir_r, von);
    return ret;
  }

  //  dung: create a dunk core for mean (noun caption)
  //
  u2_noun                                                         //  produce
  j2_mcy(Pt6, ut, dung)(u2_wire     wir_r,
                        u2_noun     van,                          //  retain
                        u2_noun     paz,                          //  retain
                        u2_noun     typ)                          //  retain
  {
    u2_noun von = u2_bn_molt(wir_r, van, u2_cv_sam, typ, 0);
    u2_noun duq = u2_bn_hook(wir_r, von, "dunk");
    u2_noun ret = u2_bn_molt(wir_r, duq, u2_cv_sam, paz, 0);

    u2_rz(wir_r, duq);
    u2_rz(wir_r, von);
    return ret;
  }

  //  dunq: create a dunk core for mean
  //
  u2_noun                                                         //  produce
  j2_mcy(Pt6, ut, dunq)(u2_wire     wir_r,
                        u2_noun     van,                          //  retain
                        const c3_c* paz_c,                        //  retain
                        u2_noun     typ)                          //  retain
  {
    u2_noun von = u2_bn_molt(wir_r, van, u2_cv_sam, typ, 0);
    u2_noun duq = u2_bn_hook(wir_r, von, "dunk");
    u2_noun paz = u2_bn_string(wir_r, paz_c);
    u2_noun ret = u2_bn_molt(wir_r, duq, u2_cv_sam, paz, 0);

    u2_rz(wir_r, paz);
    u2_rz(wir_r, duq);
    u2_rz(wir_r, von);
    return ret;
  }

  //  shew: create a show core for mean
  //
  u2_noun                                                         //  produce
  j2_mcy(Pt6, ut, shew)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun mol)                              //  submit
  {
    u2_noun sho = u2_bn_hook(wir_r, van, "show");
    u2_noun ret = u2_bn_molt(wir_r, sho, u2_cv_sam, mol, 0);

    u2_rz(wir_r, sho);
    u2_rz(wir_r, mol);
    return ret;
  }

  //  shep: show with caption and style
  //
  u2_noun                                                         //  produce
  j2_mcy(Pt6, ut, shep)(u2_wire     wir_r,
                        u2_noun     van,                          //  retain
                        const c3_c* paz_c,                        //  retain
                        u2_noun     sty,                          //  retain
                        u2_noun     mol)                          //  submit
  {
    return j2_mcy(Pt6, ut, shew)
      (wir_r, van,
              u2_bc
                (wir_r, u2_bc(wir_r, 'c', u2_bn_string(wir_r, paz_c)),
                        u2_bc(wir_r, u2_rx(wir_r, sty), mol)));
  }

/* declarations
*/
  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, repo)(u2_wire wir_r,
                       u2_noun cor);                              //  retain
  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, burn)(u2_wire wir_r,
                       u2_noun cor);                              //  retain
  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, moot)(u2_wire wir_r,
                       u2_noun cor);                              //  retain

  u2_weak
  j2_mck(Pt6, ut, moot)(u2_wire wir_r,
                        u2_noun cor);                             //  retain

  extern u2_ho_jet j2_mcj(Pt6, ut, busk)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, bust)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, crop)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, cull)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, find)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, fink)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, fino)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, fire)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, firm)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, fish)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, fuse)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, gain)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, heal)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, lose)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, mint)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, mull)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, nest)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, park)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, peek)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, play)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, rest)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, seek)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, sift)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, tack)[];
  extern u2_ho_jet j2_mcj(Pt6, ut, tock)[];

/* structures
*/
  u2_ho_driver
  j2_mbd(Pt6, ut)[] = {
    { j2_sc(Pt6, ut, busk), j2_mcj(Pt6, ut, busk), 0, 0, u2_none },
    { j2_sc(Pt6, ut, bust), j2_mcj(Pt6, ut, bust), 0, 0, u2_none },
    { j2_sc(Pt6, ut, crop), j2_mcj(Pt6, ut, crop), 0, 0, u2_none },
    { j2_sc(Pt6, ut, cull), j2_mcj(Pt6, ut, cull), 0, 0, u2_none },
    { j2_sc(Pt6, ut, find), j2_mcj(Pt6, ut, find), 0, 0, u2_none },
    { j2_sc(Pt6, ut, fino), j2_mcj(Pt6, ut, fino), 0, 0, u2_none },
    { j2_sc(Pt6, ut, fire), j2_mcj(Pt6, ut, fire), 0, 0, u2_none },
    { j2_sc(Pt6, ut, firm), j2_mcj(Pt6, ut, firm), 0, 0, u2_none },
    { j2_sc(Pt6, ut, fish), j2_mcj(Pt6, ut, fish), 0, 0, u2_none },
    { j2_sc(Pt6, ut, fuse), j2_mcj(Pt6, ut, fuse), 0, 0, u2_none },
    { j2_sc(Pt6, ut, gain), j2_mcj(Pt6, ut, gain), 0, 0, u2_none },
    { j2_sc(Pt6, ut, heal), j2_mcj(Pt6, ut, heal), 0, 0, u2_none },
    { j2_sc(Pt6, ut, lose), j2_mcj(Pt6, ut, lose), 0, 0, u2_none },
    { j2_sc(Pt6, ut, mint), j2_mcj(Pt6, ut, mint), 0, 0, u2_none },
    { j2_sc(Pt6, ut, mull), j2_mcj(Pt6, ut, mull), 0, 0, u2_none },
    { j2_sc(Pt6, ut, nest), j2_mcj(Pt6, ut, nest), 0, 0, u2_none },
    { j2_sc(Pt6, ut, park), j2_mcj(Pt6, ut, park), 0, 0, u2_none },
    { j2_sc(Pt6, ut, peek), j2_mcj(Pt6, ut, peek), 0, 0, u2_none },
    { j2_sc(Pt6, ut, play), j2_mcj(Pt6, ut, play), 0, 0, u2_none },
    { j2_sc(Pt6, ut, rest), j2_mcj(Pt6, ut, rest), 0, 0, u2_none },
    { j2_sc(Pt6, ut, seek), j2_mcj(Pt6, ut, seek), 0, 0, u2_none },
    { j2_sc(Pt6, ut, sift), j2_mcj(Pt6, ut, sift), 0, 0, u2_none },
    { j2_sc(Pt6, ut, tack), j2_mcj(Pt6, ut, tack), 0, 0, u2_none },
    { j2_sc(Pt6, ut, tock), j2_mcj(Pt6, ut, tock), 0, 0, u2_none },
    { }
  };

  u2_ho_jet
  j2_mbj(Pt6, ut)[] = {
    { "burn",
      c3__hevy,
      j2_mc(Pt6, ut, burn),
      Tier6_b,
      u2_none, u2_none },
    { "moot",
      c3__hevy,
      j2_mc(Pt6, ut, moot),
      Tier6_b_memo,
      u2_none, u2_none,
      j2_mck(Pt6, ut, moot), c3__moot },
    { "repo", c3__hevy, j2_mc(Pt6, ut, repo), Tier6_b, u2_none, u2_none },
    { }
  };

  u2_ho_driver
  j2_db(Pt6, ut) =
    { j2_sb(Pt6, ut), j2_mbj(Pt6, ut), j2_mbd(Pt6, ut), 0, u2_none };
