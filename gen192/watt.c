/* genxxx/watt.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "pit.h"

/* declarations
*/
  /* tier 1
  */
    extern u2_ho_jet j2_mbj(Pt1, add)[];
    extern u2_ho_jet j2_mbj(Pt1, dec)[];
    extern u2_ho_jet j2_mbj(Pt1, div)[];
    extern u2_ho_jet j2_mbj(Pt1, gte)[];
    extern u2_ho_jet j2_mbj(Pt1, gth)[];
    extern u2_ho_jet j2_mbj(Pt1, lte)[];
    extern u2_ho_jet j2_mbj(Pt1, lth)[];
    extern u2_ho_jet j2_mbj(Pt1, mod)[];
    extern u2_ho_jet j2_mbj(Pt1, mul)[];
    extern u2_ho_jet j2_mbj(Pt1, sub)[];

  /* tier 2
  */
    extern u2_ho_jet j2_mbj(Pt2, bind)[];
    extern u2_ho_jet j2_mbj(Pt2, clap)[];
    extern u2_ho_jet j2_mbj(Pt2, drop)[];
    extern u2_ho_jet j2_mbj(Pt2, flop)[];
    extern u2_ho_jet j2_mbj(Pt2, lent)[];
    extern u2_ho_jet j2_mbj(Pt2, levy)[];
    extern u2_ho_jet j2_mbj(Pt2, lien)[];
    extern u2_ho_jet j2_mbj(Pt2, need)[];
    extern u2_ho_jet j2_mbj(Pt2, reel)[];
    extern u2_ho_jet j2_mbj(Pt2, roll)[];
    extern u2_ho_jet j2_mbj(Pt2, skim)[];
    extern u2_ho_jet j2_mbj(Pt2, skip)[];
    extern u2_ho_jet j2_mbj(Pt2, slag)[];
    extern u2_ho_jet j2_mbj(Pt2, snag)[];
    extern u2_ho_jet j2_mbj(Pt2, turn)[];
    extern u2_ho_jet j2_mbj(Pt2, weld)[];

  /* tier 3
  */
    extern u2_ho_jet j2_mbj(Pt3, bex)[];
    extern u2_ho_jet j2_mbj(Pt3, can)[];
    extern u2_ho_jet j2_mbj(Pt3, cap)[];
    extern u2_ho_jet j2_mbj(Pt3, cat)[];
    extern u2_ho_jet j2_mbj(Pt3, con)[];
    extern u2_ho_jet j2_mbj(Pt3, cut)[];
    extern u2_ho_jet j2_mbj(Pt3, dis)[];
    extern u2_ho_jet j2_mbj(Pt3, dor)[];
    extern u2_ho_jet j2_mbj(Pt3, end)[];
    extern u2_ho_jet j2_mbj(Pt3, gor)[];
    extern u2_ho_jet j2_mbj(Pt3, hor)[];
    extern u2_ho_jet j2_mbj(Pt3, lsh)[];
    extern u2_ho_jet j2_mbj(Pt3, mas)[];
    extern u2_ho_jet j2_mbj(Pt3, met)[];
    extern u2_ho_jet j2_mbj(Pt3, mix)[];
    extern u2_ho_jet j2_mbj(Pt3, mug)[];
    extern u2_ho_jet j2_mbj(Pt3, peg)[];
    extern u2_ho_jet j2_mbj(Pt3, rap)[];
    extern u2_ho_jet j2_mbj(Pt3, rip)[];
    extern u2_ho_jet j2_mbj(Pt3, rsh)[];
    extern u2_ho_jet j2_mbj(Pt3, vor)[];

  /* tier 4
  */
    extern u2_ho_driver j2_mbd(Pt4, in)[];
    extern u2_ho_driver j2_mbd(Pt4, by)[];

  /* tier 5
  */
    extern u2_ho_jet j2_mbj(Pt5, cue)[];
    extern u2_ho_jet j2_mbj(Pt5, mat)[];
    extern u2_ho_jet j2_mbj(Pt5, mink)[];
    extern u2_ho_jet j2_mbj(Pt5, jam)[];
    extern u2_ho_jet j2_mbj(Pt5, pfix)[];
    extern u2_ho_jet j2_mbj(Pt5, plug)[];
    extern u2_ho_jet j2_mbj(Pt5, pose)[];
    extern u2_ho_jet j2_mbj(Pt5, rub)[];
    extern u2_ho_jet j2_mbj(Pt5, sfix)[];
    extern u2_ho_jet j2_mbj(Pt5, shax)[];
    extern u2_ho_jet j2_mbj(Pt5, trip)[];

    extern u2_ho_driver j2_mbd(Pt5, bend)[];
    extern u2_ho_driver j2_mbd(Pt5, cold)[];
    extern u2_ho_driver j2_mbd(Pt5, comp)[];
    extern u2_ho_driver j2_mbd(Pt5, cook)[];
    extern u2_ho_driver j2_mbd(Pt5, easy)[];
    extern u2_ho_driver j2_mbd(Pt5, glue)[];
    extern u2_ho_driver j2_mbd(Pt5, here)[];
    extern u2_ho_driver j2_mbd(Pt5, just)[];
    extern u2_ho_driver j2_mbd(Pt5, mask)[];
    extern u2_ho_driver j2_mbd(Pt5, shim)[];
    extern u2_ho_driver j2_mbd(Pt5, stag)[];
    extern u2_ho_driver j2_mbd(Pt5, stew)[];
    extern u2_ho_driver j2_mbd(Pt5, stir)[];

  /* tier 6
  */
    extern u2_ho_jet j2_mbj(Pt6, cell)[];
    extern u2_ho_jet j2_mbj(Pt6, comb)[];
    extern u2_ho_jet j2_mbj(Pt6, cons)[];
    extern u2_ho_jet j2_mbj(Pt6, core)[];
    extern u2_ho_jet j2_mbj(Pt6, cube)[];
    extern u2_ho_jet j2_mbj(Pt6, face)[];
    extern u2_ho_jet j2_mbj(Pt6, fine)[];
    extern u2_ho_jet j2_mbj(Pt6, fitz)[];
    extern u2_ho_jet j2_mbj(Pt6, flan)[];
    extern u2_ho_jet j2_mbj(Pt6, flip)[];
    extern u2_ho_jet j2_mbj(Pt6, flor)[];
    extern u2_ho_jet j2_mbj(Pt6, fork)[];
    extern u2_ho_jet j2_mbj(Pt6, hike)[];
    extern u2_ho_jet j2_mbj(Pt6, look)[];
    extern u2_ho_jet j2_mbj(Pt6, open)[];
    extern u2_ho_jet j2_mbj(Pt6, rake)[];
    extern u2_ho_jet j2_mbj(Pt6, ream)[];
    extern u2_ho_jet j2_mbj(Pt6, vest)[];

    extern u2_ho_jet j2_mbj(Pt6, ap)[];
    extern u2_ho_driver j2_mbd(Pt6, ap)[];

    extern u2_ho_jet j2_mbj(Pt6, ut)[];
    extern u2_ho_driver j2_mbd(Pt6, ut)[];

/* structures
*/
  static u2_ho_driver
    _watt_drivers_0[],
    _watt_drivers_1[],
    _watt_drivers_2[],
    _watt_drivers_3[],
    _watt_drivers_4[],
    _watt_drivers_5[],
    _watt_drivers_6[];

  u2_ho_driver
  j2_da(Pt0) = { j2_sa(Pt0), 0, _watt_drivers_0, 0, u2_none };

  static u2_ho_driver
  _watt_drivers_0[] = {
    { j2_sb(Pt0, a), 0, _watt_drivers_1, 0, u2_none },
    { }
  };

  static u2_ho_driver 
  _watt_drivers_1[] = {
    { j2_sb(Pt1, add), j2_mbj(Pt1, add), 0, 0, u2_none },
    { j2_sb(Pt1, dec), j2_mbj(Pt1, dec), 0, 0, u2_none },
    { j2_sb(Pt1, div), j2_mbj(Pt1, div), 0, 0, u2_none },
    { j2_sb(Pt1, gte), j2_mbj(Pt1, gte), 0, 0, u2_none },
    { j2_sb(Pt1, gth), j2_mbj(Pt1, gth), 0, 0, u2_none },
    { j2_sb(Pt1, lte), j2_mbj(Pt1, lte), 0, 0, u2_none },
    { j2_sb(Pt1, lth), j2_mbj(Pt1, lth), 0, 0, u2_none },
    { j2_sb(Pt1, mod), j2_mbj(Pt1, mod), 0, 0, u2_none },
    { j2_sb(Pt1, mul), j2_mbj(Pt1, mul), 0, 0, u2_none },
    { j2_sb(Pt1, sub), j2_mbj(Pt1, sub), 0, 0, u2_none },
    { j2_sb(Pt1, b), 0, _watt_drivers_2, 0, u2_none },
    { }
  };

  static u2_ho_driver 
  _watt_drivers_2[] = {
    { j2_sb(Pt2, bind), j2_mbj(Pt2, bind), 0, 0, u2_none },
    { j2_sb(Pt2, clap), j2_mbj(Pt2, clap), 0, 0, u2_none },
    { j2_sb(Pt2, drop), j2_mbj(Pt2, drop), 0, 0, u2_none },
    { j2_sb(Pt2, flop), j2_mbj(Pt2, flop), 0, 0, u2_none },
    { j2_sb(Pt2, lent), j2_mbj(Pt2, lent), 0, 0, u2_none },
    { j2_sb(Pt2, levy), j2_mbj(Pt2, levy), 0, 0, u2_none },
    { j2_sb(Pt2, lien), j2_mbj(Pt2, lien), 0, 0, u2_none },
    { j2_sb(Pt2, need), j2_mbj(Pt2, need), 0, 0, u2_none },
    { j2_sb(Pt2, reel), j2_mbj(Pt2, reel), 0, 0, u2_none },
    { j2_sb(Pt2, roll), j2_mbj(Pt2, roll), 0, 0, u2_none },
    { j2_sb(Pt2, skim), j2_mbj(Pt2, skim), 0, 0, u2_none },
    { j2_sb(Pt2, skip), j2_mbj(Pt2, skip), 0, 0, u2_none },
    { j2_sb(Pt2, slag), j2_mbj(Pt2, slag), 0, 0, u2_none },
    { j2_sb(Pt2, snag), j2_mbj(Pt2, snag), 0, 0, u2_none },
    { j2_sb(Pt2, turn), j2_mbj(Pt2, turn), 0, 0, u2_none },
    { j2_sb(Pt2, weld), j2_mbj(Pt2, weld), 0, 0, u2_none },
    { j2_sb(Pt2, c), 0, _watt_drivers_3, 0, u2_none },
    { }
  };

  static u2_ho_driver
  _watt_drivers_3[] = {
    { j2_sb(Pt3, bex), j2_mbj(Pt3, bex), 0, 0, u2_none },
    { j2_sb(Pt3, can), j2_mbj(Pt3, can), 0, 0, u2_none },
    { j2_sb(Pt3, cap), j2_mbj(Pt3, cap), 0, 0, u2_none },
    { j2_sb(Pt3, cat), j2_mbj(Pt3, cat), 0, 0, u2_none },
    { j2_sb(Pt3, con), j2_mbj(Pt3, con), 0, 0, u2_none },
    { j2_sb(Pt3, cut), j2_mbj(Pt3, cut), 0, 0, u2_none },
    { j2_sb(Pt3, dor), j2_mbj(Pt3, dor), 0, 0, u2_none },
    { j2_sb(Pt3, dis), j2_mbj(Pt3, dis), 0, 0, u2_none },
    { j2_sb(Pt3, end), j2_mbj(Pt3, end), 0, 0, u2_none },
    { j2_sb(Pt3, gor), j2_mbj(Pt3, gor), 0, 0, u2_none },
    { j2_sb(Pt3, hor), j2_mbj(Pt3, hor), 0, 0, u2_none },
    { j2_sb(Pt3, lsh), j2_mbj(Pt3, lsh), 0, 0, u2_none },
    { j2_sb(Pt3, mas), j2_mbj(Pt3, mas), 0, 0, u2_none },
    { j2_sb(Pt3, met), j2_mbj(Pt3, met), 0, 0, u2_none },
    { j2_sb(Pt3, mix), j2_mbj(Pt3, mix), 0, 0, u2_none },
    { j2_sb(Pt3, mug), j2_mbj(Pt3, mug), 0, 0, u2_none },
    { j2_sb(Pt3, peg), j2_mbj(Pt3, peg), 0, 0, u2_none },
    { j2_sb(Pt3, rap), j2_mbj(Pt3, rap), 0, 0, u2_none },
    { j2_sb(Pt3, rip), j2_mbj(Pt3, rip), 0, 0, u2_none },
    { j2_sb(Pt3, rsh), j2_mbj(Pt3, rsh), 0, 0, u2_none },
    { j2_sb(Pt3, vor), j2_mbj(Pt3, vor), 0, 0, u2_none },
    { j2_sb(Pt3, d), 0, _watt_drivers_4, 0, u2_none },
    { }
  };

  static u2_ho_driver
  _watt_drivers_4[] = {
    { j2_sb(Pt4, in), 0, j2_mbd(Pt4, in), 0, u2_none },
    { j2_sb(Pt4, by), 0, j2_mbd(Pt4, by), 0, u2_none },
    { j2_sb(Pt4, e), 0, _watt_drivers_5, 0, u2_none },
    { }
  };

  static u2_ho_driver
  _watt_drivers_5[] = {
    { j2_sb(Pt5, bend), 0, j2_mbd(Pt5, bend), 0, u2_none },
    { j2_sb(Pt5, cold), 0, j2_mbd(Pt5, cold), 0, u2_none },
    { j2_sb(Pt5, comp), 0, j2_mbd(Pt5, comp), 0, u2_none },
    { j2_sb(Pt5, cook), 0, j2_mbd(Pt5, cook), 0, u2_none },
    { j2_sb(Pt5, cue), j2_mbj(Pt5, cue), 0, 0, u2_none },
    { j2_sb(Pt5, easy), 0, j2_mbd(Pt5, easy), 0, u2_none },
    { j2_sb(Pt5, glue), 0, j2_mbd(Pt5, glue), 0, u2_none },
    { j2_sb(Pt5, here), 0, j2_mbd(Pt5, here), 0, u2_none },
    { j2_sb(Pt5, jam), j2_mbj(Pt5, jam), 0, 0, u2_none },
    { j2_sb(Pt5, just), 0, j2_mbd(Pt5, just), 0, u2_none },
    { j2_sb(Pt5, mask), 0, j2_mbd(Pt5, mask), 0, u2_none },
    { j2_sb(Pt5, mat), j2_mbj(Pt5, mat), 0, 0, u2_none },
    { j2_sb(Pt5, mink), j2_mbj(Pt5, mink), 0, 0, u2_none },
    { j2_sb(Pt5, pfix), j2_mbj(Pt5, pfix), 0, 0, u2_none },
    { j2_sb(Pt5, plug), j2_mbj(Pt5, plug), 0, 0, u2_none },
    { j2_sb(Pt5, pose), j2_mbj(Pt5, pose), 0, 0, u2_none },
    { j2_sb(Pt5, rub), j2_mbj(Pt5, rub), 0, 0, u2_none },
    { j2_sb(Pt5, sfix), j2_mbj(Pt5, sfix), 0, 0, u2_none },
    { j2_sb(Pt5, shax), j2_mbj(Pt5, shax), 0, 0, u2_none },
    { j2_sb(Pt5, shim), 0, j2_mbd(Pt5, shim), 0, u2_none },
    { j2_sb(Pt5, stag), 0, j2_mbd(Pt5, stag), 0, u2_none },
    { j2_sb(Pt5, stew), 0, j2_mbd(Pt5, stew), 0, u2_none },
    { j2_sb(Pt5, stir), 0, j2_mbd(Pt5, stir), 0, u2_none },
    { j2_sb(Pt5, trip), j2_mbj(Pt5, trip), 0, 0, u2_none },
    { j2_sb(Pt5, f), 0, _watt_drivers_6, 0, u2_none },
    { }
  };

  static u2_ho_driver
  _watt_drivers_6[] = {
    { j2_sb(Pt6, cell), j2_mbj(Pt6, cell), 0, 0, u2_none },
    { j2_sb(Pt6, comb), j2_mbj(Pt6, comb), 0, 0, u2_none },
    { j2_sb(Pt6, cons), j2_mbj(Pt6, cons), 0, 0, u2_none },
    { j2_sb(Pt6, core), j2_mbj(Pt6, core), 0, 0, u2_none },
    { j2_sb(Pt6, cube), j2_mbj(Pt6, cube), 0, 0, u2_none },
    { j2_sb(Pt6, face), j2_mbj(Pt6, face), 0, 0, u2_none },
    { j2_sb(Pt6, fine), j2_mbj(Pt6, fine), 0, 0, u2_none },
    { j2_sb(Pt6, fitz), j2_mbj(Pt6, fitz), 0, 0, u2_none },
    { j2_sb(Pt6, flan), j2_mbj(Pt6, flan), 0, 0, u2_none },
    { j2_sb(Pt6, flip), j2_mbj(Pt6, flip), 0, 0, u2_none },
    { j2_sb(Pt6, flor), j2_mbj(Pt6, flor), 0, 0, u2_none },
    { j2_sb(Pt6, fork), j2_mbj(Pt6, fork), 0, 0, u2_none },
    { j2_sb(Pt6, hike), j2_mbj(Pt6, hike), 0, 0, u2_none },
    { j2_sb(Pt6, look), j2_mbj(Pt6, look), 0, 0, u2_none },
//    { j2_sb(Pt6, ream), j2_mbj(Pt6, ream), 0, 0, u2_none },
//    { j2_sb(Pt6, vest), j2_mbj(Pt6, vest), 0, 0, u2_none },

    { j2_sb(Pt6, ap), j2_mbj(Pt6, ap), j2_mbd(Pt6, ap), 0, u2_none },
    { j2_sb(Pt6, ut), j2_mbj(Pt6, ut), j2_mbd(Pt6, ut), 0, u2_none },
    { }
  };
