/* gen?/watt.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "pit.h"

/* declarations
*/
  /* tier 1
  */
    extern u2_ho_jet j2_mbj(Pit, add)[];
    extern u2_ho_jet j2_mbj(Pit, bex)[];
    extern u2_ho_jet j2_mbj(Pit, dec)[];
    extern u2_ho_jet j2_mbj(Pit, div)[];
    extern u2_ho_jet j2_mbj(Pit, gte)[];
    extern u2_ho_jet j2_mbj(Pit, gth)[];
    extern u2_ho_jet j2_mbj(Pit, lte)[];
    extern u2_ho_jet j2_mbj(Pit, lth)[];
    extern u2_ho_jet j2_mbj(Pit, mod)[];
    extern u2_ho_jet j2_mbj(Pit, mul)[];
    extern u2_ho_jet j2_mbj(Pit, sub)[];

  /* tier 2
  */
    extern u2_ho_jet j2_mbj(Pit, bind)[];
    extern u2_ho_jet j2_mbj(Pit, clap)[];
    extern u2_ho_jet j2_mbj(Pit, drop)[];
    extern u2_ho_jet j2_mbj(Pit, flop)[];
    extern u2_ho_jet j2_mbj(Pit, lent)[];
    extern u2_ho_jet j2_mbj(Pit, levy)[];
    extern u2_ho_jet j2_mbj(Pit, lien)[];
    extern u2_ho_jet j2_mbj(Pit, need)[];
    extern u2_ho_jet j2_mbj(Pit, reel)[];
    extern u2_ho_jet j2_mbj(Pit, roll)[];
    extern u2_ho_jet j2_mbj(Pit, skim)[];
    extern u2_ho_jet j2_mbj(Pit, skip)[];
    extern u2_ho_jet j2_mbj(Pit, snag)[];
    extern u2_ho_jet j2_mbj(Pit, turn)[];
    extern u2_ho_jet j2_mbj(Pit, weld)[];

  /* tier 3
  */
    extern u2_ho_jet j2_mbj(Pit, cap)[];
    extern u2_ho_jet j2_mbj(Pit, cat)[];
    extern u2_ho_jet j2_mbj(Pit, con)[];
    extern u2_ho_jet j2_mbj(Pit, cut)[];
    extern u2_ho_jet j2_mbj(Pit, dis)[];
    extern u2_ho_jet j2_mbj(Pit, dor)[];
    extern u2_ho_jet j2_mbj(Pit, end)[];
    extern u2_ho_jet j2_mbj(Pit, gor)[];
    extern u2_ho_jet j2_mbj(Pit, lsh)[];
    extern u2_ho_jet j2_mbj(Pit, mas)[];
    extern u2_ho_jet j2_mbj(Pit, met)[];
    extern u2_ho_jet j2_mbj(Pit, mix)[];
    extern u2_ho_jet j2_mbj(Pit, mug)[];
    extern u2_ho_jet j2_mbj(Pit, peg)[];
    extern u2_ho_jet j2_mbj(Pit, rap)[];
    extern u2_ho_jet j2_mbj(Pit, rip)[];
    extern u2_ho_jet j2_mbj(Pit, rsh)[];
    extern u2_ho_jet j2_mbj(Pit, vor)[];

  /* tier 4
  */
    extern u2_ho_driver j2_mbd(Pit, in)[];
    extern u2_ho_driver j2_mbd(Pit, by)[];

  /* tier 5
  */
    extern u2_ho_jet j2_mbj(Pit, to)[];
    extern u2_ho_driver j2_mbd(Pit, to)[];

  /* tier 6
  */
    extern u2_ho_jet j2_mbj(Pit, cell)[];
    extern u2_ho_jet j2_mbj(Pit, comb)[];
    extern u2_ho_jet j2_mbj(Pit, cons)[];
    extern u2_ho_jet j2_mbj(Pit, core)[];
    extern u2_ho_jet j2_mbj(Pit, face)[];
    extern u2_ho_jet j2_mbj(Pit, flan)[];
    extern u2_ho_jet j2_mbj(Pit, flip)[];
    extern u2_ho_jet j2_mbj(Pit, flor)[];
    extern u2_ho_jet j2_mbj(Pit, fork)[];
    extern u2_ho_jet j2_mbj(Pit, hack)[];
    extern u2_ho_jet j2_mbj(Pit, hike)[];
    extern u2_ho_jet j2_mbj(Pit, look)[];
    extern u2_ho_jet j2_mbj(Pit, open)[];
    extern u2_ho_jet j2_mbj(Pit, rake)[];
    extern u2_ho_jet j2_mbj(Pit, ream)[];

    extern u2_ho_jet j2_mbj(Pit, ap)[];
    extern u2_ho_driver j2_mbd(Pit, ap)[];

    extern u2_ho_jet j2_mbj(Pit, ut)[];
    extern u2_ho_driver j2_mbd(Pit, ut)[];

/* structures
*/
  static u2_ho_driver 
  _watt_drivers[] = {
    /* tier 1
    */
      { j2_sb(Pit, add), j2_mbj(Pit, add), 0, 0, u2_none },
      { j2_sb(Pit, bex), j2_mbj(Pit, bex), 0, 0, u2_none },
      { j2_sb(Pit, dec), j2_mbj(Pit, dec), 0, 0, u2_none },
      { j2_sb(Pit, div), j2_mbj(Pit, div), 0, 0, u2_none },
      { j2_sb(Pit, gte), j2_mbj(Pit, gte), 0, 0, u2_none },
      { j2_sb(Pit, gth), j2_mbj(Pit, gth), 0, 0, u2_none },
      { j2_sb(Pit, lte), j2_mbj(Pit, lte), 0, 0, u2_none },
      { j2_sb(Pit, lth), j2_mbj(Pit, lth), 0, 0, u2_none },
      { j2_sb(Pit, mod), j2_mbj(Pit, mod), 0, 0, u2_none },
      { j2_sb(Pit, mul), j2_mbj(Pit, mul), 0, 0, u2_none },
      { j2_sb(Pit, sub), j2_mbj(Pit, sub), 0, 0, u2_none },

    /* tier 2
    */
      { j2_sb(Pit, bind), j2_mbj(Pit, bind), 0, 0, u2_none },
      { j2_sb(Pit, clap), j2_mbj(Pit, clap), 0, 0, u2_none },
      { j2_sb(Pit, drop), j2_mbj(Pit, drop), 0, 0, u2_none },
      { j2_sb(Pit, flop), j2_mbj(Pit, flop), 0, 0, u2_none },
      { j2_sb(Pit, lent), j2_mbj(Pit, lent), 0, 0, u2_none },
      { j2_sb(Pit, levy), j2_mbj(Pit, levy), 0, 0, u2_none },
      { j2_sb(Pit, lien), j2_mbj(Pit, lien), 0, 0, u2_none },
      { j2_sb(Pit, need), j2_mbj(Pit, need), 0, 0, u2_none },
      { j2_sb(Pit, reel), j2_mbj(Pit, reel), 0, 0, u2_none },
      { j2_sb(Pit, roll), j2_mbj(Pit, roll), 0, 0, u2_none },
      { j2_sb(Pit, skim), j2_mbj(Pit, skim), 0, 0, u2_none },
      { j2_sb(Pit, skip), j2_mbj(Pit, skip), 0, 0, u2_none },
      { j2_sb(Pit, snag), j2_mbj(Pit, snag), 0, 0, u2_none },
      { j2_sb(Pit, turn), j2_mbj(Pit, turn), 0, 0, u2_none },
      { j2_sb(Pit, weld), j2_mbj(Pit, weld), 0, 0, u2_none },

    /* tier 3
    */
      { j2_sb(Pit, cap), j2_mbj(Pit, cap), 0, 0, u2_none },
      { j2_sb(Pit, cat), j2_mbj(Pit, cat), 0, 0, u2_none },
      { j2_sb(Pit, con), j2_mbj(Pit, con), 0, 0, u2_none },
      { j2_sb(Pit, cut), j2_mbj(Pit, cut), 0, 0, u2_none },
      { j2_sb(Pit, dor), j2_mbj(Pit, dor), 0, 0, u2_none },
      { j2_sb(Pit, dis), j2_mbj(Pit, dis), 0, 0, u2_none },
      { j2_sb(Pit, end), j2_mbj(Pit, end), 0, 0, u2_none },
      { j2_sb(Pit, gor), j2_mbj(Pit, gor), 0, 0, u2_none },
      { j2_sb(Pit, lsh), j2_mbj(Pit, lsh), 0, 0, u2_none },
      { j2_sb(Pit, mas), j2_mbj(Pit, mas), 0, 0, u2_none },
      { j2_sb(Pit, met), j2_mbj(Pit, met), 0, 0, u2_none },
      { j2_sb(Pit, mix), j2_mbj(Pit, mix), 0, 0, u2_none },
      { j2_sb(Pit, mug), j2_mbj(Pit, mug), 0, 0, u2_none },
      { j2_sb(Pit, peg), j2_mbj(Pit, peg), 0, 0, u2_none },
      { j2_sb(Pit, rap), j2_mbj(Pit, rap), 0, 0, u2_none },
      { j2_sb(Pit, rip), j2_mbj(Pit, rip), 0, 0, u2_none },
      { j2_sb(Pit, rsh), j2_mbj(Pit, rsh), 0, 0, u2_none },
      { j2_sb(Pit, vor), j2_mbj(Pit, vor), 0, 0, u2_none },

    /* tier 4
    */
      { j2_sb(Pit, in), 0, j2_mbd(Pit, in), 0, u2_none },
      { j2_sb(Pit, by), 0, j2_mbd(Pit, by), 0, u2_none },

    /* tier 5
    */
      { j2_sb(Pit, to), j2_mbj(Pit, to), j2_mbd(Pit, to), 0, u2_none },

    /* tier 6
    */
      { j2_sb(Pit, cell), j2_mbj(Pit, cell), 0, 0, u2_none },
      { j2_sb(Pit, comb), j2_mbj(Pit, comb), 0, 0, u2_none },
      { j2_sb(Pit, cons), j2_mbj(Pit, cons), 0, 0, u2_none },
      { j2_sb(Pit, core), j2_mbj(Pit, core), 0, 0, u2_none },
      { j2_sb(Pit, face), j2_mbj(Pit, face), 0, 0, u2_none },
      { j2_sb(Pit, flan), j2_mbj(Pit, flan), 0, 0, u2_none },
      { j2_sb(Pit, flip), j2_mbj(Pit, flip), 0, 0, u2_none },
      { j2_sb(Pit, flor), j2_mbj(Pit, flor), 0, 0, u2_none },
      { j2_sb(Pit, fork), j2_mbj(Pit, fork), 0, 0, u2_none },
      { j2_sb(Pit, hack), j2_mbj(Pit, hack), 0, 0, u2_none },
      { j2_sb(Pit, hike), j2_mbj(Pit, hike), 0, 0, u2_none },
      { j2_sb(Pit, look), j2_mbj(Pit, look), 0, 0, u2_none },
      { j2_sb(Pit, ream), j2_mbj(Pit, ream), 0, 0, u2_none },

      { j2_sb(Pit, ap), j2_mbj(Pit, ap), j2_mbd(Pit, ap), 0, u2_none },
      { j2_sb(Pit, ut), j2_mbj(Pit, ut), j2_mbd(Pit, ut), 0, u2_none } 
  };

  u2_ho_driver
  j2_da(Pit) = 
    { j2_sa(Pit), 0, _watt_drivers, 0, u2_none };
