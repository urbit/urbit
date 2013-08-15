/* j/watt.c
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
    extern u2_ho_jet j2_mbj(Pit, sort)[];
    extern u2_ho_jet j2_mbj(Pit, turn)[];
    extern u2_ho_jet j2_mbj(Pit, weld)[];

  /* tier 3
  */
    extern u2_ho_jet j2_mbj(Pit, cat)[];
    extern u2_ho_jet j2_mbj(Pit, con)[];
    extern u2_ho_jet j2_mbj(Pit, cut)[];
    extern u2_ho_jet j2_mbj(Pit, dis)[];
    extern u2_ho_jet j2_mbj(Pit, end)[];
    extern u2_ho_jet j2_mbj(Pit, lsh)[];
    extern u2_ho_jet j2_mbj(Pit, met)[];
    extern u2_ho_jet j2_mbj(Pit, mix)[];
    extern u2_ho_jet j2_mbj(Pit, mug)[];
    extern u2_ho_jet j2_mbj(Pit, rap)[];
    extern u2_ho_jet j2_mbj(Pit, rip)[];
    extern u2_ho_jet j2_mbj(Pit, rsh)[];

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
      { j2_sb(Pit, sort), j2_mbj(Pit, sort), 0, 0, u2_none },
      { j2_sb(Pit, turn), j2_mbj(Pit, turn), 0, 0, u2_none },
      { j2_sb(Pit, weld), j2_mbj(Pit, weld), 0, 0, u2_none },

    /* tier 3
    */
      { j2_sb(Pit, cat), j2_mbj(Pit, cat), 0, 0, u2_none },
      { j2_sb(Pit, con), j2_mbj(Pit, con), 0, 0, u2_none },
      { j2_sb(Pit, cut), j2_mbj(Pit, cut), 0, 0, u2_none },
      { j2_sb(Pit, dis), j2_mbj(Pit, dis), 0, 0, u2_none },
      { j2_sb(Pit, end), j2_mbj(Pit, end), 0, 0, u2_none },
      { j2_sb(Pit, lsh), j2_mbj(Pit, lsh), 0, 0, u2_none },
      { j2_sb(Pit, met), j2_mbj(Pit, met), 0, 0, u2_none },
      { j2_sb(Pit, mix), j2_mbj(Pit, mix), 0, 0, u2_none },
      { j2_sb(Pit, mug), j2_mbj(Pit, mug), 0, 0, u2_none },
      { j2_sb(Pit, rap), j2_mbj(Pit, rap), 0, 0, u2_none },
      { j2_sb(Pit, rip), j2_mbj(Pit, rip), 0, 0, u2_none },
      { j2_sb(Pit, rsh), j2_mbj(Pit, rsh), 0, 0, u2_none },
    { }
  };

  u2_ho_driver
  j2_da(Pit) = 
    { j2_sa(Pit), 0, _watt_drivers, 0, u2_none };
