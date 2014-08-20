/* j/6/comb.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mbc(Pt6, comb)(
                    u2_noun mal,                                  //  retain
                    u2_noun buz)                                  //  retain
  {
    if ( (u2_none == mal) || (u2_none == buz) ) {
      return u2_none;
    }
    else {
      u2_noun p_mal, q_mal, p_buz, q_buz, pp_buz, pq_buz;

      if ( (u2_yes == u2_cr_p(mal, 0, &p_mal)) && (0 != p_mal) ) {
        if ( (u2_yes == u2_cr_p(buz, 0, &p_buz)) && (0 != p_buz) ) {
          return u2nc(
                       0,
                       j2_mbc(Pt3, peg)(p_mal, p_buz));
        }
        else if ( u2_yes == u2_cr_pq(buz, 2, &p_buz, &q_buz) &&
                  u2_yes == u2_cr_p(p_buz, 0, &pp_buz) &&
                  u2_yes == u2_cr_p(q_buz, 0, &pq_buz) )
        {
          return u2nt
            (2,
                   u2nc(0,
                                j2_mbc(Pt3, peg)(p_mal, pp_buz)),
                   u2nc(0,
                                j2_mbc(Pt3, peg)(p_mal, pq_buz)));
        }
        else return u2nt(7, u2k(mal),
                                               u2k(buz));
      }
#if 1
      else if ( (u2_yes == u2_cr_bush(mal, &p_mal, &q_mal)) &&
                (u2_yes == u2du(p_mal)) &&
                (u2_yes == u2du(q_mal)) &&
                (0 == u2h(q_mal)) &&
                (1 == u2t(q_mal)) )
      {
        return u2nt(8, u2k(p_mal),
                                       u2k(buz));
      }
#endif
      else if ( (u2_yes == u2_cr_p(buz, 0, &p_buz)) &&
                (u2_yes == u2_cr_sing(1, p_buz)) )
      {
        return u2k(mal);
      }
      else return u2nt(7, u2k(mal),
                                             u2k(buz));
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, comb)(
                   u2_noun cor)                                   //  retain
  {
    u2_noun mal, buz;

    if ( u2_no == u2_cr_mean(cor, u2_cv_sam_2, &mal, u2_cv_sam_3, &buz, 0) ) {
      return u2_none;
    } else {
      return j2_mbc(Pt6, comb)(mal, buz);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt6, comb)[] = {
    { ".2", c3__lite, j2_mb(Pt6, comb), Tier6_a, u2_none, u2_none },
    { }
  };
