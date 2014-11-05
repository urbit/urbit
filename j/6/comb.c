/* j/6/comb.c
**
** This file is in the public domain.
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3_cqf_comb(
                    u3_noun mal,
                    u3_noun buz)
  {
    if ( (u3_none == mal) || (u3_none == buz) ) {
      return u3_none;
    }
    else {
      u3_noun p_mal, q_mal, p_buz, q_buz, pp_buz, pq_buz;

      if ( (c3y == u3_cr_p(mal, 0, &p_mal)) && (0 != p_mal) ) {
        if ( (c3y == u3_cr_p(buz, 0, &p_buz)) && (0 != p_buz) ) {
          return u3nc(
                       0,
                       u3_cqc_peg(p_mal, p_buz));
        }
        else if ( c3y == u3_cr_pq(buz, 2, &p_buz, &q_buz) &&
                  c3y == u3_cr_p(p_buz, 0, &pp_buz) &&
                  c3y == u3_cr_p(q_buz, 0, &pq_buz) )
        {
          return u3nt
            (2,
                   u3nc(0,
                                u3_cqc_peg(p_mal, pp_buz)),
                   u3nc(0,
                                u3_cqc_peg(p_mal, pq_buz)));
        }
        else return u3nt(7, u3k(mal),
                                               u3k(buz));
      }
#if 1
      else if ( (c3y == u3_cr_bush(mal, &p_mal, &q_mal)) &&
                (c3y == u3du(p_mal)) &&
                (c3y == u3du(q_mal)) &&
                (0 == u3h(q_mal)) &&
                (1 == u3t(q_mal)) )
      {
        return u3nt(8, u3k(p_mal),
                                       u3k(buz));
      }
#endif
      else if ( (c3y == u3_cr_p(buz, 0, &p_buz)) &&
                (c3y == u3_cr_sing(1, p_buz)) )
      {
        return u3k(mal);
      }
      else return u3nt(7, u3k(mal),
                                             u3k(buz));
    }
  }
  u3_noun
  u3_cwf_comb(
                   u3_noun cor)
  {
    u3_noun mal, buz;

    if ( c3n == u3_cr_mean(cor, u3_cv_sam_2, &mal, u3_cv_sam_3, &buz, 0) ) {
      return u3_none;
    } else {
      return u3_cqf_comb(mal, buz);
    }
  }
