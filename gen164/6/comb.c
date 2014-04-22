/* j/6/comb.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mbc(Pt6, comb)(u2_wire wir_r,
                    u2_noun mal,                                  //  retain
                    u2_noun buz)                                  //  retain
  {
    if ( (u2_none == mal) || (u2_none == buz) ) {
      return u2_none;
    }
    else {
      u2_noun p_mal, q_mal, p_buz, q_buz, pp_buz, pq_buz;

      if ( (u2_yes == u2_as_p(mal, u2_nock_0, &p_mal)) && (_0 != p_mal) ) {
        if ( (u2_yes == u2_as_p(buz, u2_nock_0, &p_buz)) && (_0 != p_buz) ) {
          return u2_rc(wir_r,
                       u2_nock_0,
                       j2_mbc(Pt3, peg)(wir_r, p_mal, p_buz));
        }
        else if ( u2_yes == u2_as_pq(buz, u2_nock_2, &p_buz, &q_buz) &&
                  u2_yes == u2_as_p(p_buz, u2_nock_0, &pp_buz) &&
                  u2_yes == u2_as_p(q_buz, u2_nock_0, &pq_buz) )
        {
          return u2_rt
            (wir_r, u2_nock_2,
                   u2_rc(wir_r, u2_nock_0,
                                j2_mbc(Pt3, peg)(wir_r, p_mal, pp_buz)),
                   u2_rc(wir_r, u2_nock_0,
                                j2_mbc(Pt3, peg)(wir_r, p_mal, pq_buz)));
        }
        else return u2_rt(wir_r, u2_nock_7, u2_rx(wir_r, mal),
                                               u2_rx(wir_r, buz));
      }
#if 1
      else if ( (u2_yes == u2_as_bush(mal, &p_mal, &q_mal)) &&
                (u2_yes == u2_dust(p_mal)) &&
                (u2_yes == u2_dust(q_mal)) &&
                (_0 == u2_h(q_mal)) &&
                (_1 == u2_t(q_mal)) )
      {
        return u2_rt(wir_r, u2_nock_8, u2_rx(wir_r, p_mal),
                                       u2_rx(wir_r, buz));
      }
#endif
      else if ( (u2_yes == u2_as_p(buz, u2_nock_0, &p_buz)) &&
                (u2_yes == u2_sing(_1, p_buz)) )
      {
        return u2_rx(wir_r, mal);
      }
      else return u2_rt(wir_r, u2_nock_7, u2_rx(wir_r, mal),
                                             u2_rx(wir_r, buz));
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, comb)(u2_wire wir_r,
                   u2_noun cor)                                   //  retain
  {
    u2_noun mal, buz;

    if ( u2_no == u2_mean(cor, u2_cv_sam_2, &mal, u2_cv_sam_3, &buz, 0) ) {
      return u2_none;
    } else {
      return j2_mbc(Pt6, comb)(wir_r, mal, buz);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt6, comb)[] = {
    { ".2", c3__lite, j2_mb(Pt6, comb), Tier6_a, u2_none, u2_none },
    { }
  };
