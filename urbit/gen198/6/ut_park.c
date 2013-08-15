/* j/6/ut_park.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_flag                                                         //  transfer
  j2_mcx(Pt6, ut, park)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun way,                              //  retain
                        u2_atom axe)                              //  retain
  {
    if ( u2_no == u2_dust(sut) || c3__core != u2_h(sut) ) {
      return u2_bl_bail(wir_r, c3__fail);
    }
    // else if ( u2_no == u2_bn_hook(wir_r, van, "vet") ) {
    else if ( u2_no == u2_frag(j2_ut_van_vet, van) ) {
      return u2_yes;
    }
    else {
      u2_noun p_sut, q_sut, pq_sut;

      u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
      u2_bi_cell(wir_r, q_sut, &pq_sut, 0);

      if ( c3__read == way ) {
        switch ( pq_sut ) {
          default: return u2_bl_bail(wir_r, c3__fail);

          case c3__gold: return u2_yes;
          case c3__lead: return u2_no;
          case c3__iron: return u2_no;
          case c3__zinc: return 
                          u2_and(u2_not(u2_sing(_1, axe)),
                                 u2_sing(_2, j2_mbc(Pt3, cap)(wir_r, axe)));
        }
      }
      else if ( c3__rite == way ) {
        switch ( pq_sut ) {
          default: return u2_bl_bail(wir_r, c3__fail);

          case c3__gold: return u2_yes;
          case c3__lead: return u2_no;
          case c3__iron: return 
                          u2_and(u2_not(u2_sing(_1, axe)),
                                 u2_sing(_2, j2_mbc(Pt3, cap)(wir_r, axe)));
          case c3__zinc: return u2_no;
        }
      }
      else return u2_bl_bail(wir_r, c3__fail);
    }
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, park)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, park)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, way, axe, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, 
                                u2_cw_sam_2, &way, 
                                u2_cw_sam_3, &axe,
                                0)) ||
         (u2_no == u2_stud(axe)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, park)(wir_r, van, sut, way, axe);
    }
  }

  u2_flag
  j2_mci(Pt6, ut, park)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun way,                              //  retain
                        u2_noun axe)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "park");

    if ( u2_none == hoc ) {
      c3_assert(!"register park");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cw_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, 
                                      u2_cw_sam_2, u2_rx(wir_r, way), 
                                      u2_cw_sam_3, u2_rx(wir_r, axe), 
                                      0);

      if ( (u2_none == j2_mcj(Pt6, ut, park)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, park)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, park)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun axe)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, park)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pt6, ut, park)(wir_r, van, sut, way, axe);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, park)(wir_r, van, sut, way, axe);
      fol = u2_t(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      if ( u2_none == pro ) return u2_bl_bail(wir_r, c3__fail);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, park)[] = {
    { ".3", c3__hevy, j2_mc(Pt6, ut, park), Tier6_b, u2_none, u2_none },
    { }
  };
