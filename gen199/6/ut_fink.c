/* j/6/fink.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, fink)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dep,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun cog)                              //  retain
  {
    u2_noun dun = j2_mcy(Pt6, ut, dunq)(wir_r, van, "type", sut);
    u2_noun nuc = (u2_blip == cog) 
      ? j2_mcy(Pt6, ut, shew)
          (wir_r, van,
                  u2_bc
                    (wir_r, u2_bc(wir_r, 'c', u2_bn_string(wir_r, "feature")),
                            9509))
      :  j2_mcy(Pt6, ut, shep)
          (wir_r, van, "feature", 'a', u2_rx(wir_r, cog));
    u2_noun pro;

    u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, dun));
    u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, nuc));
    {
      u2_noun hoq = j2_mcy(Pt6, ut, find)(wir_r, van, sut, dep, way, cog);
      u2_noun fin = u2_t(hoq);

      if ( u2_nul == fin ) {
        return u2_bl_error(wir_r, "find-none");
      }
      else {
        pro = u2_rx(wir_r, u2_t(fin));
        u2_rl_lose(wir_r, hoq);
      }
    }
    u2_bl_drop(wir_r);
    u2_bl_drop(wir_r);

    return pro;
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, fink)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, fink)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, dep, way, cog, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, 
                                u2_cw_sam_2, &dep, 
                                u2_cw_sam_6, &way, 
                                u2_cw_sam_7, &cog, 
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, fink)(wir_r, van, sut, dep, way, cog);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, fink)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun dep,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun cog)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "fink");

    if ( u2_none == hoc ) {
      c3_assert(!"register fink");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cw_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, 
                                      u2_cw_sam_2, u2_rx(wir_r, dep), 
                                      u2_cw_sam_6, u2_rx(wir_r, way), 
                                      u2_cw_sam_7, u2_rx(wir_r, cog), 
                                      0);

      if ( (u2_none == j2_mcj(Pt6, ut, fink)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, fink)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, fink)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dep,
                        u2_noun way,                              //  retain
                        u2_noun cog)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, fink)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, fink)(wir_r, van, sut, dep, way, cog);
      }
      else {
        c3_m    fun_m = c3__fink;
        u2_noun pro   = u2_rl_find_qual(wir_r, fun_m, sut, dep, way, cog);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, fink)(wir_r, van, sut, dep, way, cog);

          return u2_rl_save_qual(wir_r, fun_m, sut, dep, way, cog, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, fink)(wir_r, van, sut, dep, way, cog);
      fol = u2_t(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      if ( u2_none == pro ) return u2_bl_bail(wir_r, c3__fail);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, fink)(u2_wire wir_r,
                        u2_noun cor)
  {
    u2_noun sut, dep, way, cog, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, 
                                u2_cw_sam_2, &dep, 
                                u2_cw_sam_6, &way, 
                                u2_cw_sam_7, &cog, 
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_none;
    } else {
      return u2_rq
        (wir_r, u2_rx(wir_r, sut), 
                u2_rx(wir_r, dep), 
                u2_rx(wir_r, way), 
                u2_rx(wir_r, cog));
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, fink)[] = {
    { ".3", c3__hevy, 
        j2_mc(Pt6, ut, fink), 
        Tier6_b,
        u2_none, u2_none,
        j2_mck(Pt6, ut, fink), c3__fink,
    },
    { }
  };
