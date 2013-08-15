/* j/6/fret.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pit, ut, fret)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dox,                              //  retain
                        u2_noun gen)                              //  retain
  {
    // u2_noun rib = u2_bn_hook(wir_r, van, "rib");
    u2_noun rib = u2_rx(wir_r, u2_frag(j2_ut_van_rib, van));
    u2_noun key = u2_bt(wir_r, u2_rx(wir_r, sut),
                               u2_rx(wir_r, dox),
                               u2_rx(wir_r, gen));
    u2_flag ret;
    
    if ( u2_yes == j2_mcc(Pit, in, has)(wir_r, rib, key) ) {
      ret = u2_yes;
    }
    else {
      u2_noun rob = j2_mcc(Pit, in, put)(wir_r, rib, key);
      // u2_noun von = u2_bn_cook(wir_r, van, "rib", rob);
      // u2_noun vin = u2_bn_cook(wir_r, von, "vet", u2_no);
      u2_noun von = u2_bn_molt(wir_r, van, j2_ut_van_rib, rob, 0);
      u2_noun vin = u2_bn_molt(wir_r, von, j2_ut_van_vet, u2_no, 0);

      u2_noun dis = j2_mcy(Pit, ut, mint)(wir_r, von, sut, c3__noun, gen);
      u2_noun dat = j2_mcy(Pit, ut, mint)(wir_r, vin, dox, c3__noun, gen);

      ret = u2_sing(u2_t(dis), u2_t(dat));

      u2_rz(wir_r, von);
      u2_rz(wir_r, vin);
      u2_rz(wir_r, rob);
      u2_rz(wir_r, dis);
      u2_rz(wir_r, dat);
    }

    u2_rz(wir_r, rib);
    u2_rz(wir_r, key);

    return ret;
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pit, ut, fret)[];

  u2_noun                                                         //  transfer
  j2_mc(Pit, ut, fret)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, dox, gen, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &dox, 
                                u2_cv_sam_3, &gen,
                                u2_cv_con, &van, 
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pit, ut, fret)(wir_r, van, sut, dox, gen);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pit, ut, fret)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun dox,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "fret");

    if ( u2_none == hoc ) {
      c3_assert(!"register fret");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, 
                                      u2_cv_sam_2, u2_rx(wir_r, dox), 
                                      u2_cv_sam_3, u2_rx(wir_r, gen), 
                                      0);

      if ( (u2_none == j2_mcj(Pit, ut, fret)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pit, ut, fret)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pit, ut, fret)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dox,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pit, ut, fret)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pit, ut, fret)(wir_r, van, sut, dox, gen);
      }
      else {
        c3_m    fun_m = u2_jet_fun_m(jet_j);
        u2_noun pro   = u2_rl_find_trel(wir_r, fun_m, sut, dox, gen);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pit, ut, fret)(wir_r, van, sut, dox, gen);

          return u2_rl_save_trel(wir_r, fun_m, sut, dox, gen, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pit, ut, fret)(wir_r, van, sut, dox, gen);
      fol = u2_t(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      if ( u2_none == pro ) return u2_bl_bail(wir_r, c3__fail);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pit, ut, fret)(u2_wire wir_r,
                        u2_noun cor)
  {
    u2_noun sut, dox, gen, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &dox, 
                                u2_cv_sam_3, &gen,
                                u2_cv_con, &van, 
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2_rt
        (wir_r, u2_rx(wir_r, sut), u2_rx(wir_r, dox), u2_rx(wir_r, gen));
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pit, ut, fret)[] = {
    { ".3", c3__hevy, 
        j2_mc(Pit, ut, fret), 
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pit, ut, fret)
    },
    { }
  };
