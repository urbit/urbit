/* j/6/bake.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_noun
  _bake_make(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_flag tov,
             u2_noun gen)
  {
    u2_noun von;
    
    switch ( tov ) {
      default: return u2_bl_bail(wir_r, c3__fail);
      case u2_yes: 
        von = u2_rx(wir_r, van); break;
      case u2_no: 
        von = u2_bn_molt(wir_r, van, j2_ut_van_vet, u2_no, 0); break;
    }
    {
      u2_noun mil = j2_mcy(Pit, ut, mint)(wir_r, von, sut, c3__noun, gen);
      u2_noun fol = u2_rx(wir_r, u2_t(mil));

      u2_rl_lose(wir_r, mil);
      u2_rl_lose(wir_r, von);
      return fol;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcx(Pit, ut, bake)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dab)                              //  retain
  {
    if ( (u2_nul == dab) ) {
      return _0;
    }
    else {
      u2_noun n_dab, l_dab, r_dab; 
     
      u2_as_trel(dab, &n_dab, &l_dab, &r_dab);
      if ( u2_no == u2_dust(n_dab) ) {
        return u2_bl_bail(wir_r, c3__fail);
      } 
      else {
        u2_noun qn_dab = u2_t(n_dab);
        u2_noun pqn_dab = u2_t(qn_dab);   //  XX actual wing support
        u2_noun ppqn_dab = u2_h(pqn_dab);
        u2_noun qpqn_dab = u2_t(pqn_dab);
        u2_noun vad = _bake_make(wir_r, van, sut, ppqn_dab, qpqn_dab);

        if ( (u2_nul == l_dab) && (u2_nul == r_dab) ) {
          return vad;
        }
        else if ( (u2_nul == l_dab) ) {
          return u2_bc
            (wir_r, vad, j2_mcx(Pit, ut, bake)(wir_r, van, sut, r_dab));
        }
        else if ( (u2_nul == r_dab) ) {
          return u2_bc
            (wir_r, vad, j2_mcx(Pit, ut, bake)(wir_r, van, sut, l_dab));
        }
        else {
          return u2_bt
            (wir_r, vad,
                    j2_mcx(Pit, ut, bake)(wir_r, van, sut, l_dab),
                    j2_mcx(Pit, ut, bake)(wir_r, van, sut, r_dab));
        }
      }
    }
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pit, ut, bake)[];

  u2_noun                                                         //  transfer
  j2_mc(Pit, ut, bake)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, dab, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &dab, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pit, ut, bake)(wir_r, van, sut, dab);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pit, ut, bake)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun dab)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "bake");

    if ( u2_none == hoc ) {
      c3_assert(!"register bake");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cv_sam, u2_rx(wir_r, dab), 0);

      if ( (u2_none == j2_mcj(Pit, ut, bake)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pit, ut, bake)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pit, ut, bake)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dab)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pit, ut, bake)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pit, ut, bake)(wir_r, van, sut, dab);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pit, ut, bake)(wir_r, van, sut, dab);
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
  j2_mcj(Pit, ut, bake)[] = {
    { ".3", c3__hevy, j2_mc(Pit, ut, bake), Tier6_b, u2_none, u2_none },
    { }
  };
