/* j/6/tusk.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

  static u2_noun
  _tusk_in(u2_wire, u2_noun, u2_noun, u2_noun, u2_noun, u2_noun, u2_atom);

/* internals
*/
  static u2_noun                                                  //  transfer
  _tusk_in(u2_wire wir_r, 
           u2_noun van,                                           //  retain
           u2_noun sut,                                           //  retain
           u2_noun peh,                                           //  retain
           u2_noun boz,                                           //  retain
           u2_noun rix,                                           //  retain
           u2_atom axe)                                           //  retain
  {
    u2_noun ret;

    if ( u2_nul == peh ) {
      return j2_mcy(Pit, ut, play)(wir_r, van, boz, rix);
    }
    else {
      u2_noun i_peh = u2_h(peh);
      u2_noun t_peh = u2_t(peh);

      if ( u2_yes == u2_stud(i_peh) ) {
        u2_noun cog = i_peh;
        u2_noun wer = j2_mcy(Pit, ut, find)(wir_r, van, sut, c3__rite, i_peh);
        u2_noun p_wer, r_wer;

        u2_as_trel(wer, &p_wer, 0, &r_wer);
        {
          u2_noun nax = j2_mbc(Pit, peg)(wir_r, axe, p_wer); 
          u2_noun giv = _tusk_in(wir_r, van, r_wer, t_peh, boz, rix, nax);
          u2_noun qog = u2_bc(wir_r, u2_nul, cog);
          u2_noun ret = j2_mcy(Pit, ut, heal)
            (wir_r, van, sut, qog, p_wer, giv);

          u2_rl_lose(wir_r, nax);
          u2_rl_lose(wir_r, qog);
          u2_rl_lose(wir_r, giv);
          u2_rl_lose(wir_r, wer);

          return ret;
        }
      }
      else {
        u2_noun bax = u2_t(i_peh);
        u2_noun vas = j2_mcy(Pit, ut, peek)(wir_r, van, sut, c3__rite, bax);
        u2_noun rox;

        if ( u2_nul == t_peh ) {
          rox = j2_mcy(Pit, ut, snap)(wir_r, van, vas, rix);
        } else rox = u2_rx(wir_r, rix);
        
        {
          u2_noun nax = j2_mbc(Pit, peg)(wir_r, axe, bax); 
          u2_noun giv = _tusk_in(wir_r, van, vas, t_peh, boz, rox, nax);

          ret = j2_mcy(Pit, ut, heal)(wir_r, van, sut, u2_nul, bax, giv);

          u2_rl_lose(wir_r, rox);
          u2_rl_lose(wir_r, nax);
          u2_rl_lose(wir_r, giv);

          return ret;
        }
      }
    }
  }

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pit, ut, tusk)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun hep,                              //  retain
                        u2_noun boz,                              //  retain
                        u2_noun rix)                              //  retain
  {
    u2_noun peh = j2_mbc(Pit, flop)(wir_r, hep);
    u2_noun pro = _tusk_in(wir_r, van, sut, peh, boz, rix, _1);

    u2_rl_lose(wir_r, peh);
    return pro;
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pit, ut, tusk)[];

  u2_noun                                                         //  transfer
  j2_mc(Pit, ut, tusk)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun van, sut, hep, boz, rix;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &hep, 
                                u2_cv_sam_6, &boz,
                                u2_cv_sam_7, &rix,
                                u2_cv_con, &van, 
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pit, ut, tusk)(wir_r, van, sut, hep, boz, rix);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pit, ut, tusk)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun hep,                              //  retain
                        u2_noun boz,                              //  retain
                        u2_noun rix)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "tusk");

    if ( u2_none == hoc ) {
      c3_assert(!"register tusk");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cv_sam_2, u2_rx(wir_r, hep), 
                                           u2_cv_sam_6, u2_rx(wir_r, boz),
                                           u2_cv_sam_7, u2_rx(wir_r, rix),
                                           0);

      if ( (u2_none == j2_mcj(Pit, ut, tusk)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pit, ut, tusk)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pit, ut, tusk)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun hep,                              //  retain
                        u2_noun boz,                              //  retain
                        u2_noun rix)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pit, ut, tusk)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pit, ut, tusk)(wir_r, van, sut, hep, boz, rix);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pit, ut, tusk)(wir_r, van, sut, hep, boz, rix);
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
  j2_mcj(Pit, ut, tusk)[] = {
    { ".3", c3__hevy, j2_mc(Pit, ut, tusk), Tier6_b, u2_none, u2_none },
    { }
  };

