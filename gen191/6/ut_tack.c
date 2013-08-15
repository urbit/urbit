/* j/6/tack.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

  static u2_noun
  _tack_in(u2_wire, u2_noun, u2_noun, u2_noun, u2_noun, u2_atom);


/* internals
*/
  static u2_noun                                                  //  transfer
  _tack_in(u2_wire wir_r, 
           u2_noun van,                                           //  retain
           u2_noun sut,                                           //  retain
           u2_noun peh,                                           //  retain
           u2_noun mur,                                           //  retain
           u2_atom axe)                                           //  retain
  {
    if ( u2_no == u2_dust(peh) ) {
      return u2_bc(wir_r, u2_rx(wir_r, axe), u2_rx(wir_r, mur));
    } 
    else {
      u2_noun i_peh = u2_h(peh);
      u2_noun t_peh = u2_t(peh);

      if ( (u2_yes == u2_stud(i_peh)) || (u2_no == u2_h(i_peh)) ) 
      {
        u2_noun pi_peh = (u2_yes == u2_stud(i_peh)) ? 0 : u2_h(u2_t(i_peh));
        u2_noun qi_peh = (u2_yes == u2_stud(i_peh)) ? i_peh : u2_t(u2_t(i_peh));
        u2_noun pok   = j2_mcy(Pt6, ut, fink)
                              (wir_r, van, sut, pi_peh, c3__rite, qi_peh);
        u2_noun wuf   = j2_mby(Pt6, flay)(wir_r, pok);
        u2_noun p_wuf = u2_h(wuf);
        u2_noun q_wuf = u2_t(wuf);
        u2_noun nax   = j2_mbc(Pt3, peg)(wir_r, axe, p_wuf); 
        u2_noun gav   = _tack_in(wir_r, van, q_wuf, t_peh, mur, nax);
        u2_noun p_gav = u2_h(gav);
        u2_noun q_gav = u2_t(gav);
        u2_noun qog   = u2_bc(wir_r, u2_nul, u2_rx(wir_r, qi_peh));
        u2_noun ret   = u2_bc
          (wir_r, u2_rx(wir_r, p_gav),
                  j2_mcy(Pt6, ut, heal)(wir_r, van, sut, qog, p_wuf, q_gav));

        u2_rz(wir_r, qog);
        u2_rz(wir_r, gav);
        u2_rz(wir_r, nax);
        u2_rz(wir_r, wuf);
        u2_rz(wir_r, pok);

        return ret;
      }
      else {
        u2_noun bax   = u2_t(i_peh);
        u2_noun vas   = j2_mcy(Pt6, ut, peek)(wir_r, van, sut, c3__rite, bax);
        u2_noun nax   = j2_mbc(Pt3, peg)(wir_r, axe, bax); 
        u2_noun gav   = _tack_in(wir_r, van, vas, t_peh, mur, nax);
        u2_noun p_gav = u2_h(gav);
        u2_noun q_gav = u2_t(gav);
        u2_noun ret   = u2_bc
          (wir_r, u2_rx(wir_r, p_gav),
                  j2_mcy(Pt6, ut, heal)(wir_r, van, sut, u2_nul, bax, q_gav));
        
        u2_rz(wir_r, gav);
        u2_rz(wir_r, nax);
        u2_rz(wir_r, vas);

        return ret;
      }
    }
  }

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, tack)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun peh,                              //  retain
                        u2_noun mur)                              //  retain
  {
    return _tack_in(wir_r, van, sut, peh, mur, _1);
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, tack)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, tack)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun van, sut, peh, mur;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &peh, 
                                u2_cv_sam_3, &mur,
                                u2_cv_con, &van, 
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, tack)(wir_r, van, sut, peh, mur);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, tack)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun peh,                              //  retain
                        u2_noun mur)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "tack");

    if ( u2_none == hoc ) {
      c3_assert(!"register tack");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cv_sam_2, u2_rx(wir_r, peh), 
                                           u2_cv_sam_3, u2_rx(wir_r, mur),
                                           0);

      if ( (u2_none == j2_mcj(Pt6, ut, tack)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, tack)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, tack)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun peh,                              //  retain
                        u2_noun mur)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, tack)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pt6, ut, tack)(wir_r, van, sut, peh, mur);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, tack)(wir_r, van, sut, peh, mur);
      fol = u2_h(cor);

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
  j2_mcj(Pt6, ut, tack)[] = {
    { ".2", c3__hevy, j2_mc(Pt6, ut, tack), 
      u2_jet_dead, 
      // Tier6_b, 
      u2_none, u2_none },
    { }
  };
