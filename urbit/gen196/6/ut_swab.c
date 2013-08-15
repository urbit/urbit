/* j/6/swab.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* internals
*/

#if 0
  static u2_noun                                                  //  produce
  _snip(u2_wire wir_r,
        u2_noun van,                                              //  retain
        u2_noun sut,                                              //  retain
        u2_noun peh,                                              //  retain
        u2_noun gen)                                              //  retain
  {
    if ( u2_no == u2_dust(peh) ) {
      return u2_rx(wir_r, gen);
    } else {
      u2_noun i_peh = u2_h(peh);
      u2_noun t_peh = u2_t(peh);

      if ( u2_no == u2_dust(i_peh) ) {
        u2_noun fic = j2_mcy(Pt6, ut, fink)(wir_r, van, sut, c3__rite, i_peh);
        u2_noun fom = j2_mby(Pt6, flay)(wir_r, fic);
        u2_noun ret = _snip(wir_r, van, u2_t(fom), t_peh, gen);

        u2_rz(wir_r, fom);
        u2_rz(wir_r, fic);

        return ret;
      } else {
        u2_noun pi_peh = u2_t(i_peh);
        u2_noun vas = j2_mcy(Pt6, ut, peek)(wir_r, van, sut, c3__rite, pi_peh);
        u2_noun wan = (u2_no == u2_dust(t_peh))
                      ? j2_mcy(Pt6, ut, snap)(wir_r, van, vas, gen)
                      : u2_rx(wir_r, gen);
        u2_noun ret = _snip(wir_r, van, vas, t_peh, wan);

        u2_rz(wir_r, wan);
        u2_rz(wir_r, vas);

        return ret;
      }
    }
  }
#endif

  static u2_noun                                                  //  produce
  _snub(u2_wire wir_r,
        u2_noun van,                                              //  retain
        u2_noun sut,                                              //  retain
        u2_noun peh,                                              //  retain
        u2_noun men,                                              //  retain
        u2_noun gen,                                              //  retain
        u2_noun gus)                                              //  retain
  {
#if 1
    return u2_rx(wir_r, gen);
#else
    if ( u2_no == u2_dust(men) ) {
      if ( u2_nul == gus ) {
        return u2_rx(wir_r, gen);
      } else {
        return u2_rx(wir_r, u2_t(gus));
      }
    }
    else {
      u2_noun i_men   = u2_h(men);
      u2_noun t_men   = u2_t(men);
      u2_noun pi_men  = u2_h(i_men);

      u2_noun nip = _snip(wir_r, van, pi_men, peh, gen);
      u2_noun nar = u2_bc(wir_r, u2_nul, nip);
      u2_noun ret;

      if ( u2_nul != gus ) {
        if ( u2_no == u2_sing(nip, u2_t(gus)) ) {
          return u2_bl_bail(wir_r, c3__exit);
        }
      }
      ret = _snub(wir_r, van, sut, peh, t_men, gen, nar);

      u2_rz(wir_r, nar);
      return ret;
    }
#endif
  }

/* functions
*/
  u2_noun                                                         //  produce
  j2_mcx(Pt6, ut, swab)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun men,                              //  retain
                        u2_noun har)                              //  retain
  {
    if ( u2_no == u2_dust(har) ) {
      return u2_nul;
    } else {
      u2_noun i_har = u2_h(har);
      u2_noun t_har = u2_t(har);
      u2_noun pi_har = u2_h(i_har);
      u2_noun qi_har = u2_t(i_har);
      u2_noun roz = j2_mcy(Pt6, ap, rake)(wir_r, pi_har);
      u2_noun peh = j2_mbc(Pt2, flop)(wir_r, roz);
      u2_noun nuk = _snub(wir_r, van, sut, peh, men, qi_har, u2_nul);
      u2_noun ret = u2_bc(wir_r, u2_bc(wir_r, peh, nuk),
                                 j2_mcx(Pt6, ut, swab)
                                    (wir_r, van, sut, men, t_har));

      u2_rz(wir_r, roz);
      return ret;
    }
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, swab)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, swab)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun van, sut, men, har;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &men, 
                                u2_cv_sam_3, &har,
                                u2_cv_con, &van, 
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, swab)(wir_r, van, sut, men, har);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, swab)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun men,                              //  retain
                        u2_noun har)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "swab");

    if ( u2_none == hoc ) {
      c3_assert(!"register swab");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cv_sam_2, u2_rx(wir_r, men), 
                                           u2_cv_sam_3, u2_rx(wir_r, har),
                                           0);

      if ( (u2_none == j2_mcj(Pt6, ut, swab)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, swab)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, swab)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun men,                              //  retain
                        u2_noun har)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, swab)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pt6, ut, swab)(wir_r, van, sut, men, har);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, swab)(wir_r, van, sut, men, har);
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
  j2_mcj(Pt6, ut, swab)[] = {
    { ".2", c3__hevy, j2_mc(Pt6, ut, swab), Tier6_c, u2_none, u2_none },
    { }
  };
