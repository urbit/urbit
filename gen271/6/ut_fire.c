/* j/6/fire.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_flag
  _fire_levy(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_noun dox,
             u2_noun hag)
  {
    if ( u2_nul == hag ) {
      return u2_yes;
    } else {
      u2_noun i_hag = u2_h(hag);
      u2_noun t_hag = u2_t(hag);

      return u2_and(j2_mcy(Pit, ut, fret)(wir_r, van, sut, dox, i_hag),
                    _fire_levy(wir_r, van, sut, dox, t_hag));
    }
  }

  static u2_noun 
  _fire_in(u2_wire wir_r,
           u2_noun van,
           u2_noun sut,
           u2_noun hag)
  {
    u2_flag vet = u2_bn_hook(wir_r, van, "vet");
    u2_noun p_sut, q_sut;
    u2_noun ret;

    if ( u2_no == u2_as_pq(sut, c3__core, &p_sut, &q_sut) ) {
      return u2_bl_bail(wir_r);
    } else {
      u2_noun pq_sut, qq_sut, rq_sut;

      u2_bi_trel(wir_r, q_sut, &pq_sut, &qq_sut, &rq_sut);
      {
        u2_noun dox = u2_bt
          (wir_r, c3__core, u2_rx(wir_r, qq_sut), u2_rx(wir_r, q_sut));

        if ( c3__wood != pq_sut ) {
          if ( (u2_yes == vet) && 
               (u2_no == j2_mcy(Pit, ut, nest)(wir_r, van, qq_sut, p_sut)) )
          {
            printf("fire: metal: nest fail\n");

            return u2_bl_bail(wir_r);
          }
          ret = u2_rx(wir_r, dox);
        }
        else {
          if ( u2_yes == vet &&
               (u2_no == u2_sing(p_sut, qq_sut)) &&
               _fire_levy(wir_r, van, sut, dox, hag) )
          {
            printf("fire: wood: levy fail\n");
            return u2_bl_bail(wir_r);
          }
          ret = u2_rx(wir_r, sut);
        }

        u2_rz(wir_r, dox);
        return ret;
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mcx(Pit, ut, fire)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun hag)                              //  retain
  {
    u2_noun typ = _fire_in(wir_r, van, sut, hag);

    return u2_bt(wir_r, c3__hold, typ, u2_rx(wir_r, hag));
  }
  
/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pit, ut, fire)[];

  u2_noun                                                         //  transfer
  j2_mc(Pit, ut, fire)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, hag, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &hag, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r);
    } else {
      return j2_mcx(Pit, ut, fire)(wir_r, van, sut, hag);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pit, ut, fire)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun hag)                              //  retain
  {
    u2_weak hoc = u2_sh_look(wir_r, van, "fire");

    if ( u2_none == hoc ) {
      c3_assert(!"register fire");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cv_sam, u2_rx(wir_r, hag), 0);

      if ( (u2_none == j2_mcj(Pit, ut, fire)[0].xip) ) {
        u2_noun xip = u2_sh_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pit, ut, fire)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pit, ut, fire)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun hag)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pit, ut, fire)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pit, ut, fire)(wir_r, van, sut, hag);
    }
    else {
      u2_noun cor, fol, xip, pro;

      cor = j2_mci(Pit, ut, fire)(wir_r, van, sut, hag);
      fol = u2_t(cor);
      xip = j2_mcj(Pit, ut, fire)[0].xip;

      pro = u2_ho_punt(wir_r, xip, cor, fol);
      c3_assert(pro != u2_none);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pit, ut, fire)[] = {
    { ".3", c3__hevy, j2_mc(Pit, ut, fire), Tier6_b, u2_none, u2_none },
    { }
  };
