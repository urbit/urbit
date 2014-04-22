/* j/6/fire.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_bean
  _fire_mull(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_noun dox,
             u2_noun gen)
  {
    u2_noun rib = u2_frag(j2_ut_van_rib, van);
    u2_noun key = u2_bt(wir_r, u2_rx(wir_r, sut),
                               u2_rx(wir_r, dox),
                               u2_rx(wir_r, gen));
    u2_bean ret;

    if ( u2_yes == j2_mcc(Pt4, in, has)(wir_r, rib, key) ) {
      ret = u2_yes;
    }
    else {
      u2_noun rob = j2_mcc(Pt4, in, put)(wir_r, rib, key);
      u2_noun von = u2_bn_molt(wir_r, van,
                                      j2_ut_van_rib, rob,
                                      0);
      ret = j2_mcy(Pt6, ut, mull)(wir_r, von, sut, c3__noun, dox, gen);

      u2_rz(wir_r, von);
      u2_rz(wir_r, rob);
    }
    u2_rz(wir_r, key);
    return ret;
  }

  static u2_noun
  _fire_each(u2_wire wir_r,
             u2_noun van,
             u2_noun vet,
             u2_noun typ,
             u2_noun gat)
  {
    u2_noun p_typ, q_typ, pq_typ, qq_typ, rq_typ;
    u2_noun h_gat, t_gat;

    if ( (u2_no == u2_dust(typ)) || (c3__core != u2_h(typ)) ) {
      return u2_bl_error(wir_r, "fire-core");
    } else if
         ( (u2_no == u2_as_cell(u2_t(typ), &p_typ, &q_typ)) ||
           (u2_no == u2_as_trel(q_typ, &pq_typ, &qq_typ, &rq_typ)) ||
           (u2_no == u2_as_cell(gat, &h_gat, &t_gat)) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      u2_noun dox = u2_bt
        (wir_r, c3__core, u2_rx(wir_r, qq_typ), u2_rx(wir_r, q_typ));

      if ( c3__ash == u2_h(gat) ) {
        if ( (u2_yes == vet) &&
             (u2_no == j2_mcy(Pt6, ut, nest)
                (wir_r, van, qq_typ, u2_yes, p_typ)) )
        {
#if 0
          u2_noun dun = j2_mcy(Pt6, ut, dunq)(wir_r, van, "need", qq_typ);
          u2_noun niz = j2_mcy(Pt6, ut, dunq)(wir_r, van, "have", p_typ);

          u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, niz));
          u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, dun));
#endif
          return u2_bl_error(wir_r, "fire-dry");
        }
        else {
          return u2_bc(wir_r, dox, u2_rx(wir_r, t_gat));
        }
      }
      else {
        c3_assert(c3__elm == u2_h(gat));

        if ( (u2_yes == vet) &&
             // (u2_no == u2_sing(p_typ, qq_typ)) &&
             (u2_no == _fire_mull(wir_r, van, typ, dox, t_gat)) )
        {
#if 0
          u2_noun dun = j2_mcy(Pt6, ut, dunq)(wir_r, van, "wild", typ);
          u2_noun niz = j2_mcy(Pt6, ut, dunq)(wir_r, van, "tame", dox);

          u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, dun));
          u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, niz));
#endif
          return u2_bl_error(wir_r, "fire-wet");
        }
        else {
          u2_rz(wir_r, dox);
          return u2_bc(wir_r, u2_rx(wir_r, typ), u2_rx(wir_r, t_gat));
        }
      }
    }
  }

  static u2_noun
  _fire_in(u2_wire wir_r,
           u2_noun van,
           u2_noun vet,
           u2_noun hag)
  {
    if ( u2_nul == hag ) {
      return u2_nul;
    }
    else {
      u2_noun i_hag = u2_h(hag);
      u2_noun t_hag = u2_t(hag);

      if ( u2_no == u2_dust(i_hag) ) {
        return u2_bl_bail(wir_r, c3__fail);
      } else {
        return u2_bc
          (wir_r, _fire_each(wir_r, van, vet, u2_h(i_hag), u2_t(i_hag)),
                  _fire_in(wir_r, van, vet, t_hag));
      }
    }
  }
  u2_noun                                                         //  produce
  j2_mcx(Pt6, ut, fire)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun hag)                              //  retain
  {
    u2_bean vet = u2_frag(j2_ut_van_vet, van);

    {
      if ( (u2_yes == u2_dust(hag)) && (u2_nul == u2_t(hag)) ) {
        u2_noun i_hag = u2_h(hag);
        u2_noun pi_hag = u2_h(i_hag);
        u2_noun qi_hag = u2_t(i_hag);

        if ( c3__elm == u2_h(qi_hag) ) {
          u2_noun qqi_hag = u2_t(qi_hag);

          if ( u2_yes == u2_dust(qqi_hag) &&
              (u2_nul == u2_h(qqi_hag)) &&
              (_1 == u2_t(qqi_hag)) )
          {
            return u2_rx(wir_r, pi_hag);
          }
        }
      }
    }
    return u2_bc(wir_r, c3__hold, _fire_in(wir_r, van, vet, hag));
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, fire)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, fire)(u2_wire wir_r,
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, hag, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &hag, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, fire)(wir_r, van, sut, hag);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, fire)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun hag)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "fire");

    if ( u2_none == hoc ) {
      c3_assert(!"register fire");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cv_sam, u2_rx(wir_r, hag), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, fire)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, fire)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, fire)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun hag)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, fire)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pt6, ut, fire)(wir_r, van, sut, hag);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, fire)(wir_r, van, sut, hag);
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
  j2_mcj(Pt6, ut, fire)[] = {
    { ".2", c3__hevy, j2_mc(Pt6, ut, fire), Tier6_b, u2_none, u2_none },
    { }
  };
