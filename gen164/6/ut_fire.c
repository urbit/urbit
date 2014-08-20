/* j/6/fire.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_bean
  _fire_mull(
             u2_noun van,
             u2_noun sut,
             u2_noun dox,
             u2_noun gen)
  {
    u2_noun rib = u2_cr_at(j2_ut_van_rib, van);
    u2_noun key = u2nt(u2k(sut),
                               u2k(dox),
                               u2k(gen));
    u2_bean ret;

    if ( u2_yes == j2_mcc(Pt4, in, has)(rib, key) ) {
      ret = u2_yes;
    }
    else {
      u2_noun rob = j2_mcc(Pt4, in, put)(rib, key);
      u2_noun von = u2_ci_molt(u2k(van),
                                      j2_ut_van_rib, u2k(rob),
                                      0);
      ret = j2_mcy(Pt6, ut, mull)(von, sut, c3__noun, dox, gen);

      u2z(von);
      u2z(rob);
    }
    u2z(key);
    return ret;
  }

  static u2_noun
  _fire_each(
             u2_noun van,
             u2_noun vet,
             u2_noun typ,
             u2_noun gat)
  {
    u2_noun p_typ, q_typ, pq_typ, qq_typ, rq_typ;
    u2_noun h_gat, t_gat;

    if ( (u2_no == u2du(typ)) || (c3__core != u2h(typ)) ) {
      return u2_cm_error("fire-core");
    } else if
         ( (u2_no == u2_cr_cell(u2t(typ), &p_typ, &q_typ)) ||
           (u2_no == u2_cr_trel(q_typ, &pq_typ, &qq_typ, &rq_typ)) ||
           (u2_no == u2_cr_cell(gat, &h_gat, &t_gat)) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      u2_noun dox = u2nt
        (c3__core, u2k(qq_typ), u2k(q_typ));

      if ( c3__ash == u2h(gat) ) {
        if ( (u2_yes == vet) &&
             (u2_no == j2_mcy(Pt6, ut, nest)
                (van, qq_typ, u2_yes, p_typ)) )
        {
#if 0
          u2_noun dun = j2_mcy(Pt6, ut, dunq)(van, "need", qq_typ);
          u2_noun niz = j2_mcy(Pt6, ut, dunq)(van, "have", p_typ);

          u2_ct_push(u2nc(c3__mean, niz));
          u2_ct_push(u2nc(c3__mean, dun));
#endif
          return u2_cm_error("fire-dry");
        }
        else {
          return u2nc(dox, u2k(t_gat));
        }
      }
      else {
        c3_assert(c3__elm == u2h(gat));

        if ( (u2_yes == vet) &&
             // (u2_no == u2_cr_sing(p_typ, qq_typ)) &&
             (u2_no == _fire_mull(van, typ, dox, t_gat)) )
        {
#if 0
          u2_noun dun = j2_mcy(Pt6, ut, dunq)(van, "wild", typ);
          u2_noun niz = j2_mcy(Pt6, ut, dunq)(van, "tame", dox);

          u2_ct_push(u2nc(c3__mean, dun));
          u2_ct_push(u2nc(c3__mean, niz));
#endif
          return u2_cm_error("fire-wet");
        }
        else {
          u2z(dox);
          return u2nc(u2k(typ), u2k(t_gat));
        }
      }
    }
  }

  static u2_noun
  _fire_in(
           u2_noun van,
           u2_noun vet,
           u2_noun hag)
  {
    if ( u2_nul == hag ) {
      return u2_nul;
    }
    else {
      u2_noun i_hag = u2h(hag);
      u2_noun t_hag = u2t(hag);

      if ( u2_no == u2du(i_hag) ) {
        return u2_cm_bail(c3__fail);
      } else {
        return u2nc
          (_fire_each(van, vet, u2h(i_hag), u2t(i_hag)),
                  _fire_in(van, vet, t_hag));
      }
    }
  }
  u2_noun                                                         //  produce
  j2_mcx(Pt6, ut, fire)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun hag)                              //  retain
  {
    u2_bean vet = u2_cr_at(j2_ut_van_vet, van);

    {
      if ( (u2_yes == u2du(hag)) && (u2_nul == u2t(hag)) ) {
        u2_noun i_hag = u2h(hag);
        u2_noun pi_hag = u2h(i_hag);
        u2_noun qi_hag = u2t(i_hag);

        if ( c3__elm == u2h(qi_hag) ) {
          u2_noun qqi_hag = u2t(qi_hag);

          if ( u2_yes == u2du(qqi_hag) &&
              (u2_nul == u2h(qqi_hag)) &&
              (1 == u2t(qqi_hag)) )
          {
            return u2k(pi_hag);
          }
        }
      }
    }
    return u2nc(c3__hold, _fire_in(van, vet, hag));
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, fire)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, fire)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, hag, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &hag, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, fire)(van, sut, hag);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, fire)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun hag)                              //  retain
  {
    u2_weak hoc = u2_cj_look(u2k(van), "fire");

    if ( u2_none == hoc ) {
      c3_assert(!"register fire");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(u2k(gat), u2_cv_sam, u2k(hag), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, fire)[0].xip) ) {
        u2_noun xip = u2_cj_find(u2k(cor));

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, fire)[0].xip = xip;
      }
      u2z(gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, fire)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun hag)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, fire)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pt6, ut, fire)(van, sut, hag);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, fire)(van, sut, hag);
      fol = u2h(cor);

      pro = u2_ho_use(jet_j, cor, fol);
      if ( u2_none == pro ) return u2_cm_bail(c3__fail);

      u2z(cor);
      u2z(fol);

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
