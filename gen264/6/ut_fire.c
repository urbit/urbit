/* j/6/fire.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_noun 
  _fire_each(u2_wire wir_r, 
             u2_noun van,
             u2_noun vet,
             u2_noun typ,
             u2_noun gat)
  {
    u2_noun p_typ, q_typ, pq_typ, qq_typ, rq_typ;
    u2_noun h_gat, t_gat;

    if ( (c3__core != u2_h(typ)) ||
         (u2_no == u2_as_cell(u2_t(typ), &p_typ, &q_typ)) ||
         (u2_no == u2_as_trel(q_typ, &pq_typ, &qq_typ, &rq_typ)) ||
         (u2_no == u2_as_cell(gat, &h_gat, &t_gat)) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      u2_noun dox = u2_bt
        (wir_r, c3__core, u2_rx(wir_r, qq_typ), u2_rx(wir_r, q_typ));

      if ( u2_yes == u2_h(gat) ) {
        if ( (u2_yes == vet) &&
             (u2_no == j2_mcy(Pit, ut, nest)(wir_r, van, qq_typ, p_typ)) )
        {
          return u2_bl_error(wir_r, "fire-dry");
        }
        else {
          return u2_bc(wir_r, dox, u2_rx(wir_r, t_gat));
        }
      }
      else {
        if ( (u2_yes == vet) &&
             (u2_no == u2_sing(p_typ, qq_typ)) &&
             (u2_no == j2_mcy(Pit, ut, fret)(wir_r, van, typ, dox, t_gat)) )
        {
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
  j2_mcx(Pit, ut, fire)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun hag)                              //  retain
  {
    u2_flag vet = u2_frag(j2_ut_van_vet, van);

    return u2_bc(wir_r, c3__hold, _fire_in(wir_r, van, vet, hag));
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
      return u2_bl_bail(wir_r, c3__fail);
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
    u2_weak hoc = u2_ds_look(wir_r, van, "fire");

    if ( u2_none == hoc ) {
      c3_assert(!"register fire");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cv_sam, u2_rx(wir_r, hag), 0);

      if ( (u2_none == j2_mcj(Pit, ut, fire)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
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
      u2_noun cor, fol, pro;

      cor = j2_mci(Pit, ut, fire)(wir_r, van, sut, hag);
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
  j2_mcj(Pit, ut, fire)[] = {
    { ".3", c3__hevy, j2_mc(Pit, ut, fire), Tier6_b, u2_none, u2_none },
    { }
  };
