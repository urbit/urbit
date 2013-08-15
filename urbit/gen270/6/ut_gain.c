/* j/6/gain.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun
  j2_mcx(Pit, ut, gain)(u2_wire, u2_noun, u2_noun, u2_noun);

  static u2_noun
  _gain_cspm(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_noun p_gen)
  {
    if ( u2_nul == p_gen ) {
      return u2_rx(wir_r, sut);
    } 
    else if ( u2_no == u2_dust(p_gen) ) {
      return u2_bl_bail(wir_r, c3__fail);
    }
    else {
      u2_noun ip_gen = u2_h(p_gen);
      u2_noun tp_gen = u2_t(p_gen);
      u2_noun lev = j2_mcx(Pit, ut, gain)(wir_r, van, sut, ip_gen);
      u2_noun pro = _gain_cspm(wir_r, van, lev, tp_gen);

      u2_rl_lose(wir_r, lev);
      return pro;
    }
  }
  static u2_noun
  _gain_in(u2_wire wir_r, 
           u2_noun van,
           u2_noun sut,
           u2_noun gen)
  {
    u2_noun p_gen, q_gen;

    if ( u2_no == u2_dust(gen) ) { 
      return u2_rx(wir_r, sut);
    } else switch ( u2_h(gen) ) {
      default: return u2_rx(wir_r, sut);

      case c3__csbn: {
        if ( u2_no == u2_mean(gen, 6, &p_gen, 7, &q_gen, 0) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          u2_noun rac = j2_mcy(Pit, ap, rake)(wir_r, q_gen);
          u2_noun hap = j2_mcy(Pit, ut, play)(wir_r, van, sut, p_gen);
          u2_noun guz = j2_mcy(Pit, ut, seek)(wir_r, van, sut, c3__nest, rac);
          u2_noun axe = u2_h(guz);
          u2_noun ret = j2_mcy(Pit, ut, cull)(wir_r, van, sut, axe, hap);

          u2_rl_lose(wir_r, rac);
          u2_rl_lose(wir_r, hap);
          u2_rl_lose(wir_r, guz);

          return ret;
        }
      }
      case c3__cspm: {
        return _gain_cspm(wir_r, van, sut, u2_t(gen));
      }
      case c3__zpcb: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      {
        return j2_mcx(Pit, ut, gain)(wir_r, van, sut, q_gen);
      }
      case c3__zpdx: p_gen = u2_t(gen);
      {
        return j2_mcx(Pit, ut, gain)(wir_r, van, sut, p_gen);
      }
    }
  }
  u2_noun                                                         //  transfer
  j2_mcx(Pit, ut, gain)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_noun fiz = _gain_in(wir_r, van, sut, gen);

    if ( u2_yes == j2_mcy(Pit, ut, orth)(wir_r, van, c3__noun, fiz) ) {
      u2_rl_lose(wir_r, fiz);
      return c3__void;
    }
    else return fiz;
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pit, ut, gain)[];

  u2_noun                                                         //  transfer
  j2_mc(Pit, ut, gain)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, gen, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &gen, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pit, ut, gain)(wir_r, van, sut, gen);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pit, ut, gain)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun gen)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "gain");

    if ( u2_none == hoc ) {
      c3_assert(!"register gain");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cv_sam, u2_rx(wir_r, gen), 0);

      if ( (u2_none == j2_mcj(Pit, ut, gain)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pit, ut, gain)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pit, ut, gain)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pit, ut, gain)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pit, ut, gain)(wir_r, van, sut, gen);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pit, ut, gain)(wir_r, van, sut, gen);
      fol = u2_t(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      c3_assert(pro != u2_none);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pit, ut, gain)[] = {
    { ".3", c3__hevy, j2_mc(Pit, ut, gain), Tier6_b, u2_none, u2_none },
    { }
  };
