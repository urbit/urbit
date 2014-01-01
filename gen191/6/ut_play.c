/* j/6/play.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
# define _play_used(wir_r)

  static u2_noun
  _play_in(u2_wire, u2_noun, u2_noun, u2_noun);

  static u2_noun                                                  //  produce
  _play_bean(u2_wire wir_r)
  {
    return u2_bt(wir_r, c3__fork, 
                        u2_bq(wir_r, c3__cube, _0, c3__atom, 'f'),
                        u2_bq(wir_r, c3__cube, _1, c3__atom, 'f'));
  }

  static u2_noun                                                  //  produce
  _play_core(u2_wire wir_r, 
             u2_noun pac,                                         //  submit
             u2_noun con)                                         //  submit
  {
    if ( (c3__void == pac) ) {
      u2_rz(wir_r, con);

      return c3__void;
    } else {
      return u2_bt(wir_r, c3__core, pac, con);
    }
  }

  static u2_noun                                                  //  produce
  _play_loc(u2_wire wir_r,
            u2_noun van,                                          //  retain
            u2_noun loc)                                          //  retain
  {
    u2_noun mol = u2_bc(wir_r, 'o', u2_rx(wir_r, loc)); 
    u2_noun sho = u2_bn_hook(wir_r, van, "show");
    u2_noun ret = u2_bn_molt(wir_r, sho, u2_cv_sam, mol, 0);
 
    u2_rz(wir_r, mol);
    u2_rz(wir_r, sho);

    return ret;
  }

  static u2_noun                                                  //  produce
  _play_foil(u2_wire wir_r,
             u2_noun pok)                                         //  submit
  {
    u2_noun p_pok = u2_h(pok);
    u2_noun q_pok = u2_t(pok);
    u2_noun ret;

    if ( u2_yes == u2_h(q_pok) ) {
      u2_noun pq_pok = u2_t(q_pok);

      ret = u2_bc(wir_r, 
                  u2_rx(wir_r, p_pok),
                  u2_bc(wir_r, u2_nul,
                               u2_bc(wir_r,
                                     u2_bc(wir_r, 
                                           u2_rx(wir_r, pq_pok),
                                           u2_bt(wir_r, c3__elm, u2_nul, _1)),
                                     u2_nul)));
    }
    else {
      u2_noun pq_pok = u2_h(u2_t(q_pok));
      u2_noun qq_pok = u2_t(u2_t(q_pok));

      ret = u2_bc(wir_r, u2_rx(wir_r, p_pok),
                         u2_bc(wir_r, u2_rx(wir_r, pq_pok),
                                      u2_rx(wir_r, qq_pok)));
    }
    u2_rz(wir_r, pok);
    return ret;
  }
 
  static u2_noun                                                  //  produce
  _play_edit(u2_wire wir_r,
             u2_noun van,                                         //  retain
             u2_noun sut,                                         //  retain
             u2_noun mew,                                         //  retain
             u2_noun rag)                                         //  submit
  {
    while ( 1 ) {
      if ( u2_no == u2_dust(mew) ) {
        return rag;
      } else {
        u2_noun i_mew = u2_h(mew);
        u2_noun t_mew = u2_t(mew);
        u2_noun pi_mew = u2_h(i_mew);
        u2_noun qi_mew = u2_t(i_mew);
        u2_noun laf = _play_in(wir_r, van, sut, qi_mew);
        u2_noun ruz = j2_mcy(Pt6, ut, tock)(wir_r, van, sut, pi_mew, laf, rag);
   
        u2_rz(wir_r, laf);
        u2_rz(wir_r, rag);
        rag = u2_rx(wir_r, u2_t(ruz));
        u2_rz(wir_r, ruz);

        mew = t_mew;
      }
    }
  }

  static u2_noun 
  _play_grow(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_atom mel,
             u2_noun ruf,
             u2_noun dab)
  {
    u2_noun dan = _play_in(wir_r, van, sut, ruf);

    return _play_core
        (wir_r, dan,
                u2_bt(wir_r, u2_rx(wir_r, mel),
                             u2_rx(wir_r, dan), 
                             u2_bc(wir_r, u2_bc(wir_r, u2_nul, u2_nul),
                                          u2_rx(wir_r, dab))));
  }

  static u2_noun
  _play_in(u2_wire wir_r,
           u2_noun van,
           u2_noun sut,
           u2_noun gen);

  static u2_noun 
  _play_x(u2_wire wir_r,
          u2_noun van,
          u2_noun sut,
          u2_noun gen)
  {
#if 1
    return _play_in(wir_r, van, sut, gen);
#else
    u2_noun zix = j2_mcy(Pt6, ut, shep)
          (wir_r, van, "gene", 'q', u2_rx(wir_r, gen));
    u2_noun ret;

    u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, zix));

    ret = _play_in(wir_r, van, sut, gen);

    u2_bl_drop(wir_r);
    return ret;
#endif
  }

  static u2_noun
  _play_in(u2_wire wir_r,
           u2_noun van,
           u2_noun sut,
           u2_noun gen)
  {
    u2_noun p_gen, q_gen, r_gen;

    if ( u2_no == u2_dust(gen) ) {
      open: {
        u2_noun ter = u2_frag(u2_cv_con_3, van);
        u2_noun rex = j2_mcy(Pt6, ap, open)(wir_r, ter, gen);
        u2_noun ret;

        if ( u2_yes == u2_sing(rex, gen) ) {
          u2_noun zix = j2_mcy(Pt6, ut, shep)
                (wir_r, van, "gene", 'q', u2_rx(wir_r, gen));

          u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, zix));
          return u2_bl_error(wir_r, "play-open");
        }
        ret = _play_x(wir_r, van, sut, rex);
        u2_rl_lose(wir_r, rex);

        return ret;
      }
    } 
    else if ( u2_yes == u2_dust(u2_h(gen)) ) {
      _play_used(wir_r);
      {
        u2_noun dis = _play_x(wir_r, van, sut, u2_h(gen));
        u2_noun dat = _play_x(wir_r, van, sut, u2_t(gen));
        u2_noun ret = j2_mby(Pt6, cell)(wir_r, dis, dat);

        u2_rz(wir_r, dis);
        u2_rz(wir_r, dat);
        return ret;
      }
    }
    else switch ( u2_h(gen) ) {
      default: goto open;
    
      case c3__wtts: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        return _play_bean(wir_r);
      }
      case c3__wtcl: u2_bi_trel(wir_r, u2_t(gen), &p_gen, &q_gen, &r_gen);
      _play_used(wir_r);
      {
        u2_noun fex = j2_mcy(Pt6, ut, gain)(wir_r, van, sut, p_gen);
        u2_noun wux = j2_mcy(Pt6, ut, lose)(wir_r, van, sut, p_gen);
        u2_noun dez = (fex == c3__void) ? c3__void 
                                        : _play_x(wir_r, van, fex, q_gen);
        u2_noun doz = (wux == c3__void) ? c3__void 
                                        : _play_x(wir_r, van, wux, r_gen);
        u2_noun ret = j2_mby(Pt6, fork)(wir_r, dez, doz);
       
        u2_rz(wir_r, dez); u2_rz(wir_r, doz);
        u2_rz(wir_r, fex); u2_rz(wir_r, wux);
        return ret;
      }
      case c3__clhp: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        u2_noun dis = _play_x(wir_r, van, sut, p_gen);
        u2_noun dat = _play_x(wir_r, van, sut, q_gen);
        u2_noun ret = j2_mby(Pt6, cell)(wir_r, dis, dat);

        u2_rz(wir_r, dis);
        u2_rz(wir_r, dat);
        return ret;
      }
      case c3__dtkt: p_gen = u2_t(gen);
      _play_used(wir_r);
      {
        return c3__noun;
      }
      case c3__dtwt: p_gen = u2_t(gen);
      _play_used(wir_r);
      {
        return _play_bean(wir_r);
      }
      case c3__dtts: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        return _play_bean(wir_r);
      }
      case c3__dtls: p_gen = u2_t(gen);
      _play_used(wir_r);
      {
        return u2_bc(wir_r, c3__atom, u2_blip);
      }
      case c3__dtsg: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        u2_noun boc = (u2_no == u2_stud(q_gen)) 
                        ? c3__noun
                        : u2_bc(wir_r, c3__atom, u2_rx(wir_r, p_gen));
        u2_noun ret = j2_mby(Pt6, cube)(wir_r, q_gen, boc);

        u2_rz(wir_r, boc);
        return ret;
      }
      case c3__dttr: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        return c3__noun;
      }
      case c3__dtpt: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        if ( 'f' == p_gen ) {
          if ( (q_gen > 1) ) {
            return u2_cm_bail(c3__exit);
          } else {
            return _play_bean(wir_r);
          }
        }
        else return u2_bc(wir_r, c3__atom, u2_rx(wir_r, p_gen));
      }
      case c3__ktbr: p_gen = u2_t(gen);
      _play_used(wir_r);
      {
        u2_noun boc = _play_x(wir_r, van, sut, p_gen);
        u2_noun pro = j2_mcy(Pt6, ut, wrap)(wir_r, van, boc, c3__iron);

        u2_rz(wir_r, boc);
        return pro;
      }
      case c3__ktpm: p_gen = u2_t(gen);
      _play_used(wir_r);
      {
        u2_noun boc = _play_x(wir_r, van, sut, p_gen);
        u2_noun pro = j2_mcy(Pt6, ut, wrap)(wir_r, van, boc, c3__zinc);

        u2_rz(wir_r, boc);
        return pro;
      }
      case c3__ktwt: p_gen = u2_t(gen);
      _play_used(wir_r);
      {
        u2_noun boc = _play_x(wir_r, van, sut, p_gen);
        u2_noun pro = j2_mcy(Pt6, ut, wrap)(wir_r, van, boc, c3__lead);

        u2_rz(wir_r, boc);
        return pro;
      }
      case c3__ktts: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        u2_noun boc = _play_x(wir_r, van, sut, q_gen);
        u2_noun ret = j2_mcy(Pt6, ut, conk)(wir_r, van, boc, p_gen);

        u2_rz(wir_r, boc);
        return ret;
      }
      case c3__ktzp: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        u2_noun boc = _play_x(wir_r, van, sut, q_gen);
        u2_noun ret = j2_mcy(Pt6, ut, conk)(wir_r, van, boc, p_gen);

        u2_rz(wir_r, boc);
        return ret;
      }
      case c3__ktsg: p_gen = u2_t(gen);
      _play_used(wir_r);
      {
        return _play_x(wir_r, van, sut, p_gen);
      }
      case c3__ktls: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        return _play_x(wir_r, van, sut, p_gen);
      }
      case c3__tsgr: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        u2_noun boc = _play_x(wir_r, van, sut, p_gen);
        u2_noun ret = _play_x(wir_r, van, boc, q_gen);

        u2_rz(wir_r, boc);
        return ret;
      }
      case c3__tstr: u2_bi_trel(wir_r, u2_t(gen), &p_gen, &q_gen, &r_gen);
      _play_used(wir_r);
      {
        u2_noun sep = j2_mcy(Pt6, ut, seep)(wir_r, van, sut, c3__both, q_gen);
        u2_noun bid = u2_bt(wir_r, u2k(p_gen), u2k(q_gen), sep);
        u2_noun boc = j2_mby(Pt6, bull)(wir_r, bid, sut);
        u2_noun ret = _play_x(wir_r, van, boc, r_gen);

        u2z(bid);
        u2z(boc);

        return ret;
      }
      case c3__cnts: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        u2_noun sec = j2_mcy(Pt6, ut, seek)(wir_r, van, sut, c3__read, p_gen);
        u2_noun lar = _play_foil(wir_r, sec);
        u2_noun q_lar = u2_t(lar);
        u2_noun qq_lar = u2_t(q_lar);
        u2_noun mew = j2_mcy(Pt6, ut, snub)(wir_r, van, sut, q_gen);
        u2_noun rag = _play_edit(wir_r, van, sut, mew, u2_rx(wir_r, qq_lar));
        u2_noun ret = j2_mcy(Pt6, ut, fire)(wir_r, van, sut, rag);

        u2_rz(wir_r, rag);
        u2_rz(wir_r, mew);
        u2_rz(wir_r, lar);

        return ret;
      }
      case c3__pmcl: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        u2_noun ruf = u2_bt
          (wir_r, c3__clhp, 
                  u2_bc(wir_r, u2_nul, _1),
                  u2_rx(wir_r, p_gen));
        u2_noun ret = _play_grow(wir_r, van, sut, c3__zinc, ruf, q_gen);

        u2_rz(wir_r, ruf);
        return ret;
      }
      case c3__brcn: p_gen = u2_t(gen);
      _play_used(wir_r);
      {
        u2_noun ruf = u2_bc(wir_r, u2_nul, _1);
        u2_noun ret = _play_grow(wir_r, van, sut, c3__gold, ruf, p_gen);

        u2_rz(wir_r, ruf);
        return ret;
      }
      case c3__pmcn: p_gen = u2_t(gen);
      _play_used(wir_r);
      {
        u2_noun ruf = u2_bc(wir_r, u2_nul, _1);
        u2_noun ret = _play_grow(wir_r, van, sut, c3__lead, ruf, p_gen);

        u2_rz(wir_r, ruf);
        return ret;
      }
      case c3__pmls: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        u2_noun ruf = u2_bt
          (wir_r, c3__clhp, 
                  u2_bc(wir_r, u2_nul, _1),
                  u2_rx(wir_r, p_gen));
        u2_noun ret = _play_grow(wir_r, van, sut, c3__iron, ruf, q_gen);

        u2_rz(wir_r, ruf);
        return ret;
      }
      case c3__sgcb: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        u2_noun typ = j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen);
        u2_noun dug = j2_mcy(Pt6, ut, duck)(wir_r, van, typ);
        u2_noun ret;

        u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, dug));
        {
          ret = _play_x(wir_r, van, sut, q_gen);
        } 
        u2_bl_drop(wir_r);

        u2_rz(wir_r, typ);
        return ret;
      }
      case c3__sggr: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        return _play_x(wir_r, van, sut, q_gen);
      }
      case c3__zpts: p_gen = u2_t(gen);
      _play_used(wir_r);
      {
        return c3__noun;
      }
      case c3__zpcm: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        return _play_x(wir_r, van, sut, p_gen);
      }
      case c3__zpcb: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      {
        u2_noun ret;

        u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, _play_loc(wir_r, van, p_gen)));
        {
          ret = _play_x(wir_r, van, sut, q_gen);
        }
        u2_bl_drop(wir_r);
        return ret;
      }
      case c3__zpcn:
      _play_used(wir_r);
      {
        u2_noun pet = u2_bn_hook(wir_r, van, "seed");
        u2_noun ret = u2_rx(wir_r, u2_h(pet));

        u2_rz(wir_r, pet);
        return ret;
      }
      case c3__zpsm: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _play_used(wir_r);
      {
        u2_noun zur = _play_x(wir_r, van, sut, p_gen);
        u2_noun vos = _play_x(wir_r, van, sut, q_gen);
        u2_noun ret = j2_mby(Pt6, cell)(wir_r, zur, vos);

        u2_rz(wir_r, zur);
        u2_rz(wir_r, vos);

        return ret;
      }
      case c3__zpfs:
      case c3__zpzp:
      _play_used(wir_r);
      {
        return c3__void;
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, play)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_noun von = u2_bn_molt(wir_r, van, j2_ut_van_vet, u2_no, 0);
    u2_noun ret = _play_x(wir_r, von, sut, gen);

    u2_rl_lose(wir_r, von);
    return ret;
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, play)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, play)(u2_wire wir_r, 
                         u2_noun cor)                             //  retain
  {
    u2_noun sut, gen, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam, &gen, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, play)(wir_r, van, sut, gen);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, play)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun gen)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "play");

    if ( u2_none == hoc ) {
      c3_assert(!"register play");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, u2_cv_sam, u2_rx(wir_r, gen), 0);

      if ( (u2_none == j2_mcj(Pt6, ut, play)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, play)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, play)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, play)[0];

    if ( jet_j->sat_s == u2_jet_live ) {
      return j2_mcx(Pt6, ut, play)(wir_r, van, sut, gen);
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, play)(wir_r, van, sut, gen);
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
  j2_mcj(Pt6, ut, play)[] = {
    { ".2", c3__hevy, j2_mc(Pt6, ut, play), Tier6_b, u2_none, u2_none },
    { }
  };
