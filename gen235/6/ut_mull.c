/* j/6/mull.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  static u2_noun
  _mull_in(u2_wire, u2_noun, u2_noun, u2_noun, u2_noun, u2_noun);

  static u2_flag
  _mull_vet(u2_wire wir_r, 
            u2_noun van)
  {
    // u2_flag vet = u2_bn_hook(wir_r, van, "vet");
    u2_flag vet = u2_frag(j2_ut_van_vet, van);

    switch ( vet ) {
      case u2_no: 
      case u2_yes: return vet;
      default: return u2_bl_bail(wir_r, c3__fail); 
    }
  }

  static u2_noun                                                  //  produce
  _mull_core(u2_wire wir_r, 
             u2_noun pac,                                         //  submit
             u2_noun con)                                         //  submit
  {
    if ( (c3__void == pac) ) {
      return c3__void;
    } else {
      return u2_bt(wir_r, c3__core, pac, con);
    }
  }

  static u2_noun
  _mull_flag(u2_wire wir_r)
  {
    return u2_bt(wir_r, c3__fork, 
                        u2_bq(wir_r, c3__cube, _0, c3__atom, 'f'),
                        u2_bq(wir_r, c3__cube, _1, c3__atom, 'f'));
  }

  static u2_noun                                                  //  produce
  _mull_loc(u2_wire wir_r,
            u2_noun van,                                          //  retain
            u2_noun loc)                                          //  retain
  {
    u2_noun mol = u2_bc(wir_r, 'o', u2_rx(wir_r, loc)); 
    u2_noun sho = u2_bn_hook(wir_r, van, "show");
    u2_noun ret = u2_bn_molt(wir_r, sho, u2_cw_sam, mol, 0);
 
    u2_rz(wir_r, mol);
    u2_rz(wir_r, sho);

    return ret;
  }

  static u2_noun                                                  //  produce
  _mull_foil(u2_wire wir_r,
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
                                           u2_bt(wir_r, u2_no, u2_nul, _1)),
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
  _mull_coke(u2_wire wir_r,
             u2_noun nug)                                         //  submit
  {
    u2_atom axe;

    if ( 0 == u2_h(nug) ) {
      axe = u2_t(nug);
    } else if ( 10 == u2_h(nug) ) {
      u2_noun xin = u2_rx(wir_r, u2_t(u2_t(nug)));

      axe = _mull_coke(wir_r, xin);
    }
    else {
      return u2_bl_error(wir_r, "mint-coke");
    }
    u2_rz(wir_r, nug);
    return axe;
  }

  static u2_noun
  _mull_doke(u2_wire wir_r,
             u2_noun van,
             u2_noun sut, 
             u2_noun gen)
  {
    u2_noun fug = j2_mcy(Pt6, ut, mint)(wir_r, van, sut, c3__noun, gen);
    u2_noun axe = _mull_coke(wir_r, u2_rx(wir_r, u2_t(fug)));

    u2_rz(wir_r, fug);
    return axe;
  }

  static u2_noun                                                  //  produce
  _mull_nice(u2_wire wir_r,
             u2_noun van,                                         //  retain
             u2_noun gol,                                         //  retain
             u2_noun typ)                                         //  submit
  {
    if ( u2_no == j2_mcy(Pt6, ut, nest)(wir_r, van, gol, u2_yes, typ) ) {
      u2_noun dun = j2_mcy(Pt6, ut, dunq)(wir_r, van, "need", gol);
      u2_noun niz = j2_mcy(Pt6, ut, dunq)(wir_r, van, "have", typ);

      u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, dun));
      u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, niz));

      return u2_bl_error(wir_r, "mull-nice");
    }
    else return typ;
  }

  static void
  _mull_bake(u2_wire wir_r, 
             u2_noun van,                                         //  retain
             u2_noun sut,                                         //  retain
             u2_noun dox,                                         //  retain
             u2_noun dab)                                         //  retain
  {
    if ( u2_nul != dab ) {
      u2_noun n_dab, l_dab, r_dab; 
 
      u2_as_trel(dab, &n_dab, &l_dab, &r_dab);
      if ( u2_no == u2_dust(n_dab) ) {
        u2_bl_bail(wir_r, c3__fail);
      } 
      else {
        u2_noun qn_dab = u2_t(n_dab);
        u2_noun pqn_dab = u2_t(qn_dab);   //  XX actual wing support
        u2_noun ppqn_dab = u2_h(pqn_dab);
        u2_noun qpqn_dab = u2_t(pqn_dab);
        u2_noun vad = (u2_yes == ppqn_dab)
                        ? _mull_in(wir_r, van, sut, c3__noun, dox, qpqn_dab)
                        : u2_nul;

        u2_rz(wir_r, vad);

        if ( (u2_nul == l_dab) && (u2_nul == r_dab) ) {
          return;
        }
        else if ( (u2_nul == l_dab) ) {
          _mull_bake(wir_r, van, sut, dox, r_dab);
        }
        else if ( (u2_nul == r_dab) ) {
          _mull_bake(wir_r, van, sut, dox, l_dab);
        }
        else {
          _mull_bake(wir_r, van, sut, dox, l_dab);
          _mull_bake(wir_r, van, sut, dox, r_dab);
        }
      }
    }
  }

  static u2_noun 
  _mull_grow(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_noun gol,
             u2_noun dox,
             u2_atom mel,
             u2_noun ruf,
             u2_noun dab)
  {
    u2_noun dan = _mull_in(wir_r, van, sut, c3__noun, dox, ruf);
    u2_noun p_dan = u2_h(dan);
    u2_noun q_dan = u2_t(dan);
    u2_noun p_toc = _mull_core
      (wir_r, u2_rx(wir_r, p_dan),
              u2_bt(wir_r, c3__gold, 
                           u2_rx(wir_r, p_dan), 
                           u2_bc(wir_r, u2_nul, u2_rx(wir_r, dab))));
    u2_noun q_toc = _mull_core
      (wir_r, u2_rx(wir_r, q_dan),
              u2_bt(wir_r, c3__gold, 
                           u2_rx(wir_r, q_dan), 
                           u2_bc(wir_r, u2_nul, u2_rx(wir_r, dab))));
    u2_noun p_ret = _mull_core
        (wir_r, u2_rx(wir_r, p_dan),
                u2_bt(wir_r, u2_rx(wir_r, mel),
                             u2_rx(wir_r, p_dan), 
                             u2_bc(wir_r, u2_bc(wir_r, u2_nul, u2_nul),
                                          u2_rx(wir_r, dab))));
    u2_noun q_ret = _mull_core
        (wir_r, u2_rx(wir_r, q_dan),
                u2_bt(wir_r, u2_rx(wir_r, mel),
                             u2_rx(wir_r, q_dan), 
                             u2_bc(wir_r, u2_bc(wir_r, u2_nul, u2_nul),
                                          u2_rx(wir_r, dab))));
    u2_noun ret = u2_bc(wir_r, _mull_nice(wir_r, van, gol, p_ret), q_ret);

    _mull_bake(wir_r, van, p_toc, q_toc, dab);

    u2_rz(wir_r, q_toc);
    u2_rz(wir_r, p_toc);
    u2_rz(wir_r, dan);

    return ret;
  }

  static u2_noun                                                  //  produce
  _mull_both(u2_wire wir_r,
             u2_noun van,                                         //  retain
             u2_noun gol,                                         //  retain
             u2_noun typ)                                         //  submit
  {
    return u2_bc(wir_r, _mull_nice(wir_r, van, gol, u2_rx(wir_r, typ)), 
                        typ);
  }

  static u2_noun                                                  //  produce
  _mull_edit(u2_wire wir_r,
             u2_noun van,                                         //  retain
             u2_noun sut,                                         //  retain
             u2_noun dox,                                         //  retain
             u2_noun mew,                                         //  retain
             u2_noun p_yom,                                       //  submit
             u2_noun q_yom)                                       //  submit
  {
    while ( 1 ) {
      if ( u2_no == u2_dust(mew) ) {
        return u2_bc(wir_r, p_yom, q_yom);
      } else {
        u2_noun i_mew = u2_h(mew);
        u2_noun t_mew = u2_t(mew);
        u2_noun pi_mew = u2_h(i_mew);
        u2_noun qi_mew = u2_t(i_mew);
        u2_noun zil = _mull_in(wir_r, van, sut, c3__noun, dox, qi_mew);
        u2_noun p_zil = u2_h(zil);
        u2_noun q_zil = u2_t(zil);
        u2_noun cuf = j2_mcy(Pt6, ut, tock)
              (wir_r, van, sut, pi_mew, p_zil, p_yom);
        u2_noun dof = j2_mcy(Pt6, ut, tock)
              (wir_r, van, sut, pi_mew, q_zil, q_yom);
        u2_noun vov = u2_sing(u2_h(cuf), u2_h(dof))
                        ? u2_bl_error(wir_r, "mull-bonk-a")
                        : 0;
       
        vov = vov;

        u2_rz(wir_r, p_yom);
        p_yom = u2_rx(wir_r, u2_t(cuf));

        u2_rz(wir_r, q_yom);
        q_yom = u2_rx(wir_r, u2_t(dof));

        u2_rz(wir_r, dof);
        u2_rz(wir_r, cuf);
        u2_rz(wir_r, zil);

        mew = t_mew;
      }
    }
  }

# define _mull_used(wir_r)

  static u2_noun                                                  //  produce
  _mull_in(u2_wire wir_r, 
           u2_noun van,                                           //  retain
           u2_noun sut,                                           //  retain
           u2_noun gol,                                           //  retain
           u2_noun dox,                                           //  retain
           u2_noun gen)                                           //  retain
  {
    u2_noun p_gen, q_gen, r_gen;
    u2_noun ret;

    if ( u2_no == u2_dust(gen) ) {
      u2_noun rex = j2_mcy(Pt6, ap, open)(wir_r, gen);

      ret = _mull_in(wir_r, van, sut, gol, dox, rex);
      u2_rl_lose(wir_r, rex);

      return ret;
    } 
    else switch ( u2_h(gen) ) {
      default: {
        u2_noun rex = j2_mcy(Pt6, ap, open)(wir_r, gen);

        if ( u2_yes == u2_sing(rex, gen) ) {
#if 1
          u2_noun zix = j2_mcy(Pt6, ut, shep)
                (wir_r, van, "gene", 'q', u2_rx(wir_r, gen));

          u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, zix));
          return u2_bl_error(wir_r, "mull-open");
#else
          u2_err(wir_r, "h", u2_h(gen));
          return u2_bl_bail(wir_r, c3__fail);
#endif
        }
        ret = _mull_in(wir_r, van, sut, gol, dox, rex);
        u2_rl_lose(wir_r, rex);

        return ret;
      }
      
      case c3__cslc: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        u2_noun p_waz = j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen);
        u2_noun q_waz = j2_mcy(Pt6, ut, play)(wir_r, van, dox, p_gen);
        u2_noun p_syx = _mull_doke(wir_r, van, sut, q_gen);
        u2_noun q_syx = _mull_doke(wir_r, van, dox, q_gen);
        u2_noun p_pov = j2_mcy(Pt6, ut, fish)(wir_r, van, p_waz, p_syx);
        u2_noun q_pov = j2_mcy(Pt6, ut, fish)(wir_r, van, q_waz, q_syx);

        if ( (u2_no == u2_sing(p_syx, q_syx)) ||
             (u2_no == u2_sing(p_pov, q_pov)) ) 
        {
          return u2_bl_error(wir_r, "mull-bonk-b");
        }
        return _mull_both(wir_r, van, gol, _mull_flag(wir_r));
      }
      case c3__csdg: u2_bi_trel(wir_r, u2_t(gen), &p_gen, &q_gen, &r_gen);
      _mull_used(wir_r);
      {
        u2_noun bol = _mull_flag(wir_r);
        u2_noun nor = _mull_in(wir_r, van, sut, bol, dox, p_gen);
        u2_noun p_fex = j2_mcy(Pt6, ut, gain)(wir_r, van, sut, p_gen);
        u2_noun q_fex = j2_mcy(Pt6, ut, gain)(wir_r, van, dox, p_gen);
        u2_noun p_wux = j2_mcy(Pt6, ut, lose)(wir_r, van, sut, p_gen);
        u2_noun q_wux = j2_mcy(Pt6, ut, lose)(wir_r, van, dox, p_gen);
        u2_noun hiq, ran;
        u2_noun dis, dat;
        u2_noun ret;
   
        if ( c3__void == p_fex ) {
          hiq = u2_bc(wir_r, 
                      c3__void, 
                      (c3__void == q_fex)
                        ? c3__void
                        : j2_mcy(Pt6, ut, play)(wir_r, van, q_fex, q_gen));
        } else if ( c3__void == q_fex ) {
          hiq = u2_bl_error(wir_r, "mull-bonk-c");
        }
        else hiq = _mull_in(wir_r, van, p_fex, gol, q_fex, q_gen);

        if ( c3__void == p_wux ) {
          ran = u2_bc(wir_r, 
                      c3__void, 
                      (c3__void == q_wux)
                        ? c3__void
                        : j2_mcy(Pt6, ut, play)(wir_r, van, q_wux, r_gen));
        } else if ( c3__void == q_wux ) {
          ran = u2_bl_error(wir_r, "mull-bonk-d");
        }
        else ran = _mull_in(wir_r, van, p_wux, gol, q_wux, r_gen);

        dis = j2_mby(Pt6, fork)(wir_r, u2_h(hiq), u2_h(ran));
        dat = j2_mby(Pt6, fork)(wir_r, u2_t(hiq), u2_t(ran));

        ret = u2_bc(wir_r, _mull_nice(wir_r, van, gol, dis), dat);

        u2_rz(wir_r, ran);
        u2_rz(wir_r, hiq);
        u2_rz(wir_r, q_wux);
        u2_rz(wir_r, p_wux);
        u2_rz(wir_r, q_fex);
        u2_rz(wir_r, p_fex);
        u2_rz(wir_r, nor);
        u2_rz(wir_r, bol);

        return ret;
      }
      case c3__dgsp: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        u2_noun hed = _mull_in(wir_r, van, sut, c3__noun, dox, p_gen);
        u2_noun tal = _mull_in(wir_r, van, sut, c3__noun, dox, q_gen);
        u2_noun dis = j2_mby(Pt6, cell)(wir_r, u2_h(hed), u2_h(tal));
        u2_noun dat = j2_mby(Pt6, cell)(wir_r, u2_t(hed), u2_t(tal));
        u2_noun ret = u2_bc(wir_r, _mull_nice(wir_r, van, gol, dis), dat);

        u2_rz(wir_r, tal);
        u2_rz(wir_r, hed);

        return ret;
      }
      case c3__dtlc: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        u2_noun hed = _mull_in(wir_r, van, sut, c3__noun, dox, p_gen);
        u2_noun tal = _mull_in(wir_r, van, sut, c3__noun, dox, q_gen);

        u2_rz(wir_r, hed);
        u2_rz(wir_r, tal);

        return _mull_both(wir_r, van, gol, _mull_flag(wir_r));
      }
      case c3__dtcs: p_gen = u2_t(gen);
      _mull_used(wir_r);
      {
        u2_noun vay = _mull_in(wir_r, van, sut, c3__noun, dox, p_gen);
        
        u2_rz(wir_r, vay);
        return _mull_both(wir_r, van, gol, _mull_flag(wir_r));
      }
      case c3__dtps: p_gen = u2_t(gen);
      _mull_used(wir_r);
      {
        u2_noun wuq = u2_bc(wir_r, c3__atom, u2_blip);
        u2_noun vay = _mull_in(wir_r, van, sut, wuq, dox, p_gen);
 
        u2_rz(wir_r, vay);
        return _mull_both(wir_r, van, gol, wuq);
      }
      case c3__dtsg: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        u2_noun tof = u2_bt
          (wir_r, c3__cube,
                  u2_rx(wir_r, q_gen),
                  (u2_yes == u2_dust(q_gen))
                    ? c3__noun
                    : u2_bc(wir_r, c3__atom, u2_rx(wir_r, p_gen)));

        return _mull_both(wir_r, van, gol, tof);
      }
      case c3__dttr: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        u2_noun hed = _mull_in(wir_r, van, sut, c3__noun, dox, p_gen);
        u2_noun tal = _mull_in(wir_r, van, sut, c3__noun, dox, q_gen);

        u2_rz(wir_r, hed);
        u2_rz(wir_r, tal);

        return _mull_both(wir_r, van, gol, c3__noun);
      }
      case c3__dtwt: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        return _mull_both(wir_r, van, gol, 
                                      u2_bc(wir_r, c3__atom, 
                                                   u2_rx(wir_r, p_gen)));
      }
      case c3__ktdl: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        u2_noun hoz = j2_mcy(Pt6, ap, late)(wir_r, gen);
        u2_noun ryd = _mull_in(wir_r, van, sut, c3__noun, dox, hoz);
        u2_noun boc = j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen);
        u2_noun ret = 
          u2_bc(wir_r, 
                _mull_nice
                  (wir_r, van, gol, j2_mby(Pt6, fine)
                                      (wir_r, c3__very, boc, u2_h(ryd))),
                u2_rx(wir_r, u2_t(ryd)));

        u2_rz(wir_r, boc);
        u2_rz(wir_r, ryd);
        u2_rz(wir_r, hoz);

        return ret;
      }
      case c3__ktdt: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        u2_noun p_wiv = j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen);
        u2_noun q_wiv = j2_mcy(Pt6, ut, play)(wir_r, van, dox, p_gen);
        u2_noun p_hef = j2_mcy(Pt6, ut, snap)(wir_r, van, p_wiv, q_gen);
        u2_noun q_hef = j2_mcy(Pt6, ut, snap)(wir_r, van, q_wiv, q_gen);
        u2_noun ret;

        if ( u2_yes == u2_sing(p_hef, q_hef) ) {
          ret = _mull_in(wir_r, van, sut, gol, dox, p_hef);
        } else {
          u2_noun zyn = _mull_in(wir_r, van, sut, gol, dox, p_hef);

          ret = u2_bc(wir_r, u2_rx(wir_r, u2_h(zyn)),
                             j2_mcy(Pt6, ut, play)(wir_r, van, dox, q_hef));
          u2_rz(wir_r, zyn);
        }
        u2_rz(wir_r, q_hef);
        u2_rz(wir_r, p_hef);
        u2_rz(wir_r, q_wiv);
        u2_rz(wir_r, p_wiv);

        return ret;
      }
      case c3__ktlc: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        u2_noun vat = _mull_in(wir_r, van, sut, gol, dox, q_gen);
        u2_noun ret = 
          u2_bc(wir_r, 
                j2_mby(Pt6, face)(wir_r, p_gen, u2_h(vat)),
                j2_mby(Pt6, face)(wir_r, p_gen, u2_t(vat)));

        u2_rz(wir_r, vat);
        return ret;
      }
      case c3__ktld: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        u2_noun hoz = j2_mcy(Pt6, ap, late)(wir_r, gen);
        u2_noun ryd = _mull_in(wir_r, van, sut, c3__noun, dox, hoz);
        u2_noun boc = j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen);
        u2_noun ret = 
          u2_bc(wir_r, 
                _mull_nice
                  (wir_r, van, gol, j2_mby(Pt6, fine)
                                      (wir_r, c3__pure, boc, u2_h(ryd))),
                u2_rx(wir_r, u2_t(ryd)));

        u2_rz(wir_r, boc);
        u2_rz(wir_r, ryd);
        u2_rz(wir_r, hoz);

        return ret;
      }
      case c3__ktsg: p_gen = u2_t(gen);
      _mull_used(wir_r);
      {
        return _mull_in(wir_r, van, sut, gol, dox, p_gen);
      }
      case c3__ktsp: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        u2_noun p_hif = j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen);
        u2_noun q_hif = j2_mcy(Pt6, ut, play)(wir_r, van, dox, p_gen);

        u2_noun zel = _mull_in(wir_r, van, sut, p_hif, dox, q_gen);
        u2_noun ret = u2_bc(wir_r, p_hif, q_hif);

        u2_rz(wir_r, zel);
        return ret;
      }
      case c3__lcld: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        u2_noun lem = _mull_in(wir_r, van, sut, c3__noun, dox, p_gen);
        u2_noun p_lem = u2_h(lem);
        u2_noun q_lem = u2_t(lem);
        u2_noun ret = _mull_in(wir_r, van, p_lem, gol, q_lem, q_gen);

        u2_rz(wir_r, lem);
        return ret;
      }
      case c3__mtlc: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        u2_noun sec = j2_mcy(Pt6, ut, seek)(wir_r, van, sut, c3__read, p_gen);
        u2_noun suc = j2_mcy(Pt6, ut, seek)(wir_r, van, dox, c3__read, p_gen);
        u2_noun lar = _mull_foil(wir_r, sec);
        u2_noun p_lar = u2_h(lar);
        u2_noun q_lar = u2_t(lar);
        u2_noun pq_lar = u2_h(q_lar);
        u2_noun qq_lar = u2_t(q_lar);
        u2_noun vug = _mull_foil(wir_r, suc);
        u2_noun p_vug = u2_h(vug);
        u2_noun q_vug = u2_t(vug);
        u2_noun pq_vug = u2_h(q_vug);
        u2_noun qq_vug = u2_t(q_vug);
        u2_noun zoz = u2_and(u2_sing(p_lar, p_vug), u2_sing(pq_lar, pq_vug))
                        ? u2_bl_error(wir_r, "mull-bonk-f")
                        : 0;
        u2_noun mew = j2_mcy(Pt6, ut, swab)(wir_r, van, sut, qq_lar, q_gen);
        u2_noun muw = j2_mcy(Pt6, ut, swab)(wir_r, van, dox, qq_vug, q_gen);
        u2_noun qoq = u2_sing(mew, muw)
                        ? u2_bl_error(wir_r, "mull-bonk-g")
                        : 0;
        u2_noun yom = _mull_edit
          (wir_r, van, sut, dox, mew, u2_rx(wir_r, qq_lar), 
                                      u2_rx(wir_r, qq_vug));
        u2_noun von = u2_bn_molt(wir_r, van, j2_ut_van_vet, u2_no, 0);
        u2_noun p_ret = j2_mcy(Pt6, ut, fire)(wir_r, van, sut, u2_h(yom));
        u2_noun q_ret = j2_mcy(Pt6, ut, fire)(wir_r, von, sut, u2_t(yom));

        qoq = qoq; zoz = zoz;

        u2_rz(wir_r, von);
        u2_rz(wir_r, yom);
        u2_rz(wir_r, muw);
        u2_rz(wir_r, mew);
        u2_rz(wir_r, vug);
        u2_rz(wir_r, lar);

        return u2_bc(wir_r, _mull_nice(wir_r, van, gol, p_ret), q_ret);
      }
      case c3__pmdg: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        u2_noun ruf = u2_bt
          (wir_r, c3__dgsp, 
                  u2_bc(wir_r, u2_nul, _1),
                  u2_rx(wir_r, p_gen));
        u2_noun ret = _mull_grow
          (wir_r, van, sut, gol, dox, c3__zinc, ruf, q_gen);

        u2_rz(wir_r, ruf);
        return ret;
      }
      case c3__pmdt: p_gen = u2_t(gen);
      _mull_used(wir_r);
      {
        u2_noun ruf = u2_bc(wir_r, u2_nul, _1);

        ret = _mull_grow(wir_r, van, sut, gol, dox, c3__gold, ruf, p_gen);
        u2_rz(wir_r, ruf);

        return ret;
      }
      case c3__pmmt: p_gen = u2_t(gen);
      _mull_used(wir_r);
      {
        u2_noun ruf = u2_bc(wir_r, u2_nul, _1);

        ret = _mull_grow(wir_r, van, sut, gol, dox, c3__lead, ruf, p_gen);
        u2_rz(wir_r, ruf);

        return ret;
      }
      case c3__pmps: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        u2_noun ruf = u2_bt
          (wir_r, c3__dgsp, 
                  u2_bc(wir_r, u2_nul, _1),
                  u2_rx(wir_r, p_gen));
        u2_noun ret = _mull_grow
          (wir_r, van, sut, gol, dox, c3__iron, ruf, q_gen);

        u2_rz(wir_r, ruf);
        return ret;
      }
      case c3__sgld: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        return _mull_in(wir_r, van, sut, gol, dox, q_gen);
      }
      case c3__zpcm: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        u2_noun p_ret = j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen);
        u2_noun q_ret = j2_mcy(Pt6, ut, play)(wir_r, van, sut, q_gen);

        return u2_bc
          (wir_r, _mull_nice(wir_r, van, gol, p_ret),
                  q_ret);
      }
      case c3__zpcb: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      {
        u2_noun ret;

        u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, _mull_loc(wir_r, van, p_gen)));
        {
          ret = _mull_in(wir_r, van, sut, gol, dox, q_gen);
        }
        u2_bl_drop(wir_r);
        return ret;
      }
      case c3__zpdx: p_gen = u2_t(gen);
      {
        u2_bx_used(wir_r);
        return _mull_in(wir_r, van, sut, gol, dox, p_gen);
      }
      case c3__zplc: p_gen = u2_t(gen);
      _mull_used(wir_r);
      {
        return _mull_both(wir_r, van, gol, c3__noun);
      }
      case c3__zpmt:
      _mull_used(wir_r);
      {
        u2_noun pet = u2_bn_hook(wir_r, van, "seed");
        u2_noun peq = u2_rx(wir_r, u2_h(pet));

        u2_rz(wir_r, pet);
        return _mull_both(wir_r, van, gol, peq);
      }
      case c3__zptm: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mull_used(wir_r);
      {
        u2_noun p_zur = j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen);
        u2_noun q_zur = j2_mcy(Pt6, ut, play) (wir_r, van, dox, p_gen);
        u2_noun vos = _mull_in(wir_r, van, sut, c3__noun, dox, q_gen);
        u2_noun p_ret = j2_mby(Pt6, cell)(wir_r, p_zur, u2_h(vos));
        u2_noun q_ret = j2_mby(Pt6, cell)(wir_r, q_zur, u2_t(vos));

        u2_rz(wir_r, vos);
        u2_rz(wir_r, q_zur);
        u2_rz(wir_r, p_zur);

        return u2_bc
          (wir_r, _mull_nice(wir_r, van, gol, p_ret), q_ret);
      }
      case c3__zpvn:
      case c3__zpzp:
      _mull_used(wir_r);
      {
        return u2_bc(wir_r, c3__void, c3__void);
      }
    }
  }
  
  u2_flag                                                         //  transfer
  j2_mcx(Pt6, ut, mull)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gol,                              //  retain
                        u2_noun dox,                              //  retain
                        u2_noun gen)                              //  retain
  {
    if ( u2_no == _mull_vet(wir_r, van) ) {
      return u2_yes;
    } else {
      u2_noun mul = _mull_in(wir_r, van, sut, gol, dox, gen);

      u2_rz(wir_r, mul);
      return u2_yes;
    }
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, mull)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, mull)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, gol, dox, gen, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, 
                                u2_cw_sam_2, &gol, 
                                u2_cw_sam_6, &dox, 
                                u2_cw_sam_7, &gen,
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, mull)(wir_r, van, sut, gol, dox, gen);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, mull)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun gol,                              //  retain 
                        u2_noun dox,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "mull");

    if ( u2_none == hoc ) {
      c3_assert(!"register mull");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cw_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, 
                                      u2_cw_sam_2, u2_rx(wir_r, gol), 
                                      u2_cw_sam_6, u2_rx(wir_r, dox), 
                                      u2_cw_sam_7, u2_rx(wir_r, gen), 
                                      0);

      if ( (u2_none == j2_mcj(Pt6, ut, mull)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, mull)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, mull)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gol,                              //  retain
                        u2_noun dox,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, mull)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, mull)(wir_r, van, sut, gol, dox, gen);
      }
      else {
        c3_m    fun_m = u2_jet_fun_m(jet_j);
        u2_noun pro   = u2_rl_find_qual(wir_r, fun_m, sut, gol, dox, gen);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, mull)(wir_r, van, sut, gol, dox, gen);

          return u2_rl_save_qual(wir_r, fun_m, sut, gol, dox, gen, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, mull)(wir_r, van, sut, gol, dox, gen);
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
  u2_weak
  j2_mck(Pt6, ut, mull)(u2_wire wir_r,
                        u2_noun cor)
  {
    u2_noun sut, gol, dox, gen, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, 
                                u2_cw_sam_2, &gol, 
                                u2_cw_sam_6, &dox,
                                u2_cw_sam_7, &gen,
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
    {
      return u2_none;
    } else {
      return u2_rq
        (wir_r, u2_rx(wir_r, sut), 
                u2_rx(wir_r, gol), 
                u2_rx(wir_r, dox), 
                u2_rx(wir_r, gen));
    }
  }

  u2_ho_jet 
  j2_mcj(Pt6, ut, mull)[] = {
    { ".3", c3__hevy, 
        j2_mc(Pt6, ut, mull), 
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pt6, ut, mull)
    },
    { }
  };
