/* j/6/mull.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  static u2_noun
  _mull_in(u2_noun, u2_noun, u2_noun, u2_noun, u2_noun);

  static u2_bean
  _mull_vet(
            u2_noun van)
  {
    // u2_bean vet = u2_cj_hook(u2k(van), "vet");
    u2_bean vet = u2_cr_at(j2_ut_van_vet, van);

    switch ( vet ) {
      case u2_no:
      case u2_yes: return vet;
      default: return u2_cm_bail(c3__fail);
    }
  }

  static u2_noun                                                  //  produce
  _mull_core(
             u2_noun pac,                                         //  submit
             u2_noun con)                                         //  submit
  {
    if ( (c3__void == pac) ) {
      return c3__void;
    } else {
      return u2nt(c3__core, pac, con);
    }
  }

  static u2_noun
  _mull_bean()
  {
    return u2nt(c3__fork,
                        u2nq(c3__cube, 0, c3__atom, 'f'),
                        u2nq(c3__cube, 1, c3__atom, 'f'));
  }

  static u2_noun                                                  //  produce
  _mull_loc(
            u2_noun van,                                          //  retain
            u2_noun loc)                                          //  retain
  {
    u2_noun mol = u2nc('o', u2k(loc));
    u2_noun sho = u2_cj_hook(u2k(van), "show");
    u2_noun ret = u2_ci_molt(u2k(sho), u2_cv_sam, u2k(mol), 0);

    u2z(mol);
    u2z(sho);

    return ret;
  }

  static u2_noun                                                  //  produce
  _mull_foil(
             u2_noun pok)                                         //  submit
  {
    u2_noun p_pok = u2h(pok);
    u2_noun q_pok = u2t(pok);
    u2_noun ret;

    if ( u2_yes == u2h(q_pok) ) {
      u2_noun pq_pok = u2t(q_pok);

      ret = u2nc(
                  u2k(p_pok),
                  u2nc(u2_nul,
                               u2nc(
                                     u2nc(
                                           u2k(pq_pok),
                                           u2nt(c3__elm, u2_nul, 1)),
                                     u2_nul)));
    }
    else {
      u2_noun pq_pok = u2h(u2t(q_pok));
      u2_noun qq_pok = u2t(u2t(q_pok));

      ret = u2nc(u2k(p_pok),
                         u2nc(u2k(pq_pok),
                                      u2k(qq_pok)));
    }
    u2z(pok);
    return ret;
  }

  static u2_noun                                                  //  produce
  _mull_coke(
             u2_noun nug)                                         //  submit
  {
    u2_atom axe;

    if ( 0 == u2h(nug) ) {
      axe = u2t(nug);
    } else if ( 10 == u2h(nug) ) {
      u2_noun xin = u2k(u2t(u2t(nug)));

      axe = _mull_coke(xin);
    }
    else {
      return u2_cm_error("mint-coke");
    }
    u2z(nug);
    return axe;
  }

  static u2_noun
  _mull_doke(
             u2_noun van,
             u2_noun sut,
             u2_noun gen)
  {
    u2_noun fug = j2_mcy(Pt6, ut, mint)(van, sut, c3__noun, gen);
    u2_noun axe = _mull_coke(u2k(u2t(fug)));

    u2z(fug);
    return axe;
  }

  static u2_noun                                                  //  produce
  _mull_nice(
             u2_noun van,                                         //  retain
             u2_noun gol,                                         //  retain
             u2_noun typ)                                         //  submit
  {
    if ( u2_no == j2_mcy(Pt6, ut, nest)(van, gol, u2_yes, typ) ) {
      // u2_noun dun = j2_mcy(Pt6, ut, dunq)(van, "need", gol);
      // u2_noun niz = j2_mcy(Pt6, ut, dunq)(van, "have", typ);

      // u2_ct_push(u2nc(c3__mean, dun));
      // u2_ct_push(u2nc(c3__mean, niz));

      return u2_cm_error("mull-nice");
    }
    else return typ;
  }

  static void
  _mull_bake(
             u2_noun van,                                         //  retain
             u2_noun sut,                                         //  retain
             u2_noun dox,                                         //  retain
             u2_noun dab)                                         //  retain
  {
    if ( u2_nul != dab ) {
      u2_noun n_dab, l_dab, r_dab;

      u2_cr_trel(dab, &n_dab, &l_dab, &r_dab);
      if ( u2_no == u2du(n_dab) ) {
        u2_cm_bail(c3__fail);
      }
      else {
        u2_noun qn_dab = u2t(n_dab);
        u2_noun vad;

        switch ( u2h(qn_dab) ) {
          default: u2_cm_bail(c3__exit);
          case c3__ash: {
            vad = _mull_in(van, sut, c3__noun, dox, u2t(qn_dab));
            break;
          }
          case c3__elm: {
            vad = u2_nul;
            break;
          }
        }
        u2z(vad);

        if ( (u2_nul == l_dab) && (u2_nul == r_dab) ) {
          return;
        }
        else if ( (u2_nul == l_dab) ) {
          _mull_bake(van, sut, dox, r_dab);
        }
        else if ( (u2_nul == r_dab) ) {
          _mull_bake(van, sut, dox, l_dab);
        }
        else {
          _mull_bake(van, sut, dox, l_dab);
          _mull_bake(van, sut, dox, r_dab);
        }
      }
    }
  }

  static u2_noun
  _mull_grow(
             u2_noun van,
             u2_noun sut,
             u2_noun gol,
             u2_noun dox,
             u2_atom mel,
             u2_noun ruf,
             u2_noun dab)
  {
    u2_noun dan = _mull_in(van, sut, c3__noun, dox, ruf);
    u2_noun p_dan = u2h(dan);
    u2_noun q_dan = u2t(dan);
    u2_noun p_toc = _mull_core
      (u2k(p_dan),
              u2nt(c3__gold,
                           u2k(p_dan),
                           u2nc(u2_nul, u2k(dab))));
    u2_noun q_toc = _mull_core
      (u2k(q_dan),
              u2nt(c3__gold,
                           u2k(q_dan),
                           u2nc(u2_nul, u2k(dab))));
    u2_noun p_ret = _mull_core
        (u2k(p_dan),
                u2nt(u2k(mel),
                             u2k(p_dan),
                             u2nc(u2nc(u2_nul, u2_nul),
                                          u2k(dab))));
    u2_noun q_ret = _mull_core
        (u2k(q_dan),
                u2nt(u2k(mel),
                             u2k(q_dan),
                             u2nc(u2nc(u2_nul, u2_nul),
                                          u2k(dab))));
    u2_noun ret = u2nc(_mull_nice(van, gol, p_ret), q_ret);

    _mull_bake(van, p_toc, q_toc, dab);

    u2z(q_toc);
    u2z(p_toc);
    u2z(dan);

    return ret;
  }

  static u2_noun                                                  //  produce
  _mull_both(
             u2_noun van,                                         //  retain
             u2_noun gol,                                         //  retain
             u2_noun typ)                                         //  submit
  {
    return u2nc(_mull_nice(van, gol, u2k(typ)),
                        typ);
  }

  static u2_noun                                                  //  produce
  _mull_edit(
             u2_noun van,                                         //  retain
             u2_noun sut,                                         //  retain
             u2_noun dox,                                         //  retain
             u2_noun mew,                                         //  retain
             u2_noun p_yom,                                       //  submit
             u2_noun q_yom)                                       //  submit
  {
    while ( 1 ) {
      if ( u2_no == u2du(mew) ) {
        return u2nc(p_yom, q_yom);
      } else {
        u2_noun i_mew = u2h(mew);
        u2_noun t_mew = u2t(mew);
        u2_noun pi_mew = u2h(i_mew);
        u2_noun qi_mew = u2t(i_mew);
        u2_noun zil = _mull_in(van, sut, c3__noun, dox, qi_mew);
        u2_noun p_zil = u2h(zil);
        u2_noun q_zil = u2t(zil);
        u2_noun cuf = j2_mcy(Pt6, ut, tock)
              (van, sut, pi_mew, p_zil, p_yom);
        u2_noun dof = j2_mcy(Pt6, ut, tock)
              (van, sut, pi_mew, q_zil, q_yom);

        if ( u2_cr_sing(u2h(cuf), u2h(dof)) ) {
          u2_cm_error("mull-bonk-a");
        }

        u2z(p_yom);
        p_yom = u2k(u2t(cuf));

        u2z(q_yom);
        q_yom = u2k(u2t(dof));

        u2z(dof);
        u2z(cuf);
        u2z(zil);

        mew = t_mew;
      }
    }
  }

# define _mull_used()

  static u2_noun                                                  //  produce
  _mull_in(
           u2_noun van,                                           //  retain
           u2_noun sut,                                           //  retain
           u2_noun gol,                                           //  retain
           u2_noun dox,                                           //  retain
           u2_noun gen)                                           //  retain
  {
    u2_noun p_gen, q_gen, r_gen;
    u2_noun ret;

    if ( u2_no == u2du(gen) ) {
      u2_noun ter = u2_cr_at(u2_cv_con_3, van);
      u2_noun rex = j2_mcy(Pt6, ap, open)(ter, gen);

      ret = _mull_in(van, sut, gol, dox, rex);
      u2z(rex);

      return ret;
    }
    else if ( u2_yes == u2du(u2h(gen)) ) {
      _mull_used();
      {
        u2_noun hed = _mull_in(van, sut, c3__noun, dox, u2h(gen));
        u2_noun tal = _mull_in(van, sut, c3__noun, dox, u2t(gen));
        u2_noun dis = j2_mby(Pt6, cell)(u2h(hed), u2h(tal));
        u2_noun dat = j2_mby(Pt6, cell)(u2t(hed), u2t(tal));
        u2_noun ret = u2nc(_mull_nice(van, gol, dis), dat);

        u2z(tal);
        u2z(hed);

        return ret;
      }
    }
    else switch ( u2h(gen) ) {
      default: {
        u2_noun ter = u2_cr_at(u2_cv_con_3, van);
        u2_noun rex = j2_mcy(Pt6, ap, open)(ter, gen);

        if ( u2_yes == u2_cr_sing(rex, gen) ) {
#if 1
          u2_noun zix = j2_mcy(Pt6, ut, shep)
                (van, "gene", 'q', u2k(gen));

          u2_ct_push(u2nc(c3__mean, zix));
          return u2_cm_error("mull-open");
#else
          u2_err("h", u2h(gen));
          return u2_cm_bail(c3__fail);
#endif
        }
        ret = _mull_in(van, sut, gol, dox, rex);
        u2z(rex);

        return ret;
      }

      case c3__bcpt: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun sep = j2_mcy(Pt6, ut, seep)(van, sut, c3__read, p_gen);
        u2_noun pox = j2_mcy(Pt6, ut, seep)(van, dox, c3__read, p_gen);
        u2_noun axe = u2h(sep);

        if ( axe != u2h(pox) ) {
          return u2_cm_error("mull-bonk-wing");
        }
        else {
          u2_noun rex = j2_mcy(Pt6, al, whip)(van, q_gen, axe);
          u2_noun ret = _mull_in(van, sut, gol, dox, rex);

          u2z(sep);
          u2z(pox);
          u2z(rex);

          return ret;
        }
      }

      case c3__wtts: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun nob = j2_mcy(Pt6, al, bunt)(van, p_gen);
        u2_noun p_waz = j2_mcy(Pt6, ut, play)(van, sut, nob);
        u2_noun q_waz = j2_mcy(Pt6, ut, play)(van, dox, nob);
        u2_noun dok   = u2nc(c3__cnzz, q_gen);
        u2_noun p_syx = _mull_doke(van, sut, dok);
        u2_noun q_syx = _mull_doke(van, dox, dok);
        u2_noun p_pov = j2_mcy(Pt6, ut, fish)(van, p_waz, p_syx);
        u2_noun q_pov = j2_mcy(Pt6, ut, fish)(van, q_waz, q_syx);

        if ( (u2_no == u2_cr_sing(p_syx, q_syx)) ||
             (u2_no == u2_cr_sing(p_pov, q_pov)) )
        {
          return u2_cm_error("mull-bonk-b");
        }
        u2z(p_waz); u2z(q_waz);
        u2z(p_syx); u2z(q_syx);
        u2z(p_pov); u2z(q_pov);
        u2z(nob);
        u2z(dok);

        return _mull_both(van, gol, _mull_bean());
      }

      case c3__wtcl: u2_cx_trel(u2t(gen), &p_gen, &q_gen, &r_gen);
      _mull_used();
      {
        u2_noun bol = _mull_bean();
        u2_noun nor = _mull_in(van, sut, bol, dox, p_gen);
        u2_noun p_fex = j2_mcy(Pt6, ut, gain)(van, sut, p_gen);
        u2_noun q_fex = j2_mcy(Pt6, ut, gain)(van, dox, p_gen);
        u2_noun p_wux = j2_mcy(Pt6, ut, lose)(van, sut, p_gen);
        u2_noun q_wux = j2_mcy(Pt6, ut, lose)(van, dox, p_gen);
        u2_noun hiq, ran;
        u2_noun dis, dat;
        u2_noun ret;

        if ( c3__void == p_fex ) {
          hiq = u2nc(
                      c3__void,
                      (c3__void == q_fex)
                        ? c3__void
                        : j2_mcy(Pt6, ut, play)(van, q_fex, q_gen));
        } else if ( c3__void == q_fex ) {
          hiq = u2_cm_error("mull-bonk-c");
        }
        else hiq = _mull_in(van, p_fex, gol, q_fex, q_gen);

        if ( c3__void == p_wux ) {
          ran = u2nc(
                      c3__void,
                      (c3__void == q_wux)
                        ? c3__void
                        : j2_mcy(Pt6, ut, play)(van, q_wux, r_gen));
        } else if ( c3__void == q_wux ) {
          ran = u2_cm_error("mull-bonk-d");
        }
        else ran = _mull_in(van, p_wux, gol, q_wux, r_gen);

        dis = j2_mby(Pt6, fork)(u2h(hiq), u2h(ran));
        dat = j2_mby(Pt6, fork)(u2t(hiq), u2t(ran));

        ret = u2nc(_mull_nice(van, gol, dis), dat);

        u2z(ran);
        u2z(hiq);
        u2z(q_wux);
        u2z(p_wux);
        u2z(q_fex);
        u2z(p_fex);
        u2z(nor);
        u2z(bol);

        return ret;
      }
      case c3__clhp: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun hed = _mull_in(van, sut, c3__noun, dox, p_gen);
        u2_noun tal = _mull_in(van, sut, c3__noun, dox, q_gen);
        u2_noun dis = j2_mby(Pt6, cell)(u2h(hed), u2h(tal));
        u2_noun dat = j2_mby(Pt6, cell)(u2t(hed), u2t(tal));
        u2_noun ret = u2nc(_mull_nice(van, gol, dis), dat);

        u2z(tal);
        u2z(hed);

        return ret;
      }
      case c3__dtts: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun hed = _mull_in(van, sut, c3__noun, dox, p_gen);
        u2_noun tal = _mull_in(van, sut, c3__noun, dox, q_gen);

        u2z(hed);
        u2z(tal);

        return _mull_both(van, gol, _mull_bean());
      }
      case c3__dtwt: p_gen = u2t(gen);
      _mull_used();
      {
        u2_noun vay = _mull_in(van, sut, c3__noun, dox, p_gen);

        u2z(vay);
        return _mull_both(van, gol, _mull_bean());
      }
      case c3__dtkt: p_gen = u2t(gen);
      _mull_used();
      {
        u2_noun wuq = c3__noun;
        u2_noun vay = _mull_in(van, sut, wuq, dox, p_gen);

        u2z(vay);
        return _mull_both(van, gol, wuq);
      }
      case c3__dtls: p_gen = u2t(gen);
      _mull_used();
      {
        u2_noun wuq = u2nc(c3__atom, u2_blip);
        u2_noun vay = _mull_in(van, sut, wuq, dox, p_gen);

        u2z(vay);
        return _mull_both(van, gol, wuq);
      }
      case c3__dtzz: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun tof = u2nt
          (c3__cube,
                  u2k(q_gen),
                  (u2_yes == u2du(q_gen))
                    ? c3__noun
                    : u2nc(c3__atom, u2k(p_gen)));

        return _mull_both(van, gol, tof);
      }
      case c3__dttr: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun hed = _mull_in(van, sut, c3__noun, dox, p_gen);
        u2_noun tal = _mull_in(van, sut, c3__noun, dox, q_gen);

        u2z(hed);
        u2z(tal);

        return _mull_both(van, gol, c3__noun);
      }
      case c3__dtzy: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun typ = j2_mcy(Pt6, ut, play)(van, sut, gen);
        return _mull_both(van, gol, typ);
      }
      case c3__ktbr: p_gen = u2t(gen);
      _mull_used();
      {
        u2_noun vat = _mull_in(van, sut, gol, dox, p_gen);
        u2_noun pro = u2nc
          (j2_mcy(Pt6, ut, wrap)(van, u2h(vat), c3__iron),
                  j2_mcy(Pt6, ut, wrap)(van, u2t(vat), c3__iron));

        u2z(vat);
        return pro;
      }
      case c3__ktpm: p_gen = u2t(gen);
      _mull_used();
      {
        u2_noun vat = _mull_in(van, sut, gol, dox, p_gen);
        u2_noun pro = u2nc
          (j2_mcy(Pt6, ut, wrap)(van, u2h(vat), c3__zinc),
                  j2_mcy(Pt6, ut, wrap)(van, u2t(vat), c3__zinc));

        u2z(vat);
        return pro;
      }
      case c3__ktwt: p_gen = u2t(gen);
      _mull_used();
      {
        u2_noun vat = _mull_in(van, sut, gol, dox, p_gen);
        u2_noun pro = u2nc
          (j2_mcy(Pt6, ut, wrap)(van, u2h(vat), c3__lead),
                  j2_mcy(Pt6, ut, wrap)(van, u2t(vat), c3__lead));

        u2z(vat);
        return pro;
      }
      case c3__ktts: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun vat = _mull_in(van, sut, gol, dox, q_gen);
        u2_noun ret =
          u2nc(
                j2_mcy(Pt6, ut, conk)(van, u2h(vat), p_gen),
                j2_mcy(Pt6, ut, conk)(van, u2t(vat), p_gen));

        u2z(vat);
        return ret;
      }
      case c3__ktzp: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun vat = _mull_in(van, sut, gol, dox, q_gen);
        u2_noun ret =
          u2nc(
                j2_mcy(Pt6, ut, conk)(van, u2h(vat), p_gen),
                j2_mcy(Pt6, ut, conk)(van, u2t(vat), p_gen));

        u2z(vat);
        return ret;
      }
      case c3__ktsg: p_gen = u2t(gen);
      _mull_used();
      {
        return _mull_in(van, sut, gol, dox, p_gen);
      }
      case c3__ktls: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun p_hif = _mull_nice
          (van, gol, j2_mcy(Pt6, ut, play)(van, sut, p_gen));
        u2_noun q_hif = j2_mcy(Pt6, ut, play)(van, dox, p_gen);

        u2_noun zel = _mull_in(van, sut, p_hif, dox, q_gen);
        u2_noun ret = u2nc(p_hif, q_hif);

        u2z(zel);
        return ret;
      }
      case c3__kthx: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun p_hif = _mull_nice
          (van, gol, j2_mcy(Pt6, ut, play)(van, sut, p_gen));
        u2_noun q_hif = j2_mcy(Pt6, ut, play)(van, dox, p_gen);

        u2_noun zel = _mull_in(van, sut, p_hif, dox, q_gen);
        u2_noun ret = u2nc(p_hif, q_hif);

        u2z(zel);
        return ret;
      }
      case c3__tsgr: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun lem = _mull_in(van, sut, c3__noun, dox, p_gen);
        u2_noun p_lem = u2h(lem);
        u2_noun q_lem = u2t(lem);
        u2_noun ret = _mull_in(van, p_lem, gol, q_lem, q_gen);

        u2z(lem);
        return ret;
      }
      case c3__tstr: u2_cx_trel(u2t(gen), &p_gen, &q_gen, &r_gen);
      _mull_used();
      {
        u2_noun sep = j2_mcy(Pt6, ut, seep)(van, sut, c3__both, q_gen);
        u2_noun pox = j2_mcy(Pt6, ut, seep)(van, dox, c3__both, q_gen);
        u2_noun bid = u2nt(u2k(p_gen), u2k(q_gen), sep);
        u2_noun yub = u2nt(u2k(p_gen), u2k(q_gen), pox);
        u2_noun boc = j2_mby(Pt6, bull)(bid, sut);
        u2_noun nuf = j2_mby(Pt6, bull)(yub, dox);
        u2_noun ret = _mull_in(van, boc, gol, nuf, r_gen);

        u2z(bid);
        u2z(yub);
        u2z(boc);
        u2z(nuf);

        return ret;
      }
      case c3__cnts: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun sec = j2_mcy(Pt6, ut, seek)(van, sut, c3__read, p_gen);
        u2_noun suc = j2_mcy(Pt6, ut, seek)(van, dox, c3__read, p_gen);
        u2_noun lar = _mull_foil(sec);
        u2_noun p_lar = u2h(lar);
        u2_noun q_lar = u2t(lar);
        u2_noun pq_lar = u2h(q_lar);
        u2_noun qq_lar = u2t(q_lar);
        u2_noun vug = _mull_foil(suc);
        u2_noun p_vug = u2h(vug);
        u2_noun q_vug = u2t(vug);
        u2_noun pq_vug = u2h(q_vug);
        u2_noun qq_vug = u2t(q_vug);

        if ( u2_and(u2_cr_sing(p_lar, p_vug), u2_cr_sing(pq_lar, pq_vug)) ) {
          u2_cm_error("mull-bonk-e");
        }
        {
          u2_noun mew = j2_mcy(Pt6, ut, snub)(van, sut, q_gen);
          u2_noun yom = _mull_edit
            (van, sut, dox, mew, u2k(qq_lar),
                                        u2k(qq_vug));
          u2_noun von = u2_ci_molt(u2k(van), j2_ut_van_vet, u2_no, 0);
          u2_noun p_ret = j2_mcy(Pt6, ut, fire)(van, sut, u2h(yom));
          u2_noun q_ret = j2_mcy(Pt6, ut, fire)(von, sut, u2t(yom));

          u2z(von);
          u2z(yom);
          u2z(mew);
          u2z(vug);
          u2z(lar);

          return u2nc(_mull_nice(van, gol, p_ret), q_ret);
        }
      }
      case c3__pmcl: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun ruf = u2nt
          (c3__clhp,
                  u2nc(u2_nul, 1),
                  u2k(p_gen));
        u2_noun ret = _mull_grow
          (van, sut, gol, dox, c3__zinc, ruf, q_gen);

        u2z(ruf);
        return ret;
      }
      case c3__brcn: p_gen = u2t(gen);
      _mull_used();
      {
        u2_noun ruf = u2nc(u2_nul, 1);

        ret = _mull_grow(van, sut, gol, dox, c3__gold, ruf, p_gen);
        u2z(ruf);

        return ret;
      }
      case c3__pmcn: p_gen = u2t(gen);
      _mull_used();
      {
        u2_noun ruf = u2nc(u2_nul, 1);

        ret = _mull_grow(van, sut, gol, dox, c3__lead, ruf, p_gen);
        u2z(ruf);

        return ret;
      }
      case c3__pmls: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun ruf = u2nt
          (c3__clhp,
                  u2nc(u2_nul, 1),
                  u2k(p_gen));
        u2_noun ret = _mull_grow
          (van, sut, gol, dox, c3__iron, ruf, q_gen);

        u2z(ruf);
        return ret;
      }
      case c3__sgzp: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun typ = j2_mcy(Pt6, ut, play)(van, sut, p_gen);
        u2_noun dug = j2_mcy(Pt6, ut, duck)(van, typ);
        u2_noun ret;

        u2_ct_push(u2nc(c3__mean, dug));
        {
          ret = _mull_in(van, sut, gol, dox, q_gen);
        }
        u2_ct_drop();

        u2z(typ);
        return ret;
      }
      case c3__sggr: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        return _mull_in(van, sut, gol, dox, q_gen);
      }
      case c3__zpcm: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun p_ret = j2_mcy(Pt6, ut, play)(van, sut, p_gen);
        u2_noun q_ret = j2_mcy(Pt6, ut, play)(van, sut, q_gen);

        return u2nc
          (_mull_nice(van, gol, p_ret),
                  q_ret);
      }
      case c3__zpcb: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      {
        u2_noun ret;

        u2_ct_push(u2nc(c3__mean, _mull_loc(van, p_gen)));
        {
          ret = _mull_in(van, sut, gol, dox, q_gen);
        }
        u2_ct_drop();
        return ret;
      }
      case c3__zpts: p_gen = u2t(gen);
      _mull_used();
      {
        return _mull_both(van, gol, c3__noun);
      }
      case c3__zpcn:
      _mull_used();
      {
        u2_noun pet = u2_cj_hook(u2k(van), "seed");
        u2_noun peq = u2k(u2h(pet));

        u2z(pet);
        return _mull_both(van, gol, peq);
      }
      case c3__zpsm: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u2_noun p_zur = j2_mcy(Pt6, ut, play)(van, sut, p_gen);
        u2_noun q_zur = j2_mcy(Pt6, ut, play) (van, dox, p_gen);
        u2_noun vos = _mull_in(van, sut, c3__noun, dox, q_gen);
        u2_noun p_ret = j2_mby(Pt6, cell)(p_zur, u2h(vos));
        u2_noun q_ret = j2_mby(Pt6, cell)(q_zur, u2t(vos));

        u2z(vos);
        u2z(q_zur);
        u2z(p_zur);

        return u2nc
          (_mull_nice(van, gol, p_ret), q_ret);
      }
      case c3__zpfs:
      case c3__zpzp:
      _mull_used();
      {
        return u2nc(c3__void, c3__void);
      }
    }
  }

  u2_bean                                                         //  transfer
  j2_mcx(Pt6, ut, mull)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gol,                              //  retain
                        u2_noun dox,                              //  retain
                        u2_noun gen)                              //  retain
  {
    if ( u2_no == _mull_vet(van) ) {
      return u2_yes;
    } else {
      u2_noun mul = _mull_in(van, sut, gol, dox, gen);

      u2z(mul);
      return u2_yes;
    }
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, mull)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, mull)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, gol, dox, gen, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &gol,
                                u2_cv_sam_6, &dox,
                                u2_cv_sam_7, &gen,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, mull)(van, sut, gol, dox, gen);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, mull)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gol,                              //  retain
                        u2_noun dox,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_weak hoc = u2_cj_look(u2k(van), "mull");

    if ( u2_none == hoc ) {
      c3_assert(!"register mull");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(u2k(gat),
                                      u2_cv_sam_2, u2k(gol),
                                      u2_cv_sam_6, u2k(dox),
                                      u2_cv_sam_7, u2k(gen),
                                      0);

      if ( (u2_none == j2_mcj(Pt6, ut, mull)[0].xip) ) {
        u2_noun xip = u2_cj_find(u2k(cor));

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, mull)[0].xip = xip;
      }
      u2z(gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, mull)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gol,                              //  retain
                        u2_noun dox,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, mull)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, mull)(van, sut, gol, dox, gen);
      }
      else {
        c3_m    fun_m = c3__mull;
        u2_noun pro   = u2_ch_find_4(fun_m, sut, gol, dox, gen);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, mull)(van, sut, gol, dox, gen);

          return u2_ch_save_4(fun_m, sut, gol, dox, gen, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, mull)(van, sut, gol, dox, gen);
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
  u2_weak
  j2_mck(Pt6, ut, mull)(
                        u2_noun cor)
  {
    u2_noun sut, gol, dox, gen, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &gol,
                                u2_cv_sam_6, &dox,
                                u2_cv_sam_7, &gen,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2nq
        (u2k(sut),
                u2k(gol),
                u2k(dox),
                u2k(gen));
    }
  }

  u2_ho_jet
  j2_mcj(Pt6, ut, mull)[] = {
    { ".2", c3__hevy,
        j2_mc(Pt6, ut, mull),
        Tier6_b,
        u2_none, u2_none,
        j2_mck(Pt6, ut, mull), c3__mull,
    },
    { }
  };
