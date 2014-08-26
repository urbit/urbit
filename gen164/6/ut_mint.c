/* j/6/mint.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

int FOO;

/* logic
*/
  static u2_noun
  _mint_in(u2_noun, u2_noun, u2_noun, u2_noun);

  static u2_noun
  _mint_bean(void)
  {
    return u2nt(c3__fork,
                        u2nq(c3__cube, 0, c3__atom, 'f'),
                        u2nq(c3__cube, 1, c3__atom, 'f'));
  }

  static u2_bean
  _mint_vet(
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
  _mint_core(
             u2_noun pac,                                         //  submit
             u2_noun con)                                         //  submit
  {
    if ( (c3__void == pac) ) {
      return c3__void;
    } else {
      return u2nt(c3__core, pac, con);
    }
  }

  static u2_noun                                                  //  produce
  _mint_foil(
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

  static u2_noun                                                //  produce
  _mint_cond(
             u2_noun pex,                                       //  submit
             u2_noun yom,                                       //  submit
             u2_noun woq)                                       //  submit
  {
    if ( 1 == u2h(pex) ) {
      if ( 0 == u2t(pex) ) {
        u2z(pex);
        u2z(woq);

        return yom;
      }
      else if ( 1 == u2t(pex) ) {
        u2z(pex);
        u2z(yom);

        return woq;
      }
    }
    return u2nq(6, pex, yom, woq);
  }

  static u2_noun
  _mint_corn(
             u2_noun van,
             u2_noun sut,
             u2_noun gen)
  {
    u2_noun mil = _mint_in(van, sut, c3__noun, gen);
    u2_noun fol = u2k(u2t(mil));

    u2z(mil);
    return fol;
  }

  static u2_noun                                                  //  produce
  _mint_nice(
             u2_noun van,                                         //  retain
             u2_noun gol,                                         //  retain
             u2_noun typ)                                         //  submit
  {
    if ( (u2_yes == _mint_vet(van)) &&
         (u2_no == j2_mcy(Pt6, ut, nest)(van, gol, u2_yes, typ)) )
    {
      // u2_noun dun = j2_mcy(Pt6, ut, dunq)(van, "need", gol);
      // u2_noun niz = j2_mcy(Pt6, ut, dunq)(van, "have", typ);

      // u2_ct_push(u2nc(c3__mean, dun));
      // u2_ct_push(u2nc(c3__mean, niz));

      return u2_cm_error("mint-nice");
    }
    else return typ;
  }

  static u2_noun                                                  //  produce
  _mint_coke(
             u2_noun nug)                                         //  submit
  {
    u2_atom axe;

    if ( 0 == u2h(nug) ) {
      axe = u2t(nug);
    } else if ( 10 == u2h(nug) ) {
      u2_noun xin = u2k(u2t(u2t(nug)));

      axe = _mint_coke(xin);
    }
    else {
      return u2_cm_error("mint-coke");
    }
    u2z(nug);
    return axe;
  }

  static u2_noun                                                  //  produce
  _mint_edit(
             u2_noun van,                                         //  retain
             u2_noun sut,                                         //  retain
             u2_noun mew,                                         //  retain
             u2_noun p_lar,                                       //  retain
             u2_noun rag,                                         //  submit
             u2_noun hej)                                         //  submit
  {
    while ( 1 ) {
      if ( u2_no == u2du(mew) ) {
        u2_noun gim = j2_mcy(Pt6, ut, fire)(van, sut, rag);
        u2_noun fol = j2_mby(Pt6, hike)(p_lar, hej);

        u2z(rag);
        u2z(hej);

        return u2nc(gim, fol);
      } else {
        u2_noun i_mew = u2h(mew);
        u2_noun t_mew = u2t(mew);
        u2_noun pi_mew = u2h(i_mew);
        u2_noun qi_mew = u2t(i_mew);
        u2_noun zil = j2_mcy(Pt6, ut, mint)(van, sut, c3__noun, qi_mew);
        u2_noun p_zil = u2h(zil);
        u2_noun q_zil = u2t(zil);
        u2_noun wip = j2_mcy(Pt6, ut, tock)
          (van, sut, pi_mew, p_zil, rag);

        u2z(rag);
        rag = u2k(u2t(wip));

        hej = u2nc(u2nc(u2k(u2h(wip)),
                                        u2k(q_zil)),
                           hej);

        u2z(zil);
        u2z(wip);

        mew = t_mew;
      }
    }
  }

  static u2_noun
  _mint_brew(
             u2_noun van,
             u2_noun sut,
             u2_bean tov,
             u2_noun gen)
  {
    u2_noun von;

    switch ( tov ) {
      default: return u2_cm_bail(c3__fail);
      case u2_yes:
        von = u2k(van); break;
      case u2_no:
        von = u2_ci_molt(u2k(van), j2_ut_van_vet, u2_no, 0); break;
    }
    {
      u2_noun mil = j2_mcy(Pt6, ut, mint)(von, sut, c3__noun, gen);
      u2_noun fol = u2k(u2t(mil));

      u2z(mil);
      u2z(von);
      return fol;
    }
  }

  static u2_noun                                                  //  produce
  _mint_bake(
             u2_noun van,                                         //  retain
             u2_noun sut,                                         //  retain
             u2_noun dab)                                         //  retain
  {
    if ( (u2_nul == dab) ) {
      return 0;
    }
    else {
      u2_noun n_dab, l_dab, r_dab;

      u2_cr_trel(dab, &n_dab, &l_dab, &r_dab);
      if ( u2_no == u2du(n_dab) ) {
        return u2_cm_bail(c3__fail);
      }
      else {
        u2_noun qn_dab = u2t(n_dab);
        u2_noun vad;

        switch ( u2h(qn_dab) ) {
          default: return u2_cm_bail(c3__exit);
          case c3__ash: {
            vad = _mint_brew(van, sut, u2_yes, u2t(qn_dab));
            break;
          }
          case c3__elm: {
            vad = _mint_brew(van, sut, u2_no, u2t(qn_dab));
            break;
          }
        }

        if ( (u2_nul == l_dab) && (u2_nul == r_dab) ) {
          return vad;
        }
        else if ( (u2_nul == l_dab) ) {
          return u2nc
            (vad, _mint_bake(van, sut, r_dab));
        }
        else if ( (u2_nul == r_dab) ) {
          return u2nc
            (vad, _mint_bake(van, sut, l_dab));
        }
        else {
          return u2nt
            (vad,
                    _mint_bake(van, sut, l_dab),
                    _mint_bake(van, sut, r_dab));
        }
      }
    }
  }

  static u2_noun
  _mint_grow(
             u2_noun van,
             u2_noun sut,
             u2_noun gol,
             u2_atom mel,
             u2_noun ruf,
             u2_noun dab)
  {
    u2_noun dan = _mint_in(van, sut, c3__noun, ruf);
    u2_noun p_dan = u2h(dan);
    u2_noun q_dan = u2t(dan);
    u2_noun toc = _mint_core
      (u2k(p_dan),
              u2nt(c3__gold,
                           u2k(p_dan),
                           u2nc(u2_nul, u2k(dab))));
    u2_noun dez = _mint_bake(van, toc, dab);
    u2_noun zod = u2nc(1, dez);
    u2_noun cot = _mint_core
      (u2k(p_dan),
              u2nt(mel,
                           u2k(p_dan),
                           u2nc(u2k(dez),
                                        u2k(dab))));
    u2_noun ret = u2nc
      (
       _mint_nice(van, gol, cot),
       j2_mby(Pt6, cons)(zod, q_dan));

    u2z(zod);
    u2z(toc);
    u2z(dan);

    return ret;
  }

  static u2_noun                                                  //  produce
  _mint_loc(
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

# define _mint_used()

  static u2_noun                                                  //  produce
  _mint_in(
           u2_noun van,                                           //  retain
           u2_noun sut,                                           //  retain
           u2_noun gol,                                           //  retain
           u2_noun gen)                                           //  retain
  {
    u2_noun p_gen, q_gen, r_gen;
    u2_noun ret;

    if ( (c3__void == sut) &&
         !((u2_yes == u2du(gen)) && (c3__zpcb == u2h(gen))) )
    {
      if ( (u2_no == _mint_vet(van))
           || ((u2_yes == u2du(gen)) &&
               ((c3__zpfs == u2h(gen)) || (c3__zpzp == u2h(gen)))) )
      {
        return u2nt(c3__void, 0, 0);
      }
      else {
        return u2_cm_error("mint-vain");
      }
    }

    if ( u2_no == u2du(gen) ) {
      u2_noun ter = u2_cr_at(u2_cv_con_3, van);
      u2_noun rex = j2_mcy(Pt6, ap, open)(ter, gen);

      ret = _mint_in(van, sut, gol, rex);
      u2z(rex);

      return ret;
    }
    else if ( u2_yes == u2du(u2h(gen)) ) {
      _mint_used();
      {
        u2_noun hed = _mint_in(van, sut, c3__noun, u2h(gen));
        u2_noun tal = _mint_in(van, sut, c3__noun, u2t(gen));
        u2_noun typ = j2_mby(Pt6, cell)(u2h(hed), u2h(tal));

        ret = u2nc
          (
           _mint_nice(van, gol, typ),
           j2_mby(Pt6, cons)(u2t(hed), u2t(tal)));

        u2z(hed);
        u2z(tal);

        return ret;
      }
    } else switch ( u2h(gen) ) {
      default: {
        u2_noun ter = u2_cr_at(u2_cv_con_3, van);
        u2_noun rex = j2_mcy(Pt6, ap, open)(ter, gen);

        if ( u2_yes == u2_cr_sing(rex, gen) ) {
#if 1
          u2_noun zix = j2_mcy(Pt6, ut, shep)
                (van, "gene", 'q', u2k(gen));

          u2_ct_push(u2nc(c3__mean, zix));
          return u2_cm_error("mint-open");
#else
          u2_err("h", u2h(gen));
          return u2_cm_bail(c3__fail);
#endif
        }
        ret = _mint_in(van, sut, gol, rex);
        u2z(rex);

        return ret;
      }

      case c3__bcpt: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun sep = j2_mcy(Pt6, ut, seep)(van, sut, c3__read, p_gen);
        u2_noun axe = u2h(sep);
        u2_noun rex = j2_mcy(Pt6, al, whip)(van, q_gen, axe);
        u2_noun ret = _mint_in(van, sut, gol, rex);

        u2z(sep);
        u2z(rex);

        return ret;
      }

      case c3__wtts: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun nob = j2_mcy(Pt6, al, bunt)(van, p_gen);
        u2_noun dok = u2nc(c3__cnzz, q_gen);
        u2_noun vol = _mint_corn(van, sut, dok);
        u2_noun axe = _mint_coke(vol);
        u2_noun wam = j2_mcy(Pt6, ut, play)(van, sut, nob);

        ret = u2nc
          (
           _mint_nice(van, gol, _mint_bean()),
           j2_mcy(Pt6, ut, fish)(van, wam, axe));

        u2z(axe);
        u2z(wam);
        u2z(nob);
        u2z(dok);

        return ret;
      }

      case c3__wtcl: u2_cx_trel(u2t(gen), &p_gen, &q_gen, &r_gen);
      _mint_used();
      {
        u2_noun bol = _mint_bean();
        u2_noun nor = _mint_in(van, sut, bol, p_gen);
        u2_noun fex = j2_mcy(Pt6, ut, gain)(van, sut, p_gen);
        u2_noun wux = j2_mcy(Pt6, ut, lose)(van, sut, p_gen);
        u2_noun duy = (c3__void == fex)
                        ? ( (c3__void == wux)
                             ?  u2nc(0, 0)
                             :  u2nc(1, 1) )
                        : ( (c3__void == wux)
                            ?  u2nc(1, 0)
                            :  u2k(u2t(nor)) );
        u2_noun hiq = _mint_in(van, fex, gol, q_gen);
        u2_noun ran = _mint_in(van, wux, gol, r_gen);

        ret = u2nc
          (j2_mby(Pt6, fork)(u2h(hiq), u2h(ran)),
                  _mint_cond(duy,
                                    u2k(u2t(hiq)),
                                    u2k(u2t(ran))));

        u2z(ran);
        u2z(hiq);
        u2z(nor);
        u2z(wux);
        u2z(fex);
        u2z(bol);

        return ret;
      }
      case c3__clhp: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun hed = _mint_in(van, sut, c3__noun, p_gen);
        u2_noun tal = _mint_in(van, sut, c3__noun, q_gen);
        u2_noun typ = j2_mby(Pt6, cell)(u2h(hed), u2h(tal));

        ret = u2nc
          (
           _mint_nice(van, gol, typ),
           j2_mby(Pt6, cons)(u2t(hed), u2t(tal)));

        u2z(hed);
        u2z(tal);

        return ret;
      }
      case c3__dtts: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun typ = _mint_nice(van, gol, _mint_bean());
        u2_noun one = _mint_corn(van, sut, p_gen);
        u2_noun two = _mint_corn(van, sut, q_gen);

        return u2nc(typ, u2nt(5, one, two));
      }
      case c3__dtwt: p_gen = u2t(gen);
      _mint_used();
      {
        u2_noun typ = _mint_nice(van, gol, _mint_bean());

        return u2nc
          (
           typ,
           u2nc(3, _mint_corn(van, sut, p_gen)));
      }
      case c3__dtkt: p_gen = u2t(gen);
      _mint_used();
      {
        u2_noun tom = c3__noun;
        u2_noun sam = _mint_in(van, sut, tom, p_gen);

        ret = u2nc
          (
           _mint_nice(van, gol, tom),
           u2nc(11, u2k(u2t(sam))));

        u2z(sam);
        return ret;
      }
      case c3__dtls: p_gen = u2t(gen);
      _mint_used();
      {
        u2_noun tom = u2nc(c3__atom, u2_blip);
        u2_noun sam = _mint_in(van, sut, tom, p_gen);

        ret = u2nc
          (
           _mint_nice(van, gol, tom),
           u2nc(4, u2k(u2t(sam))));

        u2z(sam);
        return ret;
      }
      case c3__dtzz: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun boc = (u2_no == u2ud(q_gen))
                        ? c3__noun
                        : u2nc(c3__atom, u2k(p_gen));
        u2_noun typ = j2_mby(Pt6, cube)(q_gen, boc);
        u2_noun ret =
            u2nc(
                  _mint_nice(van, gol, typ),
                  u2nc(1, u2k(q_gen)));

        u2z(boc);
        return ret;
      }
      case c3__dttr: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun one = _mint_corn(van, sut, p_gen);
        u2_noun two = _mint_corn(van, sut, q_gen);

        return u2nc
          (
           _mint_nice(van, gol, c3__noun),
           u2nt(2, one, two));
      }
      case c3__dtzy: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun typ = j2_mcy(Pt6, ut, play)(van, sut, gen);
        u2_noun ret =
            u2nc(
                  _mint_nice(van, gol, typ),
                  u2nc(1, u2k(q_gen)));

        return ret;
      }
      case c3__ktbr: p_gen = u2t(gen);
      _mint_used();
      {
        u2_noun ryd = _mint_in(van, sut, gol, p_gen);
        u2_noun tyf = j2_mcy(Pt6, ut, wrap)(van, u2h(ryd), c3__iron);
        u2_noun pro = u2nc(tyf, u2k(u2t(ryd)));

        u2z(ryd);
        return pro;
      }
      case c3__ktpm: p_gen = u2t(gen);
      _mint_used();
      {
        u2_noun ryd = _mint_in(van, sut, gol, p_gen);
        u2_noun tyf = j2_mcy(Pt6, ut, wrap)(van, u2h(ryd), c3__zinc);
        u2_noun pro = u2nc(tyf, u2k(u2t(ryd)));

        u2z(ryd);
        return pro;
      }
      case c3__ktwt: p_gen = u2t(gen);
      _mint_used();
      {
        u2_noun ryd = _mint_in(van, sut, gol, p_gen);
        u2_noun tyf = j2_mcy(Pt6, ut, wrap)(van, u2h(ryd), c3__lead);
        u2_noun pro = u2nc(tyf, u2k(u2t(ryd)));

        u2z(ryd);
        return pro;
      }
      case c3__ktts: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun vat = _mint_in(van, sut, gol, q_gen);

        ret = u2nc
          (
           j2_mcy(Pt6, ut, conk)(van, u2h(vat), p_gen),
           u2k(u2t(vat)));

        u2z(vat);
        return ret;
      }
      case c3__ktzp: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun vat = _mint_in(van, sut, gol, q_gen);

        ret = u2nc
          (
           j2_mcy(Pt6, ut, conk)(van, u2h(vat), p_gen),
           u2k(u2t(vat)));

        u2z(vat);
        return ret;
      }
      case c3__ktsg: p_gen = u2t(gen);
      _mint_used();
      {
        u2_noun nef = _mint_in(van, sut, gol, p_gen);
        u2_noun p_nef = u2h(nef);
        u2_noun q_nef = u2t(nef);
        u2_noun fom;

        {
          u2_noun cag = j2_mcy(Pt6, ut, burn)(van, sut);
          u2_noun wim = u2_cn_nock_an(cag, u2k(q_nef));

          if ( 0 == u2h(wim) ) {
            fom = u2nc(1, u2k(u2t(wim)));
          } else {
            fom = u2k(q_nef);
          }
          u2z(wim);
        }
        ret = u2nc(u2k(p_nef), fom);

        u2z(nef);
        return ret;
      }
      case c3__ktls: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun huz = j2_mcy(Pt6, ut, play)(van, sut, p_gen);
        u2_noun hif = _mint_nice(van, gol, huz);
        u2_noun zel = _mint_in(van, sut, hif, q_gen);
        u2_noun ret = u2nc(hif, u2k(u2t(zel)));

        u2z(zel);
        return ret;
      }
      case c3__kthx: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun huz = j2_mcy(Pt6, ut, play)(van, sut, p_gen);
        u2_noun hif = _mint_nice(van, gol, huz);
        u2_noun zel = _mint_in(van, sut, hif, q_gen);
        u2_noun ret = u2nc(hif, u2k(u2t(zel)));

#if 0
        {
          u2_noun goz = j2_mcy(Pt6, ut, play)(van, sut, q_gen);
          u2_noun bar;
          
          FOO = 1;
          fprintf(stderr, "\r\n");
          bar = j2_mcy(Pt6, ut, nest)(van, huz, u2_no, goz);
          fprintf(stderr, "kthx: bar %d\r\n", bar);
          FOO = 0;
        }
#endif
        u2z(zel);
        return ret;
      }
      case c3__tsgr: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun fid = _mint_in(van, sut, c3__noun, p_gen);
        u2_noun p_fid = u2h(fid);
        u2_noun q_fid = u2t(fid);
        u2_noun dov = _mint_in(van, p_fid, gol, q_gen);
        u2_noun p_dov = u2h(dov);
        u2_noun q_dov = u2t(dov);

        ret = u2nc
          (u2k(p_dov),
                  j2_mbc(Pt6, comb)(q_fid, q_dov));

        u2z(fid);
        u2z(dov);
        return ret;
      }
      case c3__tstr: u2_cx_trel(u2t(gen), &p_gen, &q_gen, &r_gen);
      _mint_used();
      {
        u2_noun sep = j2_mcy(Pt6, ut, seep)(van, sut, c3__both, q_gen);
        u2_noun bid = u2nt(u2k(p_gen), u2k(q_gen), sep);
        u2_noun boc = j2_mby(Pt6, bull)(bid, sut);
        u2_noun ret = _mint_in(van, boc, gol, r_gen);

        u2z(bid);
        u2z(boc);

        return ret;
      }
      case c3__cnts: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun sec = j2_mcy(Pt6, ut, seek)(van, sut, c3__read, p_gen);
        u2_noun lar = _mint_foil(sec);
        u2_noun p_lar = u2h(lar);
        u2_noun q_lar = u2t(lar);
        u2_noun pq_lar = u2h(q_lar);
        u2_noun qq_lar = u2t(q_lar);
        u2_noun mew = j2_mcy(Pt6, ut, snub)(van, sut, q_gen);
        u2_noun yom = _mint_edit
          (van, sut, mew, p_lar, u2k(qq_lar), u2_nul);
        u2_noun p_yom = u2h(yom);
        u2_noun q_yom = u2t(yom);
        u2_noun ret = u2nc
          (_mint_nice(van, gol, u2k(p_yom)),
                  (0 == pq_lar) ? u2k(q_yom)
                                 : u2nt(9,
                                                u2k(pq_lar),
                                                u2k(q_yom)));

        u2z(yom);
        u2z(mew);
        u2z(lar);

        return ret;
      }
      case c3__pmcl: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun ruf = u2nt
          (c3__clhp,
                  u2nc(u2_nul, 1),
                  u2k(p_gen));
        u2_noun ret = _mint_grow(van, sut, gol, c3__zinc, ruf, q_gen);

        u2z(ruf);
        return ret;
      }
      case c3__brcn: p_gen = u2t(gen);
      _mint_used();
      {
        u2_noun ruf = u2nc(u2_nul, 1);

        ret = _mint_grow(van, sut, gol, c3__gold, ruf, p_gen);
        u2z(ruf);

        return ret;
      }
      case c3__pmcn: p_gen = u2t(gen);
      _mint_used();
      {
        u2_noun ruf = u2nc(u2_nul, 1);

        ret = _mint_grow(van, sut, gol, c3__lead, ruf, p_gen);
        u2z(ruf);

        return ret;
      }
      case c3__pmls: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun ruf = u2nt
          (c3__clhp,
                  u2nc(u2_nul, 1),
                  u2k(p_gen));
        u2_noun ret = _mint_grow(van, sut, gol, c3__iron, ruf, q_gen);

        u2z(ruf);
        return ret;
      }
      case c3__sgzp: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun typ = j2_mcy(Pt6, ut, play)(van, sut, p_gen);
        u2_noun dug = j2_mcy(Pt6, ut, duck)(van, typ);

        u2_ct_push(u2nc(c3__mean, dug));
        {
          ret = _mint_in(van, sut, gol, q_gen);
        }
        u2_ct_drop();

        u2z(typ);
        return ret;
      }
      case c3__sggr: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun hum = _mint_in(van, sut, gol, q_gen);
        u2_noun bez;

          if ( u2_yes == u2ud(p_gen) ) {
            bez = u2k(p_gen);
          } else {
            bez = u2nc(u2k(u2h(p_gen)),
                               _mint_corn(van, sut, u2t(p_gen)));
          }
          ret = u2nc(
                      u2k(u2h(hum)),
                      u2nt(10, bez, u2k(u2t(hum))));

        u2z(hum);
        return ret;
      }
      case c3__zpts: p_gen = u2t(gen);
      _mint_used();
      {
        u2_noun von = u2_ci_molt(u2k(van), j2_ut_van_vet, u2_no, 0);
        u2_noun sev = _mint_corn(von, sut, p_gen);

        u2z(von);
        return u2nc(c3__noun, u2nc(1, sev));
      }
      case c3__zpcm: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        return u2nc
          (
           _mint_nice(van,
                             gol,
                             j2_mcy(Pt6, ut, play)(van, sut, p_gen)),
           u2nc(1, u2k(q_gen)));
      }
      case c3__zpcb: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      {
        u2_ct_push(u2nc(c3__mean, _mint_loc(van, p_gen)));
        {
          u2_noun hum = _mint_in(van, sut, gol, q_gen);
          u2_noun bez = u2nt(c3__spot, 1, u2k(p_gen));

          ret = u2nc(
                      u2k(u2h(hum)),
                      u2nt(10, bez, u2k(u2t(hum))));

          u2z(hum);
        }
        u2_ct_drop();
        return ret;
      }
      case c3__zpcn:
      _mint_used();
      {
        u2_noun pet   = u2_cj_hook(u2k(van), "seed");
        u2_noun p_pet = u2h(pet);
        u2_noun q_pet = u2t(pet);
        u2_noun ret;

        ret = u2nc(_mint_nice(van, gol, u2k(p_pet)),
                           u2nc(1, u2k(q_pet)));

        u2z(pet);
        return ret;
      }
      case c3__zpsm: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u2_noun vos   = _mint_in(van, sut, c3__noun, q_gen);
        u2_noun zur   = j2_mcy(Pt6, ut, play)(van, sut, p_gen);
        u2_noun p_vos = u2h(vos);
        u2_noun q_vos = u2t(vos);
        u2_noun waz   = u2nc(1, u2k(p_vos));
        u2_noun sif   = j2_mcy(Pt6, ut, sift)(van, sut, zur);
        u2_noun cig   = j2_mby(Pt6, cell)(sif, p_vos);
        u2_noun ret;

        ret = u2nc(
                    _mint_nice(van, gol, cig),
                    j2_mby(Pt6, cons)(waz, q_vos));

        u2z(waz);
        u2z(zur);
        u2z(sif);
        u2z(vos);

        return ret;
      }
      case c3__zpfs: p_gen = u2t(gen);
      _mint_used();
      {
        if ( u2_yes == _mint_vet(van) ) {
          u2_noun zur = j2_mcy(Pt6, ut, play)(van, sut, p_gen);
          u2_noun dun = j2_mcy(Pt6, ut, dunq)(van, "lost", zur);

          u2_ct_push(u2nc(c3__mean, dun));
          return u2_cm_error("mint-lost");
        }
        else {
          return u2nt(c3__void, 0, 0);
        }
      }
      case c3__zpzp:
      _mint_used();
      {
        return u2nt(c3__void, 0, 0);
      }
    }
  }

  u2_noun                                                         //  produce
  j2_mcx(Pt6, ut, mint)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gol,
                        u2_noun gen)                              //  retain
  {
    return _mint_in(van, sut, gol, gen);
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, mint)[];

  u2_noun                                                         //  produce
  j2_mc(Pt6, ut, mint)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, gol, gen, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &gol,
                                u2_cv_sam_3, &gen,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, mint)(van, sut, gol, gen);
    }
  }

  u2_weak                                                         //  produce
  j2_mci(Pt6, ut, mint)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gol,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_weak hoc = u2_cj_look(u2k(van), "mint");

    if ( u2_none == hoc ) {
      c3_assert(!"register mint");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(u2k(gat),
                                      u2_cv_sam_2, u2k(gol),
                                      u2_cv_sam_3, u2k(gen),
                                      0);

      if ( (u2_none == j2_mcj(Pt6, ut, mint)[0].xip) ) {
        u2_noun xip = u2_cj_find(u2k(cor));

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, mint)[0].xip = xip;
      }
      u2z(gat);
      return cor;
    }
  }

  u2_noun                                                         //  produce
  j2_mcy(Pt6, ut, mint)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gol,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, mint)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, mint)(van, sut, gol, gen);
      }
      else {
        c3_m    fun_m = c3__mint;
        u2_noun vrf   = u2_cr_at(j2_ut_van_vrf, van);
        u2_noun pro   = u2_cz_find_4(fun_m, vrf, sut, gol, gen);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, mint)(van, sut, gol, gen);

          return u2_cz_save_4(fun_m, vrf, sut, gol, gen, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, mint)(van, sut, gol, gen);
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
  j2_mck(Pt6, ut, mint)(
                        u2_noun cor)
  {
    u2_noun sut, gol, gen, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &gol,
                                u2_cv_sam_3, &gen,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      u2_noun vrf = u2_cr_at(j2_ut_van_vrf, van);

      return u2nq(u2k(vrf),
                          u2k(sut),
                          u2k(gol),
                          u2k(gen));
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, mint)[] = {
    { ".2", c3__hevy,
        j2_mc(Pt6, ut, mint),
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pt6, ut, mint), c3__mint,
    },
    { }
  };
