/* j/6/mull.c
**
*/
#include "all.h"


/* functions
*/
  static u3_noun
  _mull_in(u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);

  static u3_noun
  _mull_vet(u3_noun van)
  {
    // u3_noun vet = u3j_hook(u3k(van), "vet");
    u3_noun vet = u3r_at(u3qfu_van_vet, van);

    switch ( vet ) {
      case c3n:
      case c3y: return vet;
      default: return u3m_bail(c3__fail);
    }
  }

  static u3_noun
  _mull_core(u3_noun pac,
             u3_noun con)
  {
    if ( (c3__void == pac) ) {
      return c3__void;
    } else {
      return u3nt(c3__core, pac, con);
    }
  }

  static u3_noun
  _mull_bean()
  {
    return u3nt(c3__fork,
                u3nq(c3__cube, 0, c3__atom, 'f'),
                u3nq(c3__cube, 1, c3__atom, 'f'));
  }

  static u3_noun
  _mull_loc(u3_noun van,
            u3_noun loc)
  {
    u3_noun mol = u3nc('o', u3k(loc));
    u3_noun sho = u3j_hook(u3k(van), "show");
    u3_noun ret = u3i_molt(u3k(sho), u3x_sam, u3k(mol), 0);

    u3z(mol);
    u3z(sho);

    return ret;
  }

  static u3_noun
  _mull_foil(u3_noun pok)
  {
    u3_noun p_pok = u3h(pok);
    u3_noun q_pok = u3t(pok);
    u3_noun ret;

    if ( c3y == u3h(q_pok) ) {
      u3_noun pq_pok = u3t(q_pok);

      ret = u3nc(u3k(p_pok),
                 u3nc(u3_nul,
                      u3nc(u3nc(u3k(pq_pok),
                                u3nt(c3__elm, u3_nul, 1)),
                           u3_nul)));
    }
    else {
      u3_noun pq_pok = u3h(u3t(q_pok));
      u3_noun qq_pok = u3t(u3t(q_pok));

      ret = u3nc(u3k(p_pok),
                 u3nc(u3k(pq_pok),
                      u3k(qq_pok)));
    }
    u3z(pok);
    return ret;
  }

  static u3_noun
  _mull_coke(u3_noun nug)
  {
    u3_atom axe;

    if ( 0 == u3h(nug) ) {
      axe = u3t(nug);
    } else if ( 10 == u3h(nug) ) {
      u3_noun xin = u3k(u3t(u3t(nug)));

      axe = _mull_coke(xin);
    }
    else {
      return u3m_error("mint-coke");
    }
    u3z(nug);
    return axe;
  }

  static u3_noun
  _mull_doke(u3_noun van,
             u3_noun sut,
             u3_noun gen)
  {
    u3_noun fug = u3qfu_mint(van, sut, c3__noun, gen);
    u3_noun axe = _mull_coke(u3k(u3t(fug)));

    u3z(fug);
    return axe;
  }

  static u3_noun
  _mull_nice(u3_noun van,
             u3_noun gol,
             u3_noun typ)
  {
    if ( c3n == u3qfu_nest(van, gol, c3y, typ) ) {
      // u3_noun dun = u3qfu_dunq(van, "need", gol);
      // u3_noun niz = u3qfu_dunq(van, "have", typ);

      // u3t_push(u3nc(c3__mean, dun));
      // u3t_push(u3nc(c3__mean, niz));

      return u3m_error("mull-nice");
    }
    else return typ;
  }

  static void
  _mull_bake(u3_noun van,
             u3_noun sut,
             u3_noun dox,
             u3_noun dab)
  {
    if ( u3_nul != dab ) {
      u3_noun n_dab, l_dab, r_dab;

      u3r_trel(dab, &n_dab, &l_dab, &r_dab);
      if ( c3n == u3du(n_dab) ) {
        u3m_bail(c3__fail);
      }
      else {
        u3_noun qn_dab = u3t(n_dab);
        u3_noun vad;

        switch ( u3h(qn_dab) ) {
          default: u3m_bail(c3__exit);
          case c3__ash: {
            vad = _mull_in(van, sut, c3__noun, dox, u3t(qn_dab));
            break;
          }
          case c3__elm: {
            vad = u3_nul;
            break;
          }
        }
        u3z(vad);

        if ( (u3_nul == l_dab) && (u3_nul == r_dab) ) {
          return;
        }
        else if ( (u3_nul == l_dab) ) {
          _mull_bake(van, sut, dox, r_dab);
        }
        else if ( (u3_nul == r_dab) ) {
          _mull_bake(van, sut, dox, l_dab);
        }
        else {
          _mull_bake(van, sut, dox, l_dab);
          _mull_bake(van, sut, dox, r_dab);
        }
      }
    }
  }

  static u3_noun
  _mull_grow(u3_noun van,
             u3_noun sut,
             u3_noun gol,
             u3_noun dox,
             u3_atom mel,
             u3_noun ruf,
             u3_noun dab)
  {
    u3_noun dan = _mull_in(van, sut, c3__noun, dox, ruf);
    u3_noun p_dan = u3h(dan);
    u3_noun q_dan = u3t(dan);
    u3_noun p_toc = _mull_core(u3k(p_dan),
                               u3nt(c3__gold,
                                    u3k(p_dan),
                                    u3nc(u3_nul, u3k(dab))));
    u3_noun q_toc = _mull_core(u3k(q_dan),
                               u3nt(c3__gold,
                                    u3k(q_dan),
                                    u3nc(u3_nul, u3k(dab))));
    u3_noun p_ret = _mull_core(u3k(p_dan),
                               u3nt(u3k(mel),
                                    u3k(p_dan),
                                    u3nc(u3nc(u3_nul, u3_nul),
                                         u3k(dab))));
    u3_noun q_ret = _mull_core(u3k(q_dan),
                               u3nt(u3k(mel),
                                    u3k(q_dan),
                                    u3nc(u3nc(u3_nul, u3_nul),
                                         u3k(dab))));
    u3_noun ret = u3nc(_mull_nice(van, gol, p_ret), q_ret);

    _mull_bake(van, p_toc, q_toc, dab);

    u3z(q_toc);
    u3z(p_toc);
    u3z(dan);

    return ret;
  }

  static u3_noun
  _mull_both(u3_noun van,
             u3_noun gol,
             u3_noun typ)
  {
    return u3nc(_mull_nice(van, gol, u3k(typ)),
                typ);
  }

  static u3_noun
  _mull_edit(u3_noun van,
             u3_noun sut,
             u3_noun dox,
             u3_noun mew,
             u3_noun p_yom,
             u3_noun q_yom)
  {
    while ( 1 ) {
      if ( c3n == u3du(mew) ) {
        return u3nc(p_yom, q_yom);
      } else {
        u3_noun i_mew = u3h(mew);
        u3_noun t_mew = u3t(mew);
        u3_noun pi_mew = u3h(i_mew);
        u3_noun qi_mew = u3t(i_mew);
        u3_noun zil = _mull_in(van, sut, c3__noun, dox, qi_mew);
        u3_noun p_zil = u3h(zil);
        u3_noun q_zil = u3t(zil);
        u3_noun cuf = u3qfu_tock(van, sut, pi_mew, p_zil, p_yom);
        u3_noun dof = u3qfu_tock(van, sut, pi_mew, q_zil, q_yom);

        if ( u3r_sing(u3h(cuf), u3h(dof)) ) {
          u3m_error("mull-bonk-a");
        }

        u3z(p_yom);
        p_yom = u3k(u3t(cuf));

        u3z(q_yom);
        q_yom = u3k(u3t(dof));

        u3z(dof);
        u3z(cuf);
        u3z(zil);

        mew = t_mew;
      }
    }
  }

# define _mull_used()

  static u3_noun
  _mull_in(u3_noun van,
           u3_noun sut,
           u3_noun gol,
           u3_noun dox,
           u3_noun gen)
  {
    u3_noun p_gen, q_gen, r_gen;
    u3_noun ret;

    if ( c3n == u3du(gen) ) {
      u3_noun ter = u3r_at(u3x_con_3, van);
      u3_noun rex = u3qfp_open(ter, gen);

      ret = _mull_in(van, sut, gol, dox, rex);
      u3z(rex);

      return ret;
    }
    else if ( c3y == u3du(u3h(gen)) ) {
      _mull_used();
      {
        u3_noun hed = _mull_in(van, sut, c3__noun, dox, u3h(gen));
        u3_noun tal = _mull_in(van, sut, c3__noun, dox, u3t(gen));
        u3_noun dis = u3qf_cell(u3h(hed), u3h(tal));
        u3_noun dat = u3qf_cell(u3t(hed), u3t(tal));
        u3_noun ret = u3nc(_mull_nice(van, gol, dis), dat);

        u3z(tal);
        u3z(hed);

        return ret;
      }
    }
    else switch ( u3h(gen) ) {
      default: {
        u3_noun ter = u3r_at(u3x_con_3, van);
        u3_noun rex = u3qfp_open(ter, gen);

        if ( c3y == u3r_sing(rex, gen) ) {
#if 1
          u3_noun zix = u3qfu_shep(van, "gene", 'q', u3k(gen));

          u3t_push(u3nc(c3__mean, zix));
          return u3m_error("mull-open");
#else
          u3_err("h", u3h(gen));
          return u3m_bail(c3__fail);
#endif
        }
        ret = _mull_in(van, sut, gol, dox, rex);
        u3z(rex);

        return ret;
      }

      case c3__bcpt: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun sep = u3qfu_seep(van, sut, c3__read, p_gen);
        u3_noun pox = u3qfu_seep(van, dox, c3__read, p_gen);
        u3_noun axe = u3h(sep);

        if ( axe != u3h(pox) ) {
          return u3m_error("mull-bonk-wing");
        }
        else {
          u3_noun rex = u3qfl_whip(van, q_gen, axe);
          u3_noun ret = _mull_in(van, sut, gol, dox, rex);

          u3z(sep);
          u3z(pox);
          u3z(rex);

          return ret;
        }
      }

      case c3__wtts: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun nob = u3qfl_bunt(van, p_gen);
        u3_noun p_waz = u3qfu_play(van, sut, nob);
        u3_noun q_waz = u3qfu_play(van, dox, nob);
        u3_noun dok   = u3nc(c3__cnzz, u3k(q_gen));
        u3_noun p_syx = _mull_doke(van, sut, dok);
        u3_noun q_syx = _mull_doke(van, dox, dok);
        u3_noun p_pov = u3qfu_fish(van, p_waz, p_syx);
        u3_noun q_pov = u3qfu_fish(van, q_waz, q_syx);

        if ( (c3n == u3r_sing(p_syx, q_syx)) ||
             (c3n == u3r_sing(p_pov, q_pov)) )
        {
          return u3m_error("mull-bonk-b");
        }
        u3z(p_waz); u3z(q_waz);
        u3z(p_syx); u3z(q_syx);
        u3z(p_pov); u3z(q_pov);
        u3z(nob);
        u3z(dok);

        return _mull_both(van, gol, _mull_bean());
      }

      case c3__wtcl: u3x_trel(u3t(gen), &p_gen, &q_gen, &r_gen);
      _mull_used();
      {
        u3_noun bol = _mull_bean();
        u3_noun nor = _mull_in(van, sut, bol, dox, p_gen);
        u3_noun p_fex = u3qfu_gain(van, sut, p_gen);
        u3_noun q_fex = u3qfu_gain(van, dox, p_gen);
        u3_noun p_wux = u3qfu_lose(van, sut, p_gen);
        u3_noun q_wux = u3qfu_lose(van, dox, p_gen);
        u3_noun hiq, ran;
        u3_noun dis, dat;
        u3_noun ret;

        if ( c3__void == p_fex ) {
          hiq = u3nc(c3__void,
                     (c3__void == q_fex)
                        ? c3__void
                        : u3qfu_play(van, q_fex, q_gen));
        } else if ( c3__void == q_fex ) {
          hiq = u3m_error("mull-bonk-c");
        }
        else hiq = _mull_in(van, p_fex, gol, q_fex, q_gen);

        if ( c3__void == p_wux ) {
          ran = u3nc(c3__void,
                     (c3__void == q_wux)
                        ? c3__void
                        : u3qfu_play(van, q_wux, r_gen));
        } else if ( c3__void == q_wux ) {
          ran = u3m_error("mull-bonk-d");
        }
        else ran = _mull_in(van, p_wux, gol, q_wux, r_gen);

        dis = u3qf_fork(u3h(hiq), u3h(ran));
        dat = u3qf_fork(u3t(hiq), u3t(ran));

        ret = u3nc(_mull_nice(van, gol, dis), dat);

        u3z(ran);
        u3z(hiq);
        u3z(q_wux);
        u3z(p_wux);
        u3z(q_fex);
        u3z(p_fex);
        u3z(nor);
        u3z(bol);

        return ret;
      }
      case c3__clhp: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun hed = _mull_in(van, sut, c3__noun, dox, p_gen);
        u3_noun tal = _mull_in(van, sut, c3__noun, dox, q_gen);
        u3_noun dis = u3qf_cell(u3h(hed), u3h(tal));
        u3_noun dat = u3qf_cell(u3t(hed), u3t(tal));
        u3_noun ret = u3nc(_mull_nice(van, gol, dis), dat);

        u3z(tal);
        u3z(hed);

        return ret;
      }
      case c3__dtts: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun hed = _mull_in(van, sut, c3__noun, dox, p_gen);
        u3_noun tal = _mull_in(van, sut, c3__noun, dox, q_gen);

        u3z(hed);
        u3z(tal);

        return _mull_both(van, gol, _mull_bean());
      }
      case c3__dtwt: p_gen = u3t(gen);
      _mull_used();
      {
        u3_noun vay = _mull_in(van, sut, c3__noun, dox, p_gen);

        u3z(vay);
        return _mull_both(van, gol, _mull_bean());
      }
      case c3__dtkt: p_gen = u3t(gen);
      _mull_used();
      {
        u3_noun wuq = c3__noun;
        u3_noun vay = _mull_in(van, sut, wuq, dox, p_gen);

        u3z(vay);
        return _mull_both(van, gol, wuq);
      }
      case c3__dtls: p_gen = u3t(gen);
      _mull_used();
      {
        u3_noun wuq = u3nc(c3__atom, u3_blip);
        u3_noun vay = _mull_in(van, sut, wuq, dox, p_gen);

        u3z(vay);
        return _mull_both(van, gol, wuq);
      }
      case c3__dtzz: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun tof = u3nt(c3__cube,
                           u3k(q_gen),
                           (c3y == u3du(q_gen))
                             ? c3__noun
                             : u3nc(c3__atom, u3k(p_gen)));

        return _mull_both(van, gol, tof);
      }
      case c3__dttr: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun hed = _mull_in(van, sut, c3__noun, dox, p_gen);
        u3_noun tal = _mull_in(van, sut, c3__noun, dox, q_gen);

        u3z(hed);
        u3z(tal);

        return _mull_both(van, gol, c3__noun);
      }
      case c3__dtzy: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun typ = u3qfu_play(van, sut, gen);
        return _mull_both(van, gol, typ);
      }
      case c3__ktbr: p_gen = u3t(gen);
      _mull_used();
      {
        u3_noun vat = _mull_in(van, sut, gol, dox, p_gen);
        u3_noun pro = u3nc(u3qfu_wrap(van, 
                                      u3h(vat),
                                      c3__iron),
                           u3qfu_wrap(van, 
                                      u3t(vat),
                                      c3__iron));

        u3z(vat);
        return pro;
      }
      case c3__ktpm: p_gen = u3t(gen);
      _mull_used();
      {
        u3_noun vat = _mull_in(van, sut, gol, dox, p_gen);
        u3_noun pro = u3nc(u3qfu_wrap(van, 
                                      u3h(vat),
                                      c3__zinc),
                           u3qfu_wrap(van,
                                      u3t(vat),
                                      c3__zinc));

        u3z(vat);
        return pro;
      }
      case c3__ktwt: p_gen = u3t(gen);
      _mull_used();
      {
        u3_noun vat = _mull_in(van, sut, gol, dox, p_gen);
        u3_noun pro = u3nc(u3qfu_wrap(van,
                                      u3h(vat),
                                      c3__lead),
                           u3qfu_wrap(van,
                                      u3t(vat),
                                      c3__lead));

        u3z(vat);
        return pro;
      }
      case c3__ktts: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun vat = _mull_in(van, sut, gol, dox, q_gen);
        u3_noun ret = u3nc(u3qfu_conk(van, u3h(vat), p_gen),
                           u3qfu_conk(van, u3t(vat), p_gen));

        u3z(vat);
        return ret;
      }
      case c3__ktzp: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun vat = _mull_in(van, sut, gol, dox, q_gen);
        u3_noun ret =u3nc(u3qfu_conk(van, u3h(vat), p_gen),
                          u3qfu_conk(van, u3t(vat), p_gen));

        u3z(vat);
        return ret;
      }
      case c3__ktsg: p_gen = u3t(gen);
      _mull_used();
      {
        return _mull_in(van, sut, gol, dox, p_gen);
      }
      case c3__ktls: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun p_hif = _mull_nice(van, gol, u3qfu_play(van, sut, p_gen));
        u3_noun q_hif = u3qfu_play(van, dox, p_gen);

        u3_noun zel = _mull_in(van, sut, p_hif, dox, q_gen);
        u3_noun ret = u3nc(p_hif, q_hif);

        u3z(zel);
        return ret;
      }
      case c3__kthx: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun p_hif = _mull_nice(van, gol, u3qfu_play(van, sut, p_gen));
        u3_noun q_hif = u3qfu_play(van, dox, p_gen);

        u3_noun zel = _mull_in(van, sut, p_hif, dox, q_gen);
        u3_noun ret = u3nc(p_hif, q_hif);

        u3z(zel);
        return ret;
      }
      case c3__tsgr: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun lem = _mull_in(van, sut, c3__noun, dox, p_gen);
        u3_noun p_lem = u3h(lem);
        u3_noun q_lem = u3t(lem);
        u3_noun ret = _mull_in(van, p_lem, gol, q_lem, q_gen);

        u3z(lem);
        return ret;
      }
      case c3__tstr: u3x_trel(u3t(gen), &p_gen, &q_gen, &r_gen);
      _mull_used();
      {
        u3_noun sep = u3qfu_seep(van, sut, c3__both, q_gen);
        u3_noun pox = u3qfu_seep(van, dox, c3__both, q_gen);
        u3_noun bid = u3nt(u3k(p_gen), u3k(q_gen), sep);
        u3_noun yub = u3nt(u3k(p_gen), u3k(q_gen), pox);
        u3_noun boc = u3qf_bull(bid, sut);
        u3_noun nuf = u3qf_bull(yub, dox);
        u3_noun ret = _mull_in(van, boc, gol, nuf, r_gen);

        u3z(bid);
        u3z(yub);
        u3z(boc);
        u3z(nuf);

        return ret;
      }
      case c3__cnts: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun sec = u3qfu_seek(van, sut, c3__read, p_gen);
        u3_noun suc = u3qfu_seek(van, dox, c3__read, p_gen);
        u3_noun lar = _mull_foil(sec);
        u3_noun p_lar = u3h(lar);
        u3_noun q_lar = u3t(lar);
        u3_noun pq_lar = u3h(q_lar);
        u3_noun qq_lar = u3t(q_lar);
        u3_noun vug = _mull_foil(suc);
        u3_noun p_vug = u3h(vug);
        u3_noun q_vug = u3t(vug);
        u3_noun pq_vug = u3h(q_vug);
        u3_noun qq_vug = u3t(q_vug);

        if ( c3a(u3r_sing(p_lar, p_vug), u3r_sing(pq_lar, pq_vug)) ) {
          u3m_error("mull-bonk-e");
        }
        {
          u3_noun mew = u3qfu_snub(van, sut, q_gen);
          u3_noun yom = _mull_edit
            (van, sut, dox, mew, u3k(qq_lar),
                                        u3k(qq_vug));
          u3_noun von = u3i_molt(u3k(van), u3qfu_van_vet, c3n, 0);
          u3_noun p_ret = u3qfu_fire(van, sut, u3h(yom));
          u3_noun q_ret = u3qfu_fire(von, sut, u3t(yom));

          u3z(von);
          u3z(yom);
          u3z(mew);
          u3z(vug);
          u3z(lar);

          return u3nc(_mull_nice(van, gol, p_ret), q_ret);
        }
      }
      case c3__pmcl: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun ruf = u3nt(c3__clhp,
                           u3nc(u3_nul, 1),
                           u3k(p_gen));
        u3_noun ret = _mull_grow(van, sut, gol, dox, c3__zinc, ruf, q_gen);

        u3z(ruf);
        return ret;
      }
      case c3__brcn: p_gen = u3t(gen);
      _mull_used();
      {
        u3_noun ruf = u3nc(u3_nul, 1);

        ret = _mull_grow(van, sut, gol, dox, c3__gold, ruf, p_gen);
        u3z(ruf);

        return ret;
      }
      case c3__pmcn: p_gen = u3t(gen);
      _mull_used();
      {
        u3_noun ruf = u3nc(u3_nul, 1);

        ret = _mull_grow(van, sut, gol, dox, c3__lead, ruf, p_gen);
        u3z(ruf);

        return ret;
      }
      case c3__pmls: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun ruf = u3nt(c3__clhp,
                           u3nc(u3_nul, 1),
                           u3k(p_gen));
        u3_noun ret = _mull_grow(van, sut, gol, dox, c3__iron, ruf, q_gen);

        u3z(ruf);
        return ret;
      }
      case c3__sgzp: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun typ = u3qfu_play(van, sut, p_gen);
        u3_noun dug = u3qfu_duck(van, typ);
        u3_noun ret;

        u3t_push(u3nc(c3__mean, dug));
        {
          ret = _mull_in(van, sut, gol, dox, q_gen);
        }
        u3t_drop();

        u3z(typ);
        return ret;
      }
      case c3__sggr: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        return _mull_in(van, sut, gol, dox, q_gen);
      }
      case c3__zpcm: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun p_ret = u3qfu_play(van, sut, p_gen);
        u3_noun q_ret = u3qfu_play(van, sut, q_gen);

        return u3nc(_mull_nice(van, gol, p_ret),
                    q_ret);
      }
      case c3__zpcb: u3x_cell(u3t(gen), &p_gen, &q_gen);
      {
        u3_noun ret;

        u3t_push(u3nc(c3__mean, _mull_loc(van, p_gen)));
        {
          ret = _mull_in(van, sut, gol, dox, q_gen);
        }
        u3t_drop();
        return ret;
      }
      case c3__zpts: p_gen = u3t(gen);
      _mull_used();
      {
        return _mull_both(van, gol, c3__noun);
      }
      case c3__zpcn:
      _mull_used();
      {
        u3_noun pet = u3j_hook(u3k(van), "seed");
        u3_noun peq = u3k(u3h(pet));

        u3z(pet);
        return _mull_both(van, gol, peq);
      }
      case c3__zpsm: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun p_zur = u3qfu_play(van, sut, p_gen);
        u3_noun q_zur = u3qfu_play (van, dox, p_gen);
        u3_noun vos = _mull_in(van, sut, c3__noun, dox, q_gen);
        u3_noun p_ret = u3qf_cell(p_zur, u3h(vos));
        u3_noun q_ret = u3qf_cell(q_zur, u3t(vos));

        u3z(vos);
        u3z(q_zur);
        u3z(p_zur);

        return u3nc
          (_mull_nice(van, gol, p_ret), q_ret);
      }
      case c3__zpfs:
      case c3__zpzp:
      _mull_used();
      {
        return u3nc(c3__void, c3__void);
      }
    }
  }

  u3_noun
  _cqfu_mull(u3_noun van,
             u3_noun sut,
             u3_noun gol,
             u3_noun dox,
             u3_noun gen)
  {
    if ( c3n == _mull_vet(van) ) {
      return c3y;
    } else {
      u3_noun mul = _mull_in(van, sut, gol, dox, gen);

      u3z(mul);
      return c3y;
    }
  }

/* boilerplate
*/
  u3_noun
  u3wfu_mull(u3_noun cor)
  {
    u3_noun sut, gol, dox, gen, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &gol,
                               u3x_sam_6, &dox,
                               u3x_sam_7, &gen,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_mull(van, sut, gol, dox, gen);
    }
  }

  u3_noun
  u3qfu_mull(u3_noun van,
             u3_noun sut,
             u3_noun gol,
             u3_noun dox,
             u3_noun gen)
  {
    c3_m    fun_m = c3__mull;
    u3_noun pro   = u3z_find_4(fun_m, sut, gol, dox, gen);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_mull(van, sut, gol, dox, gen);

      return u3z_save_4(fun_m, sut, gol, dox, gen, pro);
    }
  }

