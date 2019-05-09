/* j/6/mull.c
**
*/
#include "all.h"

/* functions
*/
  static u3_noun
  _mull_in(u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);

  static u3_noun
  _mull_bean()
  {
    return u3kf_fork(u3nt(u3nq(c3__atom, 'f', u3_nul, 0),
                          u3nq(c3__atom, 'f', u3_nul, 1),
                          u3_nul));
  }

  static u3_noun
  _mull_loc(u3_noun van,
            u3_noun loc)
  {
    u3_noun mol = u3nc('o', u3k(loc));
    u3_noun sho = u3j_cook("_mull_loc-show", u3k(van), "show");
    u3_noun ret = u3i_molt(u3k(sho), u3x_sam, u3k(mol), 0);

    u3z(mol);
    u3z(sho);

    return ret;
  }

  static u3_noun
  _mull_feel(u3_noun van,
             u3_noun sut,
             u3_noun rot)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "feel");

    return u3n_kick_on(u3i_molt(gat,
                                u3x_sam,
                                u3k(rot),
                                0));
  }

  static u3_noun
  _mull_tend(u3_noun vit)
  {
    if ( u3_nul == vit ) {
      return 1;
    } else {
      u3_noun nex = _mull_tend(u3t(vit));

      if ( u3_nul == u3h(vit) ) {
        return nex;
      } else {
        u3_noun boy = u3qc_peg(u3t(u3h(vit)), nex);

        u3z(nex);
        return boy;
      }
    }
  }
  static u3_noun
  _mull_cove(u3_noun nug)
  {
    if ( 0 == u3h(nug) ) {
      return u3k(u3t(nug));
    }
    else if ( 11 == u3h(nug) ) {
      return _mull_cove(u3t(u3t(nug)));
    }
    else {
      return u3m_error("mull-cove");
    }
  }

  static u3_noun
  _mull_mile(u3_noun van,
             u3_noun sut,
             u3_noun dox,
             u3_noun mel,
             u3_noun nym,
             u3_noun hud,
             u3_noun dom)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "mile");

    // u3l_log("mile\r\n");
    return u3n_kick_on(u3i_molt(gat,
                                u3x_sam_2,
                                u3k(dox),
                                u3x_sam_6,
                                u3k(mel),
                                u3x_sam_14,
                                u3k(nym),
                                u3x_sam_30,
                                u3k(hud),
                                u3x_sam_31,
                                u3k(dom),
                                0));
  }

  static u3_noun
  _mull_doke(u3_noun van,
             u3_noun sut,
             u3_noun gen)
  {
    u3_noun fug = u3qfu_mint(van, sut, c3__noun, gen);
    u3_noun axe = _mull_cove(u3t(fug));

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

  static u3_noun
  _mull_grow(u3_noun van,
             u3_noun sut,
             u3_noun gol,
             u3_noun dox,
             u3_atom mel,
             u3_noun nym,
             u3_noun hud,
             u3_noun ruf,
             u3_noun dom)
  {
    u3_noun dan = _mull_in(van, sut, c3__noun, dox, ruf);
    u3_noun p_dan = u3h(dan);
    u3_noun q_dan = u3t(dan);
    u3_noun yaz   = _mull_mile(van, p_dan, q_dan, mel, nym, hud, dom);
    u3_noun p_yaz = u3h(yaz);
    u3_noun q_yaz = u3t(yaz);
    u3_noun ret = u3nc(_mull_nice(van, gol, u3k(p_yaz)), u3k(q_yaz));

    u3z(yaz);
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

#if 1
  static u3_noun
  _mull_cnts(u3_noun van,
             u3_noun sut,
             u3_noun gol,
             u3_noun dox,
             u3_noun hyp,
             u3_noun rig)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_cook("_mull_cnts-emul", von, "emul");

    return u3n_kick_on(u3i_molt(gat,
                                u3x_sam_2,
                                u3k(gol),
                                u3x_sam_6,
                                u3k(dox),
                                u3x_sam_14,
                                u3k(hyp),
                                u3x_sam_15,
                                u3k(rig),
                                0));
  }
#else
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
        u3_noun cuf = u3qfu_toss(van, sut, pi_mew, p_zil, p_yom);
        u3_noun dof = u3qfu_toss(van, sut, pi_mew, q_zil, q_yom);

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

  static u3_noun
  _mull_cnts_old(u3_noun van,
                 u3_noun sut,
                 u3_noun gol,
                 u3_noun dox,
                 u3_noun hyp,
                 u3_noun rig)
  {
    u3_noun lar = u3qfu_seek(van, sut, c3__read, hyp);
    u3_noun vug = u3qfu_seek(van, dox, c3__read, hyp);
    u3_noun p_lar = u3h(lar);
    u3_noun q_lar = u3t(lar);
    u3_noun pq_lar = u3h(q_lar);
    u3_noun qq_lar = u3t(q_lar);
    u3_noun p_vug = u3h(vug);
    u3_noun q_vug = u3t(vug);
    u3_noun pq_vug = u3h(q_vug);
    u3_noun qq_vug = u3t(q_vug);

    if ( c3a(u3r_sing(p_lar, p_vug), u3r_sing(pq_lar, pq_vug)) ) {
      u3m_error("mull-bonk-e");
    }
    {
      u3_noun mew = rig;
      u3_noun yom = _mull_edit
        (van, sut, dox, mew, u3k(qq_lar),
                                    u3k(qq_vug));
      u3_noun von = u3i_molt(u3k(van), u3qfu_van_vet, c3n, 0);
      u3_noun p_ret = u3qfu_fire(van, sut, u3h(yom));
      u3_noun q_ret = u3qfu_fire(von, sut, u3t(yom));

      u3z(von);
      u3z(yom);
      u3z(vug);
      u3z(lar);

      return u3nc(_mull_nice(van, gol, p_ret), q_ret);
    }
  }
#endif

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
      u3_noun fab = u3r_at(u3qfu_van_fab, van);
      u3_noun rex = u3qfp_open(ter, fab, gen);

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
        u3_noun fab = u3r_at(u3qfu_van_fab, van);
        u3_noun rex = u3qfp_open(ter, fab, gen);

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

      case c3__fits: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun p_waz = u3qfu_play(van, sut, p_gen);
        u3_noun q_waz = u3qfu_play(van, dox, p_gen);
        u3_noun dok   = u3nc(c3__wing, u3k(q_gen));
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
        u3z(dok);

        return _mull_both(van, gol, _mull_bean());
      }

      case c3__wthx: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun hyp = u3nc(u3nc(c3y, 1), u3k(q_gen));
        u3_noun fid = u3qfu_find(van, sut, c3__free, hyp);
        u3_noun gax = u3qfu_find(van, dox, c3__free, hyp);
        u3_noun old_type, old_axis;
        u3_noun new_type, new_axis;

        {
          if ( c3y != u3h(fid) ) {
            return u3m_error("mull-bonk-x");
          }
          else {
            u3_noun pp_fid = u3h(u3t(fid));
            u3_noun qp_fid = u3t(u3t(fid));

            if ( c3y != u3h(qp_fid) ) {
              return u3m_error("mull-bonk-x");
            }
            new_type = u3t(qp_fid);
            new_axis = _mull_tend(pp_fid);
          }
        }

        {
          if ( c3y != u3h(gax) ) {
            return u3m_error("mull-bonk-x");
          }
          else {
            u3_noun pp_gax = u3h(u3t(gax));
            u3_noun qp_gax = u3t(u3t(gax));

            if ( c3y != u3h(qp_gax) ) {
              return u3m_error("mull-bonk-x");
            }
            old_type = u3t(qp_gax);
            old_axis = _mull_tend(pp_gax);
          }
        }
        if ( c3n == u3r_sing(old_axis, new_axis) ) {
          return u3m_error("mull-bonk-x");
        }
        else if ( c3n == u3qfu_nest(van, old_type, c3y, new_type) ) {
          return u3m_error("mull-bonk-x");
        }

        u3z(old_axis);
        u3z(new_axis);
        u3z(gax);
        u3z(fid);

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

        dis = u3qf_forq(u3h(hiq), u3h(ran));
        dat = u3qf_forq(u3t(hiq), u3t(ran));

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

      case c3__dtkt: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun nog = u3nc(c3__kttr, u3k(p_gen));
        u3_noun vay = _mull_in(van, sut, c3__noun, dox, q_gen);
        u3_noun ret;

        u3z(vay);
        ret = _mull_in(van, sut, gol, dox, nog);
        u3z(nog);
        return ret;
      }

      case c3__dtls: p_gen = u3t(gen);
      _mull_used();
      {
        u3_noun wuq = u3nt(c3__atom, u3_blip, u3_nul);
        u3_noun vay = _mull_in(van, sut, wuq, dox, p_gen);

        u3z(vay);
        return _mull_both(van, gol, wuq);
      }

      case c3__rock: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun typ = u3qfu_play(van, sut, gen);

        return _mull_both(van, gol, typ);
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

      case c3__sand: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun typ = u3qfu_play(van, sut, gen);
        return _mull_both(van, gol, typ);
      }

      case c3__hand: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        return u3nc(u3k(p_gen), u3k(p_gen));
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

      case c3__ktpd:
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

      case c3__note: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun vat = _mull_in(van, sut, gol, dox, q_gen);
        u3_noun hit = u3nc(u3k(sut), u3k(p_gen));
        u3_noun hix = u3nc(u3k(dox), u3k(p_gen));
        u3_noun ret = u3nc(u3qf_hint(hit, u3h(vat)),
                           u3qf_hint(hix, u3t(vat)));

        u3z(hix);
        u3z(hit);
        u3z(vat);
        return ret;
      }

      case c3__tune: p_gen = u3t(gen);
      _mull_used();
      {
        return u3nc(u3qf_face(p_gen, sut),
                    u3qf_face(p_gen, dox));
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

      case c3__tsbn: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun lem = _mull_in(van, sut, c3__noun, dox, p_gen);
        u3_noun p_lem = u3h(lem);
        u3_noun q_lem = u3t(lem);
        u3_noun ret = _mull_in(van, p_lem, gol, q_lem, q_gen);

        u3z(lem);
        return ret;
      }

      case c3__tscm: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun boc = u3qfu_busk(van, sut, p_gen);
        u3_noun nuf = u3qfu_busk(van, dox, p_gen);
        u3_noun ret = _mull_in(van, boc, gol, nuf, q_gen);

        u3z(boc);
        u3z(nuf);

        return ret;
      }

      case c3__cnts: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        return _mull_cnts(van, sut, gol, dox, p_gen, q_gen);
      }

      case c3__brcn: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun ruf = u3nc(u3_nul, 1);

        ret = _mull_grow
          (van, sut, gol, dox, c3__gold, p_gen, c3__dry, ruf, q_gen);
        u3z(ruf);

        return ret;
      }

      case c3__brvt: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mull_used();
      {
        u3_noun ruf = u3nc(u3_nul, 1);

        ret = _mull_grow
          (van, sut, gol, dox, c3__gold, p_gen, c3__wet, ruf, q_gen);
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

      case c3__sgbn: u3x_cell(u3t(gen), &p_gen, &q_gen);
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

      case c3__dbug: u3x_cell(u3t(gen), &p_gen, &q_gen);
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

      case c3__ktcn: p_gen = u3t(gen);
      _mull_used();
      {
        u3_noun von = u3i_molt(u3k(van), u3qfu_van_fab, c3n, 0);
        u3_noun ret = _mull_in(von, sut, gol, dox, p_gen);

        u3z(von);
        return ret;
      }

      case c3__zpmc:
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
      case c3__zpvt: u3x_trel(u3t(gen), &p_gen, &q_gen, &r_gen);
      _mull_used();
      {
        c3_o fes = _mull_feel(van, sut, p_gen);
        c3_o fed = _mull_feel(van, dox, p_gen);

        if ( fes != fed ) {
          return u3m_error("mull-bonk-f");
        }
        else {
          if ( c3y == fes ) {
            return _mull_in(van, sut, gol, dox, q_gen);
          } else {
            return _mull_in(van, sut, gol, dox, r_gen);
          }
        }
      }

      case c3__lost:
      case c3__fail:
      case c3__zpzp:
      _mull_used();
      {
        return u3nc(c3__void, c3__void);
      }
    }
  }

  static u3_noun
  _cqfu_mull(u3_noun van,
             u3_noun sut,
             u3_noun gol,
             u3_noun dox,
             u3_noun gen)
  {
    return _mull_in(van, sut, gol, dox, gen);
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
    c3_m    fun_m = 141 + c3__mull + ((!!u3r_at(u3qfu_van_vet, van)) << 8);
    u3_noun pro   = u3z_find_4(fun_m, sut, gol, dox, gen);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_mull(van, sut, gol, dox, gen);

      return u3z_save_4(fun_m, sut, gol, dox, gen, pro);
    }
  }

