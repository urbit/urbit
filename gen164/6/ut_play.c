/* j/6/play.c
**
** This file is in the public domain.
*/
#include "all.h"


/* logic
*/
# define _play_used()

  static u2_noun
  _play_in(u2_noun, u2_noun, u2_noun);

  static u2_noun
  _play_bean(void)
  {
    return u2nt(c3__fork,
                        u2nq(c3__cube, 0, c3__atom, 'f'),
                        u2nq(c3__cube, 1, c3__atom, 'f'));
  }

  static u2_noun
  _play_core(
             u2_noun pac,
             u2_noun con)
  {
    if ( (c3__void == pac) ) {
      u2z(con);

      return c3__void;
    } else {
      return u2nt(c3__core, pac, con);
    }
  }

  static u2_noun
  _play_loc(
            u2_noun van,
            u2_noun loc)
  {
    u2_noun mol = u2nc('o', u2k(loc));
    u2_noun sho = u2_cj_hook(u2k(van), "show");
    u2_noun ret = u2_ci_molt(u2k(sho), u2_cv_sam, u2k(mol), 0);

    u2z(mol);
    u2z(sho);

    return ret;
  }

  static u2_noun
  _play_foil(
             u2_noun pok)
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

  static u2_noun
  _play_edit(
             u2_noun van,
             u2_noun sut,
             u2_noun mew,
             u2_noun rag)
  {
    while ( 1 ) {
      if ( u2_no == u2du(mew) ) {
        return rag;
      } else {
        u2_noun i_mew = u2h(mew);
        u2_noun t_mew = u2t(mew);
        u2_noun pi_mew = u2h(i_mew);
        u2_noun qi_mew = u2t(i_mew);
        u2_noun laf = _play_in(van, sut, qi_mew);
        u2_noun ruz = u2_cqfu_tock(van, sut, pi_mew, laf, rag);

        u2z(laf);
        u2z(rag);
        rag = u2k(u2t(ruz));
        u2z(ruz);

        mew = t_mew;
      }
    }
  }

  static u2_noun
  _play_grow(
             u2_noun van,
             u2_noun sut,
             u2_atom mel,
             u2_noun ruf,
             u2_noun dab)
  {
    u2_noun dan = _play_in(van, sut, ruf);

    return _play_core
        (dan,
                u2nt(u2k(mel),
                             u2k(dan),
                             u2nc(u2nc(u2_nul, u2_nul),
                                          u2k(dab))));
  }

  static u2_noun
  _play_in(
           u2_noun van,
           u2_noun sut,
           u2_noun gen);

  static u2_noun
  _play_x(
          u2_noun van,
          u2_noun sut,
          u2_noun gen)
  {
#if 1
    return _play_in(van, sut, gen);
#else
    u2_noun zix = u2_cqfu_shep
          (van, "gene", 'q', u2k(gen));
    u2_noun ret;

    u2_ct_push(u2nc(c3__mean, zix));

    ret = _play_in(van, sut, gen);

    u2_ct_drop();
    return ret;
#endif
  }

  static u2_noun
  _play_in(
           u2_noun van,
           u2_noun sut,
           u2_noun gen)
  {
    u2_noun p_gen, q_gen, r_gen;

    if ( u2_no == u2du(gen) ) {
      open: {
        u2_noun ter = u2_cr_at(u2_cv_con_3, van);
        u2_noun rex = u2_cqfp_open(ter, gen);
        u2_noun ret;

        if ( u2_yes == u2_cr_sing(rex, gen) ) {
          u2_noun zix = u2_cqfu_shep
                (van, "gene", 'q', u2k(gen));

          u2_ct_push(u2nc(c3__mean, zix));
          return u2_cm_error("play-open");
        }
        ret = _play_x(van, sut, rex);
        u2z(rex);

        return ret;
      }
    }
    else if ( u2_yes == u2du(u2h(gen)) ) {
      _play_used();
      {
        u2_noun dis = _play_x(van, sut, u2h(gen));
        u2_noun dat = _play_x(van, sut, u2t(gen));
        u2_noun ret = u2_cqf_cell(dis, dat);

        u2z(dis);
        u2z(dat);
        return ret;
      }
    }
    else switch ( u2h(gen) ) {
      default: goto open;

      case c3__bcpt: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u2_noun sep = u2_cqfu_seep(van, sut, c3__read, p_gen);
        u2_noun axe = u2h(sep);
        u2_noun rex = u2_cqfl_whip(van, q_gen, axe);
        u2_noun ret = _play_x(van, sut, rex);

        u2z(sep);
        u2z(rex);

        return ret;
      }
      case c3__wtts: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_bean();
      }
      case c3__wtcl: u2_cx_trel(u2t(gen), &p_gen, &q_gen, &r_gen);
      _play_used();
      {
        u2_noun fex = u2_cqfu_gain(van, sut, p_gen);
        u2_noun wux = u2_cqfu_lose(van, sut, p_gen);
        u2_noun dez = (fex == c3__void) ? c3__void
                                        : _play_x(van, fex, q_gen);
        u2_noun doz = (wux == c3__void) ? c3__void
                                        : _play_x(van, wux, r_gen);
        u2_noun ret = u2_cqf_fork(dez, doz);

        u2z(dez); u2z(doz);
        u2z(fex); u2z(wux);
        return ret;
      }
      case c3__clhp: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u2_noun dis = _play_x(van, sut, p_gen);
        u2_noun dat = _play_x(van, sut, q_gen);
        u2_noun ret = u2_cqf_cell(dis, dat);

        u2z(dis);
        u2z(dat);
        return ret;
      }
      case c3__dtkt: p_gen = u2t(gen);
      _play_used();
      {
        return c3__noun;
      }
      case c3__dtwt: p_gen = u2t(gen);
      _play_used();
      {
        return _play_bean();
      }
      case c3__dtts: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_bean();
      }
      case c3__dtls: p_gen = u2t(gen);
      _play_used();
      {
        return u2nc(c3__atom, u2_blip);
      }
      case c3__dtzz: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u2_noun boc = (u2_no == u2ud(q_gen))
                        ? c3__noun
                        : u2nc(c3__atom, u2k(p_gen));
        u2_noun ret = u2_cqf_cube(q_gen, boc);

        u2z(boc);
        return ret;
      }
      case c3__dttr: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return c3__noun;
      }
      case c3__dtzy: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        if ( 'f' == p_gen ) {
          if ( (q_gen > 1) ) {
            return u2_cm_bail(c3__exit);
          } else {
            return _play_bean();
          }
        }
        else return u2nc(c3__atom, u2k(p_gen));
      }
      case c3__ktbr: p_gen = u2t(gen);
      _play_used();
      {
        u2_noun boc = _play_x(van, sut, p_gen);
        u2_noun pro = u2_cqfu_wrap(van, boc, c3__iron);

        u2z(boc);
        return pro;
      }
      case c3__ktpm: p_gen = u2t(gen);
      _play_used();
      {
        u2_noun boc = _play_x(van, sut, p_gen);
        u2_noun pro = u2_cqfu_wrap(van, boc, c3__zinc);

        u2z(boc);
        return pro;
      }
      case c3__ktwt: p_gen = u2t(gen);
      _play_used();
      {
        u2_noun boc = _play_x(van, sut, p_gen);
        u2_noun pro = u2_cqfu_wrap(van, boc, c3__lead);

        u2z(boc);
        return pro;
      }
      case c3__ktts: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u2_noun boc = _play_x(van, sut, q_gen);
        u2_noun ret = u2_cqfu_conk(van, boc, p_gen);

        u2z(boc);
        return ret;
      }
      case c3__ktzp: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u2_noun boc = _play_x(van, sut, q_gen);
        u2_noun ret = u2_cqfu_conk(van, boc, p_gen);

        u2z(boc);
        return ret;
      }
      case c3__ktsg: p_gen = u2t(gen);
      _play_used();
      {
        return _play_x(van, sut, p_gen);
      }
      case c3__kthx: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_x(van, sut, p_gen);
      }
      case c3__ktls: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_x(van, sut, p_gen);
      }
      case c3__tsgr: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u2_noun boc = _play_x(van, sut, p_gen);
        u2_noun ret = _play_x(van, boc, q_gen);

        u2z(boc);
        return ret;
      }
      case c3__tstr: u2_cx_trel(u2t(gen), &p_gen, &q_gen, &r_gen);
      _play_used();
      {
        u2_noun sep = u2_cqfu_seep(van, sut, c3__both, q_gen);
        u2_noun bid = u2nt(u2k(p_gen), u2k(q_gen), sep);
        u2_noun boc = u2_cqf_bull(bid, sut);
        u2_noun ret = _play_x(van, boc, r_gen);

        u2z(bid);
        u2z(boc);

        return ret;
      }
      case c3__cnts: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u2_noun sec = u2_cqfu_seek(van, sut, c3__read, p_gen);
        u2_noun lar = _play_foil(sec);
        u2_noun q_lar = u2t(lar);
        u2_noun qq_lar = u2t(q_lar);
        u2_noun mew = u2_cqfu_snub(van, sut, q_gen);
        u2_noun rag = _play_edit(van, sut, mew, u2k(qq_lar));
        u2_noun ret = u2_cqfu_fire(van, sut, rag);

        u2z(rag);
        u2z(mew);
        u2z(lar);

        return ret;
      }
      case c3__pmcl: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u2_noun ruf = u2nt
          (c3__clhp,
                  u2nc(u2_nul, 1),
                  u2k(p_gen));
        u2_noun ret = _play_grow(van, sut, c3__zinc, ruf, q_gen);

        u2z(ruf);
        return ret;
      }
      case c3__brcn: p_gen = u2t(gen);
      _play_used();
      {
        u2_noun ruf = u2nc(u2_nul, 1);
        u2_noun ret = _play_grow(van, sut, c3__gold, ruf, p_gen);

        u2z(ruf);
        return ret;
      }
      case c3__pmcn: p_gen = u2t(gen);
      _play_used();
      {
        u2_noun ruf = u2nc(u2_nul, 1);
        u2_noun ret = _play_grow(van, sut, c3__lead, ruf, p_gen);

        u2z(ruf);
        return ret;
      }
      case c3__pmls: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u2_noun ruf = u2nt
          (c3__clhp,
                  u2nc(u2_nul, 1),
                  u2k(p_gen));
        u2_noun ret = _play_grow(van, sut, c3__iron, ruf, q_gen);

        u2z(ruf);
        return ret;
      }
      case c3__sgzp: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u2_noun typ = u2_cqfu_play(van, sut, p_gen);
        u2_noun dug = u2_cqfu_duck(van, typ);
        u2_noun ret;

        u2_ct_push(u2nc(c3__mean, dug));
        {
          ret = _play_x(van, sut, q_gen);
        }
        u2_ct_drop();

        u2z(typ);
        return ret;
      }
      case c3__sggr: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_x(van, sut, q_gen);
      }
      case c3__zpts: p_gen = u2t(gen);
      _play_used();
      {
        return c3__noun;
      }
      case c3__zpcm: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_x(van, sut, p_gen);
      }
      case c3__zpcb: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      {
        u2_noun ret;

        u2_ct_push(u2nc(c3__mean, _play_loc(van, p_gen)));
        {
          ret = _play_x(van, sut, q_gen);
        }
        u2_ct_drop();
        return ret;
      }
      case c3__zpcn:
      _play_used();
      {
        u2_noun pet = u2_cj_hook(u2k(van), "seed");
        u2_noun ret = u2k(u2h(pet));

        u2z(pet);
        return ret;
      }
      case c3__zpsm: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u2_noun zur = _play_x(van, sut, p_gen);
        u2_noun vos = _play_x(van, sut, q_gen);
        u2_noun ret = u2_cqf_cell(zur, vos);

        u2z(zur);
        u2z(vos);

        return ret;
      }
      case c3__zpfs:
      case c3__zpzp:
      _play_used();
      {
        return c3__void;
      }
    }
  }

  u2_noun
  _cqfu_play(
                        u2_noun van,
                        u2_noun sut,
                        u2_noun gen)
  {
    u2_noun von = u2_ci_molt(u2k(van), u2_cqfu_van_vet, u2_no, 0);
    u2_noun ret = _play_x(von, sut, gen);

    u2z(von);
    return ret;
  }

/* boilerplate
*/
  u2_noun
  u2_cwfu_play(
                         u2_noun cor)
  {
    u2_noun sut, gen, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam, &gen, u2_cv_con, &van, 0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqfu_play(van, sut, gen);
    }
  }

  u2_noun
  u2_cqfu_play(u2_noun van,
                        u2_noun sut,
                        u2_noun gen)
  {
    return _cqfu_play(van, sut, gen);
  }
