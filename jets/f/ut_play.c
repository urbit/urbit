/* j/6/play.c
**
*/
#include "all.h"


/* logic
*/
# define _play_used()

  static u3_noun
  _play_in(u3_noun, u3_noun, u3_noun);

  static u3_noun
  _play_bean(void)
  {
    return u3nt(c3__fork,
                u3nq(c3__cube, 0, c3__atom, 'f'),
                u3nq(c3__cube, 1, c3__atom, 'f'));
  }

  static u3_noun
  _play_core(u3_noun pac,
             u3_noun con)
  {
    if ( (c3__void == pac) ) {
      u3z(con);

      return c3__void;
    } else {
      return u3nt(c3__core, pac, con);
    }
  }

  static u3_noun
  _play_loc(u3_noun van,
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
  _play_foil(u3_noun pok)
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
  _play_edit(u3_noun van,
             u3_noun sut,
             u3_noun mew,
             u3_noun rag)
  {
    while ( 1 ) {
      if ( c3n == u3du(mew) ) {
        return rag;
      } else {
        u3_noun i_mew = u3h(mew);
        u3_noun t_mew = u3t(mew);
        u3_noun pi_mew = u3h(i_mew);
        u3_noun qi_mew = u3t(i_mew);
        u3_noun laf = _play_in(van, sut, qi_mew);
        u3_noun ruz = u3qfu_tock(van, sut, pi_mew, laf, rag);

        u3z(laf);
        u3z(rag);
        rag = u3k(u3t(ruz));
        u3z(ruz);

        mew = t_mew;
      }
    }
  }

  static u3_noun
  _play_grow(u3_noun van,
             u3_noun sut,
             u3_atom mel,
             u3_noun ruf,
             u3_noun dab)
  {
    u3_noun dan = _play_in(van, sut, ruf);

    return _play_core(dan,
                      u3nt(u3k(mel),
                           u3k(dan),
                           u3nc(u3nc(u3_nul, u3_nul),
                                u3k(dab))));
  }

  static u3_noun
  _play_in(u3_noun van,
           u3_noun sut,
           u3_noun gen);

  static u3_noun
  _play_x(u3_noun van,
          u3_noun sut,
          u3_noun gen)
  {
#if 1
    return _play_in(van, sut, gen);
#else
    u3_noun zix = u3qfu_shep
          (van, "gene", 'q', u3k(gen));
    u3_noun ret;

    u3t_push(u3nc(c3__mean, zix));

    ret = _play_in(van, sut, gen);

    u3t_drop();
    return ret;
#endif
  }

  static u3_noun
  _play_in(u3_noun van,
           u3_noun sut,
           u3_noun gen)
  {
    u3_noun p_gen, q_gen, r_gen;

    if ( c3n == u3du(gen) ) {
      open: {
        u3_noun ter = u3r_at(u3x_con_3, van);
        u3_noun rex = u3qfp_open(ter, gen);
        u3_noun ret;

        if ( c3y == u3r_sing(rex, gen) ) {
          u3_noun zix = u3qfu_shep(van, "gene", 'q', u3k(gen));

          u3t_push(u3nc(c3__mean, zix));
          return u3m_error("play-open");
        }
        ret = _play_x(van, sut, rex);
        u3z(rex);

        return ret;
      }
    }
    else if ( c3y == u3du(u3h(gen)) ) {
      _play_used();
      {
        u3_noun dis = _play_x(van, sut, u3h(gen));
        u3_noun dat = _play_x(van, sut, u3t(gen));
        u3_noun ret = u3qf_cell(dis, dat);

        u3z(dis);
        u3z(dat);
        return ret;
      }
    }
    else switch ( u3h(gen) ) {
      default: goto open;

      case c3__bcpt: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u3_noun sep = u3qfu_seep(van, sut, c3__read, p_gen);
        u3_noun axe = u3h(sep);
        u3_noun rex = u3qfl_whip(van, q_gen, axe);
        u3_noun ret = _play_x(van, sut, rex);

        u3z(sep);
        u3z(rex);

        return ret;
      }
      case c3__wtts: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_bean();
      }
      case c3__wtcl: u3x_trel(u3t(gen), &p_gen, &q_gen, &r_gen);
      _play_used();
      {
        u3_noun fex = u3qfu_gain(van, sut, p_gen);
        u3_noun wux = u3qfu_lose(van, sut, p_gen);
        u3_noun dez = (fex == c3__void) ? c3__void
                                        : _play_x(van, fex, q_gen);
        u3_noun doz = (wux == c3__void) ? c3__void
                                        : _play_x(van, wux, r_gen);
        u3_noun ret = u3qf_fork(dez, doz);

        u3z(dez); u3z(doz);
        u3z(fex); u3z(wux);
        return ret;
      }
      case c3__clhp: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u3_noun dis = _play_x(van, sut, p_gen);
        u3_noun dat = _play_x(van, sut, q_gen);
        u3_noun ret = u3qf_cell(dis, dat);

        u3z(dis);
        u3z(dat);
        return ret;
      }
      case c3__dtkt: p_gen = u3t(gen);
      _play_used();
      {
        return c3__noun;
      }
      case c3__dtwt: p_gen = u3t(gen);
      _play_used();
      {
        return _play_bean();
      }
      case c3__dtts: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_bean();
      }
      case c3__dtls: p_gen = u3t(gen);
      _play_used();
      {
        return u3nc(c3__atom, u3_blip);
      }
      case c3__dtzz: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u3_noun boc = (c3n == u3ud(q_gen))
                        ? c3__noun
                        : u3nc(c3__atom, u3k(p_gen));
        u3_noun ret = u3qf_cube(q_gen, boc);

        u3z(boc);
        return ret;
      }
      case c3__dttr: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return c3__noun;
      }
      case c3__dtzy: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        if ( 'n' == p_gen ) {
          if ( (q_gen != 0) ) {
            return u3m_bail(c3__exit);
          } else {
            return u3nq(c3__cube, q_gen, c3__atom, p_gen);
          }
        }
        if ( 'f' == p_gen ) {
          if ( (q_gen > 1) ) {
            return u3m_bail(c3__exit);
          } else {
            return _play_bean();
          }
        }
        else return u3nc(c3__atom, u3k(p_gen));
      }
      case c3__ktbr: p_gen = u3t(gen);
      _play_used();
      {
        u3_noun boc = _play_x(van, sut, p_gen);
        u3_noun pro = u3qfu_wrap(van, boc, c3__iron);

        u3z(boc);
        return pro;
      }
      case c3__ktpm: p_gen = u3t(gen);
      _play_used();
      {
        u3_noun boc = _play_x(van, sut, p_gen);
        u3_noun pro = u3qfu_wrap(van, boc, c3__zinc);

        u3z(boc);
        return pro;
      }
      case c3__ktwt: p_gen = u3t(gen);
      _play_used();
      {
        u3_noun boc = _play_x(van, sut, p_gen);
        u3_noun pro = u3qfu_wrap(van, boc, c3__lead);

        u3z(boc);
        return pro;
      }
      case c3__ktts: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u3_noun boc = _play_x(van, sut, q_gen);
        u3_noun ret = u3qfu_conk(van, boc, p_gen);

        u3z(boc);
        return ret;
      }
      case c3__ktzp: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u3_noun boc = _play_x(van, sut, q_gen);
        u3_noun ret = u3qfu_conk(van, boc, p_gen);

        u3z(boc);
        return ret;
      }
      case c3__ktsg: p_gen = u3t(gen);
      _play_used();
      {
        return _play_x(van, sut, p_gen);
      }
      case c3__kthx: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_x(van, sut, p_gen);
      }
      case c3__ktls: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_x(van, sut, p_gen);
      }
      case c3__tsgr: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u3_noun boc = _play_x(van, sut, p_gen);
        u3_noun ret = _play_x(van, boc, q_gen);

        u3z(boc);
        return ret;
      }
      case c3__tstr: u3x_trel(u3t(gen), &p_gen, &q_gen, &r_gen);
      _play_used();
      {
        u3_noun sep = u3qfu_seep(van, sut, c3__both, q_gen);
        u3_noun bid = u3nt(u3k(p_gen), u3k(q_gen), sep);
        u3_noun boc = u3qf_bull(bid, sut);
        u3_noun ret = _play_x(van, boc, r_gen);

        u3z(bid);
        u3z(boc);

        return ret;
      }
      case c3__cnts: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u3_noun sec = u3qfu_seek(van, sut, c3__read, p_gen);
        u3_noun lar = _play_foil(sec);
        u3_noun q_lar = u3t(lar);
        u3_noun qq_lar = u3t(q_lar);
        u3_noun mew = u3qfu_snub(van, sut, q_gen);
        u3_noun rag = _play_edit(van, sut, mew, u3k(qq_lar));
        u3_noun ret = u3qfu_fire(van, sut, rag);

        u3z(rag);
        u3z(mew);
        u3z(lar);

        return ret;
      }
      case c3__pmcl: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u3_noun ruf = u3nt(c3__clhp,
                           u3nc(u3_nul, 1),
                           u3k(p_gen));
        u3_noun ret = _play_grow(van, sut, c3__zinc, ruf, q_gen);

        u3z(ruf);
        return ret;
      }
      case c3__brcn: p_gen = u3t(gen);
      _play_used();
      {
        u3_noun ruf = u3nc(u3_nul, 1);
        u3_noun ret = _play_grow(van, sut, c3__gold, ruf, p_gen);

        u3z(ruf);
        return ret;
      }
      case c3__pmcn: p_gen = u3t(gen);
      _play_used();
      {
        u3_noun ruf = u3nc(u3_nul, 1);
        u3_noun ret = _play_grow(van, sut, c3__lead, ruf, p_gen);

        u3z(ruf);
        return ret;
      }
      case c3__pmls: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u3_noun ruf = u3nt(c3__clhp,
                           u3nc(u3_nul, 1),
                           u3k(p_gen));
        u3_noun ret = _play_grow(van, sut, c3__iron, ruf, q_gen);

        u3z(ruf);
        return ret;
      }
      case c3__sgzp: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u3_noun typ = u3qfu_play(van, sut, p_gen);
        u3_noun dug = u3qfu_duck(van, typ);
        u3_noun ret;

        u3t_push(u3nc(c3__mean, dug));
        {
          ret = _play_x(van, sut, q_gen);
        }
        u3t_drop();

        u3z(typ);
        return ret;
      }
      case c3__sggr: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_x(van, sut, q_gen);
      }
      case c3__zpts: p_gen = u3t(gen);
      _play_used();
      {
        return c3__noun;
      }
      case c3__zpcm: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_x(van, sut, p_gen);
      }
      case c3__zpcb: u3x_cell(u3t(gen), &p_gen, &q_gen);
      {
        u3_noun ret;

        u3t_push(u3nc(c3__mean, _play_loc(van, p_gen)));
        {
          ret = _play_x(van, sut, q_gen);
        }
        u3t_drop();
        return ret;
      }
      case c3__zpcn:
      _play_used();
      {
        u3_noun pet = u3j_hook(u3k(van), "seed");
        u3_noun ret = u3k(u3h(pet));

        u3z(pet);
        return ret;
      }
      case c3__zpsm: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u3_noun zur = _play_x(van, sut, p_gen);
        u3_noun vos = _play_x(van, sut, q_gen);
        u3_noun ret = u3qf_cell(zur, vos);

        u3z(zur);
        u3z(vos);

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

  u3_noun
  _cqfu_play(u3_noun van,
             u3_noun sut,
             u3_noun gen)
  {
    u3_noun von = u3i_molt(u3k(van), u3qfu_van_vet, c3n, 0);
    u3_noun ret = _play_x(von, sut, gen);

    u3z(von);
    return ret;
  }

/* boilerplate
*/
  u3_noun
  u3wfu_play(u3_noun cor)
  {
    u3_noun sut, gen, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &gen, u3x_con, &van, 0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_play(van, sut, gen);
    }
  }

  u3_noun
  u3qfu_play(u3_noun van,
             u3_noun sut,
             u3_noun gen)
  {
    return _cqfu_play(van, sut, gen);
  }
