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
  _play_bean()
  {
    return u3kf_fork(u3nt(u3nq(c3__atom, 'f', u3_nul, 0),
                          u3nq(c3__atom, 'f', u3_nul, 1),
                          u3_nul));
  }

  static u3_noun
  _play_rock(u3_noun odo, u3_noun bob)
  { 
    if ( c3y == u3ud(bob) ) {
      return u3nq(c3__atom, u3k(odo), u3_nul, u3k(bob));
    }
    else return u3nt(c3__cell, _play_rock(odo, u3h(bob)), 
                               _play_rock(odo, u3t(bob)));
  }

  static u3_noun
  _play_sand(u3_noun odo, u3_noun bob)
  { 
    if ( c3y == u3ud(bob) ) {
      if ( 'f' == odo ) {
        if ( (bob > 1) ) {
          return u3m_bail(c3__exit);
        } else {
          return _play_bean();
        }
      }
      return u3nt(c3__atom, u3k(odo), u3_nul);
    }
    else return u3nt(c3__cell, _play_rock(odo, u3h(bob)), 
                               _play_rock(odo, u3t(bob)));
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

#if 1
  static u3_noun
  _play_cnts(u3_noun van,
             u3_noun sut,
             u3_noun hyp,
             u3_noun rig)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "epla");

    return u3n_kick_on(u3i_molt(gat, 
                                u3x_sam_2, 
                                u3k(hyp), 
                                u3x_sam_3, 
                                u3k(rig),
                                0));
  }
#else
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
        u3_noun ruz = u3qfu_toss(van, sut, pi_mew, laf, rag);

        u3z(laf);
        u3z(rag);
        rag = u3k(u3t(ruz));
        u3z(ruz);

        mew = t_mew;
      }
    }
  }
  static u3_noun
  _play_cnts_old(u3_noun van,
                 u3_noun sut,
                 u3_noun hyp,
                 u3_noun rig)
  {
    u3_noun lar = u3qfu_seek(van, sut, c3__read, hyp);
    u3_noun q_lar = u3t(lar);
    u3_noun qq_lar = u3t(q_lar);
    u3_noun mew = rig;
    u3_noun rag = _play_edit(van, sut, mew, u3k(qq_lar));
    u3_noun ret = u3qfu_fire(van, sut, rag);

    u3z(rag);
    u3z(lar);

    return ret;
  }
#endif

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

      case c3__wtts:
      case c3__fit: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_bean();
      }

      case c3__wtcl:
      case c3__if: u3x_trel(u3t(gen), &p_gen, &q_gen, &r_gen);
      _play_used();
      {
        u3_noun fex = u3qfu_gain(van, sut, p_gen);
        u3_noun wux = u3qfu_lose(van, sut, p_gen);
        u3_noun dez = (fex == c3__void) ? c3__void
                                        : _play_x(van, fex, q_gen);
        u3_noun doz = (wux == c3__void) ? c3__void
                                        : _play_x(van, wux, r_gen);
        u3_noun ret = u3qf_forq(dez, doz);

        u3z(dez); u3z(doz);
        u3z(fex); u3z(wux);
        return ret;
      }

      case c3__clhp:
      case c3__dub: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u3_noun dis = _play_x(van, sut, p_gen);
        u3_noun dat = _play_x(van, sut, q_gen);
        u3_noun ret = u3qf_cell(dis, dat);

        u3z(dis);
        u3z(dat);
        return ret;
      }

      case c3__dtkt:
      case c3__wish: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u3_noun nog = u3nc(c3__cold, u3k(p_gen));
        u3_noun ret = _play_x(van, sut, nog);

        u3z(nog);
        return ret;
      }

      case c3__dtwt: 
      case c3__deep: p_gen = u3t(gen);
      _play_used();
      {
        return _play_bean();
      }

      case c3__dtts:
      case c3__same: u3x_cell(u3t(gen), &p_gen, &q_gen); 
      _play_used();
      {
        return _play_bean();
      }
      
      case c3__bump:
      case c3__dtls: p_gen = u3t(gen);
      _play_used();
      {
        return u3nt(c3__atom, u3_blip, u3_nul);
      }

      case c3__rock: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_rock(p_gen, q_gen);
      }

      case c3__dttr: 
      case c3__kick: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return c3__noun;
      }

      case c3__sand: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_sand(p_gen, q_gen);
      }

      case c3__hand: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return u3k(p_gen);
      }

      case c3__ktbr: 
      case c3__iron: p_gen = u3t(gen);
      _play_used();
      {
        u3_noun boc = _play_x(van, sut, p_gen);
        u3_noun pro = u3qfu_wrap(van, boc, c3__iron);

        u3z(boc);
        return pro;
      }

      case c3__ktpm: 
      case c3__zinc: p_gen = u3t(gen);
      _play_used();
      {
        u3_noun boc = _play_x(van, sut, p_gen);
        u3_noun pro = u3qfu_wrap(van, boc, c3__zinc);

        u3z(boc);
        return pro;
      }

      case c3__ktwt: 
      case c3__lead: p_gen = u3t(gen);
      _play_used();
      {
        u3_noun boc = _play_x(van, sut, p_gen);
        u3_noun pro = u3qfu_wrap(van, boc, c3__lead);

        u3z(boc);
        return pro;
      }

      case c3__ktts: 
      case c3__name: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u3_noun boc = _play_x(van, sut, q_gen);
        u3_noun ret = u3qfu_conk(van, boc, p_gen);

        u3z(boc);
        return ret;
      }

      case c3__ktsg: 
      case c3__burn: p_gen = u3t(gen);
      _play_used();
      {
        return _play_x(van, sut, p_gen);
      }

      case c3__ktls: 
      case c3__like: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_x(van, sut, p_gen);
      }

      case c3__tsgr: 
      case c3__per: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u3_noun boc = _play_x(van, sut, p_gen);
        u3_noun ret = _play_x(van, boc, q_gen);

        u3z(boc);
        return ret;
      }

      case c3__tstr: 
      case c3__aka: u3x_trel(u3t(gen), &p_gen, &q_gen, &r_gen);
      _play_used();
      {
        u3_noun boc = u3qfu_buss(van, sut, p_gen, q_gen);
        u3_noun ret = _play_x(van, boc, r_gen);

        u3z(boc);
        return ret;
      }

      case c3__cnts: 
      case c3__make: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_cnts(van, sut, p_gen, q_gen);
      }

      case c3__brcn: 
      case c3__core: p_gen = u3t(gen);
      _play_used();
      {
        u3_noun ruf = u3nc(u3_nul, 1);
        u3_noun ret = _play_grow(van, sut, c3__gold, ruf, p_gen);

        u3z(ruf);
        return ret;
      }

      case c3__sgzp: 
      case c3__type: u3x_cell(u3t(gen), &p_gen, &q_gen);
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

      case c3__sggr: 
      case c3__hint: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_x(van, sut, q_gen);
      }

      case c3__zpts: 
      case c3__nock: p_gen = u3t(gen);
      _play_used();
      {
        return c3__noun;
      }

      case c3__zpcm: 
      case c3__twig: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        return _play_x(van, sut, p_gen);
      }

      case c3__dbug: u3x_cell(u3t(gen), &p_gen, &q_gen);
      {
        u3_noun ret;

        u3t_push(u3nc(c3__mean, _play_loc(van, p_gen)));
        {
          ret = _play_x(van, sut, q_gen);
        }
        u3t_drop();
        return ret;
      }

      case c3__zpsm: 
      case c3__spit: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _play_used();
      {
        u3_noun zur = _play_x(van, sut, p_gen);
        u3_noun vos = _play_x(van, sut, q_gen);
        u3_noun ret = u3qf_cell(zur, vos);

        u3z(zur);
        u3z(vos);

        return ret;
      }

      case c3__lost:
      case c3__zpzp:
      case c3__fail:
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
