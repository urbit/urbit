/* j/6/mint.c
**
*/
#include "all.h"

/* logic
*/
  static u3_noun
  _mint_in(u3_noun, u3_noun, u3_noun, u3_noun);

  static u3_noun
  _mint_bean()
  {
    return u3kf_fork(u3nt(u3nq(c3__atom, 'f', u3_nul, 0),
                          u3nq(c3__atom, 'f', u3_nul, 1),
                          u3_nul));
  }

  static u3_noun
  _mint_tend(u3_noun vit)
  {
    if ( u3_nul == vit ) {
      return 1;
    } else {
      u3_noun nex = _mint_tend(u3t(vit));

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
  _mint_vet(u3_noun van)
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
  _mint_feel(u3_noun van,
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
  _mint_mine(u3_noun van,
             u3_noun sut,
             u3_noun mel,
             u3_noun nym,
             u3_noun hud,
             u3_noun dom)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "mine");

    return u3n_kick_on(u3i_molt(gat,
                                u3x_sam_2,
                                u3k(mel),
                                u3x_sam_6,
                                u3k(nym),
                                u3x_sam_14,
                                u3k(hud),
                                u3x_sam_15,
                                u3k(dom),
                                0));
  }

  static u3_noun
  _mint_burp(u3_noun van,
             u3_noun sut)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);

    return u3j_hook(von, "burp");
  }

  static u3_noun
  _mint_cond(u3_noun pex,
             u3_noun yom,
             u3_noun woq)
  {
    if ( 1 == u3h(pex) ) {
      if ( 0 == u3t(pex) ) {
        u3z(pex);
        u3z(woq);

        return yom;
      }
      else if ( 1 == u3t(pex) ) {
        u3z(pex);
        u3z(yom);

        return woq;
      }
    }
    return u3nq(6, pex, yom, woq);
  }

  static u3_noun
  _mint_corn(u3_noun van,
             u3_noun sut,
             u3_noun gen)
  {
    u3_noun mil = _mint_in(van, sut, c3__noun, gen);
    u3_noun fol = u3k(u3t(mil));

    u3z(mil);
    return fol;
  }

  static u3_noun
  _mint_nice(u3_noun van,
             u3_noun gol,
             u3_noun typ)
  {
    if ( (c3y == _mint_vet(van)) &&
         (c3n == u3qfu_nest(van, gol, c3y, typ)) )
    {
      // u3_noun dun = u3qfu_dunq(van, "need", gol);
      // u3_noun niz = u3qfu_dunq(van, "have", typ);

      // u3t_push(u3nc(c3__mean, dun));
      // u3t_push(u3nc(c3__mean, niz));

      return u3m_error("mint-nice");
    }
    else return typ;
  }

  static u3_noun
  _mint_cove(u3_noun nug)
  {
    if ( 0 == u3h(nug) ) {
      return u3k(u3t(nug));
    }
    else if ( 11 == u3h(nug) ) {
      return _mint_cove(u3t(u3t(nug)));
    }
    else {
      return u3m_error("mint-cove");
    }
  }

  static u3_noun
  _mint_grow(u3_noun van,
             u3_noun sut,
             u3_noun gol,
             u3_atom mel,
             u3_noun nym,
             u3_noun hud,
             u3_noun ruf,
             u3_noun dom)
  {
    u3_noun dan = _mint_in(van, sut, c3__noun, ruf);
    u3_noun p_dan = u3h(dan);
    u3_noun q_dan = u3t(dan);
    u3_noun pul = _mint_mine(van, p_dan, mel, nym, hud, dom);
    u3_noun ret = u3nc(_mint_nice(van, gol, u3k(u3h(pul))),
                       u3qf_cons(u3t(pul), q_dan));

    u3z(pul);
    u3z(dan);
    return ret;
  }

  static u3_noun
  _mint_loc(u3_noun van,
            u3_noun loc)
  {
    u3_noun mol = u3nc('o', u3k(loc));
    u3_noun sho = u3j_cook("_mint_loc-show", u3k(van), "show");
    u3_noun ret = u3i_molt(u3k(sho), u3x_sam, u3k(mol), 0);

    u3z(mol);
    u3z(sho);

    return ret;
  }

#if 1
  static u3_noun
  _mint_cnts(u3_noun van,
             u3_noun sut,
             u3_noun gol,
             u3_noun hyp,
             u3_noun rig)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_cook("_mint_cnts-emin", von, "emin");

    return u3n_kick_on(u3i_molt(gat,
                                u3x_sam_2,
                                u3k(gol),
                                u3x_sam_6,
                                u3k(hyp),
                                u3x_sam_7,
                                u3k(rig),
                                0));
  }
#else
  static u3_noun
  _mint_edit(u3_noun van,
             u3_noun sut,
             u3_noun mew,
             u3_noun p_lar,
             u3_noun rag,
             u3_noun hej)
  {
    while ( 1 ) {
      if ( c3n == u3du(mew) ) {
        u3_noun gim = u3qfu_fire(van, sut, rag);
        u3_noun fol = u3qf_hike(p_lar, hej);

        u3z(rag);
        u3z(hej);

        return u3nc(gim, fol);
      } else {
        u3_noun i_mew = u3h(mew);
        u3_noun t_mew = u3t(mew);
        u3_noun pi_mew = u3h(i_mew);
        u3_noun qi_mew = u3t(i_mew);
        u3_noun zil = u3qfu_mint(van, sut, c3__noun, qi_mew);
        u3_noun p_zil = u3h(zil);
        u3_noun q_zil = u3t(zil);
        u3_noun wip = u3qfu_toss(van, sut, pi_mew, p_zil, rag);

        u3z(rag);
        rag = u3k(u3t(wip));

        hej = u3nc(u3nc(u3k(u3h(wip)),
                        u3k(q_zil)),
                   hej);

        u3z(zil);
        u3z(wip);

        mew = t_mew;
      }
    }
  }
  static u3_noun
  _mint_cnts_old(u3_noun van,
                 u3_noun sut,
                 u3_noun gol,
                 u3_noun hyp,
                 u3_noun rig)
  {
    u3_noun lar = u3qfu_seek(van, sut, c3__read, hyp);
    u3_noun p_lar = u3h(lar);
    u3_noun q_lar = u3t(lar);
    u3_noun pq_lar = u3h(q_lar);
    u3_noun qq_lar = u3t(q_lar);
    u3_noun mew = rig;
    u3_noun yom = _mint_edit(van, sut, mew, p_lar, u3k(qq_lar), u3_nul);
    u3_noun p_yom = u3h(yom);
    u3_noun q_yom = u3t(yom);
    u3_noun ret = u3nc(_mint_nice(van, gol, u3k(p_yom)),
                       (0 == pq_lar) ? u3k(q_yom)
                                     : u3nt(9, u3k(pq_lar), u3k(q_yom)));

    u3z(yom);
    u3z(lar);

    return ret;
  }
#endif

# define _mint_used()

  static u3_noun
  _mint_in(u3_noun van,
           u3_noun sut,
           u3_noun gol,
           u3_noun gen)
  {
    u3_noun p_gen, q_gen, r_gen;
    u3_noun ret;

    if ( (c3__void == sut) &&
         !((c3y == u3du(gen)) && (c3__dbug == u3h(gen))) )
    {
      if ( (c3n == _mint_vet(van))
           || ((c3y == u3du(gen)) &&
               ((c3__zpfs == u3h(gen)) ||
                (c3__zpsk == u3h(gen)) ||
                (c3__lost == u3h(gen)) ||
                (c3__fail == u3h(gen)) ||
                (c3__zpzp == u3h(gen)))) )
      {
        return u3nt(c3__void, 0, 0);
      }
      else {
        return u3m_error("mint-vain");
      }
    }

    if ( c3n == u3du(gen) ) {
      u3_noun ter = u3r_at(u3x_con_3, van);
      u3_noun fab = u3r_at(u3qfu_van_fab, van);
      u3_noun rex = u3qfp_open(ter, fab, gen);

      ret = _mint_in(van, sut, gol, rex);
      u3z(rex);

      return ret;
    }
    else if ( c3y == u3du(u3h(gen)) ) {
      _mint_used();
      {
        u3_noun hed = _mint_in(van, sut, c3__noun, u3h(gen));
        u3_noun tal = _mint_in(van, sut, c3__noun, u3t(gen));
        u3_noun typ = u3qf_cell(u3h(hed), u3h(tal));

        ret = u3nc(_mint_nice(van, gol, typ),
                   u3qf_cons(u3t(hed),
                             u3t(tal)));

        u3z(hed);
        u3z(tal);

        return ret;
      }
    } else switch ( u3h(gen) ) {
      default: {
        u3_noun ter = u3r_at(u3x_con_3, van);
        u3_noun fab = u3r_at(u3qfu_van_fab, van);
        u3_noun rex = u3qfp_open(ter, fab, gen);

        if ( c3y == u3r_sing(rex, gen) ) {
#if 1
          u3_noun zix = u3qfu_shep(van, "gene", 'q', u3k(gen));

          u3t_push(u3nc(c3__mean, zix));
          return u3m_error("mint-open");
#else
          u3_err("h", u3h(gen));
          return u3m_bail(c3__fail);
#endif
        }
        ret = _mint_in(van, sut, gol, rex);
        u3z(rex);

        return ret;
      }

      case c3__fits: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun wam = u3qfu_play(van, sut, p_gen);
        u3_noun dok = u3nc(c3__wing, u3k(q_gen));
        u3_noun vol = _mint_corn(van, sut, dok);
        u3_noun axe = _mint_cove(vol);

        ret = u3nc(_mint_nice(van, gol, _mint_bean()),
                   u3qfu_fish(van, wam, axe));

        u3z(axe);
        u3z(vol);
        u3z(wam);
        u3z(dok);

        return ret;
      }

      case c3__wtcl: u3x_trel(u3t(gen), &p_gen, &q_gen, &r_gen);
      _mint_used();
      {
        u3_noun bol = _mint_bean();
        u3_noun nor = _mint_in(van, sut, bol, p_gen);
        u3_noun fex = u3qfu_gain(van, sut, p_gen);
        u3_noun wux = u3qfu_lose(van, sut, p_gen);
        u3_noun duy = (c3__void == fex)
                        ? ( (c3__void == wux)
                             ?  u3nc(0, 0)
                             :  u3nc(1, 1) )
                        : ( (c3__void == wux)
                            ?  u3nc(1, 0)
                            :  u3k(u3t(nor)) );
        u3_noun hiq = _mint_in(van, fex, gol, q_gen);
        u3_noun ran = _mint_in(van, wux, gol, r_gen);

        ret = u3nc(u3qf_forq(u3h(hiq),
                   u3h(ran)),
                   _mint_cond(duy,
                              u3k(u3t(hiq)),
                              u3k(u3t(ran))));

        u3z(ran);
        u3z(hiq);
        u3z(nor);
        u3z(wux);
        u3z(fex);
        u3z(bol);

        return ret;
      }

      case c3__clhp: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun hed = _mint_in(van, sut, c3__noun, p_gen);
        u3_noun tal = _mint_in(van, sut, c3__noun, q_gen);
        u3_noun typ = u3qf_cell(u3h(hed), u3h(tal));

        ret = u3nc(_mint_nice(van, gol, typ),
                   u3qf_cons(u3t(hed),
                             u3t(tal)));

        u3z(hed);
        u3z(tal);

        return ret;
      }

      case c3__dtts: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun typ = _mint_nice(van, gol, _mint_bean());
        u3_noun one = _mint_in(van, sut, c3__noun, p_gen);
        u3_noun two = _mint_in(van, sut, c3__noun, q_gen);
        u3_noun ret;

        if ( (c3y == _mint_vet(van)) &&
             (c3n == u3qfu_nest(van, u3h(one), c3n, u3h(two))) &&
             (c3n == u3qfu_nest(van, u3h(two), c3y, u3h(one))) )
        {
          return u3m_error("nest");
        }
        ret = u3nc(typ, u3nt(5, u3k(u3t(one)), u3k(u3t(two))));
        u3z(one);
        u3z(two);

        return ret;
      }

      case c3__dtwt: p_gen = u3t(gen);
      _mint_used();
      {
        u3_noun typ = _mint_nice(van, gol, _mint_bean());

        return u3nc(typ,
                    u3nc(3, _mint_corn(van, sut, p_gen)));
      }

      case c3__dtkt: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun nog = u3nc(c3__kttr, u3k(p_gen));
        u3_noun nef = _mint_in(van, sut, gol, nog);
        u3_noun viz = _mint_in(van, sut, c3__noun, q_gen);

        ret = u3nc(u3k(u3h(nef)),
                   u3nt(12, u3nc(1, u3nc(151, u3k(u3h(nef)))), u3k(u3t(viz))));

        u3z(viz);
        u3z(nef);
        u3z(nog);
        return ret;
      }

      case c3__dtls: p_gen = u3t(gen);
      _mint_used();
      {
        u3_noun tom = u3nt(c3__atom, u3_blip, u3_nul);
        u3_noun sam = _mint_in(van, sut, tom, p_gen);

        ret = u3nc(_mint_nice(van, gol, tom),
                   u3nc(4, u3k(u3t(sam))));

        u3z(sam);
        return ret;
      }

      case c3__rock: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun typ = u3qfu_play(van, sut, gen);
        u3_noun ret = u3nc(_mint_nice(van, gol, typ),
                           u3nc(1, u3k(q_gen)));

        return ret;
      }

      case c3__dttr: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun one = _mint_corn(van, sut, p_gen);
        u3_noun two = _mint_corn(van, sut, q_gen);

        return u3nc(_mint_nice(van, gol, c3__noun),
                    u3nt(2, one, two));
      }

      case c3__sand: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun typ = u3qfu_play(van, sut, gen);
        u3_noun ret = u3nc(_mint_nice(van, gol, typ),
                           u3nc(1, u3k(q_gen)));

        return ret;
      }

      case c3__hand:
      _mint_used();
      {
        return u3k(u3t(gen));
      }

      case c3__ktbr: p_gen = u3t(gen);
      _mint_used();
      {
        u3_noun ryd = _mint_in(van, sut, gol, p_gen);
        u3_noun tyf = u3qfu_wrap(van, u3h(ryd), c3__iron);
        u3_noun tyn = _mint_nice(van, gol, tyf);
        u3_noun pro = u3nc(tyn, u3k(u3t(ryd)));

        u3z(ryd);
        return pro;
      }

      case c3__ktpd:
      case c3__ktpm: p_gen = u3t(gen);
      _mint_used();
      {
        u3_noun ryd = _mint_in(van, sut, gol, p_gen);
        u3_noun tyf = u3qfu_wrap(van, u3h(ryd), c3__zinc);
        u3_noun tyn = _mint_nice(van, gol, tyf);
        u3_noun pro = u3nc(tyn, u3k(u3t(ryd)));

        u3z(ryd);
        return pro;
      }

      case c3__ktwt: p_gen = u3t(gen);
      _mint_used();
      {
        u3_noun ryd = _mint_in(van, sut, gol, p_gen);
        u3_noun tyf = u3qfu_wrap(van, u3h(ryd), c3__lead);
        u3_noun tyn = _mint_nice(van, gol, tyf);
        u3_noun pro = u3nc(tyn, u3k(u3t(ryd)));

        u3z(ryd);
        return pro;
      }

      case c3__note: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun hit = u3nc(u3k(sut), u3k(p_gen));
        u3_noun hum = _mint_in(van, sut, gol, q_gen);

        u3_noun ret = u3nc(u3qf_hint(hit, u3h(hum)),
                           u3k(u3t(hum)));
        u3z(hum);
        u3z(hit);
        return ret;
      }

      case c3__tune: p_gen = u3t(gen);
      _mint_used();
      {
        return u3nc(u3qf_face(p_gen, sut), u3nc(0, 1));
      }

      case c3__ktls: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun huz = u3qfu_play(van, sut, p_gen);
        u3_noun hif = _mint_nice(van, gol, huz);
        u3_noun zel = _mint_in(van, sut, hif, q_gen);
        u3_noun ret = u3nc(hif, u3k(u3t(zel)));

        u3z(zel);
        return ret;
      }

      case c3__tsbn: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun fid = _mint_in(van, sut, c3__noun, p_gen);
        u3_noun p_fid = u3h(fid);
        u3_noun q_fid = u3t(fid);

        if ( 0 == p_fid ) {
          u3m_p("bad subject: p_gen", p_gen);
          c3_assert(0);
        }
        u3_noun dov = _mint_in(van, p_fid, gol, q_gen);
        u3_noun p_dov = u3h(dov);
        u3_noun q_dov = u3t(dov);

        ret = u3nc(u3k(p_dov),
                   u3qf_comb(q_fid, q_dov));

        u3z(fid);
        u3z(dov);
        return ret;
      }

      case c3__tscm: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun boc = u3qfu_busk(van, sut, p_gen);
        u3_noun ret = _mint_in(van, boc, gol, q_gen);

        u3z(boc);
        return ret;
      }

      case c3__cnts: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        return _mint_cnts(van, sut, gol, p_gen, q_gen);
      }

      case c3__brcn: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun ruf = u3nc(u3_nul, 1);

        ret = _mint_grow(van, sut, gol, c3__gold, p_gen, c3__dry, ruf, q_gen);
        u3z(ruf);

        return ret;
      }

      case c3__brvt: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun ruf = u3nc(u3_nul, 1);

        ret = _mint_grow(van, sut, gol, c3__gold, p_gen, c3__wet, ruf, q_gen);
        u3z(ruf);

        return ret;
      }

      case c3__sgzp: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun typ = u3qfu_play(van, sut, p_gen);
        u3_noun dug = u3qfu_duck(van, typ);

        u3t_push(u3nc(c3__mean, dug));
        {
          ret = _mint_in(van, sut, gol, q_gen);
        }
        u3t_drop();

        u3z(typ);
        return ret;
      }

      case c3__sgbn: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun hum = _mint_in(van, sut, gol, q_gen);
        u3_noun bez;

          if ( c3y == u3ud(p_gen) ) {
            bez = u3k(p_gen);
          } else {
            bez = u3nc(u3k(u3h(p_gen)),
                       _mint_corn(van, sut, u3t(p_gen)));
          }
          ret = u3nc(u3k(u3h(hum)),
                     u3nt(11, bez, u3k(u3t(hum))));

        u3z(hum);
        return ret;
      }

      case c3__zpts: p_gen = u3t(gen);
      _mint_used();
      {
        u3_noun von = u3i_molt(u3k(van), u3qfu_van_vet, c3n, 0);
        u3_noun sev = _mint_corn(von, sut, p_gen);

        u3z(von);
        return u3nc(c3__noun, u3nc(1, sev));
      }

      case c3__ktcn: p_gen = u3t(gen);
      _mint_used();
      {
        u3_noun von = u3i_molt(u3k(van), u3qfu_van_fab, c3n, 0);
        u3_noun ret = _mint_in(von, sut, gol, p_gen);

        u3z(von);
        return ret;
      }

      case c3__wthx: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun hyp = u3nc(u3nc(c3y, 1), u3k(q_gen));
        u3_noun fid = u3qfu_find(van, sut, c3__free, hyp);

        if ( c3y != u3h(fid) ) {
          return u3m_error("mint-fragment");
        }
        else {
          u3_noun pp_fid = u3h(u3t(fid));
          u3_noun qp_fid = u3t(u3t(fid));

          if ( c3y != u3h(qp_fid) ) {
            return u3m_error("mint-fragment");
          }
          else {
            u3_noun pqp_fid = u3t(qp_fid);
            u3_noun axe     = _mint_tend(pp_fid);
            u3_noun ret;

            ret = u3nc(_mint_nice(van, gol, _mint_bean()),
                       u3qfr_fish(van, pqp_fid, p_gen, axe));

            u3z(axe);
            u3z(fid);
            u3z(hyp);

            return ret;
          }
        }
      }

      case c3__cold:
      case c3__ktsg: p_gen = u3t(gen);
      _mint_used();
      {

        c3_m    fun_m = 141 + c3__blow;
        u3_noun vrf   = u3r_at(u3qfu_van_vrf, van);
        u3_noun pro   = u3z_find_4(fun_m, vrf, sut, gol, p_gen);

        if ( u3_none != pro ) {
          return pro;
        }
        else {
          u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
          u3_noun gat = u3j_cook("_mint_in-blow", von, "blow");
          u3_noun pro;

          pro = u3n_kick_on(u3i_molt(gat,
                            u3x_sam_2,
                            u3k(gol),
                            u3x_sam_3,
                            u3k(p_gen),
                            0));

          return u3z_save_4(fun_m, vrf, sut, gol, p_gen, pro);
        }
      }

      case c3__zpcm: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        return u3nc(_mint_nice(van,
                               gol,
                               u3qfu_play(van, sut, p_gen)),
                    u3nc(1, u3k(q_gen)));
      }

      case c3__dbug: u3x_cell(u3t(gen), &p_gen, &q_gen);
      {
        u3t_push(u3nc(c3__mean, _mint_loc(van, p_gen)));
        {
          u3_noun hum = _mint_in(van, sut, gol, q_gen);
          u3_noun bez = u3nt(c3__spot, 1, u3k(p_gen));

          ret = u3nc(u3k(u3h(hum)), u3nt(11, bez, u3k(u3t(hum))));
          u3z(hum);
        }
        u3t_drop();
        return ret;
      }

      case c3__zpmc:
      case c3__zpsm: u3x_cell(u3t(gen), &p_gen, &q_gen);
      _mint_used();
      {
        u3_noun vos   = _mint_in(van, sut, c3__noun, q_gen);
        u3_noun zur   = u3qfu_play(van, sut, p_gen);
        u3_noun p_vos = u3h(vos);
        u3_noun q_vos = u3t(vos);
        u3_noun waz   = u3nc(1, _mint_burp(van, p_vos));
        //  u3_noun waz   = u3nc(1, u3k(p_vos));
        u3_noun sif   = u3k(zur);
        u3_noun cig   = u3qf_cell(sif, p_vos);
        u3_noun ret;

        ret = u3nc(_mint_nice(van, gol, cig),
                   u3qf_cons(waz, q_vos));

        u3z(waz);
        u3z(zur);
        u3z(sif);
        u3z(vos);

        return ret;
      }

      case c3__zpvt: u3x_trel(u3t(gen), &p_gen, &q_gen, &r_gen);
      _mint_used();
      {
        if ( c3y == _mint_feel(van, sut, p_gen) ) {
          return _mint_in(van, sut, gol, q_gen);
        } else {
          return _mint_in(van, sut, gol, r_gen);
        }
      }

      case c3__lost: p_gen = u3t(gen);
      _mint_used();
      {
        if ( c3y == _mint_vet(van) ) {
          u3_noun zur = u3qfu_play(van, sut, p_gen);
          u3_noun dun = u3qfu_dunq(van, "lost", zur);

          u3t_push(u3nc(c3__mean, dun));
          return u3m_error("mint-lost");
        }
        else {
          return u3nt(c3__void, 0, 0);
        }
      }

      case c3__fail:
      case c3__zpzp:
      _mint_used();
      {
        return u3nt(c3__void, 0, 0);
      }
    }
  }

  static u3_noun
  _cqfu_mint(u3_noun van,
             u3_noun sut,
             u3_noun gol,
             u3_noun gen)
  {
    return _mint_in(van, sut, gol, gen);
  }

/* boilerplate
*/
  u3_noun
  u3wfu_mint(u3_noun cor)
  {
    u3_noun sut, gol, gen, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &gol,
                               u3x_sam_3, &gen,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_mint(van, sut, gol, gen);
    }
  }

  u3_noun
  u3qfu_mint(u3_noun van,
             u3_noun sut,
             u3_noun gol,
             u3_noun gen)
  {
    c3_m    fun_m = 141 + c3__mint;
    u3_noun vrf   = u3r_at(u3qfu_van_vrf, van);
    u3_noun pro   = u3z_find_4(fun_m, vrf, sut, gol, gen);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_mint(van, sut, gol, gen);

      return u3z_save_4(fun_m, vrf, sut, gol, gen, pro);
    }
  }
