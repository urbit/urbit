/* j/6/mint.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_noun 
  _mint_in(u2_wire, u2_noun, u2_noun, u2_noun, u2_noun);

  static u2_noun
  _mint_bean(u2_wire wir_r)
  {
    return u2_bt(wir_r, c3__fork, 
                        u2_bq(wir_r, c3__cube, _0, c3__atom, 'f'),
                        u2_bq(wir_r, c3__cube, _1, c3__atom, 'f'));
  }

  static u2_bean
  _mint_vet(u2_wire wir_r, 
            u2_noun van)
  {
    // u2_bean vet = u2_bn_hook(wir_r, van, "vet");
    u2_bean vet = u2_frag(j2_ut_van_vet, van);

    switch ( vet ) {
      case u2_no: 
      case u2_yes: return vet;
      default: return u2_bl_bail(wir_r, c3__fail); 
    }
  }

  static u2_noun                                                  //  produce
  _mint_core(u2_wire wir_r, 
             u2_noun pac,                                         //  submit
             u2_noun con)                                         //  submit
  {
    if ( (c3__void == pac) ) {
      return c3__void;
    } else {
      return u2_bt(wir_r, c3__core, pac, con);
    }
  }

  static u2_noun                                                  //  produce
  _mint_foil(u2_wire wir_r,
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
 
  static u2_noun                                                //  produce
  _mint_cond(u2_wire wir_r,
             u2_noun pex,                                       //  submit
             u2_noun yom,                                       //  submit
             u2_noun woq)                                       //  submit
  {
    if ( _1 == u2_h(pex) ) {
      if ( _0 == u2_t(pex) ) {
        u2_rz(wir_r, pex); 
        u2_rz(wir_r, woq);

        return yom;
      } 
      else if ( _1 == u2_t(pex) ) {
        u2_rz(wir_r, pex); 
        u2_rz(wir_r, yom);

        return woq;
      }
    }
    return u2_bq(wir_r, _6, pex, yom, woq);
  }

  static u2_noun
  _mint_corn(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_noun gen)
  {
    u2_noun mil = _mint_in(wir_r, van, sut, c3__noun, gen);
    u2_noun fol = u2_rx(wir_r, u2_t(mil));

    u2_rl_lose(wir_r, mil);
    return fol;
  }

  static u2_noun                                                  //  produce
  _mint_nice(u2_wire wir_r,
             u2_noun van,                                         //  retain
             u2_noun gol,                                         //  retain
             u2_noun typ)                                         //  submit
  {
    if ( (u2_yes == _mint_vet(wir_r, van)) &&
         (u2_no == j2_mcy(Pt6, ut, nest)(wir_r, van, gol, u2_yes, typ)) ) 
    {
      // u2_noun dun = j2_mcy(Pt6, ut, dunq)(wir_r, van, "need", gol);
      // u2_noun niz = j2_mcy(Pt6, ut, dunq)(wir_r, van, "have", typ);

      // u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, dun));
      // u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, niz));

      return u2_bl_error(wir_r, "mint-nice");
    }
    else return typ;
  }

  static u2_noun                                                  //  produce
  _mint_coke(u2_wire wir_r,
             u2_noun nug)                                         //  submit
  {
    u2_atom axe;

    if ( 0 == u2_h(nug) ) {
      axe = u2_t(nug);
    } else if ( 10 == u2_h(nug) ) {
      u2_noun xin = u2_rx(wir_r, u2_t(u2_t(nug)));

      axe = _mint_coke(wir_r, xin);
    }
    else {
      return u2_bl_error(wir_r, "mint-coke");
    }
    u2_rz(wir_r, nug);
    return axe;
  }

  static u2_noun                                                  //  produce
  _mint_edit(u2_wire wir_r,
             u2_noun van,                                         //  retain
             u2_noun sut,                                         //  retain
             u2_noun mew,                                         //  retain
             u2_noun p_lar,                                       //  retain
             u2_noun rag,                                         //  submit
             u2_noun hej)                                         //  submit
  {
    while ( 1 ) {
      if ( u2_no == u2_dust(mew) ) {
        u2_noun gim = j2_mcy(Pt6, ut, fire)(wir_r, van, sut, rag);
        u2_noun fol = j2_mby(Pt6, hike)(wir_r, p_lar, hej);

        u2_rz(wir_r, rag);
        u2_rz(wir_r, hej);

        return u2_bc(wir_r, gim, fol);
      } else {
        u2_noun i_mew = u2_h(mew);
        u2_noun t_mew = u2_t(mew);
        u2_noun pi_mew = u2_h(i_mew);
        u2_noun qi_mew = u2_t(i_mew);
        u2_noun zil = j2_mcy(Pt6, ut, mint)(wir_r, van, sut, c3__noun, qi_mew);
        u2_noun p_zil = u2_h(zil);
        u2_noun q_zil = u2_t(zil);
        u2_noun wip = j2_mcy(Pt6, ut, tock)
          (wir_r, van, sut, pi_mew, p_zil, rag);
 
        u2_rz(wir_r, rag);
        rag = u2_rx(wir_r, u2_t(wip));

        hej = u2_bc(wir_r, u2_bc(wir_r, u2_rx(wir_r, u2_h(wip)),
                                        u2_rx(wir_r, q_zil)),
                           hej);

        u2_rz(wir_r, zil);
        u2_rz(wir_r, wip);

        mew = t_mew;
      }
    }
  }

  static u2_noun
  _mint_brew(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_bean tov,
             u2_noun gen)
  {
    u2_noun von;
    
    switch ( tov ) {
      default: return u2_bl_bail(wir_r, c3__fail);
      case u2_yes: 
        von = u2_rx(wir_r, van); break;
      case u2_no: 
        von = u2_bn_molt(wir_r, van, j2_ut_van_vet, u2_no, 0); break;
    }
    {
      u2_noun mil = j2_mcy(Pt6, ut, mint)(wir_r, von, sut, c3__noun, gen);
      u2_noun fol = u2_rx(wir_r, u2_t(mil));

      u2_rl_lose(wir_r, mil);
      u2_rl_lose(wir_r, von);
      return fol;
    }
  }

  static u2_noun                                                  //  produce
  _mint_bake(u2_wire wir_r, 
             u2_noun van,                                         //  retain
             u2_noun sut,                                         //  retain
             u2_noun dab)                                         //  retain
  {
    if ( (u2_nul == dab) ) {
      return _0;
    }
    else {
      u2_noun n_dab, l_dab, r_dab; 
     
      u2_as_trel(dab, &n_dab, &l_dab, &r_dab);
      if ( u2_no == u2_dust(n_dab) ) {
        return u2_bl_bail(wir_r, c3__fail);
      } 
      else {
        u2_noun qn_dab = u2_t(n_dab);
        u2_noun vad;

        switch ( u2_h(qn_dab) ) {
          default: return u2_bl_bail(wir_r, c3__exit);
          case c3__ash: {
            vad = _mint_brew(wir_r, van, sut, u2_yes, u2_t(qn_dab));
            break;
          }
          case c3__elm: {
            vad = _mint_brew(wir_r, van, sut, u2_no, u2_t(qn_dab));
            break;
          }
        }

        if ( (u2_nul == l_dab) && (u2_nul == r_dab) ) {
          return vad;
        }
        else if ( (u2_nul == l_dab) ) {
          return u2_bc
            (wir_r, vad, _mint_bake(wir_r, van, sut, r_dab));
        }
        else if ( (u2_nul == r_dab) ) {
          return u2_bc
            (wir_r, vad, _mint_bake(wir_r, van, sut, l_dab));
        }
        else {
          return u2_bt
            (wir_r, vad,
                    _mint_bake(wir_r, van, sut, l_dab),
                    _mint_bake(wir_r, van, sut, r_dab));
        }
      }
    }
  }

  static u2_noun 
  _mint_grow(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_noun gol,
             u2_atom mel,
             u2_noun ruf,
             u2_noun dab)
  {
    u2_noun dan = _mint_in(wir_r, van, sut, c3__noun, ruf);
    u2_noun p_dan = u2_h(dan);
    u2_noun q_dan = u2_t(dan);
    u2_noun toc = _mint_core
      (wir_r, u2_rx(wir_r, p_dan),
              u2_bt(wir_r, c3__gold, 
                           u2_rx(wir_r, p_dan), 
                           u2_bc(wir_r, u2_nul, u2_rx(wir_r, dab))));
    u2_noun dez = _mint_bake(wir_r, van, toc, dab);
    u2_noun zod = u2_bc(wir_r, 1, dez);
    u2_noun cot = _mint_core
      (wir_r, u2_rx(wir_r, p_dan),
              u2_bt(wir_r, mel, 
                           u2_rx(wir_r, p_dan), 
                           u2_bc(wir_r, u2_rx(wir_r, dez), 
                                        u2_rx(wir_r, dab))));
    u2_noun ret = u2_bc
      (wir_r, 
       _mint_nice(wir_r, van, gol, cot),
       j2_mby(Pt6, cons)(wir_r, zod, q_dan));

    u2_rz(wir_r, zod);
    u2_rz(wir_r, toc);
    u2_rz(wir_r, dan);

    return ret;
  }

  static u2_noun                                                  //  produce
  _mint_loc(u2_wire wir_r,
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

# define _mint_used(wir_r)

  static u2_noun                                                  //  produce
  _mint_in(u2_wire wir_r, 
           u2_noun van,                                           //  retain
           u2_noun sut,                                           //  retain
           u2_noun gol,                                           //  retain
           u2_noun gen)                                           //  retain
  {
    u2_noun p_gen, q_gen, r_gen;
    u2_noun ret;

    if ( (c3__void == sut) && 
         !((u2_yes == u2_dust(gen)) && (c3__zpcb == u2_h(gen))) )
    {
      if ( (u2_no == _mint_vet(wir_r, van))
           || ((u2_yes == u2_dust(gen)) && 
               ((c3__zpfs == u2_h(gen)) || (c3__zpzp == u2_h(gen)))) )
      {
        return u2_bt(wir_r, c3__void, _0, _0);
      }
      else {
        return u2_bl_error(wir_r, "mint-vain");
      }
    }

    if ( u2_no == u2_dust(gen) ) {
      u2_noun ter = u2_frag(u2_cv_con_3, van);
      u2_noun rex = j2_mcy(Pt6, ap, open)(wir_r, ter, gen);

      ret = _mint_in(wir_r, van, sut, gol, rex);
      u2_rl_lose(wir_r, rex);

      return ret;
    } 
    else if ( u2_yes == u2_dust(u2_h(gen)) ) {
      _mint_used(wir_r);
      {
        u2_noun hed = _mint_in(wir_r, van, sut, c3__noun, u2_h(gen));
        u2_noun tal = _mint_in(wir_r, van, sut, c3__noun, u2_t(gen));
        u2_noun typ = j2_mby(Pt6, cell)(wir_r, u2_h(hed), u2_h(tal));

        ret = u2_bc
          (wir_r,
           _mint_nice(wir_r, van, gol, typ),
           j2_mby(Pt6, cons)(wir_r, u2_t(hed), u2_t(tal)));

        u2_rl_lose(wir_r, hed);
        u2_rl_lose(wir_r, tal);

        return ret;
      }
    } else switch ( u2_h(gen) ) {
      default: {
        u2_noun ter = u2_frag(u2_cv_con_3, van);
        u2_noun rex = j2_mcy(Pt6, ap, open)(wir_r, ter, gen);

        if ( u2_yes == u2_sing(rex, gen) ) {
#if 1
          u2_noun zix = j2_mcy(Pt6, ut, shep)
                (wir_r, van, "gene", 'q', u2_rx(wir_r, gen));

          u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, zix));
          return u2_bl_error(wir_r, "mint-open");
#else
          u2_err(wir_r, "h", u2_h(gen));
          return u2_bl_bail(wir_r, c3__fail);
#endif
        }
        ret = _mint_in(wir_r, van, sut, gol, rex);
        u2_rl_lose(wir_r, rex);

        return ret;
      }
      
      case c3__bcpt: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun sep = j2_mcy(Pt6, ut, seep)(wir_r, van, sut, c3__read, p_gen);
        u2_noun axe = u2h(sep);
        u2_noun rex = j2_mcy(Pt6, al, whip)(wir_r, van, q_gen, axe);
        u2_noun ret = _mint_in(wir_r, van, sut, gol, rex);

        u2z(sep);
        u2z(rex);

        return ret;
      }

      case c3__wtts: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun nob = j2_mcy(Pt6, al, bunt)(wir_r, van, p_gen);
        u2_noun dok = u2nc(c3__cnhx, q_gen);
        u2_noun vol = _mint_corn(wir_r, van, sut, dok);
        u2_noun axe = _mint_coke(wir_r, vol);
        u2_noun wam = j2_mcy(Pt6, ut, play)(wir_r, van, sut, nob);

        ret = u2_bc
          (wir_r,
           _mint_nice(wir_r, van, gol, _mint_bean(wir_r)),
           j2_mcy(Pt6, ut, fish)(wir_r, van, wam, axe));

        u2_rl_lose(wir_r, axe);
        u2_rl_lose(wir_r, wam);
        u2_rl_lose(wir_r, nob);
        u2_rl_lose(wir_r, dok);

        return ret;
      }

      case c3__wtcl: u2_bi_trel(wir_r, u2_t(gen), &p_gen, &q_gen, &r_gen);
      _mint_used(wir_r);
      {
        u2_noun bol = _mint_bean(wir_r);
        u2_noun nor = _mint_in(wir_r, van, sut, bol, p_gen);
        u2_noun fex = j2_mcy(Pt6, ut, gain)(wir_r, van, sut, p_gen);
        u2_noun wux = j2_mcy(Pt6, ut, lose)(wir_r, van, sut, p_gen);
        u2_noun duy = (c3__void == fex)
                        ? ( (c3__void == wux)
                             ?  u2_bc(wir_r, _0, _0)
                             :  u2_bc(wir_r, _1, _1) )
                        : ( (c3__void == wux)
                            ?  u2_bc(wir_r, _1, _0)
                            :  u2_rx(wir_r, u2_t(nor)) );
        u2_noun hiq = _mint_in(wir_r, van, fex, gol, q_gen);
        u2_noun ran = _mint_in(wir_r, van, wux, gol, r_gen);

        ret = u2_bc
          (wir_r, j2_mby(Pt6, fork)(wir_r, u2_h(hiq), u2_h(ran)),
                  _mint_cond(wir_r, duy, 
                                    u2_rx(wir_r, u2_t(hiq)),
                                    u2_rx(wir_r, u2_t(ran))));

        u2_rl_lose(wir_r, ran);
        u2_rl_lose(wir_r, hiq);
        u2_rl_lose(wir_r, nor);
        u2_rl_lose(wir_r, wux);
        u2_rl_lose(wir_r, fex);
        u2_rl_lose(wir_r, bol);

        return ret;
      }
      case c3__clhp: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun hed = _mint_in(wir_r, van, sut, c3__noun, p_gen);
        u2_noun tal = _mint_in(wir_r, van, sut, c3__noun, q_gen);
        u2_noun typ = j2_mby(Pt6, cell)(wir_r, u2_h(hed), u2_h(tal));

        ret = u2_bc
          (wir_r,
           _mint_nice(wir_r, van, gol, typ),
           j2_mby(Pt6, cons)(wir_r, u2_t(hed), u2_t(tal)));

        u2_rl_lose(wir_r, hed);
        u2_rl_lose(wir_r, tal);

        return ret;
      }
      case c3__dtts: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun typ = _mint_nice(wir_r, van, gol, _mint_bean(wir_r));

        return u2_bc
          (wir_r, 
           typ,
           u2_bt(wir_r, _5, _mint_corn(wir_r, van, sut, p_gen),
                            _mint_corn(wir_r, van, sut, q_gen)));
      }
      case c3__dtwt: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_noun typ = _mint_nice(wir_r, van, gol, _mint_bean(wir_r));

        return u2_bc
          (wir_r, 
           typ,
           u2_bc(wir_r, _3, _mint_corn(wir_r, van, sut, p_gen)));
      }
      case c3__dtkt: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_noun tom = c3__noun;
        u2_noun sam = _mint_in(wir_r, van, sut, tom, p_gen);

        ret = u2_bc
          (wir_r, 
           _mint_nice(wir_r, van, gol, tom),
           u2_bc(wir_r, _11, u2_rx(wir_r, u2_t(sam))));
        
        u2_rz(wir_r, sam);
        return ret;
      }
      case c3__dtls: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_noun tom = u2_bc(wir_r, c3__atom, u2_blip);
        u2_noun sam = _mint_in(wir_r, van, sut, tom, p_gen);

        ret = u2_bc
          (wir_r, 
           _mint_nice(wir_r, van, gol, tom),
           u2_bc(wir_r, _4, u2_rx(wir_r, u2_t(sam))));
        
        u2_rz(wir_r, sam);
        return ret;
      }
      case c3__dtsg: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun boc = (u2_no == u2_stud(q_gen)) 
                        ? c3__noun
                        : u2_bc(wir_r, c3__atom, u2_rx(wir_r, p_gen));
        u2_noun typ = j2_mby(Pt6, cube)(wir_r, q_gen, boc);
        u2_noun ret = 
            u2_bc(wir_r,
                  _mint_nice(wir_r, van, gol, typ),
                  u2_bc(wir_r, _1, u2_rx(wir_r, q_gen)));

        u2_rz(wir_r, boc);
        return ret;
      }
      case c3__dttr: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        return u2_bc
          (wir_r, 
           _mint_nice(wir_r, van, gol, c3__noun),
           u2_bt(wir_r, _2, _mint_corn(wir_r, van, sut, p_gen),
                            _mint_corn(wir_r, van, sut, q_gen)));
      }
      case c3__dtpt: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun typ = j2_mcy(Pt6, ut, play)(wir_r, van, sut, gen);
        u2_noun ret = 
            u2_bc(wir_r,
                  _mint_nice(wir_r, van, gol, typ),
                  u2_bc(wir_r, _1, u2_rx(wir_r, q_gen)));

        return ret;
      }
      case c3__ktbr: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_noun ryd = _mint_in(wir_r, van, sut, gol, p_gen);
        u2_noun tyf = j2_mcy(Pt6, ut, wrap)(wir_r, van, u2_h(ryd), c3__iron);
        u2_noun pro = u2_bc(wir_r, tyf, u2_rx(wir_r, u2_t(ryd)));

        u2_rz(wir_r, ryd);
        return pro;
      }
      case c3__ktpm: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_noun ryd = _mint_in(wir_r, van, sut, gol, p_gen);
        u2_noun tyf = j2_mcy(Pt6, ut, wrap)(wir_r, van, u2_h(ryd), c3__zinc);
        u2_noun pro = u2_bc(wir_r, tyf, u2_rx(wir_r, u2_t(ryd)));

        u2_rz(wir_r, ryd);
        return pro;
      }
      case c3__ktwt: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_noun ryd = _mint_in(wir_r, van, sut, gol, p_gen);
        u2_noun tyf = j2_mcy(Pt6, ut, wrap)(wir_r, van, u2_h(ryd), c3__lead);
        u2_noun pro = u2_bc(wir_r, tyf, u2_rx(wir_r, u2_t(ryd)));

        u2_rz(wir_r, ryd);
        return pro;
      }
      case c3__ktts: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun vat = _mint_in(wir_r, van, sut, gol, q_gen);
     
        ret = u2_bc
          (wir_r,
           j2_mcy(Pt6, ut, conk)(wir_r, van, u2h(vat), p_gen),
           u2_rx(wir_r, u2_t(vat)));

        u2_rl_lose(wir_r, vat);
        return ret;
      }
      case c3__ktzp: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun vat = _mint_in(wir_r, van, sut, gol, q_gen);
     
        ret = u2_bc
          (wir_r,
           j2_mcy(Pt6, ut, conk)(wir_r, van, u2h(vat), p_gen),
           u2_rx(wir_r, u2_t(vat)));

        u2_rl_lose(wir_r, vat);
        return ret;
      }
      case c3__ktsg: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_noun nef = _mint_in(wir_r, van, sut, gol, p_gen);
        u2_noun p_nef = u2_h(nef);
        u2_noun q_nef = u2_t(nef);
        u2_noun fom;

        {
          u2_noun cag = j2_mcy(Pt6, ut, burn)(wir_r, van, sut);
          u2_noun wim = u2_cn_moch(cag, u2k(q_nef));

          if ( 0 == u2h(wim) ) {
            fom = u2nc(1, u2k(u2t(wim)));
          } else {
            fom = u2k(q_nef);
          }
          u2z(wim);
        }
        ret = u2_bc(wir_r, u2_rx(wir_r, p_nef), fom);
 
        u2_rz(wir_r, nef);
        return ret;
      }
      case c3__ktls: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun huz = j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen);
        u2_noun hif = _mint_nice(wir_r, van, gol, huz);
        u2_noun zel = _mint_in(wir_r, van, sut, hif, q_gen);
        u2_noun ret = u2_bc(wir_r, hif, u2_rx(wir_r, u2_t(zel)));

        u2_rz(wir_r, zel);
        return ret;
      }
      case c3__tsgr: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun fid = _mint_in(wir_r, van, sut, c3__noun, p_gen);
        u2_noun p_fid = u2_h(fid);
        u2_noun q_fid = u2_t(fid);
        u2_noun dov = _mint_in(wir_r, van, p_fid, gol, q_gen);
        u2_noun p_dov = u2_h(dov);
        u2_noun q_dov = u2_t(dov);

        ret = u2_bc
          (wir_r, u2_rx(wir_r, p_dov), 
                  j2_mbc(Pt6, comb)(wir_r, q_fid, q_dov));

        u2_rl_lose(wir_r, fid); 
        u2_rl_lose(wir_r, dov);
        return ret;
      }
      case c3__tstr: u2_bi_trel(wir_r, u2_t(gen), &p_gen, &q_gen, &r_gen);
      _mint_used(wir_r);
      {
        u2_noun sep = j2_mcy(Pt6, ut, seep)(wir_r, van, sut, c3__both, q_gen);
        u2_noun bid = u2_bt(wir_r, u2k(p_gen), u2k(q_gen), sep);
        u2_noun boc = j2_mby(Pt6, bull)(wir_r, bid, sut);
        u2_noun ret = _mint_in(wir_r, van, boc, gol, r_gen);

        u2z(bid);
        u2z(boc);

        return ret;
      }
      case c3__cnts: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun sec = j2_mcy(Pt6, ut, seek)(wir_r, van, sut, c3__read, p_gen);
        u2_noun lar = _mint_foil(wir_r, sec);
        u2_noun p_lar = u2_h(lar);
        u2_noun q_lar = u2_t(lar);
        u2_noun pq_lar = u2_h(q_lar);
        u2_noun qq_lar = u2_t(q_lar);
        u2_noun mew = j2_mcy(Pt6, ut, snub)(wir_r, van, sut, q_gen);
        u2_noun yom = _mint_edit
          (wir_r, van, sut, mew, p_lar, u2_rx(wir_r, qq_lar), u2_nul);
        u2_noun p_yom = u2_h(yom);
        u2_noun q_yom = u2_t(yom);
        u2_noun ret = u2_bc
          (wir_r, _mint_nice(wir_r, van, gol, u2_rx(wir_r, p_yom)),
                  (_0 == pq_lar) ? u2_rx(wir_r, q_yom)
                                 : u2_bt(wir_r, _9,
                                                u2_rx(wir_r, pq_lar),
                                                u2_rx(wir_r, q_yom)));
                  
        u2_rz(wir_r, yom);
        u2_rz(wir_r, mew);
        u2_rz(wir_r, lar);

        return ret;
      }
      case c3__pmcl: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun ruf = u2_bt
          (wir_r, c3__clhp, 
                  u2_bc(wir_r, u2_nul, _1),
                  u2_rx(wir_r, p_gen));
        u2_noun ret = _mint_grow(wir_r, van, sut, gol, c3__zinc, ruf, q_gen);

        u2_rz(wir_r, ruf);
        return ret;
      }
      case c3__brcn: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_noun ruf = u2_bc(wir_r, u2_nul, _1);

        ret = _mint_grow(wir_r, van, sut, gol, c3__gold, ruf, p_gen);
        u2_rz(wir_r, ruf);

        return ret;
      }
      case c3__pmcn: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_noun ruf = u2_bc(wir_r, u2_nul, _1);

        ret = _mint_grow(wir_r, van, sut, gol, c3__lead, ruf, p_gen);
        u2_rz(wir_r, ruf);

        return ret;
      }
      case c3__pmls: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun ruf = u2_bt
          (wir_r, c3__clhp, 
                  u2_bc(wir_r, u2_nul, _1),
                  u2_rx(wir_r, p_gen));
        u2_noun ret = _mint_grow(wir_r, van, sut, gol, c3__iron, ruf, q_gen);

        u2_rz(wir_r, ruf);
        return ret;
      }
      case c3__sgcb: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun typ = j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen);
        u2_noun dug = j2_mcy(Pt6, ut, duck)(wir_r, van, typ);

        u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, dug));
        {
          ret = _mint_in(wir_r, van, sut, gol, q_gen);
        } 
        u2_bl_drop(wir_r);

        u2_rz(wir_r, typ);
        return ret;
      }
      case c3__sggr: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun hum = _mint_in(wir_r, van, sut, gol, q_gen);
        u2_noun bez;
      
          if ( u2_yes == u2_stud(p_gen) ) {
            bez = u2_rx(wir_r, p_gen);
          } else {
            bez = u2_bc(wir_r, u2_rx(wir_r, u2_h(p_gen)),
                               _mint_corn(wir_r, van, sut, u2_t(p_gen)));
          }
          ret = u2_bc(wir_r, 
                      u2_rx(wir_r, u2_h(hum)),
                      u2_bt(wir_r, _10, bez, u2_rx(wir_r, u2_t(hum))));
        
        u2_rl_lose(wir_r, hum);
        return ret;
      }
      case c3__zpts: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_noun von = u2_bn_molt(wir_r, van, j2_ut_van_vet, u2_no, 0);
        u2_noun sev = _mint_corn(wir_r, von, sut, p_gen);

        u2_rz(wir_r, von);
        return u2_bc(wir_r, c3__noun, u2_bc(wir_r, _1, sev));
      }
      case c3__zpcm: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        return u2_bc
          (wir_r, 
           _mint_nice(wir_r, van, 
                             gol, 
                             j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen)),
           u2_bc(wir_r, 1, u2_rx(wir_r, q_gen)));
      }
      case c3__zpcb: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      {
        u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, _mint_loc(wir_r, van, p_gen)));
        {
          u2_noun hum = _mint_in(wir_r, van, sut, gol, q_gen);
          u2_noun bez = u2_bt(wir_r, c3__spot, _1, u2_rx(wir_r, p_gen));

          ret = u2_bc(wir_r, 
                      u2_rx(wir_r, u2_h(hum)),
                      u2_bt(wir_r, _10, bez, u2_rx(wir_r, u2_t(hum))));

          u2_rz(wir_r, hum);
        }
        u2_bl_drop(wir_r);
        return ret;
      }
      case c3__zpcn:
      _mint_used(wir_r);
      {
        u2_noun pet   = u2_bn_hook(wir_r, van, "seed");
        u2_noun p_pet = u2_h(pet);
        u2_noun q_pet = u2_t(pet);
        u2_noun ret;

        ret = u2_bc(wir_r, _mint_nice(wir_r, van, gol, u2_rx(wir_r, p_pet)),
                           u2_bc(wir_r, _1, u2_rx(wir_r, q_pet)));

        u2_rz(wir_r, pet);
        return ret;
      }
      case c3__zpsm: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun vos   = _mint_in(wir_r, van, sut, c3__noun, q_gen);
        u2_noun zur   = j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen);
        u2_noun p_vos = u2_h(vos);
        u2_noun q_vos = u2_t(vos);
        u2_noun waz   = u2_bc(wir_r, _1, u2_rx(wir_r, p_vos));
        u2_noun cig   = j2_mby(Pt6, cell)(wir_r, zur, p_vos);
        u2_noun ret;

        ret = u2_bc(wir_r,
                    _mint_nice(wir_r, van, gol, cig),
                    j2_mby(Pt6, cons)(wir_r, waz, q_vos));

        u2_rz(wir_r, waz);
        u2_rz(wir_r, zur);
        u2_rz(wir_r, vos);

        return ret;
      }
      case c3__zpfs: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        if ( u2_yes == _mint_vet(wir_r, van) ) {
          u2_noun zur = j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen);
          u2_noun dun = j2_mcy(Pt6, ut, dunq)(wir_r, van, "lost", zur);

          u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, dun));
          return u2_bl_error(wir_r, "mint-lost");
        }
        else {
          return u2_bt(wir_r, c3__void, _0, _0);
        }
      }
      case c3__zpzp:
      _mint_used(wir_r);
      {
        return u2_bt(wir_r, c3__void, _0, _0);
      }
    }
  }

  u2_noun                                                         //  produce
  j2_mcx(Pt6, ut, mint)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gol,
                        u2_noun gen)                              //  retain
  {
    return _mint_in(wir_r, van, sut, gol, gen);
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, mint)[];

  u2_noun                                                         //  produce
  j2_mc(Pt6, ut, mint)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, gol, gen, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &gol, 
                                u2_cv_sam_3, &gen,
                                u2_cv_con, &van, 
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, mint)(wir_r, van, sut, gol, gen);
    }
  }

  u2_weak                                                         //  produce
  j2_mci(Pt6, ut, mint)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun gol,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "mint");

    if ( u2_none == hoc ) {
      c3_assert(!"register mint");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, 
                                      u2_cv_sam_2, u2_rx(wir_r, gol), 
                                      u2_cv_sam_3, u2_rx(wir_r, gen), 
                                      0);

      if ( (u2_none == j2_mcj(Pt6, ut, mint)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, mint)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  produce
  j2_mcy(Pt6, ut, mint)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gol,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, mint)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, mint)(wir_r, van, sut, gol, gen);
      }
      else {
        c3_m    fun_m = c3__mint;
        u2_noun vrf   = u2_frag(j2_ut_van_vrf, van);
        u2_noun pro   = u2_rl_find_qual(wir_r, fun_m, vrf, sut, gol, gen);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, mint)(wir_r, van, sut, gol, gen);

          return u2_rl_save_qual(wir_r, fun_m, vrf, sut, gol, gen, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, mint)(wir_r, van, sut, gol, gen);
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
  u2_weak
  j2_mck(Pt6, ut, mint)(u2_wire wir_r,
                        u2_noun cor)
  {
    u2_noun sut, gol, gen, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &gol, 
                                u2_cv_sam_3, &gen,
                                u2_cv_con, &van, 
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      u2_noun vrf = u2_frag(j2_ut_van_vrf, van);

      return u2_rq(wir_r, u2_rx(wir_r, vrf),
                          u2_rx(wir_r, sut),
                          u2_rx(wir_r, gol),
                          u2_rx(wir_r, gen));
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
