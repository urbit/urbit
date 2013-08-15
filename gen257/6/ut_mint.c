/* j/6/mint.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun 
  j2_mcx(Pt6, ut, mint)(u2_wire, u2_noun, u2_noun, u2_noun, u2_noun);

  static u2_noun
  _mint_flag(u2_wire wir_r)
  {
    return u2_bt(wir_r, c3__fork, u2_bc(wir_r, c3__cube, _0),
                                  u2_bc(wir_r, c3__cube, _1));
  }

  static u2_flag
  _mint_vet(u2_wire wir_r, 
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
  static u2_flag
  _mint_fab(u2_wire wir_r,
            u2_noun van)
  {
    // u2_flag fab = u2_bn_hook(wir_r, van, "fab");
    u2_flag fab = u2_frag(j2_ut_van_fab, van);

    switch ( fab ) {
      case u2_no: 
      case u2_yes: return fab;
      default: return u2_bl_bail(wir_r, c3__fail); 
    }
  }

  static u2_noun
  _mint_make(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_noun gol,
             u2_noun gen)
  {
    u2_noun mil = j2_mcy(Pt6, ut, mint)(wir_r, van, sut, gol, gen);
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
         (u2_no == j2_mcy(Pt6, ut, nest)(wir_r, van, gol, typ)) ) 
    {
      u2_noun dun = j2_mcy(Pt6, ut, dunq)(wir_r, van, "need", gol);
      u2_noun niz = j2_mcy(Pt6, ut, dunq)(wir_r, van, "have", typ);

      u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, dun));
      u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, niz));

      return u2_bl_error(wir_r, "mint-nice");
    }
    else return typ;
  }

  static u2_noun                                                  //  produce
  _mint_coke(u2_wire wir_r,
             u2_noun nug)                                         //  retain
  {
    if ( 0 == u2_h(nug) ) {
      return u2_rx(wir_r, u2_t(nug));
    } else if ( 10 == u2_h(nug) ) {
      return _mint_coke(wir_r, u2_t(u2_t(nug)));
    }
    else {
      u2_err(wir_r, "nug", nug);
      return u2_bl_error(wir_r, "mint-coke");
    }
  }

  static u2_noun                                                  //  produce
  _mint_core(u2_wire wir_r,
             u2_noun pac,                                         //  submit
             u2_noun con)                                         //  submit
  {
    if ( c3__void == pac ) {
      u2_rz(wir_r, con);
      return c3__void;
    } else {
      return u2_bt(wir_r, c3__core, pac, con);
    }
  }

  static u2_noun 
  _mint_crow(u2_wire wir_r,
             u2_noun van,
             u2_noun sut,
             u2_noun gol,
             u2_atom mel,
             u2_noun ruf,
             u2_noun dab)
  {
    u2_noun fab = _mint_fab(wir_r, van);
    u2_noun dan = j2_mcx(Pt6, ut, mint)(wir_r, van, sut, c3__noun, ruf);
    u2_noun p_dan = u2_h(dan);
    u2_noun q_dan = u2_t(dan);
    
    if ( u2_no == fab ) {
      u2_noun tuf = u2_bc(wir_r, _0, _0);
      u2_noun cot = _mint_core
        (wir_r, u2_rx(wir_r, p_dan),
                u2_bt(wir_r, mel, 
                             u2_rx(wir_r, p_dan), 
                             u2_bc(wir_r, u2_rx(wir_r, tuf), 
                                          u2_rx(wir_r, dab))));
      u2_noun ret = u2_bc
        (wir_r, 
         _mint_nice(wir_r, van, gol, cot),
         tuf);

      u2_rz(wir_r, dan);
      return ret;
    } 
    else {
      u2_noun toc = _mint_core
        (wir_r, u2_rx(wir_r, p_dan),
                u2_bt(wir_r, c3__gold, 
                             u2_rx(wir_r, p_dan), 
                             u2_bc(wir_r, u2_nul, u2_rx(wir_r, dab))));
      u2_noun dez = j2_mcy(Pt6, ut, bake)(wir_r, van, toc, dab);
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
         j2_mby(Pt6, cons)(wir_r, q_dan, zod));

      u2_rz(wir_r, toc);
      u2_rz(wir_r, dan);
      u2_rz(wir_r, zod);
      return ret;
    }
  }

  static u2_noun                                                  //  produce
  _mint_loc(u2_wire wir_r,
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

# define _mint_used(wir_r)

  u2_noun                                                         //  produce
  j2_mcx(Pt6, ut, mint)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun gol,
                        u2_noun gen)                              //  retain
  {
    u2_noun p_gen, q_gen, r_gen;
    u2_noun ret;

    if ( u2_no == u2_dust(gen) ) {
      u2_noun rex = j2_mcy(Pt6, ap, open)(wir_r, gen);

      ret = j2_mcx(Pt6, ut, mint)(wir_r, van, sut, gol, rex);
      u2_rl_lose(wir_r, rex);

      return ret;
    } 
    else switch ( u2_h(gen) ) {
      default: {
        u2_noun rex = j2_mcy(Pt6, ap, open)(wir_r, gen);

        if ( u2_yes == u2_sing(rex, gen) ) {
#if 0
          u2_noun zix = j2_mcy(Pt6, ut, shep)
                (wir_r, van, "gene", 'q', u2_rx(wir_r, gen));

          u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, zix));
          return u2_bl_error(wir_r, "mint-open");
#else
          return u2_bl_bail(wir_r, c3__fail);
#endif
        }
        ret = j2_mcx(Pt6, ut, mint)(wir_r, van, sut, gol, rex);
        u2_rl_lose(wir_r, rex);

        return ret;
      }
      
      case c3__bnld: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_flag fab = _mint_fab(wir_r, van);
        u2_noun fid = j2_mcx(Pt6, ut, mint)(wir_r, van, sut, c3__noun, p_gen);
        u2_noun p_fid = u2_h(fid);
        u2_noun q_fid = u2_t(fid);
        u2_noun dov = j2_mcx(Pt6, ut, mint)(wir_r, van, p_fid, gol, q_gen);
        u2_noun p_dov = u2_h(dov);
        u2_noun q_dov = u2_t(dov);

        ret = u2_bc
          (wir_r, u2_rx(wir_r, p_dov),
                  (u2_no == fab) 
                    ? u2_bc(wir_r, _0, _0)
                    : j2_mbc(Pt6, comb)(wir_r, q_fid, q_dov));

        u2_rl_lose(wir_r, fid); 
        u2_rl_lose(wir_r, dov);
        return ret;
      }
      case c3__csbn: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_flag fab = _mint_fab(wir_r, van);
        u2_noun vol = _mint_make(wir_r, van, sut, c3__noun, q_gen);
        u2_noun wam = j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen);
        u2_noun axe = _mint_coke(wir_r, vol);

        ret = u2_bc
          (wir_r,
           _mint_nice(wir_r, van, gol, _mint_flag(wir_r)),
           (u2_no == fab)
             ? u2_bc(wir_r, _0, _0)
             : j2_mcy(Pt6, ut, fish)(wir_r, van, wam, axe));

        u2_rl_lose(wir_r, vol);
        u2_rl_lose(wir_r, wam);

        return ret;
      }
      case c3__csdg: u2_bi_trel(wir_r, u2_t(gen), &p_gen, &q_gen, &r_gen);
      _mint_used(wir_r);
      {
        u2_flag fab = _mint_fab(wir_r, van);
        u2_noun bol = _mint_flag(wir_r);
        u2_noun nor = j2_mcx(Pt6, ut, mint)(wir_r, van, sut, bol, p_gen);
        u2_noun fex = j2_mcy(Pt6, ut, gain)(wir_r, van, sut, p_gen);
        u2_noun rog = (c3__void == fex) ? sut : fex;
        u2_noun hiq = j2_mcx(Pt6, ut, mint)(wir_r, van, rog, gol, q_gen);
        u2_noun ran = j2_mcx(Pt6, ut, mint)(wir_r, van, sut, gol, r_gen);

        ret = u2_bc
          (wir_r, j2_mby(Pt6, fork)(wir_r, u2_h(hiq), u2_h(ran)),
                  (u2_no == fab)
                    ? u2_bc(wir_r, _0, _0)
                    : u2_bq(wir_r, 
                            _6,
                            u2_rx(wir_r, u2_t(nor)),
                            u2_rx(wir_r, u2_t(hiq)),
                            u2_rx(wir_r, u2_t(ran))));

        u2_rl_lose(wir_r, bol);
        u2_rl_lose(wir_r, nor);
        u2_rl_lose(wir_r, fex);
        u2_rl_lose(wir_r, hiq);
        u2_rl_lose(wir_r, ran);

        return ret;
      }
      case c3__cssg: u2_bi_trel(wir_r, u2_t(gen), &p_gen, &q_gen, &r_gen);
      _mint_used(wir_r);
      {
        u2_flag fab = _mint_fab(wir_r, van);
        u2_noun hec = j2_mcy(Pt6, ap, rake)(wir_r, p_gen);
        u2_noun guz = j2_mcy(Pt6, ut, seek)(wir_r, van, sut, c3__read, hec);
        u2_noun axe = u2_h(guz);
        u2_noun vim = u2_bt(wir_r, c3__cell, c3__noun, c3__noun);
        u2_noun bor = j2_mcy(Pt6, ut, cull)(wir_r, van, sut, axe, c3__atom);
        u2_noun wul = j2_mcy(Pt6, ut, cull)(wir_r, van, sut, axe, vim);
        u2_noun rog = (c3__void == bor) ? sut : bor;
        u2_noun dix = (c3__void == wul) ? sut : wul;
        u2_noun hiq = j2_mcx(Pt6, ut, mint)(wir_r, van, rog, gol, q_gen);
        u2_noun ran = j2_mcx(Pt6, ut, mint)(wir_r, van, dix, gol, r_gen);

        ret = u2_bc
          (wir_r, j2_mby(Pt6, fork)(wir_r, u2_h(hiq), u2_h(ran)),
                  (u2_no == fab)
                    ? u2_bc(wir_r, _0, _0)
                    : u2_bq(wir_r, 
                            _6,
                            u2_bt(wir_r, _3, _0, u2_rx(wir_r, axe)),
                            u2_rx(wir_r, u2_t(ran)),
                            u2_rx(wir_r, u2_t(hiq))));

        u2_rl_lose(wir_r, hec);
        u2_rl_lose(wir_r, guz);
        u2_rl_lose(wir_r, vim);
        u2_rl_lose(wir_r, bor);
        u2_rl_lose(wir_r, wul);
        u2_rl_lose(wir_r, hiq);
        u2_rl_lose(wir_r, ran);

        return ret;
      }
      case c3__dgdp: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_flag fab = _mint_fab(wir_r, van);
        u2_noun hed = j2_mcx(Pt6, ut, mint)(wir_r, van, sut, c3__noun, p_gen);
        u2_noun tal = j2_mcx(Pt6, ut, mint)(wir_r, van, sut, c3__noun, q_gen);
        u2_noun typ = j2_mby(Pt6, cell)(wir_r, u2_h(hed), u2_h(tal));

        ret = u2_bc
          (wir_r,
           _mint_nice(wir_r, van, gol, typ),
           (u2_no == fab)
             ? u2_bc(wir_r, _0, _0)
             : j2_mby(Pt6, cons)(wir_r, u2_t(hed), u2_t(tal)));

        u2_rl_lose(wir_r, hed);
        u2_rl_lose(wir_r, tal);

        return ret;
      }
      case c3__dtbn: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_flag fab = _mint_fab(wir_r, van);
        u2_noun typ = _mint_nice(wir_r, van, gol, _mint_flag(wir_r));

        if ( u2_no == fab ) {
          return u2_bc
            (wir_r, typ, u2_bc(wir_r, _0, _0));
        } else {
          return u2_bc
            (wir_r, 
             typ,
             u2_bt(wir_r, _5, _mint_make(wir_r, van, sut, c3__noun, p_gen),
                              _mint_make(wir_r, van, sut, c3__noun, q_gen)));
        }
      }
      case c3__dtcs: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_flag fab = _mint_fab(wir_r, van);
        u2_noun typ = _mint_nice(wir_r, van, gol, _mint_flag(wir_r));

        if ( u2_no == fab ) {
          return u2_bc
            (wir_r, typ, u2_bc(wir_r, _0, _0));
        } else {
          return u2_bc
            (wir_r, 
             typ,
             u2_bc(wir_r, _3, _mint_make(wir_r, van, sut, c3__noun, p_gen)));
        }
      }
      case c3__dtpd: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_flag fab = _mint_fab(wir_r, van);
        u2_noun sam = j2_mcx(Pt6, ut, mint)(wir_r, van, sut, c3__atom, p_gen);

        if ( u2_no == fab ) {
          ret = u2_bc
            (wir_r, c3__atom, u2_bc(wir_r, _0, _0));
        } else {
          ret = u2_bc
            (wir_r, 
             c3__atom,
             u2_bc(wir_r, _4, u2_rx(wir_r, u2_t(sam))));
        }
        u2_rz(wir_r, sam);
        return ret;
      }
      case c3__dtsg: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_noun typ = u2_bc(wir_r, c3__cube, u2_rx(wir_r, p_gen));

        return u2_bc
          (wir_r,
           _mint_nice(wir_r, van, gol, typ),
           u2_bc(wir_r, _1, u2_rx(wir_r, p_gen)));
      }
      case c3__dttr: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_flag fab = _mint_fab(wir_r, van);
        u2_noun typ = _mint_nice(wir_r, van, gol, c3__noun);

        if ( u2_no == fab ) {
          return u2_bc
            (wir_r, typ, u2_bc(wir_r, _0, _0));
        } else {
          return u2_bc
            (wir_r, 
             typ,
             u2_bt(wir_r, _2, _mint_make(wir_r, van, sut, c3__noun, p_gen),
                              _mint_make(wir_r, van, sut, c3__noun, q_gen)));
        }
      }
      case c3__ktbn: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun vat = j2_mcx(Pt6, ut, mint)(wir_r, van, sut, gol, q_gen);
     
        ret = u2_bc
          (wir_r,
           u2_bt(wir_r, c3__face,
                        u2_rx(wir_r, p_gen), 
                        u2_rx(wir_r, u2_h(vat))),
           u2_rx(wir_r, u2_t(vat)));

        u2_rl_lose(wir_r, vat);
        return ret;
      }
      case c3__ktdp: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun hif = j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen);
        u2_flag fab = _mint_fab(wir_r, van);

        return u2_bc
          (wir_r,
           _mint_nice(wir_r, van, gol, hif),
           (u2_no == fab)
             ? u2_bc(wir_r, _0, _0)
             : _mint_make(wir_r, van, sut, hif, q_gen));
      }
      case c3__ktpd: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun zut = j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen);

        if ( (u2_yes ==_mint_vet(wir_r, van)) &&
             (u2_no == j2_mcy(Pt6, ut, nest)(wir_r, van, gol, zut)) )
        {
          u2_noun dun = j2_mcy(Pt6, ut, dunq)(wir_r, van, "need", gol);
          u2_noun niz = j2_mcy(Pt6, ut, dunq)(wir_r, van, "have", sut);

          u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, dun));
          u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, niz));

          return u2_bl_error(wir_r, "mint-cool");
        }
        else {
          ret = j2_mcx(Pt6, ut, mint)(wir_r, van, sut, zut, q_gen);

          u2_rl_lose(wir_r, zut);
          return ret;
        }
      }
      case c3__ktsg: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_noun nef = j2_mcx(Pt6, ut, mint)(wir_r, van, sut, gol, p_gen);
        u2_noun p_nef = u2_h(nef);
        u2_noun q_nef = u2_t(nef);
        u2_flag fab = _mint_fab(wir_r, van);

        if ( u2_no == fab ) {
          ret = u2_bc(wir_r, u2_rx(wir_r, p_nef), u2_bc(wir_r, _0, _0));
        } else {
          u2_noun cag = j2_mcy(Pt6, ut, burn)(wir_r, van, sut);
          u2_noun cod;

          {
            cod = u2_nk_nock(wir_r, cag, q_nef);
            if ( u2_none == cod ) {
              // u2_err(wir_r, "ktsg: cag", cag);
              // u2_err(wir_r, "ktsg: q_nef", q_nef);
              return u2_bl_error(wir_r, "mint-burn");
            }
            // u2_err(wir_r, "ktsg: cod", cod);
          }
          ret = u2_bc(wir_r, u2_rx(wir_r, p_nef), u2_bc(wir_r, _1, cod));
        }
        u2_rz(wir_r, nef);
        return ret;
      }
      case c3__mtbn: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun lar = j2_mcy(Pt6, ut, seek)(wir_r, van, sut, c3__read, p_gen);
        u2_noun p_lar, q_lar;
        u2_noun ret;

        u2_bi_cell(wir_r, lar, &p_lar, &q_lar);

        if ( u2_yes == u2_h(q_lar) ) {
          u2_noun pq_lar = u2_t(q_lar);
          u2_noun gix = u2_bc
            (wir_r,
             u2_bc(wir_r, u2_rx(wir_r, pq_lar),
                          u2_bt(wir_r, u2_yes, u2_nul, _1)),
             u2_nul);
          u2_noun fup = j2_mcy(Pt6, ut, emit)
                          (wir_r, van, sut, p_lar, gix, q_gen);
          u2_noun p_fup = u2_h(fup);
          u2_noun q_fup = u2_t(fup);

          ret = u2_bc(wir_r,
                      _mint_nice
                        (wir_r, van, gol, u2_rx(wir_r, u2_h(u2_h(p_fup)))),
                      u2_rx(wir_r, q_fup));
          
          u2_rz(wir_r, gix);
          u2_rz(wir_r, fup);
        }
        else {
          u2_noun pq_lar, qq_lar;

          u2_bi_cell(wir_r, u2_t(q_lar), &pq_lar, &qq_lar);
          {
            u2_noun fup = j2_mcy(Pt6, ut, emit)
              (wir_r, van, sut, p_lar, qq_lar, q_gen);
            u2_noun p_fup = u2_h(fup);
            u2_noun q_fup = u2_t(fup);
            u2_noun qir = j2_mcy(Pt6, ut, fire)(wir_r, van, sut, p_fup);

            ret = u2_bc(wir_r,
                        _mint_nice(wir_r, van, gol, qir),
                        u2_bt(wir_r,
                              _9,
                              u2_rx(wir_r, pq_lar), 
                              u2_rx(wir_r, q_fup)));

            u2_rz(wir_r, fup);
          }
        }
        u2_rz(wir_r, lar);
        return ret;
      }
      case c3__pmdg: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun ruf = u2_bt
          (wir_r, c3__dgdp, 
                  u2_bc(wir_r, u2_nul, _1),
                  u2_rx(wir_r, p_gen));
        u2_noun ret = _mint_crow(wir_r, van, sut, gol, c3__zinc, ruf, q_gen);

        u2_rz(wir_r, ruf);
        return ret;
      }
      case c3__pmdt: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_noun ruf = u2_bc(wir_r, u2_nul, _1);

        ret = _mint_crow(wir_r, van, sut, gol, c3__gold, ruf, p_gen);
        u2_rz(wir_r, ruf);

        return ret;
      }
      case c3__pmmt: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_noun ruf = u2_bc(wir_r, u2_nul, _1);

        ret = _mint_crow(wir_r, van, sut, gol, c3__lead, ruf, p_gen);
        u2_rz(wir_r, ruf);

        return ret;
      }
      case c3__pmpd: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun ruf = u2_bt
          (wir_r, c3__dgdp, 
                  u2_bc(wir_r, u2_nul, _1),
                  u2_rx(wir_r, p_gen));
        u2_noun ret = _mint_crow(wir_r, van, sut, gol, c3__iron, ruf, q_gen);

        u2_rz(wir_r, ruf);
        return ret;
      }
      case c3__sgld: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_noun hum = j2_mcx(Pt6, ut, mint)(wir_r, van, sut, gol, q_gen);
        u2_flag fab = _mint_fab(wir_r, van);

        if ( u2_no == fab ) {
          ret = u2_bc(wir_r, 
                      u2_rx(wir_r, u2_h(hum)),
                      u2_bc(wir_r, _0, _0));
        } else {
          u2_noun bez;
        
            if ( u2_yes == u2_stud(p_gen) ) {
              bez = u2_rx(wir_r, p_gen);
            } else {
              bez = u2_bc(wir_r, u2_rx(wir_r, u2_h(p_gen)),
                                 _mint_make
                                   (wir_r, van, sut, c3__noun, u2_t(p_gen)));
            }
            ret = u2_bc(wir_r, 
                        u2_rx(wir_r, u2_h(hum)),
                        u2_bt(wir_r, _10, bez, u2_rx(wir_r, u2_t(hum))));
        }
        u2_rl_lose(wir_r, hum);
        return ret;
      }
      case c3__zpbn: p_gen = u2_t(gen);
      _mint_used(wir_r);
      {
        u2_flag fab = _mint_fab(wir_r, van);

        if ( u2_no == fab ) {
          return u2_bc(wir_r, c3__noun, u2_bc(wir_r, _0, _0));
        }
        else {
          u2_noun von = u2_bn_molt(wir_r, van, j2_ut_van_vet, u2_no, 0);
          u2_noun sev = _mint_make(wir_r, von, sut, c3__noun, p_gen);

          u2_rz(wir_r, von);

          return u2_bc(wir_r, c3__noun, u2_bc(wir_r, _1, sev));
        }
      }
      case c3__zpcb: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      {
        u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, _mint_loc(wir_r, van, p_gen)));
        {
          u2_noun hum = j2_mcx(Pt6, ut, mint)(wir_r, van, sut, gol, q_gen);
          u2_noun bez = u2_bt(wir_r, c3__spot, _1, u2_rx(wir_r, p_gen));

          ret = u2_bc(wir_r, 
                      u2_rx(wir_r, u2_h(hum)),
                      u2_bt(wir_r, _10, bez, u2_rx(wir_r, u2_t(hum))));

          u2_rz(wir_r, hum);
        }
        u2_bl_drop(wir_r);
        return ret;
      }
      case c3__zpdx: p_gen = u2_t(gen);
      {
        u2_bx_used(wir_r);
        return j2_mcx(Pt6, ut, mint)(wir_r, van, sut, gol, p_gen);
      }
      case c3__zpmt:
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
      case c3__zptm: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
      _mint_used(wir_r);
      {
        u2_flag fab = _mint_fab(wir_r, van);
        u2_noun vos   = j2_mcx(Pt6, ut, mint)(wir_r, van, sut, c3__noun, q_gen);
        u2_noun zur   = j2_mcy(Pt6, ut, play)(wir_r, van, sut, p_gen);
        u2_noun p_vos = u2_h(vos);
        u2_noun q_vos = u2_t(vos);
        u2_noun waz   = u2_bc(wir_r, _1, u2_rx(wir_r, p_vos));
        u2_noun cig   = j2_mby(Pt6, cell)(wir_r, zur, p_vos);
        u2_noun ret;

        ret = u2_bc(wir_r,
                    _mint_nice(wir_r, van, gol, cig),
                    (u2_no == fab) 
                      ? u2_bc(wir_r, _0, _0)
                      : j2_mby(Pt6, cons)(wir_r, waz, q_vos));

        u2_rz(wir_r, waz);
        u2_rz(wir_r, zur);
        u2_rz(wir_r, vos);

        return ret;
      }
      case c3__zpzp:
      _mint_used(wir_r);
      {
        return u2_bt(wir_r, c3__void, _0, _0);
      }
    }
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

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, 
                                u2_cw_sam_2, &gol, 
                                u2_cw_sam_3, &gen,
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
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
      u2_weak von = u2_rl_molt(wir_r, van, u2_cw_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, 
                                      u2_cw_sam_2, u2_rx(wir_r, gol), 
                                      u2_cw_sam_3, u2_rx(wir_r, gen), 
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
        c3_m    fun_m = u2_jet_fun_m(jet_j);
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
  j2_mck(Pt6, ut, mint)(u2_wire wir_r,
                        u2_noun cor)
  {
    u2_noun sut, gol, gen, van;

    if ( (u2_no == u2_mean(cor, u2_cw_con, &van, 
                                u2_cw_sam_2, &gol, 
                                u2_cw_sam_3, &gen,
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cw_sam, van))) )
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
    { ".3", c3__hevy, 
        j2_mc(Pt6, ut, mint), 
        Tier6_b_memo,
        // u2_jet_live | u2_jet_test | u2_jet_memo,
        // u2_jet_live | u2_jet_memo,
        // u2_jet_dead | u2_jet_memo,
        u2_none, u2_none,
        j2_mck(Pt6, ut, mint)
    },
    { }
  };
