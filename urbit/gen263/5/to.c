/* j/5/to.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* forward declares
*/
  u2_atom j2_mcy(Pit, to, wit)(u2_wire, u2_noun);

/* functions
*/
  static u2_noun                                                  //  transfer
  _to_wc(u2_wire wir_r,
         u2_noun a,                                               //  transfer
         u2_noun b)                                               //  transfer
  {
    u2_noun c = j2_mbc(Pit, weld)(wir_r, a, b);

    u2_rz(wir_r, a);
    u2_rz(wir_r, b);

    return u2_bl_good(wir_r, c);
  }
#if 0
  static u2_noun                                                  //  transfer
  _to_wt(u2_wire wir_r,
         u2_noun a,                                               //  transfer
         u2_noun b,                                               //  transfer 
         u2_noun c)                                               //  transfer
  {
    u2_noun d = j2_mbc(Pit, weld)(wir_r, b, c);
    u2_noun e = j2_mbc(Pit, weld)(wir_r, a, d);

    u2_rz(wir_r, a);
    u2_rz(wir_r, b);
    u2_rz(wir_r, c);
    u2_rz(wir_r, d);

    return u2_bl_good(wir_r, e);
  }
#endif

  /* fly
  */
    static u2_noun
    _fly_a(u2_wire, u2_noun, c3_w, c3_w, u2_noun);

    static u2_noun                                                //  transfer
    _fly_ind(u2_wire wir_r,
             c3_w    col_w,
             u2_noun tep)                                         //  transfer
    {
      while ( col_w ) {
        col_w--;
        tep = u2_bc(wir_r, ' ', tep);
      }
      return tep;
    }
    static u2_noun
    _fly_grip_a(u2_wire wir_r,
                u2_noun qp_pup,                                   //  retain
                u2_noun q_pup,                                    //  retain
                c3_w    bor_w,
                c3_w    col_w,
                u2_noun lux)                                      //  transfer
    {
      if ( u2_no == u2_dust(q_pup) ) {
        return u2_bc
          (wir_r, _fly_ind(wir_r, col_w, j2_mbc(Pit, rip)(wir_r, 3, qp_pup)),
                  lux);
      }
      else {
        return _fly_a
          (wir_r, 
           u2_h(q_pup),
           bor_w,
           (col_w >= (2 * (bor_w / 3)) ? 0 : (col_w + 2)),
           _fly_grip_a(wir_r, qp_pup, u2_t(q_pup), bor_w, col_w, lux));
      }
    } 

    static u2_noun                                                //  transfer
    _fly_a(u2_wire wir_r,
           u2_noun pup,                                           //  retain
           c3_w    bor_w,
           c3_w    col_w,
           u2_noun lux)                                           //  transfer
    {
      u2_noun p_pup, q_pup, pp_pup, qp_pup;

      if ( u2_yes == u2_dust(pup) ) switch ( u2_h(pup) ) {
        default: break;

        case c3__duct: p_pup = u2_t(pup);
        {
          break;
        }
        case c3__grip: u2_bi_cell(wir_r, u2_t(pup), &p_pup, &q_pup);
                       u2_bi_cell(wir_r, p_pup, &pp_pup, &qp_pup);
        {
          u2_noun zit = j2_mcy(Pit, to, wit)(wir_r, pup);
          u2_noun nox = j2_mbc(Pit, add)(wir_r, zit, col_w);
          u2_flag mef = j2_mbc(Pit, gte)(wir_r, nox, bor_w);

          u2_rz(wir_r, zit);
          u2_rz(wir_r, nox);
          if ( u2_no == mef ) {
            break;
          } else {
            return u2_bc
              (wir_r, _fly_ind(wir_r, col_w, 
                                      j2_mbc(Pit, rip)(wir_r, 3, pp_pup)),
                      _fly_grip_a(wir_r, qp_pup, q_pup, bor_w, col_w, lux));
          }
        }
        case c3__meld: p_pup = u2_t(pup);
        {
          break;
        }
      }
      return u2_bc
        (wir_r, _fly_ind(wir_r, col_w, j2_mcy(Pit, to, ram)(wir_r, pup)),
                lux);
    }

  u2_noun                                                         //  transfer
  j2_mcy(Pit, to, fly)(u2_wire wir_r,
                       u2_noun pup,                               //  retain
                       u2_noun bor)                               //  retain
  {
    if ( !u2_fly_is_cat(bor) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      c3_w bor_w = bor;

      return _fly_a(wir_r, pup, bor_w, 0, u2_nul);
    }
  }
  u2_weak                                                         //  transfer
  j2_mc(Pit, to, fly)(u2_wire wir_r, 
                      u2_noun cor)                                //  retain
  {
    u2_noun pup, bor;

    if ( u2_no == u2_mean(cor, 4, &bor, 20, &pup, 0) ) {
      return u2_none;
    } else {
      return j2_mcy(Pit, to, fly)(wir_r, pup, bor);
    }
  }
    
  /* ram
  */
    static u2_noun                                                //  transfer
    _ram_meld_a(u2_wire wir_r,
                u2_noun p_pup)                                    //  retain
    {
      if ( u2_no == u2_dust(p_pup) ) {
        return u2_nul;
      } else {
        return _to_wc(wir_r, j2_mcy(Pit, to, ram)(wir_r, u2_h(p_pup)),
                             _ram_meld_a(wir_r, u2_t(p_pup)));
      } 
    }
    static u2_noun                                                //  transfer
    _ram_grip_a(u2_wire wir_r, 
                u2_noun qp_pup,                                   //  retain
                u2_noun q_pup)                                    //  retain
    {
      if ( u2_nul == q_pup) {
        return j2_mbc(Pit, rip)(wir_r, 3, q_pup);
      } else if ( u2_no == u2_dust(q_pup) ) {
        return u2_bl_bail(wir_r, c3__fail);
      } else if ( u2_nul == u2_t(q_pup) ) {
        return _to_wc(wir_r, j2_mcy(Pit, to, ram)(wir_r, u2_h(q_pup)),
                             j2_mbc(Pit, rip)(wir_r, 3, qp_pup));
      } else {
        return _to_wc
          (wir_r, j2_mcy(Pit, to, ram)(wir_r, u2_h(q_pup)),
                  u2_bc(wir_r, ' ', 
                               _ram_grip_a(wir_r, qp_pup, u2_t(q_pup))));
      }
    } 
                
  u2_atom                                                         //  transfer
  j2_mcy(Pit, to, ram)(u2_wire wir_r,
                       u2_noun pup)                               //  retain
  {
    u2_noun p_pup, q_pup, pp_pup, qp_pup;

    if ( u2_no == u2_dust(pup) ) {
      return j2_mbc(Pit, rip)(wir_r, 3, pup);
    } else switch ( u2_h(pup) ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__duct: p_pup = u2_t(pup);
      {
        return u2_rx(wir_r, p_pup);
      }
      case c3__grip: u2_bi_cell(wir_r, u2_t(pup), &p_pup, &q_pup);
                     u2_bi_cell(wir_r, p_pup, &pp_pup, &qp_pup);
      {
        return _to_wc
          (wir_r,
           j2_mbc(Pit, rip)(wir_r, 3, pp_pup),
           _ram_grip_a(wir_r, qp_pup, q_pup));
      }
      case c3__meld: p_pup = u2_t(pup);
      {
        return _ram_meld_a(wir_r, p_pup);
      }
    }
  }
  u2_noun                                                         //  transfer
  j2_mc(Pit, to, ram)(u2_wire wir_r, 
                      u2_noun cor)                                //  retain
  {
    u2_noun pup;

    if ( u2_none == (pup = u2_frag(4, cor)) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcy(Pit, to, ram)(wir_r, pup);
    }
  }

  /* wit
  */
    static u2_atom
    _wit_grip_a(u2_wire wir_r,
                 u2_noun q_pup)
    {
      u2_atom foz = 0;
      
      while ( u2_yes == u2_dust(q_pup) ) {
        u2_noun iq_pup = u2_h(q_pup);
        u2_noun tq_pup = u2_t(q_pup);
        u2_noun tiw    = j2_mcy(Pit, to, wit)(wir_r, iq_pup);
        u2_noun xot;

        if ( 0 == foz ) {
          xot = u2_rx(wir_r, foz);
        } else {
          xot = j2_mbc(Pit, add)(wir_r, 1, foz);
        }
        u2_rz(wir_r, foz);

        foz = j2_mbc(Pit, add)(wir_r, xot, tiw);
        u2_rz(wir_r, xot);
        u2_rz(wir_r, tiw);

        q_pup = tq_pup;
      }
      return foz;
    }
    static u2_atom
    _wit_meld_a(u2_wire wir_r,
                 u2_noun p_pup)
    {
      u2_atom foz = 0;
  
      while ( u2_yes == u2_dust(p_pup) ) {
        u2_noun ip_pup = u2_h(p_pup);
        u2_noun tp_pup = u2_t(p_pup);
        u2_noun tiw = j2_mcy(Pit, to, wit)(wir_r, ip_pup);
        u2_noun xot = j2_mbc(Pit, add)(wir_r, foz, tiw);

        u2_rz(wir_r, foz);
        u2_rz(wir_r, tiw);
        foz = xot;

        p_pup = tp_pup;
      }
      return foz;
    }

  u2_atom                                                         //  transfer
  j2_mcy(Pit, to, wit)(u2_wire wir_r,
                       u2_noun pup)                               //  retain
  {
    u2_noun p_pup, q_pup, pp_pup, qp_pup;

    if ( u2_no == u2_dust(pup) ) {
      return j2_mbc(Pit, met)(wir_r, _3, pup);
    } else  switch ( u2_h(pup) ) {
      default: return u2_bl_bail(wir_r, c3__fail);

      case c3__duct: p_pup = u2_t(pup);
      {
        return j2_mbc(Pit, lent)(wir_r, p_pup);
      }
      case c3__grip: u2_bi_cell(wir_r, u2_t(pup), &p_pup, &q_pup);
                     u2_bi_cell(wir_r, p_pup, &pp_pup, &qp_pup);
      {
        u2_noun zaq = j2_mbc(Pit, met)(wir_r, _3, pp_pup);
        u2_noun lor = j2_mbc(Pit, met)(wir_r, _3, qp_pup);
        u2_noun fod = j2_mbc(Pit, add)(wir_r, zaq, lor);
        u2_noun vux = _wit_grip_a(wir_r, q_pup);
        u2_noun ley = j2_mbc(Pit, add)(wir_r, vux, fod);

        u2_rz(wir_r, zaq);
        u2_rz(wir_r, lor);
        u2_rz(wir_r, vux);
        u2_rz(wir_r, fod);

        return ley;
      }
      case c3__meld: p_pup = u2_t(pup);
      {
        return _wit_meld_a(wir_r, p_pup);
      }
    }
  }
  u2_noun                                                         //  transfer
  j2_mc(Pit, to, wit)(u2_wire wir_r, 
                      u2_noun cor)                                //  retain
  {
    u2_noun pup;

    if ( u2_none == (pup = u2_frag(4, cor)) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcy(Pit, to, wit)(wir_r, pup);
    }
  }

/* declarations
*/
  u2_ho_jet 
  j2_mcj(Pit, to, fly)[] = {
    { ".3", c3__hevy, j2_mc(Pit, to, fly), Tier5, u2_none, u2_none },
    { }
  };

  u2_ho_driver 
  j2_mbd(Pit, to)[] = {
    { j2_sc(Pit, to, fly), j2_mcj(Pit, to, fly), 0, 0, u2_none },
    {}
  };

  u2_ho_jet 
  j2_mbj(Pit, to)[] = {
    { "wit", c3__hevy, j2_mc(Pit, to, wit), Tier5, u2_none, u2_none },
    { "ram", c3__hevy, j2_mc(Pit, to, ram), Tier5, u2_none, u2_none },
    { }
  };
