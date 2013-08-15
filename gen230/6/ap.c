/* j/6/ap.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/** forward declares
**/
    u2_noun
    j2_mcy(Pt6, ap, mold)(u2_wire, u2_noun, u2_noun);

    u2_noun
    j2_mcy(Pt6, ap, pick)(u2_wire, u2_noun, u2_noun, u2_noun);

    u2_ho_jet 
    j2_mbj(Pt6, ap)[];

/** open cases
**/

#define _open_do_p(stem)  \
  static u2_noun _open_in_##stem \
    (u2_wire wir_r, u2_noun p_gen)

#define _open_do_pq(stem)  \
  static u2_noun _open_in_##stem \
    (u2_wire wir_r, u2_noun p_gen, u2_noun q_gen)

#define _open_do_pqr(stem)  \
  static u2_noun _open_in_##stem \
    (u2_wire wir_r, u2_noun p_gen, u2_noun q_gen, u2_noun r_gen)

#define _open_do_pqrs(stem)  \
  static u2_noun _open_in_##stem \
    (u2_wire wir_r, u2_noun p_gen, u2_noun q_gen, u2_noun r_gen, u2_noun s_gen)

    static u2_noun
    _frag(u2_wire wir_r, u2_noun axe) 
      { return u2_bc(wir_r, u2_nul, u2_rx(wir_r, axe)); }

    static u2_noun
    _lone(u2_wire wir_r, u2_noun one)
      { return u2_bc(wir_r, u2_rx(wir_r, one), u2_nul); }
 
/***
****
***/
  _open_do_pq(tsgl)   //  =<
  {
    return u2_bt(wir_r, c3__tsgr, u2_rx(wir_r, q_gen), 
                                  u2_rx(wir_r, p_gen));
  }

  _open_do_pq(tsms)   //  =-
  {
    return u2_bt(wir_r, c3__tsls, u2_rx(wir_r, q_gen), 
                                  u2_rx(wir_r, p_gen));
  }

  _open_do_pq(tsls)   //  =+
  {
    return u2_bt
      (wir_r, c3__tsgr,
              u2_bt(wir_r, c3__clms, 
                           u2_bc(wir_r, u2_nul, _1),
                           u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
/***
****
***/
  _open_do_pq(brts)   //  |=
  {
    return u2_bt
      (wir_r, c3__pmts,
              u2_rx(wir_r, p_gen), 
              u2_bt(wir_r,
                    u2_bq(wir_r, u2_blip, u2_yes, u2_yes, u2_rx(wir_r, q_gen)),
                    u2_nul, 
                    u2_nul));
  }
  _open_do_p(brms)    //  |-
  {
    return u2_bt
      (wir_r, c3__tsgr,
              u2_bc(wir_r, c3__brdt, u2_rx(wir_r, p_gen)),
              u2_blip);
  }
  _open_do_pq(brcl)    //  |:
  {
    return u2_bt
      (wir_r, c3__pmcl,
              u2_rx(wir_r, p_gen), 
              u2_bt(wir_r,
                    u2_bq(wir_r, u2_blip, u2_yes, u2_yes, u2_rx(wir_r, q_gen)),
                    u2_nul, 
                    u2_nul));
  }
  _open_do_p(brdt)   //  |.
  {
    return u2_bc
      (wir_r, c3__pmdt,
              u2_bt(wir_r,
                    u2_bq(wir_r, u2_blip, u2_yes, u2_yes, u2_rx(wir_r, p_gen)),
                    u2_nul, 
                    u2_nul));
  }
  _open_do_p(brcn)   //  |.
  {
    return u2_bc
      (wir_r, c3__pmcn,
              u2_bt(wir_r,
                    u2_bq(wir_r, u2_blip, u2_yes, u2_yes, u2_rx(wir_r, p_gen)),
                    u2_nul, 
                    u2_nul));
  }
  _open_do_pq(brls)   //  |+
  {
    return u2_bt
      (wir_r, c3__pmls,
              u2_rx(wir_r, p_gen), 
              u2_bt(wir_r,
                    u2_bq(wir_r, u2_blip, u2_yes, u2_yes, u2_rx(wir_r, q_gen)),
                    u2_nul, 
                    u2_nul));
  }
  _open_do_pq(brtr)   //  |*
  {
    return u2_bt
      (wir_r, c3__pmts,
              u2_rx(wir_r, p_gen), 
              u2_bt(wir_r,
                    u2_bq(wir_r, u2_blip, u2_yes, u2_no, u2_rx(wir_r, q_gen)),
                    u2_nul, 
                    u2_nul));
  }
  _open_do_pq(brzp)   //  |!
  {
    return u2_bt
      (wir_r, c3__tsgr,
              u2_bc(wir_r, c3__zpcn, u2_nul),
              u2_bt
                (wir_r, c3__brts,
                        u2_rx(wir_r, p_gen),
                        u2_rx(wir_r, q_gen)));
  }
/***
****
***/
  _open_do_p(smts)    //  ..
  {
    if ( u2_yes == u2_sing(c3__atom, p_gen) ) {
      return u2_bt
        (wir_r, c3__ktms, 
              u2_bq(wir_r, c3__dtls, c3__dtsg, u2_blip, _0),
              u2_bt(wir_r, c3__dtsg, u2_blip, _0));
    }
    else if ( u2_yes == u2_sing(c3__noun, p_gen) ) {
      u2_noun dud = u2_bt(wir_r, c3__dtsg, u2_blip, u2_nul);

      return u2_bt
        (wir_r, c3__ktms, 
                u2_bt(wir_r, c3__dttr, 
                             u2_rx(wir_r, dud), 
                             u2_rx(wir_r, dud)),
                dud);
    }
    else if ( u2_yes == u2_sing(c3__cell, p_gen) ) {
      return u2_bt
        (wir_r, c3__clms,
              u2_bc(wir_r, c3__smts, c3__noun),
              u2_bc(wir_r, c3__smts, c3__noun));
    }
    else if ( u2_yes == u2_sing(c3__flag, p_gen) ) {
      return u2_bt
        (wir_r, c3__ktms,
              u2_bq(wir_r, c3__dtwt, c3__dtsg, u2_blip, _0),
              u2_bt(wir_r, c3__dtsg, u2_blip, u2_yes));
    }
    else if ( u2_yes == u2_sing(c3__null, p_gen) ) {
        return u2_bt(wir_r, c3__dtsg, 'n', u2_nul);
    }
    else return u2_bl_bail(wir_r, c3__fail);
  }
/***
****
***/
  static u2_noun                                                  //  produce
  _ap_knit(u2_wire wir_r, 
           u2_noun gen,                                           //  retain
           u2_noun nug,                                           //  retain
           u2_noun dab)                                           //  retain
  {
    u2_noun diz = u2_bt(wir_r, u2_yes, u2_yes, u2_rx(wir_r, nug));
    u2_noun ret;

    ret = u2_bt
      (wir_r,
       c3__pmts,
       u2_bt(wir_r, c3__ktms, 
                    u2_bc(wir_r, c3__smts, c3__noun), 
                    u2_rx(wir_r, gen)),
       j2_mcc(Pt4, by, put)(wir_r, dab, u2_blip, diz));

    u2_rz(wir_r, diz);
    return ret;
  }

  static u2_noun                                                  //  produce
  _ap_lung(u2_wire wir_r,
           u2_noun gen,                                           //  retain
           u2_noun hep,                                           //  retain
           u2_noun doy)                                           //  retain
  {
    if ( u2_no == u2_dust(doy) ) {
      return u2_rx(wir_r, gen);
    } else {
      u2_noun haq = j2_mcy(Pt6, ap, hack)(wir_r, u2_h(doy));

      if ( u2_no == u2_h(haq) ) {
        return u2_bl_error(wir_r, "lung-hack");
      }
      else {
        u2_noun p_haq = u2_h(u2_t(haq));
        u2_noun q_haq = u2_t(u2_t(haq));
        u2_noun dog = u2_bt(wir_r,
                            c3__cnts,
                            u2_bc(wir_r, u2_bc(wir_r, u2_nul, _2),
                                         u2_rx(wir_r, hep)),
                            u2_nul);
        u2_noun cat = u2_bc(wir_r, u2_bc(wir_r, u2_nul, _3), 
                                   u2_rx(wir_r, hep));
        u2_noun ret;

        ret = u2_bq
          (wir_r,
           c3__wtcl,
           u2_bt(wir_r, c3__wtts, u2_rx(wir_r, p_haq), u2_rx(wir_r, dog)),
           u2_bt(wir_r, 
                 c3__clms,
                 dog,
                 j2_mcy(Pt6, ap, mold)(wir_r, q_haq, cat)),
           _ap_lung(wir_r, gen, hep, u2_t(doy)));

        u2_rz(wir_r, cat);
        u2_rz(wir_r, haq);
        return ret;
      }
    }
  }

  _open_do_pqr(hsts)  //  $=  vine
  {
    u2_noun duq = u2_bc(wir_r, u2_nul, _5);
    u2_noun riq = u2_bc(wir_r, u2_rx(wir_r, duq), u2_nul);
    u2_noun vaq = u2_bc(wir_r, c3__zpzp, u2_nul);
    u2_noun pix = j2_mcy(Pt6, ap, pick)(wir_r, p_gen, riq, vaq);
    u2_noun muq = j2_mcy(Pt6, ap, mold)(wir_r, q_gen, riq);
    u2_noun nug = u2_bq(wir_r, c3__wtsg, duq, pix, muq); 
    u2_noun ret;

    ret = _ap_knit(wir_r, p_gen, nug, r_gen);

    u2_rz(wir_r, nug);
    u2_rz(wir_r, riq);
    u2_rz(wir_r, vaq);

    return ret;
  }

  _open_do_pqr(hsbr)   //  $|  fern
  {
    u2_noun duq = _frag(wir_r, 5);
    u2_noun doq = _frag(wir_r, 10);
    u2_noun riq = _lone(wir_r, u2_rx(wir_r, duq));
    u2_noun vaq = u2_bc(wir_r, c3__zpzp, u2_nul);
    u2_noun hel = (u2_no == u2_dust(q_gen)) 
                    ? u2_bt(wir_r, c3__dtsg, 'n', u2_nul)
                    : u2_rx(wir_r, u2_h(q_gen));
    u2_noun lug = _ap_lung(wir_r, vaq, riq, q_gen);
    u2_noun dat = j2_mcy(Pt6, ap, mold)(wir_r, p_gen, riq);
    u2_noun cag = u2_bq(wir_r, c3__wtsg, doq, lug, dat); 
    u2_noun ret;

    ret = _ap_knit(wir_r, hel, cag, r_gen);
    
    u2_rz(wir_r, cag);
    u2_rz(wir_r, hel);
    u2_rz(wir_r, vaq); 
    u2_rz(wir_r, riq); 

    return ret;
  }

  _open_do_pq(hssg)   //  $~  herb
  {
    u2_noun riq = _lone(wir_r, _frag(wir_r, _5));
    u2_noun hul = u2_bc(wir_r, c3__cltr, u2_rx(wir_r, p_gen));
    u2_noun zex = j2_mcy(Pt6, ap, mold)(wir_r, hul, riq);
    u2_noun ret = _ap_knit(wir_r, hul, zex, q_gen);

    u2_rz(wir_r, zex);
    u2_rz(wir_r, hul);
    u2_rz(wir_r, riq);

    return ret;
  }
 
  _open_do_pq(hskt)   //  $^  moss
  {
    u2_noun riq = _lone(wir_r, _frag(wir_r, _5));
    u2_noun vaq = u2_bc(wir_r, c3__zpzp, u2_nul);
    u2_noun hel = u2_no == u2_dust(p_gen)
                     ? u2_bt(wir_r, c3__dtsg, 'n', u2_nul)
                     : u2_rx(wir_r, u2_h(p_gen));
    u2_noun lug = _ap_lung(wir_r, vaq, riq, p_gen);
    u2_noun ret;

    ret = _ap_knit(wir_r, hel, lug, q_gen);

    u2_rz(wir_r, hel);
    u2_rz(wir_r, vaq);
    u2_rz(wir_r, riq);
    u2_rz(wir_r, lug);

    return ret;
  }
    static u2_noun                                                //  produce
    _hscn_in(u2_wire wir_r, 
             u2_noun dix,                                         //  retain
             u2_noun p_gen)                                       //  retain
    {
      if ( u2_no == u2_dust(p_gen) ) {
        return u2_bc(wir_r, c3__zpzp, u2_nul);
      }
      else {
        u2_noun dyx = j2_mbc(Pt1, inc)(wir_r, dix);
        u2_noun ret =
           u2_bq
            (wir_r, c3__wtcl,
                    u2_bt(wir_r,
                          c3__dtts, 
                          u2_bc(wir_r, u2_nul, 5),
                          u2_bt(wir_r, c3__dtsg, 'u', u2_rx(wir_r, dix))),
                    u2_rx(wir_r, u2_h(p_gen)),
                    _hscn_in(wir_r, dyx, u2_t(p_gen)));

        u2_rz(wir_r, dyx);
        return ret;
      }
    }
  _open_do_pq(hscn)   //  _%  weed
  {
    u2_noun hel = u2_bt(wir_r, c3__dtsg, 'n', u2_nul);
    u2_noun zex = _hscn_in(wir_r, _0, p_gen);
    u2_noun ret = _ap_knit(wir_r, hel, zex, q_gen);

    u2_rz(wir_r, zex);
    u2_rz(wir_r, hel);

    return ret;
  }

  _open_do_pqr(hspm)  //  _&  bush
  {
    u2_noun riq = _lone(wir_r, _frag(wir_r, _5));
    u2_noun vaq = u2_bc(wir_r, c3__zpzp, u2_nul);
    u2_noun hel = u2_rx(wir_r, p_gen);
    u2_noun pix = j2_mcy(Pt6, ap, pick)(wir_r, p_gen, riq, vaq);
    u2_noun lug = _ap_lung(wir_r, vaq, riq, q_gen);
    u2_noun cag = u2_bq(wir_r, c3__wtsg, _frag(wir_r, 5), pix, lug);
    u2_noun ret;

    ret = _ap_knit(wir_r, hel, cag, r_gen);

    u2_rz(wir_r, cag);
    u2_rz(wir_r, hel);
    u2_rz(wir_r, vaq);
    u2_rz(wir_r, riq);

    return ret;
  }

    static u2_noun                                                //  produce
    _hstr_in(u2_wire wir_r,
             u2_noun riq,                                         //  retain
             u2_noun p_gen)                                       //  retain
    {
      if ( u2_no == u2_dust(p_gen) ) {
        return u2_bc(wir_r, c3__zpzp, u2_nul);
      } else {
        return j2_mcy(Pt6, ap, pick)
          (wir_r, u2_h(p_gen), riq, _hstr_in(wir_r, riq, u2_t(p_gen)));
      }
    }
  _open_do_pq(hstr)   //  _*  pine
  {
    u2_noun riq = _lone(wir_r, _frag(wir_r, _5));
    u2_noun hel = (u2_no == u2_dust(p_gen)) 
                    ? u2_bt(wir_r, c3__dtsg, 'n', u2_nul)
                    : u2_rx(wir_r, u2_h(p_gen));
    u2_noun zex = _hstr_in(wir_r, riq, p_gen);
    u2_noun ret;

    ret = _ap_knit(wir_r, hel, zex, q_gen);

    u2_rz(wir_r, zex);
    u2_rz(wir_r, hel);
    u2_rz(wir_r, riq);

    return ret;
  }
/***
****
***/
  _open_do_p(wtbr)    //  ?|
  {
    if ( (u2_nul == p_gen) ) {
      return u2_bt(wir_r, c3__dtsg, 'f', u2_no);
    }
    else {
      u2_noun ip_gen = u2_h(p_gen);
      u2_noun tp_gen = u2_t(p_gen);

      return u2_bq
        (wir_r, c3__wtcl,
                u2_rx(wir_r, ip_gen),
                u2_bt(wir_r, c3__dtsg, 'f', u2_yes),
                _open_in_wtbr(wir_r, tp_gen));
    }
  }
  _open_do_pq(wtgl)   //  ?<
  {
    return u2_bq
      (wir_r, c3__wtcl,
            u2_rx(wir_r, p_gen),
            u2_bc(wir_r, c3__zpzp, u2_nul),
            u2_rx(wir_r, q_gen));
  }
  _open_do_pqr(wtdt)  //  ?.
  {
    return u2_bq(wir_r, c3__wtcl, 
                        u2_rx(wir_r, p_gen), 
                        u2_rx(wir_r, r_gen), 
                        u2_rx(wir_r, q_gen));
  }
  _open_do_pq(wtgr)   //  ?>
  {
    return u2_bq
      (wir_r, c3__wtcl, 
              u2_rx(wir_r, p_gen), 
              u2_rx(wir_r, q_gen), 
              u2_bc(wir_r, c3__zpzp, u2_nul));
  }
  _open_do_pq(wtms)   //  ?-
  {
    if ( (u2_nul == q_gen) ) {
      return u2_bc(wir_r, c3__zpfs, u2_rx(wir_r, p_gen));
    }
    else {
      u2_noun iq_gen = u2_h(q_gen);
      u2_noun tq_gen = u2_t(q_gen);
      u2_noun piq_gen = u2_h(iq_gen);
      u2_noun qiq_gen = u2_t(iq_gen);

      return u2_bq
        (wir_r, 
         c3__wtcl,
         u2_bt(wir_r, c3__wtts, 
                      u2_rx(wir_r, piq_gen), 
                      u2_rx(wir_r, p_gen)),
         u2_rx(wir_r, qiq_gen),
         _open_in_wtms(wir_r, p_gen, tq_gen));
    }
  }
  _open_do_p(wtpm)    //  ?&
  {
    if ( (u2_nul == p_gen) ) {
      return u2_bt(wir_r, c3__dtsg, 'f', u2_yes);
    }
    else {
      u2_noun ip_gen = u2_h(p_gen);
      u2_noun tp_gen = u2_t(p_gen);

      return u2_bq
        (wir_r, c3__wtcl,
                u2_rx(wir_r, ip_gen),
                _open_in_wtpm(wir_r, tp_gen),
                u2_bt(wir_r, c3__dtsg, 'f', u2_no));
    }
  }
  _open_do_pqr(wtls)  //  ?+
  {
    u2_noun tul = u2_bc(wir_r, 
                        u2_bc(wir_r, u2_bc(wir_r, c3__smts, c3__noun),
                                     u2_rx(wir_r, q_gen)),
                        u2_nul);
    u2_noun zal = j2_mbc(Pt2, weld)(wir_r, r_gen, tul);
    u2_noun ret = u2_bt(wir_r, c3__wtms, u2_rx(wir_r, p_gen), zal);

    u2_rz(wir_r, tul);
    return ret;
  }
  _open_do_pqr(wtsg)  //  ?~
  {
    return u2_bq
      (wir_r,
       c3__wtcl,
       u2_bt(wir_r,
             c3__wtts,
             u2_bt(wir_r, c3__dtpt, u2_blip, _0),
             u2_rx(wir_r, p_gen)),
       u2_rx(wir_r, q_gen),
       u2_rx(wir_r, r_gen));
  }
  _open_do_p(wtzp)    //  ?!
  {
    return u2_bq
      (wir_r, c3__wtcl,
              u2_rx(wir_r, p_gen),
              u2_bt(wir_r, c3__dtsg, 'f', u2_no),
              u2_bt(wir_r, c3__dtsg, 'f', u2_yes));
  }
/***
****
***/
  _open_do_pqrs(clkt) //  :^
  {
    return u2_bt
      (wir_r, c3__clms,
              u2_rx(wir_r, p_gen),
              u2_bt
                (wir_r, c3__clms,
                        u2_rx(wir_r, q_gen),
                        u2_bt(wir_r, c3__clms, u2_rx(wir_r, r_gen), 
                                               u2_rx(wir_r, s_gen))));
  }
  _open_do_pqr(clls)  //  :+
  {
    return u2_bt
      (wir_r, c3__clms,
              u2_rx(wir_r, p_gen),
              u2_bt(wir_r, c3__clms, u2_rx(wir_r, q_gen), 
                                     u2_rx(wir_r, r_gen)));
  }
  _open_do_p(clsg)    //  :~
  {
    if ( (u2_nul == p_gen) ) {
      return u2_bt(wir_r, c3__dtsg, 'n', u2_nul);
    }
    else {
      u2_noun ip_gen = u2_h(p_gen);
      u2_noun tp_gen = u2_t(p_gen);

      return u2_bt(wir_r, c3__clms, 
                          u2_rx(wir_r, ip_gen), 
                          _open_in_clsg(wir_r, tp_gen));
    }
  }
  _open_do_p(cltr)    //  :*
  {
    if ( (u2_nul == p_gen) ) {
      return u2_bc(wir_r, c3__zpzp, u2_nul);
    }
    else {
      u2_noun ip_gen = u2_h(p_gen);
      u2_noun tp_gen = u2_t(p_gen);

      if ( (u2_nul == tp_gen) ) {
        return u2_rx(wir_r, ip_gen);
      } else {
        return u2_bt
          (wir_r, c3__clms, u2_rx(wir_r, ip_gen), 
                            _open_in_cltr(wir_r, tp_gen));
      }
    }
  }
/***
****
***/
  _open_do_pq(ktcl)   //  ^:
  {
    return 
      u2_bt
        (wir_r, c3__tsls,
                u2_rx(wir_r, q_gen),
                u2_bt(wir_r, c3__wtgr,
                             u2_bt(wir_r, 
                                   c3__wtts,
                                   u2_bt(wir_r, c3__tsgr,
                                                u2_bc(wir_r, u2_nul, _2),
                                                u2_rx(wir_r, p_gen)),
                                   u2_bc(wir_r, u2_nul, _3)),
                             u2_bc(wir_r, u2_nul, _3)));
  }
/***
****
***/
    static u2_noun
    _cnbr_a(u2_wire wir_r,
            u2_noun r_gen)
    {
      if ( (u2_nul == r_gen) ) {
        return u2_nul;
      } 
      else {
        u2_noun ir_gen = u2_h(r_gen);
        u2_noun tr_gen = u2_t(r_gen);
        u2_noun pir_gen = u2_h(ir_gen);
        u2_noun qir_gen = u2_t(ir_gen);

        return u2_bc
          (wir_r, u2_bc(wir_r, 
                        u2_rx(wir_r, pir_gen),
                        u2_bt(wir_r, c3__ktdt,
                                     u2_bc(wir_r, u2_nul, 13), 
                                     u2_bt(wir_r, c3__tsgr, 
                                                  u2_bc(wir_r, u2_nul, _2),
                                                  u2_rx(wir_r, qir_gen)))),
                  _cnbr_a(wir_r, tr_gen));
      }
    }
  _open_do_pqr(cnbr)  //  %|
  {
    u2_noun vop = u2_bc(wir_r, u2_bc(wir_r, u2_nul, _3), u2_nul);
    u2_noun zil = j2_mbc(Pt2, weld)(wir_r, p_gen, vop);

    u2_rz(wir_r, vop);

    return u2_bt
      (wir_r, c3__tsls,
              u2_rx(wir_r, q_gen),
              u2_bt
                (wir_r, c3__cnts, zil, _cnbr_a(wir_r, r_gen)));
  }
  _open_do_pq(cncl)   //  %:
  {
    return u2_bq
      (wir_r, c3__cnsg,
            u2_bc(wir_r, u2_blip, u2_nul),
            u2_rx(wir_r, p_gen),
            u2_rx(wir_r, q_gen));
  }
  _open_do_pq(cndt)   //  %.
  {
    return u2_bt
      (wir_r, c3__cnms, 
              u2_rx(wir_r, q_gen), 
              u2_bc(wir_r, u2_rx(wir_r, p_gen), u2_nul));
  }
  _open_do_pqrs(cnkt) //  %^
  {
    return u2_bq
      (wir_r, c3__cnms, 
              u2_rx(wir_r, p_gen),
              u2_rx(wir_r, q_gen),
              u2_bt(wir_r, u2_rx(wir_r, r_gen), 
                           u2_rx(wir_r, s_gen), 
                           u2_nul));
  }
  _open_do_pq(cnms)   //  %-
  {
    if ( (u2_nul == q_gen) ) {
      return u2_bt
        (wir_r, c3__tsgr, 
                u2_rx(wir_r, p_gen),
                u2_blip);
    } else {
      return u2_bq(wir_r, c3__cncl, 
                          u2_rx(wir_r, p_gen), 
                          c3__cltr, 
                          u2_rx(wir_r, q_gen));
    }
  }
  _open_do_pqr(cnls)  //  %+
  {
    return u2_bc
      (wir_r, c3__cnms, 
              u2_bq(wir_r, u2_rx(wir_r, p_gen), 
                           u2_rx(wir_r, q_gen), 
                           u2_rx(wir_r, r_gen), 
                           u2_nul));
  }
  _open_do_p(cntr)    //  %*
  {
    return u2_bq(wir_r, c3__ktsg,
                        c3__tsgr,
                        u2_rx(wir_r, p_gen),
                        u2_bt(wir_r, c3__cnts, 
                                     u2_bo(wir_r, u2_blip),
                                     u2_nul));
  }
  _open_do_pqr(cnsg)  //  %~
  {
    return u2_bq
      (wir_r, c3__cnbr,
              u2_rx(wir_r, p_gen),
              u2_rx(wir_r, q_gen),
              u2_bo(wir_r,
                    u2_bc(wir_r,
                          u2_bc(wir_r, u2_nul, _5),
                          u2_rx(wir_r, r_gen))));
  }
/***
****
***/
  _open_do_pq(pmts)   //  &=
  {
    return u2_bt
      (wir_r, c3__tsls, 
              u2_rx(wir_r, p_gen),
              u2_bc(wir_r, c3__pmdt, u2_rx(wir_r, q_gen)));
  }
  _open_do_pq(pmms)   //  &-
  {
    //  [%pmms *]   [%tsgr [%pmdt (~(put by q.gen) %% [& & p.gen])] %%]
    //
    u2_noun diz = u2_bt(wir_r, u2_yes, u2_yes, u2_rx(wir_r, p_gen));
    u2_noun ret = u2_bt
      (wir_r,
       c3__tsgr,
       u2_bc(wir_r,
             c3__pmdt,
             j2_mcc(Pt4, by, put)(wir_r, q_gen, u2_blip, diz)),
       u2_blip);

    u2_rz(wir_r, diz);
    return ret;
  }
  _open_do_pq(pmzp)   //  |!
  {
    return u2_bt
      (wir_r, c3__tsgr,
              u2_bc(wir_r, c3__zpcn, u2_nul),
              u2_bt
                (wir_r, c3__pmts,
                        u2_rx(wir_r, p_gen),
                        u2_rx(wir_r, q_gen)));
  }
/***
****
***/
  _open_do_pq(sgts)   //  ~=
  {
    return u2_bt
      (wir_r, c3__sggr,
              u2_bc(wir_r, c3__germ, u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
  _open_do_pq(sgbr)   //  ~|
  {
    return u2_bt
      (wir_r, c3__sggr,
              u2_bc(wir_r, c3__bean, u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
  _open_do_pq(sgcl)   //  ~:
  {
    return u2_bt
      (wir_r, c3__sggr,
              u2_bq(wir_r, c3__bank, c3__dtsg, u2_blip, u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
  _open_do_pq(sggl)   //  ~>
  {
    return u2_bt
      (wir_r, c3__tsgl, 
              u2_bq(wir_r, c3__sggr, u2_rx(wir_r, p_gen), u2_nul, _1),
              u2_rx(wir_r, q_gen));
  }
  _open_do_pq(sgms)   //  ~-
  {
    return u2_bt
      (wir_r, c3__sggr,
              u2_bq(wir_r, c3__sole, c3__dtsg, u2_blip, u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
  _open_do_pq(sgdt)   //  ~.
  {
    return u2_bc
      (wir_r, c3__sgcn, 
              u2_bq(wir_r, 
                    u2_rx(wir_r, p_gen),
                    u2_bc(wir_r, u2_nul, _4),
                    u2_nul,
                    u2_rx(wir_r, q_gen)));
  }
  _open_do_p(sghx)    //  ~#
  {
    return u2_bt(wir_r, c3__sggr, c3__ping, u2_rx(wir_r, p_gen));
  }
  _open_do_p(sgkt)    //  ~^
  {
    return u2_bc(wir_r, c3__keep, u2_rx(wir_r, p_gen));
  }
    static u2_noun
    _sgcn_a(u2_wire wir_r,
            u2_noun r_gen,
            u2_noun nob)
    {
      if ( u2_no == u2_dust(r_gen) ) {
        return u2_rx(wir_r, nob);
      } else {
        u2_noun ir_gen = u2_h(r_gen);
        u2_noun tr_gen = u2_t(r_gen);
        u2_noun pir_gen, qir_gen;

        u2_bi_cell(wir_r, ir_gen, &pir_gen, &qir_gen);
        
        return u2_bc
          (wir_r, u2_bt
                    (wir_r,
                     c3__clms,
                     u2_bt(wir_r, c3__dtsg, u2_blip, u2_rx(wir_r, pir_gen)),
                     u2_bc(wir_r, c3__zpts, u2_rx(wir_r, qir_gen))),
                  _sgcn_a(wir_r, tr_gen, nob));
      }
    }
  _open_do_pqrs(sgcn) //  ~%
  {
    return u2_bt
      (wir_r, c3__sggl,
              u2_bq
                (wir_r, c3__fast,
                        c3__clls,
                        u2_bt(wir_r, c3__dtsg, u2_blip, u2_rx(wir_r, p_gen)),
                        u2_bt
                          (wir_r, u2_bc(wir_r, c3__zpts, u2_rx(wir_r, q_gen)),
                                  c3__clsg,
                                  _sgcn_a(wir_r, r_gen, u2_nul))),
              u2_rx(wir_r, s_gen));
  }
  _open_do_pq(sgls)   //  ~+
  {
    return u2_bt
      (wir_r, c3__sggr,
              u2_bq(wir_r, c3__memo, c3__dtsg, u2_blip, u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
  _open_do_pq(sgpm)   //  ~&
  {
    return u2_bt
      (wir_r, c3__sggr,
              u2_bc(wir_r, c3__loaf, u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
  _open_do_pq(sgsg)   //  ~~
  {
    return u2_bt
      (wir_r, c3__sggr,
              u2_bt(wir_r, c3__mean, c3__brdt, u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
/***
****
***/
    static u2_noun                                                //  produce
    _smcl_in(u2_wire wir_r,
             u2_noun q_gen)                                       //  retain
    {
      u2_noun hq_gen = u2_h(q_gen);
      u2_noun tq_gen = u2_t(q_gen);

      if ( u2_no == u2_dust(tq_gen) ) {
        return u2_bt(wir_r, c3__tsgr,
                            u2_bc(wir_r, u2_nul, _2),
                            u2_rx(wir_r, hq_gen));
      } else {
        return u2_bc
          (wir_r,
           c3__cnms,
           u2_bq(wir_r, 
                 u2_bc(wir_r, u2_nul, _3),
                 u2_bt(wir_r, c3__tsgr,
                              u2_bc(wir_r, u2_nul, _2),
                              u2_rx(wir_r, hq_gen)),
                 _smcl_in(wir_r, tq_gen),
                 u2_nul));
      }
    }
  _open_do_pq(smcl)
  {
    if ( u2_no == u2_dust(q_gen) ) {
      return u2_bc(wir_r, c3__zpzp, u2_nul);
    } 
    else if ( u2_nul == u2_t(q_gen) ) {
      return u2_rx(wir_r, u2_h(q_gen));
    }
    else {
      return u2_bt
        (wir_r,
         c3__tsls,
         u2_rx(wir_r, p_gen),
         _smcl_in(wir_r, q_gen));
    }
  }
  _open_do_pq(smsg)   //  ;~
  {
    if ( (u2_nul == q_gen) ) {
      return u2_bl_error(wir_r, "open-smsg");
    }
    else {
      u2_noun iq_gen = u2_h(q_gen);
      u2_noun tq_gen = u2_t(q_gen);

      if ( (u2_nul == tq_gen) ) {
        return u2_rx(wir_r, iq_gen);
      }
      else {
        return u2_bt
          (wir_r,
           c3__tsls,
           _open_in_smsg(wir_r, p_gen, tq_gen),
           u2_bt
            (wir_r,
             c3__tsls,
             u2_bt(wir_r, c3__tsgr,
                        u2_bc(wir_r, u2_nul, _2),
                        u2_rx(wir_r, iq_gen)),
             u2_bt
              (wir_r, 
               c3__brts,
               u2_bc(wir_r, u2_nul, _13), 
               u2_bq
                (wir_r,
                 c3__cnls,
                 u2_bt(wir_r,
                       c3__tsgr,
                       u2_bc(wir_r, u2_nul, (16)),
                       u2_rx(wir_r, p_gen)),
                 u2_bq
                  (wir_r,
                   c3__cnms,
                   u2_bc(wir_r, u2_nul, _9),
                   u2_bc(wir_r, u2_nul, _5),
                   u2_nul),
                u2_bt
                  (wir_r,
                   c3__cnts,
                   u2_bc(wir_r, 
                         u2_bc(wir_r, u2_nul, (17)), 
                         u2_nul),
                   u2_bc
                    (wir_r,
                     u2_bc(wir_r, u2_bc(wir_r, u2_nul, _5),
                                  u2_bc(wir_r, u2_nul, _5)),
                     u2_nul))))));
      }
    }
  }

/* functions
*/
  /** pick
  **/
    u2_noun                                                       //  produce
    j2_mcy(Pt6, ap, pick)(u2_wire wir_r,
                          u2_noun gen,                            //  retain
                          u2_noun hep,                            //  retain
                          u2_noun dug)                            //  retain
    {
      return u2_bq
        (wir_r,
         c3__wtcl, 
         u2_bt(wir_r, c3__wtts, 
                      u2_rx(wir_r, gen), 
                      u2_bt(wir_r, c3__cnts, u2_rx(wir_r, hep), u2_nul)),
         u2_bt(wir_r, c3__cnts, u2_rx(wir_r, hep), u2_nul),
         u2_rx(wir_r, dug));
    } 

  /** mold
  **/
    u2_noun
    j2_mcy(Pt6, ap, mold)(u2_wire wir_r,
                          u2_noun gen,
                          u2_noun hep)
    {
      u2_noun p_gen, q_gen;

      if ( u2_no == u2_dust(gen) ) {
        goto plain;
      } else switch ( u2_h(gen) ) {
        default: goto plain;

        case c3__clms:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          u2_noun hed = u2_bc(wir_r, u2_bc(wir_r, u2_nul, _2), 
                                     u2_rx(wir_r, hep));
          u2_noun tal = u2_bc(wir_r, u2_bc(wir_r, u2_nul, _3), 
                                     u2_rx(wir_r, hep));
          u2_noun lag = u2_bt
            (wir_r, c3__clms,
                    j2_mcy(Pt6, ap, mold)(wir_r, p_gen, hed),
                    j2_mcy(Pt6, ap, mold)(wir_r, q_gen, tal));

          u2_rl_lose(wir_r, hed); 
          u2_rl_lose(wir_r, tal);
          return lag;
        }
        case c3__ktts:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          return u2_bt
            (wir_r, c3__ktts, 
                    u2_rx(wir_r, p_gen), 
                    j2_mcy(Pt6, ap, mold)(wir_r, q_gen, hep));
        }
        case c3__ktcl:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          return u2_bt
            (wir_r, c3__ktcl, 
                    u2_rx(wir_r, p_gen), 
                    j2_mcy(Pt6, ap, mold)(wir_r, q_gen, hep));
        }
        case c3__ktgl:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          return u2_bt
            (wir_r, c3__ktgl, 
                    u2_rx(wir_r, p_gen), 
                    j2_mcy(Pt6, ap, mold)(wir_r, q_gen, hep));
        }
        case c3__ktms:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          return j2_mcy(Pt6, ap, mold)(wir_r, p_gen, hep);
        }
        case c3__ktgr:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          return u2_bt
            (wir_r, c3__ktgr, 
                    u2_rx(wir_r, p_gen), 
                    j2_mcy(Pt6, ap, mold)(wir_r, q_gen, hep));
        }
        case c3__cnts:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          if ( u2_nul == q_gen ) {
            return u2_bt
              (wir_r, c3__cnts,
                      u2_rx(wir_r, p_gen),
                      _lone(wir_r,
                            u2_bc(wir_r, 
                                  _frag(wir_r, _5),
                                  u2_bt(wir_r, c3__cnts, 
                                               u2_rx(wir_r, hep), 
                                               u2_nul))));
          } else {
            goto plain;
          }
        }
        case c3__cnms:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          if ( u2_nul == q_gen ) {
            return u2_bq
              (wir_r, c3__cnms,
                      u2_rx(wir_r, p_gen),
                      u2_bt(wir_r, c3__cnts, u2_rx(wir_r, hep), u2_nul),
                      u2_rx(wir_r, q_gen));
          } else {
            goto plain;
          }
        }
        case c3__cntr:  p_gen = u2_t(gen);
        {
          return u2_bq
            (wir_r, c3__cnms,
                    u2_rx(wir_r, p_gen),
                    u2_bt(wir_r, c3__cnts, u2_rx(wir_r, hep), u2_nul),
                    u2_nul);
        }
        case c3__zpcb:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          return u2_bt
            (wir_r, c3__zpcb, 
                    u2_rx(wir_r, p_gen), 
                    j2_mcy(Pt6, ap, mold)(wir_r, q_gen, hep));
        }
        case c3__zphx:  p_gen = u2_t(gen);
        {
          return u2_bc
            (wir_r, c3__zpcb, j2_mcy(Pt6, ap, mold)(wir_r, q_gen, hep));
        }
      }

      plain: {
        u2_noun bog = j2_mcy(Pt6, ap, open)(wir_r, gen);
        u2_noun gad;

        if ( u2_no == u2_sing(bog, gen) ) {
          gad = j2_mcy(Pt6, ap, mold)(wir_r, bog, hep);
        }
        else {
          gad = u2_bt
            (wir_r,
             c3__wtgr, 
             u2_bt(wir_r, c3__wtts, 
                          u2_rx(wir_r, gen), 
                          u2_bt(wir_r, c3__cnts, u2_rx(wir_r, hep), u2_nul)),
                   u2_bt(wir_r, c3__cnts, u2_rx(wir_r, hep), u2_nul));
        }
        u2_rl_lose(wir_r, bog);
        return gad;
      }
    }
    u2_noun                                                       //  transfer
    j2_mc(Pt6, ap, mold)(u2_wire wir_r, 
                         u2_noun cor)                             //  retain
    {
      u2_noun hep, van, gen;

      if ( (u2_no == u2_mean(cor, u2_cw_con, &van, u2_cw_sam, &hep, 0)) ||
           (u2_none == (gen = u2_frag(u2_cw_sam, van))) ) {
        return u2_bl_bail(wir_r, c3__fail);
      } else {
        return j2_mcy(Pt6, ap, mold)(wir_r, gen, hep);
      }
    }


  /** open
  **/
    static u2_noun 
    _open_in(u2_wire wir_r, 
             u2_noun gen)
    {
      u2_noun p_gen, q_gen, r_gen, s_gen;

      if ( u2_yes == u2_stud(gen) ) {
        return u2_bt
          (wir_r, c3__cnts, 
                  u2_bc(wir_r, u2_rx(wir_r, gen), u2_nul),
                  u2_nul);
      }
      else switch ( u2_h(gen) ) {
        default: return u2_rx(wir_r, gen);

        case u2_nul: {
          return u2_bt
            (wir_r, c3__cnts, 
                    u2_bc(wir_r, u2_rx(wir_r, gen), u2_nul),
                    u2_nul);
        }
      
#     define _open_p(stem) \
          case c3__##stem: \
            return _open_in_##stem(wir_r, u2_t(gen)); \

#     define _open_pq(stem) \
          case c3__##stem: \
            if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) ) { \
              return u2_bl_bail(wir_r, c3__fail); \
            } else return _open_in_##stem(wir_r, p_gen, q_gen);

#     define _open_pqr(stem) \
          case c3__##stem: \
            if ( u2_no == u2_as_trel(u2_t(gen), &p_gen, &q_gen, &r_gen) ) { \
              return u2_bl_bail(wir_r, c3__fail); \
            } else return _open_in_##stem(wir_r, p_gen, q_gen, r_gen);

#     define _open_pqrs(stem) \
          case c3__##stem: \
            if ( u2_no == u2_as_qual\
                          (u2_t(gen), &p_gen, &q_gen, &r_gen, &s_gen) )\
            { \
              return u2_bl_bail(wir_r, c3__fail); \
            } else return _open_in_##stem(wir_r, p_gen, q_gen, r_gen, s_gen);

        _open_pq  (tsgl);
        _open_pq  (tsms);
        _open_pq  (tsls);

        _open_pq  (brts);
        _open_p   (brms);
        _open_pq  (brcl);
        _open_p   (brdt);
        _open_p   (brcn);
        _open_pq  (brls);
        _open_pq  (brtr);
        _open_pq  (brzp);

        _open_pqrs(clkt);
        _open_pqr (clls);
        _open_p   (cltr);
        _open_p   (clsg);

        _open_p   (smts);

        _open_pqr (hsts);
        _open_pqr (hsbr);
        _open_pq  (hskt);
        _open_pq  (hscn);
        _open_pqr (hspm);
        _open_pq  (hssg);
        _open_pq  (hstr);

        _open_pqr (wtdt);
        _open_pq  (wtgl);
        _open_p   (wtzp);
        _open_p   (wtbr);
        _open_p   (wtpm);
        _open_pqr (wtls);
        _open_pqr (wtsg);
        _open_pq  (wtms);
        _open_pq  (wtgr);

        _open_pq  (ktcl);

        _open_pqr (cnbr);
        _open_pq  (cncl);
        _open_pq  (cndt);
        _open_pqrs(cnkt);
        _open_pq  (cnms);
        _open_pqr (cnls);
        _open_p   (cntr);
        _open_pqr (cnsg);

        _open_pq  (pmts);
        _open_pq  (pmms);
        _open_pq  (pmzp);

        _open_pq  (sgts);
        _open_pq  (sgbr);
        _open_pq  (sgcl);
        _open_pq  (sggl);
        _open_pq  (sgms);
        _open_pq  (sgdt);
        _open_p   (sghx);
        _open_p   (sgkt);
        _open_pqrs(sgcn);
        _open_pq  (sgls);
        _open_pq  (sgpm);
        _open_pq  (sgsg);

        _open_pq  (smcl);
        _open_pq  (smsg);
      }
    }

    u2_noun
    j2_mcy(Pt6, ap, open)(u2_wire wir_r,
                          u2_noun gen)
    {
      u2_ho_jet *jet_j = &j2_mbj(Pt6, ap)[0];

      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return _open_in(wir_r, gen);
      } else {
        c3_m    fun_m = u2_jet_fun_m(jet_j);
        u2_noun pro   = u2_rl_find(wir_r, fun_m, gen);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = _open_in(wir_r, gen);

          return u2_rl_save(wir_r, fun_m, gen, pro);
        }
      }
    }
    u2_noun                                                       //  transfer
    j2_mc(Pt6, ap, open)(u2_wire wir_r, 
                         u2_noun cor)                             //  retain
    {
      u2_noun gen;

      if ( u2_none == (gen = u2_frag(u2_cw_sam, cor)) ) {
        return u2_bl_bail(wir_r, c3__fail);
      } else {
        return j2_mcy(Pt6, ap, open)(wir_r, gen);
      }
    }

  /** rake
  **/
    u2_noun
    j2_mcy(Pt6, ap, rake)(u2_wire wir_r,
                          u2_noun gen)
    {
      u2_noun p_gen, q_gen;

      if ( u2_yes == u2_stud(gen) ) {
        return u2_bc(wir_r, u2_rx(wir_r, gen), u2_nul);
      }
      else switch ( u2_h(gen) ) {
        default: return u2_bl_error(wir_r, "rake-gene");

        case u2_nul:  return u2_bc(wir_r, u2_rx(wir_r, gen), u2_nul);

        case c3__cnts: {
          if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) ) {
            return u2_bl_bail(wir_r, c3__fail);
          }
          else {
            if ( u2_nul != q_gen ) {
              return u2_bl_bail(wir_r, c3__fail);
            }
            else {
              return u2_rx(wir_r, p_gen);
            }
          }
        }
        case c3__zpcb: {
          if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) ) {
            return u2_bl_bail(wir_r, c3__fail);
          }
          else return j2_mcy(Pt6, ap, rake)(wir_r, q_gen);
        }
        case c3__zphx: { 
          return j2_mcy(Pt6, ap, rake)(wir_r, u2_t(gen));
        }
      }
    }
    u2_noun                                                       //  transfer
    j2_mc(Pt6, ap, rake)(u2_wire wir_r, 
                         u2_noun cor)                             //  retain
    {
      u2_noun gen;

      if ( u2_none == (gen = u2_frag(u2_cw_sam, cor)) ) {
        return u2_bl_bail(wir_r, c3__fail);
      } else {
        return j2_mcy(Pt6, ap, rake)(wir_r, gen);
      }
    }

  /** late
  **/
    u2_noun                                                       //  transfer
    j2_mcy(Pt6, ap, late)(u2_wire wir_r, 
                          u2_noun gen)                            //  retain
    {
      u2_noun p_gen, q_gen;

      if ( u2_yes == u2_dust(gen) ) switch ( u2_h(gen) ) {
        case c3__ktgl: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
        { 
          return  
            u2_bt(wir_r, 
                  c3__tsls,
                  u2_bt(wir_r, c3__ktts, 'a', u2_rx(wir_r, p_gen)),
                  u2_bt(wir_r, 
                        c3__tsls,
                        u2_bt(wir_r, c3__ktts, 'b', 
                                     u2_bt(wir_r, c3__tsgr,
                                                  u2_bc(wir_r, _0, _2),
                                                  u2_rx(wir_r, q_gen))),
                        u2_bt(wir_r, 
                              c3__wtgr,
                              u2_bq(wir_r, c3__cnms, 'a', 'b', u2_nul),
                              'b')));
        }
        case c3__ktgr: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
        { 
          return 
          u2_bt(wir_r, 
                c3__tsls,
                u2_bt(wir_r, c3__ktts, 'a', u2_rx(wir_r, p_gen)),
                u2_bt(wir_r, 
                      c3__tsls,
                      u2_bt(wir_r, c3__ktts, 'b', 
                                   u2_bt(wir_r, c3__tsgr,
                                                u2_bc(wir_r, _0, _2),
                                                u2_rx(wir_r, q_gen))),
                      u2_bc(wir_r,
                            c3__brms,
                            u2_bt(wir_r, 
                                  c3__ktms, 
                                  u2_bc(wir_r, c3__cntr, 'a'),
                                  u2_bt(wir_r, 
                                        c3__tsls,
                                        u2_bt(wir_r, 
                                              c3__ktts,
                                              'c',
                                              u2_bq(wir_r,
                                                    c3__cnms,
                                                    'a',
                                                    'b',
                                                    u2_nul)),
                                        u2_bq(wir_r,
                                              c3__wtcl,
                                              u2_bt(wir_r, c3__dtts, 'b', 'c'),
                                              'c',
                                              u2_bt(wir_r,
                                                    c3__cnts,
                                                    u2_bc(wir_r, u2_blip,
                                                                 u2_nul),
                                                    u2_bc(wir_r,
                                                          u2_bc(wir_r, 
                                                                'b', 
                                                                'c'),
                                                          u2_nul))))))));
        }
      }
      return u2_bl_bail(wir_r, c3__fail);
    }

  /** hack
  **/
    u2_noun                                                       //  transfer
    j2_mcy(Pt6, ap, hack)(u2_wire wir_r, 
                          u2_noun gen)                            //  retain
    {
      u2_noun p_gen, q_gen;
      u2_noun ret;

      if ( u2_yes == u2_dust(gen) ) switch ( u2_h(gen) ) {
        case c3__tsgr: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
        {
          if ( (u2_no == u2_dust(p_gen)) || (u2_nul != u2_h(p_gen)) ) {
            return u2_bc(wir_r, u2_no, u2_rx(wir_r, gen));
          }
          else {
            u2_noun pyr = j2_mcy(Pt6, ap, hack)(wir_r, q_gen);

            if ( u2_yes == u2_h(pyr) ) {
              ret = u2_bt
                (wir_r, u2_yes,
                        u2_bt(wir_r, c3__tsgr, 
                                     u2_rx(wir_r, p_gen), 
                                     u2_rx(wir_r, u2_h(u2_t(pyr)))),
                        u2_bt(wir_r, c3__tsgr, 
                                     u2_rx(wir_r, p_gen), 
                                     u2_rx(wir_r, u2_t(u2_t(pyr)))));
            }
            else {
              ret = u2_bc
                (wir_r, u2_no,
                        u2_bt(wir_r, c3__tsgr, 
                                     u2_rx(wir_r, p_gen), 
                                     u2_rx(wir_r, u2_t(pyr))));
            }
            u2_rz(wir_r, pyr);
            return ret;
          }
        }
        case c3__clms: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
        {
          return u2_bt(wir_r, u2_yes, 
                              u2_rx(wir_r, p_gen),
                              u2_rx(wir_r, q_gen));
        }
        case c3__zpcb: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
        {
          u2_noun pyr = j2_mcy(Pt6, ap, hack)(wir_r, q_gen);

          if ( u2_yes == u2_h(pyr) ) {
            ret = u2_bt
              (wir_r, u2_yes,
                      u2_bt(wir_r, c3__zpcb, 
                                   u2_rx(wir_r, p_gen), 
                                   u2_rx(wir_r, u2_h(u2_t(pyr)))),
                      u2_bt(wir_r, c3__zpcb, 
                                   u2_rx(wir_r, p_gen), 
                                   u2_rx(wir_r, u2_t(u2_t(pyr)))));
          }
          else {
            ret = u2_bc
              (wir_r, u2_no,
                      u2_bt(wir_r, c3__zpcb, 
                                   u2_rx(wir_r, p_gen), 
                                   u2_rx(wir_r, u2_t(pyr))));
          }
          u2_rz(wir_r, pyr);
          return ret;
        } 
        default: break;
      }

      {
        u2_noun voq = j2_mcy(Pt6, ap, open)(wir_r, gen);

        if ( u2_yes == u2_sing(voq, gen) ) {

          return u2_bc(wir_r, u2_no, voq);
        }
        else {
          ret = j2_mcy(Pt6, ap, hack)(wir_r, voq); 

          u2_rl_lose(wir_r, voq);
          return ret;
        }
      }
    }

    u2_noun                                                       //  transfer
    j2_mc(Pt6, ap, hack)(u2_wire wir_r, 
                         u2_noun cor)                             //  retain
    {
      u2_noun gen;

      if ( u2_none == (gen = u2_frag(u2_cw_sam, cor)) ) {
        return u2_bl_bail(wir_r, c3__fail);
      } else {
        return j2_mcy(Pt6, ap, hack)(wir_r, gen);
      }
    }

    u2_noun
    j2_mc(Pt6, ap, late)(u2_wire wir_r,
                         u2_noun cor)
    {
      u2_noun gen;

      if ( u2_none == (gen = u2_frag(u2_cw_sam, cor)) ) {
        return u2_bl_bail(wir_r, c3__fail);
      } else {
        return j2_mcy(Pt6, ap, late)(wir_r, gen);
      }
    }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt6, ap, mold)[] = {
    { ".3", c3__hevy, j2_mc(Pt6, ap, mold), Tier6_c, u2_none, u2_none },
    { }
  };

  u2_ho_driver 
  j2_mbd(Pt6, ap)[] = {
    { j2_sc(Pt6, ap, mold), j2_mcj(Pt6, ap, mold), 0, 0, u2_none },
    { }
  };

  u2_ho_jet 
  j2_mbj(Pt6, ap)[] = {
    { "late", c3__hevy, j2_mc(Pt6, ap, late), Tier6_c, u2_none, u2_none },
    { "open", c3__hevy, j2_mc(Pt6, ap, open), Tier6_c, u2_none, u2_none },
    { "rake", c3__hevy, j2_mc(Pt6, ap, rake), Tier6_c, u2_none, u2_none },
    { "hack", c3__hevy, j2_mc(Pt6, ap, hack), Tier6_c, u2_none, u2_none },
    { }
  };
