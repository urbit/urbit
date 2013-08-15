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

    u2_noun
    j2_mcy(Pt6, ap, mold)(u2_wire, u2_noun, u2_noun);
    
    u2_noun j2_mc(Pt6, ap, rake)(u2_wire, u2_noun); 
    u2_noun j2_mc(Pt6, ap, late)(u2_wire, u2_noun); 
    u2_noun j2_mc(Pt6, ap, open)(u2_wire, u2_noun); 
    u2_noun j2_mc(Pt6, ap, hack)(u2_wire, u2_noun); 

    // make sure these match the array below!
    //
#   define _ap_jet_late  0
#   define _ap_jet_open  1
#   define _ap_jet_rake  2
#   define _ap_jet_hack  3

    u2_ho_jet 
    j2_mbj(Pt6, ap)[] = {
      { "late", c3__hevy, j2_mc(Pt6, ap, late), Tier6_c, u2_none, u2_none },
      { "open", c3__hevy, j2_mc(Pt6, ap, open), Tier6_c, u2_none, u2_none },
      { "rake", c3__hevy, j2_mc(Pt6, ap, rake), Tier6_c, u2_none, u2_none },
      { "hack", c3__hevy, j2_mc(Pt6, ap, hack), Tier6_c, u2_none, u2_none },
      { }
    };

    static u2_weak 
    _open_in(u2_wire wir_r, 
             u2_noun gen);

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

/***
****
***/
  _open_do_pq(tsgl)   //  =<
  {
    return u2_bt(wir_r, c3__tsgr, u2_rx(wir_r, q_gen), 
                                  u2_rx(wir_r, p_gen));
  }

  _open_do_pq(tshp)   //  =-
  {
    return u2_bt(wir_r, c3__tsls, u2_rx(wir_r, q_gen), 
                                  u2_rx(wir_r, p_gen));
  }

  _open_do_pq(tsls)   //  =+
  {
    return u2_bt
      (wir_r, c3__tsgr,
              u2_bc(wir_r, u2_bc(wir_r, u2_nul, _1),
                           u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
/***
****
***/
  _open_do_p(brhp)    //  |-
  {
    return u2_bt
      (wir_r, c3__tsgr,
              u2_bc(wir_r, c3__brdt, u2_rx(wir_r, p_gen)),
              u2_bc(wir_r, c3__cnbc, u2_blip));
  }
  _open_do_p(brdt)   //  |.
  {
    return u2_bc
      (wir_r, c3__brcn,
              u2_bt(wir_r,
                    u2_bq(wir_r, u2_blip, u2_yes, u2_yes, u2_rx(wir_r, p_gen)),
                    u2_nul, 
                    u2_nul));
  }
/***
****
***/
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
#if 0
  _open_do_pq(wthp)   //  ?-
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
         _open_in_wthp(wir_r, p_gen, tq_gen));
    }
  }
#endif
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
                        u2_bc(wir_r, u2_bc(wir_r, c3__bcts, c3__noun),
                                     u2_rx(wir_r, q_gen)),
                        u2_nul);
    u2_noun zal = j2_mbc(Pt2, weld)(wir_r, r_gen, tul);
    u2_noun ret = u2_bt(wir_r, c3__wtcb, u2_rx(wir_r, p_gen), zal);

    u2_rz(wir_r, tul);
    return ret;
  }
  _open_do_pqr(wtsg)  //  ?~
  {
    return u2_bq
      (wir_r,
       c3__wtcl,
       u2_bt(wir_r,
             c3__wtcn,
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
    return u2_bq
      (wir_r, u2_rx(wir_r, p_gen), 
              u2_rx(wir_r, q_gen),
              u2_rx(wir_r, r_gen), 
              u2_rx(wir_r, s_gen));
  }
  _open_do_pqr(clls)  //  :+
  {
    return u2_bt
      (wir_r, u2_rx(wir_r, p_gen), u2_rx(wir_r, q_gen), u2_rx(wir_r, r_gen));
  }
  _open_do_p(clsg)    //  :~
  {
    if ( (u2_nul == p_gen) ) {
      return u2_bt(wir_r, c3__dtsg, 'n', u2_nul);
    }
    else {
      u2_noun ip_gen = u2_h(p_gen);
      u2_noun tp_gen = u2_t(p_gen);

      return u2_bc(wir_r, u2_rx(wir_r, ip_gen), 
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
        return u2_bc
          (wir_r, u2_rx(wir_r, ip_gen), 
                  _open_in_cltr(wir_r, tp_gen));
      }
    }
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
      (wir_r, c3__cnhp, 
              u2_rx(wir_r, q_gen), 
              u2_bc(wir_r, u2_rx(wir_r, p_gen), u2_nul));
  }
  _open_do_pqrs(cnkt) //  %^
  {
    return u2_bq
      (wir_r, c3__cnhp, 
              u2_rx(wir_r, p_gen),
              u2_rx(wir_r, q_gen),
              u2_bt(wir_r, u2_rx(wir_r, r_gen), 
                           u2_rx(wir_r, s_gen), 
                           u2_nul));
  }
  _open_do_pq(cnhp)   //  %-
  {
    if ( (u2_nul == q_gen) ) {
      return u2_bt
        (wir_r, c3__tsgr, 
                u2_rx(wir_r, p_gen),
                u2_bc(wir_r, c3__cnbc, u2_blip));
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
      (wir_r, c3__cnhp, 
              u2_bq(wir_r, u2_rx(wir_r, p_gen), 
                           u2_rx(wir_r, q_gen), 
                           u2_rx(wir_r, r_gen), 
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
  _open_do_pq(brkt)   //  &-
  {
    //  [%brkt *]   [%tsgr [%brcn (~(put by q.gen) %% [& & p.gen])] %%]
    //
    u2_noun diz = u2_bt(wir_r, u2_yes, u2_yes, u2_rx(wir_r, p_gen));
    u2_noun ret = u2_bt
      (wir_r,
       c3__tsgr,
       u2_bc(wir_r,
             c3__brcn,
             j2_mcc(Pt4, by, put)(wir_r, q_gen, u2_blip, diz)),
       u2_bc(wir_r, c3__cnbc, u2_blip));

    u2_rz(wir_r, diz);
    return ret;
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
  _open_do_pq(sghp)   //  ~-
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
  _open_do_pq(sgbc)    //  ~$
  {
    return u2_bt(wir_r, c3__sggr, 
                        u2_bq(wir_r, c3__live,
                                     c3__dtsg, u2_blip, u2_rx(wir_r, p_gen)),
                        u2_rx(wir_r, q_gen));
  }
  _open_do_pq(sghx)    //  ~#
  {
    return u2_bt(wir_r, c3__sggr, 
                        u2_bq(wir_r, c3__ping,
                                     c3__dtsg, u2_blip, u2_rx(wir_r, p_gen)),
                        u2_rx(wir_r, q_gen));
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
          (wir_r, u2_bc
                    (wir_r,
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
    static u2_noun
    _ap_snig_in(u2_noun gop)
    {
      if ( u2_nul == gop ) {
        return u2nc(c3__bcts, c3__null);
      } else {
        u2_noun i_gop = u2h(gop);
        u2_noun t_gop = u2t(gop);
        u2_noun res = _ap_snig_in(t_gop);

        if ( u2_no == u2du(i_gop) ) {
          return u2nc(u2nt(c3__dtpt, c3_s2('t','a'), u2k(i_gop)),
                      res);
        }
        else {
          u2_noun pi_gop = u2t(i_gop);

          return u2nt
            (c3__tsls,
             u2nc(u2k(pi_gop), res),
             u2nc
              (c3__brhp,
               u2nq(c3__wtsg,
                    u2nc(u2_nul, 6),
                    u2nc(u2_nul, 7),
                    u2nc(u2nc(u2_nul, 12),
                         u2nq(c3__cnts,
                              u2nc(u2_blip, u2_nul),
                              u2nc(u2nc(u2_nul, 6), u2nc(u2_nul, 13)),
                              u2_nul)))));
        }
      }
    }
    static u2_noun                                                //  produce
    _ap_snig(u2_noun gop)                                         //  retain
    {
      return u2nt
        (c3__ktls,
         u2nc
          (c3__brhp,
           u2nq
            (c3__wtcl,
             u2nc(c3__bcts, c3__flag),
             u2nc(c3__bcts, c3__null),
             u2nc(u2nt(c3__dtpt, c3_s2('t','a'), 0),
                       u2nc(c3__cnbc, u2_blip)))),
          _ap_snig_in(gop));

    }

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
           c3__cnhp,
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
  _open_do_p(smdq)
  {
    return _ap_snig(p_gen);
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
               c3__tsls,
               u2_bc(wir_r, u2_nul, _13), 
               u2_bc
                (wir_r, 
                 c3__brdt,
                 u2_bq
                  (wir_r,
                   c3__cnls,
                   u2_bt(wir_r,
                         c3__tsgr,
                         u2_bc(wir_r, u2_nul, (16)),
                         u2_rx(wir_r, p_gen)),
                   u2_bq
                    (wir_r,
                     c3__cnhp,
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
                       u2_nul)))))));
      }
    }
  }

/* functions
*/
  /** open
  **/
    static u2_weak 
    _open_in(u2_wire wir_r, 
             u2_noun gen)
    {
      u2_noun p_gen, q_gen, r_gen, s_gen;

      if ( u2_yes == u2_stud(gen) ) {
        // printf("studly\n");
        // u2_err(wir_r, "stud m", gen);
        return u2_cm_bail(c3__exit);

        return u2_bt
          (wir_r, c3__cnts, 
                  u2_bc(wir_r, u2_rx(wir_r, gen), u2_nul),
                  u2_nul);
      }
      else switch ( u2_h(gen) ) {
        default: return u2_none;

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
        _open_pq  (tshp);
        _open_pq  (tsls);

        _open_p   (brhp);
        _open_p   (brdt);

        _open_pqrs(clkt);
        _open_pqr (clls);
        _open_p   (cltr);
        _open_p   (clsg);

        _open_pqr (wtdt);
        _open_pq  (wtgl);
        _open_p   (wtzp);
        _open_p   (wtbr);
        _open_p   (wtpm);
        _open_pqr (wtls);
        _open_pqr (wtsg);
        // _open_pq  (wthp);
        _open_pq  (wtgr);

        _open_pqr (cnbr);
        _open_pq  (cncl);
        _open_pq  (cndt);
        _open_pqrs(cnkt);
        _open_pq  (cnhp);
        _open_pqr (cnls);
        _open_pqr (cnsg);

        _open_pq  (brkt);

        _open_pq  (sgts);
        _open_pq  (sgbr);
        _open_pq  (sgcl);
        _open_pq  (sggl);
        _open_pq  (sghp);
        _open_pq  (sgdt);
        _open_pq  (sgbc);
        _open_pq  (sghx);
        _open_p   (sgkt);
        _open_pqrs(sgcn);
        _open_pq  (sgls);
        _open_pq  (sgpm);
        _open_pq  (sgsg);

        _open_pq  (smcl);
        _open_p   (smdq);
        _open_pq  (smsg);
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

        case c3__cnbc: {
          return u2_bc(wir_r, u2_rx(wir_r, u2_t(gen)), u2_nul);
        }
        case c3__cnhx: {
          return u2_rx(wir_r, u2_t(gen));
        }
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
                  u2_bt(wir_r, c3__ktts, 'a', 
                                         u2_rx(wir_r, p_gen)),
                  u2_bt(wir_r, 
                        c3__tsls,
                        u2_bt(wir_r, c3__ktts, 'b', 
                                     u2_bt(wir_r, c3__tsgr,
                                                  u2_bc(wir_r, _0, _2),
                                                  u2_rx(wir_r, q_gen))),
                        u2_bt(wir_r, 
                              c3__wtgr,
                              u2_bq(wir_r, c3__cnhp, 
                                           u2_bc(wir_r, c3__cnbc, 'a'), 
                                           u2_bc(wir_r, c3__cnbc, 'b'), u2_nul),
                                           u2_bc(wir_r, c3__cnbc, 'b'))));
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
                            c3__brhp,
                            u2_bt(wir_r, 
                                  c3__ktls, 
                                  u2_bc(wir_r, c3__bctr, 
                                               u2_bc(wir_r, c3__cnbc, 'a')),
                                  u2_bt(wir_r, 
                                        c3__tsls,
                                        u2_bt(wir_r, 
                                              c3__ktts,
                                              'c',
                                              u2_bq(wir_r,
                                                    c3__cnhp,
                                                    u2_bc(wir_r, c3__cnbc, 'a'),
                                                    u2_bc(wir_r, c3__cnbc, 'b'),
                                                    u2_nul)),
                                        u2_bq(wir_r,
                                              c3__wtcl,
                                              u2_bt(wir_r, 
                                                    c3__dtts, 
                                                    u2_bc(wir_r, c3__cnbc, 'b'),
                                                    u2_bc(wir_r, c3__cnbc, 
                                                                 'c')),
                                              u2_bc(wir_r, c3__cnbc, 'c'),
                                              u2_bt(wir_r,
                                                    c3__cnts,
                                                    u2_bc(wir_r, u2_blip,
                                                                 u2_nul),
                                                    u2_bc(wir_r,
                                                          u2_bc(wir_r, 
                                                                u2_bc(wir_r, 
                                                                      c3__cnbc,
                                                                      'b'), 
                                                                u2_bc(wir_r, 
                                                                      c3__cnbc, 
                                                                      'c')),
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

      if ( u2_yes == u2_dust(u2_h(gen)) ) {
        return u2_bt(wir_r, u2_yes, 
                            u2_rx(wir_r, u2_h(gen)),
                            u2_rx(wir_r, u2_t(gen)));
      }
      else switch ( u2_h(gen) ) {
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
        case c3__clhp: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
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
        u2_noun voq = _open_in(wir_r, gen);

        if ( u2_none == voq ) {
          return u2_bc(wir_r, u2_no, u2_rx(wir_r, gen));
        }
        else if ( u2_yes == u2_sing(voq, gen) ) {
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

/* boilerplate
*/
  u2_noun                                                         //  transfer
  j2_mbi(Pt6, ap)(u2_wire wir_r,
                  u2_noun ter,                                    //  retain
                  u2_noun gen)                                    //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, ter, "ap");
    
    if ( u2_none == hoc ) {
      return u2_cm_bail(c3__fail);
    } else {
      u2_noun gat = u2_nk_soft(wir_r, u2_rx(wir_r, ter), hoc);
      u2_noun cor = u2_rl_molt(wir_r, gat,
                                      u2_cw_sam, u2_rx(wir_r, gen),
                                      0);

      u2_rz(wir_r, hoc);
      u2_rz(wir_r, gat);
      return cor;
    }
  }

/* open
*/
  static u2_noun 
  _ap_open_n(u2_wire wir_r,
             u2_noun ter,
             u2_noun gen)
  {
    u2_noun cor = j2_mbi(Pt6, ap)(wir_r, ter, gen); 
    u2_weak hoc = u2_ds_look(wir_r, cor, "open");

    if ( u2_none == hoc ) {
      return u2_cm_bail(c3__fail);
    } else {
      u2_noun pro = u2_nk_soft(wir_r, cor, hoc);

      u2_rz(wir_r, hoc);
      return pro;
    }
  } 
  static u2_noun                                                  //  transfer
  _ap_open_l(u2_wire wir_r,
             u2_noun ter,                                         //  retain
             u2_noun gen)                                         //  retain
  {
    u2_weak pro = _open_in(wir_r, gen);

    if ( u2_none != pro ) {
      return pro;
    } else {
      u2_ho_jet *jet_j = &j2_mbj(Pt6, ap)[_ap_jet_open];
      
      c3_assert(jet_j->sat_s & u2_jet_live);
      jet_j->sat_s &= ~u2_jet_live;
      {
        pro = _ap_open_n(wir_r, ter, gen);
        // u2_err(wir_r, "gen", gen);
        // u2_err(wir_r, "pro", pro);
      }
      jet_j->sat_s |= u2_jet_live;

      return pro;
    }
  }
 
  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ap, open)(u2_wire wir_r,
                        u2_noun ter,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mbj(Pt6, ap)[_ap_jet_open];

    if ( !(jet_j->sat_s & u2_jet_live) ) {
      return _ap_open_n(wir_r, ter, gen);
    }
    else {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return _ap_open_l(wir_r, ter, gen);
      }
      else {
        c3_m fun_m   = c3__open;
        u2_noun pro = u2_rl_find(wir_r, fun_m, gen);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = _ap_open_l(wir_r, ter, gen);
        }
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
      u2_noun ter = u2_frag(u2_cw_con, cor);

      return j2_mcy(Pt6, ap, open)(wir_r, ter, gen);
    }
  }

/* structures
*/
  u2_ho_driver 
  j2_mbd(Pt6, ap)[] = {
    { }
  };

