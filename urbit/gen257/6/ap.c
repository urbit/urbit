/* j/6/ap.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/** forward declares
**/
    u2_noun
    j2_mcy(Pt6, ap, sift)(u2_wire, u2_noun);

    u2_noun
    j2_mcy(Pt6, ap, modl)(u2_wire, u2_noun, u2_noun);

    u2_noun
    j2_mcy(Pt6, ap, grow)(u2_wire, u2_noun, u2_noun);

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

/***
****
***/
  _open_do_pq(bndl)   //  =<
  {
    return u2_bt(wir_r, c3__bnld, u2_rx(wir_r, q_gen), 
                                  u2_rx(wir_r, p_gen));
  }

  _open_do_pq(bndp)   //  =-
  {
    return u2_bt(wir_r, c3__bnpd, u2_rx(wir_r, q_gen), 
                                  u2_rx(wir_r, p_gen));
  }

  _open_do_pq(bnpd)   //  =+
  {
    return u2_bt
      (wir_r, c3__bnld,
              u2_bt(wir_r, c3__dgdp, 
                           u2_bc(wir_r, u2_nul, _1),
                           u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
/***
****
***/
  _open_do_pq(brbn)   //  |=
  {
    return u2_bt
      (wir_r, c3__pmbn,
              u2_rx(wir_r, p_gen), 
              u2_bt(wir_r,
                    u2_bq(wir_r, u2_blip, u2_yes, u2_yes, u2_rx(wir_r, q_gen)),
                    u2_nul, 
                    u2_nul));
  }
  _open_do_p(brcs)    //  |?
  {
    if ( (u2_nul == p_gen) ) {
      return u2_bc(wir_r, c3__zpzp, u2_nul);
    }
    else {
      u2_noun ip_gen = u2_h(p_gen);

      return u2_bt
        (wir_r,
         c3__brbn,
         u2_bt(wir_r, c3__ktdp, u2_bc(wir_r, c3__tmbn, c3__noun),
                                u2_rx(wir_r, ip_gen)),
         u2_bt(wir_r, c3__cstr,
                      u2_bc(wir_r, u2_nul, _5),
                      u2_rx(wir_r, p_gen)));
    }
  }
  _open_do_p(brdp)    //  |-
  {
    return u2_bt
      (wir_r, c3__bnld,
              u2_bc(wir_r, c3__brdt, u2_rx(wir_r, p_gen)),
              u2_blip);
  }
  _open_do_pq(brdg)    //  |:
  {
    return u2_bt
      (wir_r, c3__pmdg,
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
  _open_do_p(brmt)   //  |.
  {
    return u2_bc
      (wir_r, c3__pmmt,
              u2_bt(wir_r,
                    u2_bq(wir_r, u2_blip, u2_yes, u2_yes, u2_rx(wir_r, p_gen)),
                    u2_nul, 
                    u2_nul));
  }
  _open_do_pq(brpd)   //  |+
  {
    return u2_bt
      (wir_r, c3__pmpd,
              u2_rx(wir_r, p_gen), 
              u2_bt(wir_r,
                    u2_bq(wir_r, u2_blip, u2_yes, u2_yes, u2_rx(wir_r, q_gen)),
                    u2_nul, 
                    u2_nul));
  }
  _open_do_pq(brtr)   //  |*
  {
    return u2_bt
      (wir_r, c3__pmbn,
              u2_rx(wir_r, p_gen), 
              u2_bt(wir_r,
                    u2_bq(wir_r, u2_blip, u2_yes, u2_no, u2_rx(wir_r, q_gen)),
                    u2_nul, 
                    u2_nul));
  }
  _open_do_pq(brzp)   //  |!
  {
    return u2_bt
      (wir_r, c3__bnld,
              u2_bc(wir_r, c3__zpmt, u2_nul),
              u2_bt
                (wir_r, c3__brbn,
                        u2_rx(wir_r, p_gen),
                        u2_rx(wir_r, q_gen)));
  }
/***
****
***/
  _open_do_p(csbr)    //  ?|
  {
    if ( (u2_nul == p_gen) ) {
      return u2_bc(wir_r, c3__dtsg, u2_no);
    }
    else {
      u2_noun ip_gen = u2_h(p_gen);
      u2_noun tp_gen = u2_t(p_gen);

      return u2_bq
        (wir_r, c3__csdg,
                u2_rx(wir_r, ip_gen),
                u2_bc(wir_r, c3__dtsg, u2_yes),
                _open_in_csbr(wir_r, tp_gen));
    }
  }
  _open_do_pq(csdl)   //  ?<
  {
    return u2_bq
      (wir_r, c3__csdg,
            u2_rx(wir_r, p_gen),
            u2_bc(wir_r, c3__zpzp, u2_nul),
            u2_rx(wir_r, q_gen));
  }
  _open_do_pqr(csdt)  //  ?.
  {
    return u2_bq(wir_r, c3__csdg, 
                        u2_rx(wir_r, p_gen), 
                        u2_rx(wir_r, r_gen), 
                        u2_rx(wir_r, q_gen));
  }
  _open_do_pq(csld)   //  ?>
  {
    return u2_bq
      (wir_r, c3__csdg, 
              u2_rx(wir_r, p_gen), 
              u2_rx(wir_r, q_gen), 
              u2_bc(wir_r, c3__zpzp, u2_nul));
  }
  _open_do_pq(csdp)   //  ?-
  {
    if ( (u2_nul == q_gen) ) {
      return u2_bc(wir_r, c3__zpzp, u2_nul);
    }
    else {
      u2_noun iq_gen = u2_h(q_gen);
      u2_noun tq_gen = u2_t(q_gen);
      u2_noun piq_gen = u2_h(iq_gen);
      u2_noun qiq_gen = u2_t(iq_gen);

      return u2_bq
        (wir_r, 
         c3__csdg,
         u2_bt(wir_r, c3__csbn, 
                      u2_rx(wir_r, piq_gen), 
                      u2_rx(wir_r, p_gen)),
         u2_bt
          (wir_r, c3__bnld,
                  u2_bq(wir_r, c3__mtbn,
                               u2_nul,
                               u2_bc(wir_r, 
                                     u2_rx(wir_r, p_gen),               
                                     u2_bt(wir_r, c3__ktpd,
                                                  u2_rx(wir_r, piq_gen),
                                                  u2_rx(wir_r, p_gen))),
                               u2_nul),
                  u2_rx(wir_r, qiq_gen)),
        _open_in_csdp(wir_r, p_gen, tq_gen));
    }
  }
  _open_do_p(cspm)    //  ?&
  {
    if ( (u2_nul == p_gen) ) {
      return u2_bc(wir_r, c3__dtsg, u2_yes);
    }
    else {
      u2_noun ip_gen = u2_h(p_gen);
      u2_noun tp_gen = u2_t(p_gen);

      return u2_bq
        (wir_r, c3__csdg,
                u2_rx(wir_r, ip_gen),
                _open_in_cspm(wir_r, tp_gen),
                u2_bc(wir_r, c3__dtsg, u2_no));
    }
  }
  _open_do_pq(cstr)   //  ?*
  {
    if ( (u2_nul == q_gen) ) {
      return u2_bc(wir_r, c3__zpzp, u2_nul);
    }
    else {
      u2_noun iq_gen = u2_h(q_gen);
      u2_noun tq_gen = u2_t(q_gen);
      u2_noun rak = j2_mcy(Pt6, ap, rake)(wir_r, p_gen);
      u2_noun sif = j2_mcy(Pt6, ap, sift)(wir_r, iq_gen);
      u2_noun mol = j2_mcy(Pt6, ap, modl)(wir_r, iq_gen, rak);

      return u2_bq
        (wir_r,
         c3__csdg,
         u2_bt(wir_r, c3__csbn,
                      sif,
                      u2_bt(wir_r, c3__mtbn, rak, u2_nul)),
         mol,
         _open_in_cstr(wir_r, p_gen, tq_gen));
    }
  }
#if 0
  _open_do_pqr(cssg)  //  ?~
  {
    return u2_bt
      (wir_r,
       c3__csdp,
       u2_rx(wir_r, p_gen),
       u2_bt
        (wir_r,
         u2_bc(wir_r, u2_bc(wir_r, c3__dtsg, u2_nul), u2_rx(wir_r, q_gen)),
         u2_bc(wir_r, u2_bc(wir_r, c3__tmbn, c3__cell), u2_rx(wir_r, r_gen)),
         u2_nul));
  }
#endif
  _open_do_p(cszp)    //  ?!
  {
    return u2_bq
      (wir_r, c3__csdg,
              u2_rx(wir_r, p_gen),
              u2_bc(wir_r, c3__dtsg, u2_no),
              u2_bc(wir_r, c3__dtsg, u2_yes));
  }
/***
****
***/
  _open_do_pqrs(dgkt) //  :^
  {
    return u2_bt
      (wir_r, c3__dgdp,
              u2_rx(wir_r, p_gen),
              u2_bt
                (wir_r, c3__dgdp,
                        u2_rx(wir_r, q_gen),
                        u2_bt(wir_r, c3__dgdp, u2_rx(wir_r, r_gen), 
                                               u2_rx(wir_r, s_gen))));
  }
  _open_do_pqr(dgpd)  //  :+
  {
    return u2_bt
      (wir_r, c3__dgdp,
              u2_rx(wir_r, p_gen),
              u2_bt(wir_r, c3__dgdp, u2_rx(wir_r, q_gen), 
                                     u2_rx(wir_r, r_gen)));
  }
  _open_do_p(dgsg)    //  :~
  {
    if ( (u2_nul == p_gen) ) {
      return u2_bc(wir_r, c3__dtsg, u2_nul);
    }
    else {
      u2_noun ip_gen = u2_h(p_gen);
      u2_noun tp_gen = u2_t(p_gen);

      return u2_bt(wir_r, c3__dgdp, 
                          u2_rx(wir_r, ip_gen), 
                          _open_in_dgsg(wir_r, tp_gen));
    }
  }
  _open_do_p(dgtr)    //  :*
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
          (wir_r, c3__dgdp, u2_rx(wir_r, ip_gen), 
                            _open_in_dgtr(wir_r, tp_gen));
      }
    }
  }
/***
****
***/
  _open_do_pq(ktdl)   //  ^<
  {
    return 
      u2_bt
        (wir_r, c3__bndl,
                u2_bt(wir_r, c3__ktld, 
                             u2_rx(wir_r, p_gen), 
                             u2_bc(wir_r, u2_nul, _1)),
                u2_rx(wir_r, q_gen));
  }
/***
****
***/
    static u2_noun
    _mtbr_a(u2_wire wir_r,
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
                        u2_bt(wir_r, c3__bnld, 
                                     u2_bc(wir_r, u2_nul, _2),
                                     u2_rx(wir_r, qir_gen))),
                  _mtbr_a(wir_r, tr_gen));
      }
    }
  _open_do_pqr(mtbr)  //  %|
  {
    u2_noun vop = u2_bc(wir_r, u2_bc(wir_r, u2_nul, _3), u2_nul);
    u2_noun zil = j2_mbc(Pt2, weld)(wir_r, p_gen, vop);

    u2_rz(wir_r, vop);

    return u2_bt
      (wir_r, c3__bnpd,
              u2_rx(wir_r, q_gen),
              u2_bt
                (wir_r, c3__mtbn, zil, _mtbr_a(wir_r, r_gen)));
  }
  _open_do_pq(mtdg)   //  %:
  {
    return u2_bq
      (wir_r, c3__mtsg,
            u2_bc(wir_r, u2_blip, u2_nul),
            u2_rx(wir_r, p_gen),
            u2_rx(wir_r, q_gen));
  }
  _open_do_pq(mtdt)   //  %.
  {
    return u2_bt
      (wir_r, c3__mtdp, 
              u2_rx(wir_r, q_gen), 
              u2_bc(wir_r, u2_rx(wir_r, p_gen), u2_nul));
  }
  _open_do_pqrs(mtkt) //  %^
  {
    return u2_bq
      (wir_r, c3__mtdp, 
              u2_rx(wir_r, p_gen),
              u2_rx(wir_r, q_gen),
              u2_bt(wir_r, u2_rx(wir_r, r_gen), 
                           u2_rx(wir_r, s_gen), 
                           u2_nul));
  }
  _open_do_pq(mtdp)   //  %-
  {
    if ( (u2_nul == q_gen) ) {
      return u2_bt
        (wir_r, c3__bnld, 
                u2_rx(wir_r, p_gen),
                u2_blip);
    } else {
      return u2_bq(wir_r, c3__mtdg, 
                          u2_rx(wir_r, p_gen), 
                          c3__dgtr, 
                          u2_rx(wir_r, q_gen));
    }
  }
  _open_do_pqr(mtpd)  //  %+
  {
    return u2_bc
      (wir_r, c3__mtdp, 
              u2_bq(wir_r, u2_rx(wir_r, p_gen), 
                           u2_rx(wir_r, q_gen), 
                           u2_rx(wir_r, r_gen), 
                           u2_nul));
  }
  _open_do_p(mttr)    //  %*
  {
    return u2_bq(wir_r, c3__ktsg,
                        c3__bnld,
                        u2_rx(wir_r, p_gen),
                        u2_bt(wir_r, c3__mtbn, 
                                     u2_bo(wir_r, u2_blip),
                                     u2_nul));
  }
  _open_do_pqr(mtsg)  //  %~
  {
    return u2_bq
      (wir_r, c3__mtbr,
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
  _open_do_pq(pmbn)   //  &=
  {
    return u2_bt
      (wir_r, c3__bnpd, 
              u2_rx(wir_r, p_gen),
              u2_bc(wir_r, c3__pmdt, u2_rx(wir_r, q_gen)));
  }
  _open_do_pq(pmdp)   //  &-
  {
    return u2_bt
      (wir_r, c3__bnld, 
              u2_bc(wir_r, c3__pmdt, u2_rx(wir_r, q_gen)),
              u2_rx(wir_r, p_gen));
  }
  _open_do_pq(pmzp)   //  |!
  {
    return u2_bt
      (wir_r, c3__bnld,
              u2_bc(wir_r, c3__zpmt, u2_nul),
              u2_bt
                (wir_r, c3__pmbn,
                        u2_rx(wir_r, p_gen),
                        u2_rx(wir_r, q_gen)));
  }
/***
****
***/
  _open_do_pq(cbdg)   //  +:
  {
    u2_noun tog, bey, wor, hoo, ret;

    tog = u2_bc(wir_r, u2_bc(wir_r, u2_nul, _5), u2_nul);
    bey = j2_mcy(Pt6, ap, grow)(wir_r, p_gen, tog);
    wor = u2_bt(wir_r, u2_yes, u2_yes, bey);
    hoo = j2_mcc(Pt4, by, put)(wir_r, q_gen, u2_nul, wor);

    ret = u2_bt(wir_r, c3__pmbn, u2_bc(wir_r, c3__tmbn, c3__noun), hoo);

    u2_rz(wir_r, wor); u2_rz(wir_r, tog);

    return ret;
  }
/***
****
***/
  _open_do_pq(sgbn)   //  ~=
  {
    return u2_bt
      (wir_r, c3__sgld,
              u2_bc(wir_r, c3__germ, u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
  _open_do_pq(sgbr)   //  ~|
  {
    return u2_bt
      (wir_r, c3__sgld,
              u2_bc(wir_r, c3__bean, u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
  _open_do_pq(sgdg)   //  ~:
  {
    return u2_bt
      (wir_r, c3__sgld,
              u2_bt(wir_r, c3__bank, c3__dtsg, u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
  _open_do_pq(sgdl)   //  ~>
  {
    return u2_bt
      (wir_r, c3__bndl, 
              u2_bq(wir_r, c3__sgld, u2_rx(wir_r, p_gen), u2_nul, _1),
              u2_rx(wir_r, q_gen));
  }
  _open_do_pq(sgdp)   //  ~-
  {
    return u2_bt
      (wir_r, c3__sgld,
              u2_bt(wir_r, c3__sole, c3__dtsg, u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
  _open_do_pq(sgdt)   //  ~.
  {
    return u2_bc
      (wir_r, c3__sgmt, 
              u2_bq(wir_r, 
                    u2_rx(wir_r, p_gen),
                    u2_bc(wir_r, u2_nul, _4),
                    u2_nul,
                    u2_rx(wir_r, q_gen)));
  }
  _open_do_p(sgdx)    //  ~#
  {
    return u2_bt(wir_r, c3__sgld, c3__ping, u2_rx(wir_r, p_gen));
  }
  _open_do_p(sgkt)    //  ~^
  {
    return u2_bc(wir_r, c3__keep, u2_rx(wir_r, p_gen));
  }
    static u2_noun
    _sgmt_a(u2_wire wir_r,
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
                     c3__dgdp,
                     u2_bc(wir_r, c3__dtsg, u2_rx(wir_r, pir_gen)),
                     u2_bc(wir_r, c3__zpbn, u2_rx(wir_r, qir_gen))),
                  _sgmt_a(wir_r, tr_gen, nob));
      }
    }
  _open_do_pqrs(sgmt) //  ~%
  {
    return u2_bt
      (wir_r, c3__sgdl,
              u2_bq
                (wir_r, c3__fast,
                        c3__dgpd,
                        u2_bc(wir_r, c3__dtsg, u2_rx(wir_r, p_gen)),
                        u2_bt
                          (wir_r, u2_bc(wir_r, c3__zpbn, u2_rx(wir_r, q_gen)),
                                  c3__dgsg,
                                  _sgmt_a(wir_r, r_gen, u2_nul))),
              u2_rx(wir_r, s_gen));
  }
  _open_do_pq(sgpd)   //  ~+
  {
    return u2_bt
      (wir_r, c3__sgld,
              u2_bt(wir_r, c3__memo, c3__dtsg, u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
  _open_do_pq(sgpm)   //  ~&
  {
    return u2_bt
      (wir_r, c3__sgld,
              u2_bc(wir_r, c3__loaf, u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
  _open_do_pq(sgsg)   //  ~~
  {
    return u2_bt
      (wir_r, c3__sgld,
              u2_bt(wir_r, c3__mean, c3__brdt, u2_rx(wir_r, p_gen)),
              u2_rx(wir_r, q_gen));
  }
/***
****
***/
  _open_do_p(tmbn)    //  ;=
  {
    if ( u2_yes == u2_sing(c3__atom, p_gen) ) {
      return u2_bt
        (wir_r, c3__ktdp, 
              u2_bt(wir_r, c3__dtpd, c3__dtsg, _0),
              u2_bc(wir_r, c3__dtsg, _0));
    }
    else if ( u2_yes == u2_sing(c3__noun, p_gen) ) {
      u2_noun dud = u2_bc(wir_r, c3__dtsg, u2_nul);

      return u2_bt
        (wir_r, c3__ktdp, 
                u2_bt(wir_r, c3__dttr, 
                             u2_rx(wir_r, dud), 
                             u2_rx(wir_r, dud)),
                dud);
    }
    else if ( u2_yes == u2_sing(c3__cell, p_gen) ) {
      return u2_bt
        (wir_r, c3__dgdp,
              u2_bc(wir_r, c3__tmbn, c3__noun),
              u2_bc(wir_r, c3__tmbn, c3__noun));
    }
    else if ( u2_yes == u2_sing(c3__flag, p_gen) ) {
      return u2_bt
        (wir_r, c3__ktdp,
              u2_bt(wir_r, c3__dtcs, c3__dtsg, _0),
              u2_bc(wir_r, c3__dtsg, u2_yes));
    }
    else if ( u2_yes == u2_sing(c3__null, p_gen) ) {
        return u2_bc(wir_r, c3__dtsg, u2_nul);
    }
    else return u2_bl_bail(wir_r, c3__fail);
  }
    static u2_noun                                                //  produce
    _tmdg_in(u2_wire wir_r,
             u2_noun q_gen)                                       //  retain
    {
      u2_noun hq_gen = u2_h(q_gen);
      u2_noun tq_gen = u2_t(q_gen);

      if ( u2_no == u2_dust(tq_gen) ) {
        return u2_rx(wir_r, hq_gen);
      } else {
        return u2_bc
          (wir_r,
           c3__mtdp,
           u2_bq(wir_r, 
                 u2_bc(wir_r, u2_nul, _3),
                 u2_bt(wir_r, c3__bnld,
                              u2_bc(wir_r, u2_nul, _2),
                              u2_rx(wir_r, hq_gen)),
                 _tmdg_in(wir_r, tq_gen),
                 u2_nul));
      }
    }
  _open_do_pq(tmdg)
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
         c3__bnpd,
         u2_rx(wir_r, p_gen),
         _tmdg_in(wir_r, q_gen));
    }
  }
  _open_do_p(tmdp)    //  ;-
  {
    return j2_mcy(Pt6, ap, sift)(wir_r, p_gen);
  }
  _open_do_pq(tmpd)    //  ;+
  {
    u2_noun rak = j2_mcy(Pt6, ap, rake)(wir_r, q_gen);
    u2_noun pro = j2_mcy(Pt6, ap, modl)(wir_r, p_gen, rak);

    u2_rl_lose(wir_r, rak);
    return pro;
  }
  _open_do_pq(tmsg)   //  ;~
  {
    if ( (u2_nul == q_gen) ) {
      return u2_bl_error(wir_r, "open-tmsg");
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
           c3__bnpd,
           _open_in_tmsg(wir_r, p_gen, tq_gen),
           u2_bt
            (wir_r,
             c3__bnpd,
             u2_bt(wir_r, c3__bnld,
                        u2_bc(wir_r, u2_nul, _2),
                        u2_rx(wir_r, iq_gen)),
             u2_bt
              (wir_r, 
               c3__brbn,
               u2_bc(wir_r, u2_nul, _13), 
               u2_bq
                (wir_r,
                 c3__mtpd,
                 u2_bt(wir_r,
                       c3__bnld,
                       u2_bc(wir_r, u2_nul, (16)),
                       u2_rx(wir_r, p_gen)),
                 u2_bq
                  (wir_r,
                   c3__mtdp,
                   u2_bc(wir_r, u2_nul, _9),
                   u2_bc(wir_r, u2_nul, _5),
                   u2_nul),
                u2_bt
                  (wir_r,
                   c3__mtbn,
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
         c3__csdg, 
         u2_bt(wir_r, c3__csbn, 
                      u2_rx(wir_r, gen), 
                      u2_bt(wir_r, c3__mtbn, u2_rx(wir_r, hep), u2_nul)),
         u2_bt(wir_r, c3__mtbn, u2_rx(wir_r, hep), u2_nul),
         u2_rx(wir_r, dug));
    } 

  /** grow
  **/
    u2_noun
    j2_mcy(Pt6, ap, grow)(u2_wire wir_r,
                          u2_noun gen,
                          u2_noun hep)
    {
      u2_noun p_gen, q_gen;

      if ( u2_no == u2_dust(gen) ) {
        goto plain;
      } else switch ( u2_h(gen) ) {
        default: goto plain;

        case c3__dgdp:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          u2_noun hed = u2_bc(wir_r, u2_bc(wir_r, u2_nul, _2), 
                                     u2_rx(wir_r, hep));
          u2_noun tal = u2_bc(wir_r, u2_bc(wir_r, u2_nul, _3), 
                                     u2_rx(wir_r, hep));
          u2_noun lag = u2_bt
            (wir_r, c3__dgdp,
                    j2_mcy(Pt6, ap, grow)(wir_r, p_gen, hed),
                    j2_mcy(Pt6, ap, grow)(wir_r, q_gen, tal));

          u2_rl_lose(wir_r, hed); 
          u2_rl_lose(wir_r, tal);
          return lag;
        }
        case c3__ktbn:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          return u2_bt
            (wir_r, c3__ktbn, 
                    u2_rx(wir_r, p_gen), 
                    j2_mcy(Pt6, ap, grow)(wir_r, q_gen, hep));
        }
        case c3__ktdp:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          return j2_mcy(Pt6, ap, modl)(wir_r, p_gen, hep);
        }
#if 0
        case c3__ktdp:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          return u2_bt
            (wir_r, c3__ktdp,
                    u2_rx(wir_r, p_gen), 
                    j2_mcy(Pt6, ap, grow)(wir_r, q_gen, hep));
        }
#endif
        case c3__mttr:  p_gen = u2_t(gen);
        {
          return u2_bq
            (wir_r, c3__mtdp,
                    u2_rx(wir_r, p_gen),
                    u2_bt(wir_r, c3__mtbn, u2_rx(wir_r, hep), u2_nul),
                    u2_nul);
        }
        case c3__mtbn:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          if ( u2_nul == q_gen ) {
            return u2_bq
              (wir_r, c3__mtdp,
                      u2_rx(wir_r, p_gen),
                      u2_bc(wir_r, 
                            u2_bc(wir_r, 
                                  u2_bc(wir_r, u2_nul, _5),
                                  u2_bt(wir_r, c3__mtbn, 
                                               u2_rx(wir_r, hep), 
                                               u2_nul)),
                            u2_nul),
                      u2_rx(wir_r, q_gen));
          } else {
            goto plain;
          }
        }
        case c3__mtdp:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          if ( u2_nul == q_gen ) {
            return u2_bq
              (wir_r, c3__mtdp,
                      u2_rx(wir_r, p_gen),
                      u2_bt(wir_r, c3__mtbn, u2_rx(wir_r, hep), u2_nul),
                      u2_rx(wir_r, q_gen));
          } else {
            goto plain;
          }
        }
        case c3__zpcb:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          return u2_bt
            (wir_r, c3__zpcb, 
                    u2_rx(wir_r, p_gen), 
                    j2_mcy(Pt6, ap, grow)(wir_r, q_gen, hep));
        }
        case c3__zpdx:  p_gen = u2_t(gen);
        {
          return u2_bc
            (wir_r, c3__zpcb, j2_mcy(Pt6, ap, grow)(wir_r, q_gen, hep));
        }
      }

      plain: {
        u2_noun bog = j2_mcy(Pt6, ap, open)(wir_r, gen);
        u2_noun ret;

        if ( u2_no == u2_sing(bog, gen) ) {
          ret = j2_mcy(Pt6, ap, grow)(wir_r, bog, hep);
        }
        else {
          u2_noun dug = u2_bc(wir_r, c3__zpzp, u2_nul);

          ret = j2_mcy(Pt6, ap, pick)(wir_r, bog, hep, dug);
          u2_rz(wir_r, dug);
        }
        u2_rl_lose(wir_r, bog);
        return ret;
      }
    }
    u2_noun                                                       //  transfer
    j2_mc(Pt6, ap, grow)(u2_wire wir_r, 
                         u2_noun cor)                             //  retain
    {
      u2_noun hep, van, gen;

      if ( (u2_no == u2_mean(cor, u2_cw_con, &van, u2_cw_sam, &hep, 0)) ||
           (u2_none == (gen = u2_frag(u2_cw_sam, van))) ) {
        return u2_bl_bail(wir_r, c3__fail);
      } else {
        return j2_mcy(Pt6, ap, grow)(wir_r, gen, hep);
      }
    }

  /** modl
  **/
    u2_noun
    j2_mcy(Pt6, ap, modl)(u2_wire wir_r,
                          u2_noun gen,
                          u2_noun hep)
    {
      u2_noun p_gen, q_gen;

      if ( u2_no == u2_dust(gen) ) {
        goto plain;
      } else switch ( u2_h(gen) ) {
        default: goto plain;

        case c3__dgdp:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          u2_noun hed = u2_bc(wir_r, u2_bc(wir_r, u2_nul, _2), 
                                     u2_rx(wir_r, hep));
          u2_noun tal = u2_bc(wir_r, u2_bc(wir_r, u2_nul, _3), 
                                     u2_rx(wir_r, hep));
          u2_noun lag = u2_bt
            (wir_r, c3__dgdp,
                    j2_mcy(Pt6, ap, modl)(wir_r, p_gen, hed),
                    j2_mcy(Pt6, ap, modl)(wir_r, q_gen, tal));

          u2_rl_lose(wir_r, hed); 
          u2_rl_lose(wir_r, tal);
          return lag;
        }
        case c3__ktbn:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          return u2_bt
            (wir_r, c3__ktbn, 
                    u2_rx(wir_r, p_gen), 
                    j2_mcy(Pt6, ap, modl)(wir_r, q_gen, hep));
        }
        case c3__ktdp:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          return j2_mcy(Pt6, ap, modl)(wir_r, p_gen, hep);
        }
        case c3__ktpd:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          return u2_bt
            (wir_r, c3__ktpd, 
                    u2_rx(wir_r, p_gen), 
                    j2_mcy(Pt6, ap, modl)(wir_r, q_gen, hep));
        }
        case c3__mttr:  p_gen = u2_t(gen);
        {
          return u2_bq
            (wir_r, c3__mtdp,
                    u2_rx(wir_r, p_gen),
                    u2_bt(wir_r, c3__mtbn, u2_rx(wir_r, hep), u2_nul),
                    u2_nul);
        }
        case c3__mtdp:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          if ( u2_nul == q_gen ) {
            return u2_bq
              (wir_r, c3__mtdp,
                      u2_rx(wir_r, p_gen),
                      u2_bt(wir_r, c3__mtbn, u2_rx(wir_r, hep), u2_nul),
                      u2_rx(wir_r, q_gen));
          } else {
            goto plain;
          }
        }
        case c3__zpcb:  if ( u2_no == u2_as_cell(u2_t(gen), &p_gen, &q_gen) )
                          return u2_bl_bail(wir_r, c3__fail); else
        {
          return u2_bt
            (wir_r, c3__zpcb, 
                    u2_rx(wir_r, p_gen), 
                    j2_mcy(Pt6, ap, modl)(wir_r, q_gen, hep));
        }
        case c3__zpdx:  p_gen = u2_t(gen);
        {
          return u2_bc
            (wir_r, c3__zpcb, j2_mcy(Pt6, ap, modl)(wir_r, q_gen, hep));
        }
      }

      plain: {
        u2_noun bog = j2_mcy(Pt6, ap, open)(wir_r, gen);
        u2_noun gad;

        if ( u2_no == u2_sing(bog, gen) ) {
          gad = j2_mcy(Pt6, ap, modl)(wir_r, bog, hep);
        }
        else {
          gad = u2_bt
            (wir_r,
             c3__csld, 
             u2_bt(wir_r, c3__csbn, 
                          u2_rx(wir_r, gen), 
                          u2_bt(wir_r, c3__mtbn, u2_rx(wir_r, hep), u2_nul)),
                   u2_bt(wir_r, c3__mtbn, u2_rx(wir_r, hep), u2_nul));
        }
        u2_rl_lose(wir_r, bog);
        return gad;
      }
    }
    u2_noun                                                       //  transfer
    j2_mc(Pt6, ap, modl)(u2_wire wir_r, 
                         u2_noun cor)                             //  retain
    {
      u2_noun hep, van, gen;

      if ( (u2_no == u2_mean(cor, u2_cw_con, &van, u2_cw_sam, &hep, 0)) ||
           (u2_none == (gen = u2_frag(u2_cw_sam, van))) ) {
        return u2_bl_bail(wir_r, c3__fail);
      } else {
        return j2_mcy(Pt6, ap, modl)(wir_r, gen, hep);
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
          (wir_r, c3__mtbn, 
                  u2_bc(wir_r, u2_rx(wir_r, gen), u2_nul),
                  u2_nul);
      }
      else switch ( u2_h(gen) ) {
        default: return u2_rx(wir_r, gen);

        case u2_nul: {
          return u2_bt
            (wir_r, c3__mtbn, 
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

        _open_pq  (bndl);
        _open_pq  (bndp);
        _open_pq  (bnpd);

        _open_pq  (brbn);
        _open_p   (brcs);
        _open_p   (brdp);
        _open_pq  (brdg);
        _open_p   (brdt);
        _open_p   (brmt);
        _open_pq  (brpd);
        _open_pq  (brtr);
        _open_pq  (brzp);

        _open_pqrs(dgkt);
        _open_pqr (dgpd);
        _open_p   (dgtr);
        _open_p   (dgsg);

        _open_pq  (cbdg);

        _open_p   (csbr);
        _open_pqr (csdt);
        _open_pq  (csdl);
        _open_p   (cszp);
        _open_pq  (cstr);
        _open_p   (cspm);
        _open_pq  (csdp);
        // _open_pqr (cssg);
        _open_pq  (csld);

        _open_pq  (ktdl);

        _open_pqr (mtbr);
        _open_pq  (mtdg);
        _open_pq  (mtdt);
        _open_pqrs(mtkt);
        _open_pq  (mtdp);
        _open_pqr (mtpd);
        _open_p   (mttr);
        _open_pqr (mtsg);

        _open_pq  (pmbn);
        _open_pq  (pmdp);
        _open_pq  (pmzp);

        _open_pq  (sgbn);
        _open_pq  (sgbr);
        _open_pq  (sgdg);
        _open_pq  (sgdl);
        _open_pq  (sgdp);
        _open_pq  (sgdt);
        _open_p   (sgdx);
        _open_p   (sgkt);
        _open_pqrs(sgmt);
        _open_pq  (sgpd);
        _open_pq  (sgpm);
        _open_pq  (sgsg);

        _open_p   (tmbn);
        _open_p   (tmdp);
        _open_pq  (tmdg);
        _open_pq  (tmpd);
        _open_pq  (tmsg);
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
        default: return u2_bl_bail(wir_r, c3__fail);

        case u2_nul:  return u2_bc(wir_r, u2_rx(wir_r, gen), u2_nul);

        case c3__mtbn: {
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
        case c3__zpdx: { 
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

  /** hack
  **/
    u2_noun                                                       //  transfer
    j2_mcy(Pt6, ap, hack)(u2_wire wir_r, 
                          u2_noun gen)                            //  retain
    {
      u2_noun p_gen, q_gen;
      u2_noun ret;

      if ( u2_yes == u2_dust(gen) ) switch ( u2_h(gen) ) {
        case c3__bnld: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
        {
          if ( (u2_no == u2_dust(p_gen)) || (u2_nul != u2_h(p_gen)) ) {
            return u2_bc(wir_r, u2_no, u2_rx(wir_r, gen));
          }
          else {
            u2_noun pyr = j2_mcy(Pt6, ap, hack)(wir_r, q_gen);

            if ( u2_yes == u2_h(pyr) ) {
              ret = u2_bt
                (wir_r, u2_yes,
                        u2_bt(wir_r, c3__bnld, 
                                     u2_rx(wir_r, p_gen), 
                                     u2_rx(wir_r, u2_h(u2_t(pyr)))),
                        u2_bt(wir_r, c3__bnld, 
                                     u2_rx(wir_r, p_gen), 
                                     u2_rx(wir_r, u2_t(u2_t(pyr)))));
            }
            else {
              ret = u2_bc
                (wir_r, u2_no,
                        u2_bt(wir_r, c3__bnld, 
                                     u2_rx(wir_r, p_gen), 
                                     u2_rx(wir_r, u2_t(pyr))));
            }
            u2_rz(wir_r, pyr);
            return ret;
          }
        }
        case c3__dgdp: u2_bi_cell(wir_r, u2_t(gen), &p_gen, &q_gen);
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

  /** sift
  **/
      static u2_list
      _open_sift_a(u2_wire wir_r,
                   u2_noun ban)
      {
        if ( (u2_nul == ban) ) {
          return u2_nul;
        }
        else {
          return u2_bc
            (wir_r, j2_mcy(Pt6, ap, sift)(wir_r, u2_h(ban)), 
                    _open_sift_a(wir_r, u2_t(ban)));
        }
      }
    u2_noun
    j2_mcy(Pt6, ap, sift)(u2_wire wir_r,
                          u2_noun gen)
    {
      u2_noun p_gen, q_gen;

      if ( u2_yes == u2_as_pq(gen, c3__dgdp, &p_gen, &q_gen) ) {
        return u2_bt
          (wir_r, c3__dgdp, 
                  j2_mcy(Pt6, ap, sift)(wir_r, p_gen), 
                  j2_mcy(Pt6, ap, sift)(wir_r, q_gen));
      }
      else if ( u2_yes == u2_as_p(gen, c3__dtsg, &p_gen) ) {
        return u2_rx(wir_r, gen);
      }
      else if ( u2_yes == u2_as_pq(gen, c3__ktbn, &p_gen, &q_gen) ) {
        return j2_mcy(Pt6, ap, sift)(wir_r, q_gen);
      }
      else if ( u2_yes == u2_as_pq(gen, c3__ktpd, &p_gen, &q_gen) ) {
        return j2_mcy(Pt6, ap, sift)(wir_r, p_gen);
      }
      else if ( u2_yes == u2_as_p(gen, c3__ktsg, &p_gen) ) {
        return u2_rx(wir_r, gen);
      }
      else if ( u2_yes == u2_as_pq(gen, c3__ktmt, &p_gen, &q_gen) ) {
        return u2_bt
          (wir_r, c3__ktmt, 
                  j2_mcy(Pt6, ap, sift)(wir_r, p_gen), 
                  j2_mcy(Pt6, ap, sift)(wir_r, q_gen));
      }
      else if ( u2_yes == u2_as_p(gen, c3__mttr, &p_gen) ) {
        u2_noun pp_gen;

        if ( u2_yes == u2_as_p(p_gen, c3__brcs, &pp_gen) ) {
          return u2_bt(wir_r, c3__mttr,  
                              c3__brcs, 
                              _open_sift_a(wir_r, pp_gen));
        }
        else {
          return u2_bc(wir_r, c3__tmbn, c3__noun);
        }
      }
      else if ( u2_yes == u2_as_p(gen, c3__tmbn, &p_gen) ) {
        return u2_rx(wir_r, gen);
      }
      else if ( u2_yes == u2_as_pq(gen, c3__zpcb, &p_gen, &q_gen) ) {
        return u2_bt
          (wir_r, c3__zpcb, u2_rx(wir_r, p_gen), 
                            j2_mcy(Pt6, ap, sift)(wir_r, q_gen));
      }
      else if ( u2_yes == u2_as_p(gen, c3__zpdx, &p_gen) ) {
        return u2_bc
          (wir_r, c3__zpdx, j2_mcy(Pt6, ap, sift)(wir_r, q_gen));
      }
      else {
        u2_noun bog = j2_mcy(Pt6, ap, open)(wir_r, gen);
        u2_noun gad;

        if ( u2_no == u2_sing(gen, bog) ) {
          gad = j2_mcy(Pt6, ap, sift)(wir_r, bog);
        }
        else {
          gad = u2_bc(wir_r, c3__tmbn, c3__noun);
        }
        u2_rl_lose(wir_r, bog);
        return gad;
      }
    }
    u2_noun                                                       //  transfer
    j2_mc(Pt6, ap, sift)(u2_wire wir_r, 
                         u2_noun cor)                             //  retain
    {
      u2_noun gen;

      if ( u2_none == (gen = u2_frag(u2_cw_sam, cor)) ) {
        return u2_bl_bail(wir_r, c3__fail);
      } else {
        return j2_mcy(Pt6, ap, sift)(wir_r, gen);
      }
    }


/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt6, ap, sift)[] = {
    { ".3", c3__hevy, j2_mc(Pt6, ap, sift), Tier6_c, u2_none, u2_none },
    { }
  };
  u2_ho_jet 
  j2_mcj(Pt6, ap, modl)[] = {
    { ".3", c3__hevy, j2_mc(Pt6, ap, modl), Tier6_c, u2_none, u2_none },
    { }
  };
  u2_ho_jet 
  j2_mcj(Pt6, ap, grow)[] = {
    { ".3", c3__hevy, j2_mc(Pt6, ap, modl), Tier6_c, u2_none, u2_none },
    { }
  };

  u2_ho_driver 
  j2_mbd(Pt6, ap)[] = {
    { j2_sc(Pt6, ap, modl), j2_mcj(Pt6, ap, modl), 0, 0, u2_none },
    { j2_sc(Pt6, ap, sift), j2_mcj(Pt6, ap, sift), 0, 0, u2_none },
    { }
  };

  u2_ho_jet 
  j2_mbj(Pt6, ap)[] = {
    { "open", c3__hevy, j2_mc(Pt6, ap, open), Tier6_c, u2_none, u2_none },
    { "rake", c3__hevy, j2_mc(Pt6, ap, rake), Tier6_c, u2_none, u2_none },
    { "hack", c3__hevy, j2_mc(Pt6, ap, hack), Tier6_c, u2_none, u2_none },
    { }
  };
