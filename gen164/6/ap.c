/* j/6/ap.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/** forward declares
**/
    u2_noun
    j2_mcy(Pt6, ap, mold)(u2_noun, u2_noun);

    u2_noun
    j2_mcy(Pt6, ap, pick)(u2_noun, u2_noun, u2_noun);

    u2_noun
    j2_mcy(Pt6, ap, mold)(u2_noun, u2_noun);

    u2_noun j2_mc(Pt6, ap, rake)(u2_noun);
    u2_noun j2_mc(Pt6, ap, open)(u2_noun);
    u2_noun j2_mc(Pt6, ap, hack)(u2_noun);

    static u2_noun
    _ap_open_l(u2_noun, u2_noun);

    // make sure these match the array below!
    //
#   define _ap_jet_open  0
#   define _ap_jet_rake  1
#   define _ap_jet_hack  2

    u2_ho_jet
    j2_mbj(Pt6, ap)[] = {
      { "open", c3__hevy, j2_mc(Pt6, ap, open), Tier6_c, u2_none, u2_none },
      { "rake", c3__hevy, j2_mc(Pt6, ap, rake), Tier6_c, u2_none, u2_none },
      // { "hack", c3__hevy, j2_mc(Pt6, ap, hack), Tier6_c, u2_none, u2_none },
      { }
    };

    static u2_weak
    _open_in(
	     u2_noun ter,
             u2_noun gen);

    static u2_noun
    _al_core(
	     u2_noun ter,
	     u2_noun gen)
    {
      u2_weak hoc = u2_cj_look(u2k(ter), "al");

      if ( u2_none == hoc ) {
	return u2_cm_bail(c3__fail);
      }
      else {
	u2_noun gat = u2_cn_nock_on(u2k(ter), u2k(hoc));
	u2_noun cor = u2_ci_molt(u2k(gat),
					u2_cv_sam, u2k(gen),
					0);

	u2z(hoc);
	u2z(gat);

	return cor;
      }
    }

/** open cases
**/

#define _open_do_p(stem)  \
  static u2_noun _open_in_##stem \
    ( u2_noun ter, u2_noun p_gen)

#define _open_do_pq(stem)  \
  static u2_noun _open_in_##stem \
    ( u2_noun ter, u2_noun p_gen, u2_noun q_gen)

#define _open_do_pqr(stem)  \
  static u2_noun _open_in_##stem \
    ( u2_noun ter, u2_noun p_gen, u2_noun q_gen, u2_noun r_gen)

#define _open_do_pqrs(stem)  \
  static u2_noun _open_in_##stem \
    (  u2_noun ter, u2_noun p_gen, u2_noun q_gen, u2_noun r_gen, \
     u2_noun s_gen)

/***
****
***/
  _open_do_pq(tsbr)   //  =:
  {
    return
      u2nt(c3__tsls,
	           j2_mcy(Pt6, al, bunt)(
					 _al_core(ter, p_gen), p_gen),
	           u2k(q_gen));
  }
  _open_do_pq(tscl)   //  =:
  {
    return u2nt(c3__tsgr,
		        u2nt(c3__cncb,
                                     u2nc(u2nc(u2_nul, 1),
                                                  u2_nul),
                                     u2k(p_gen)),
		        u2k(q_gen));
  }
  _open_do_pqr(tsdt)   //  =.
  {
    return
      u2nt(c3__tsgr,
	           u2nt(c3__cncb,
			        u2nc(u2nc(u2_nul, 1),
				             u2_nul),
                                u2nc(u2nc(u2k(p_gen),
					                  u2k(q_gen)),
				             u2_nul)),
	           u2k(r_gen));
  }
  _open_do_pq(tsgl)   //  =<
  {
    return u2nt(c3__tsgr, u2k(q_gen),
                                  u2k(p_gen));
  }
  _open_do_pq(tshp)   //  =-
  {
    return u2nt(c3__tsls, u2k(q_gen),
                                  u2k(p_gen));
  }
  _open_do_pq(tsls)   //  =+
  {
    return u2nt
      (c3__tsgr,
              u2nc(u2k(p_gen),
                           u2nc(u2_nul, 1)),
              u2k(q_gen));
  }
  _open_do_p(tssg)   //  =~
  {
    u2_noun tp_gen = u2t(p_gen);
    u2_noun ip_gen = u2h(p_gen);

    if ( (u2_nul == p_gen) ) {
      return u2nc(u2_blip, 1);
    }
    else if ( (u2_nul == tp_gen) ) {
      return u2k(ip_gen);
    }
    else {
      return u2nt(c3__tsgr,
		          u2k(ip_gen),
		          _open_in_tssg(ter, tp_gen));
    }
  }
/***
****
***/
  _open_do_p(bccb)    //  $_
  {
    return j2_mcy(Pt6, al, bunt)(_al_core(ter, p_gen), p_gen);
  }
  _open_do_p(bctr)    //  $*
  {
    return
      u2nc(c3__ktsg,
	           j2_mcy(Pt6, al, bunt)(
					 _al_core(ter, p_gen),
					 p_gen));
  }
  _open_do_p(bczp)    //  $!
  {
    return u2nt
      (c3__bccb, c3__axil, u2k(p_gen));
  }
/***
****
***/
  _open_do_p(brhp)    //  |-
  {
    return u2nt
      (c3__tsgl,
              u2nc(c3__cnzy, u2_blip),
              u2nc(c3__brdt, u2k(p_gen)));
  }
  _open_do_p(brdt)   //  |.
  {
    return u2nc
      (c3__brcn,
              u2nt(
                    u2nt(u2_blip, c3__ash, u2k(p_gen)),
                    u2_nul,
                    u2_nul));
  }

/***
****
***/
  _open_do_p(wtbr)    //  ?|
  {
    if ( (u2_nul == p_gen) ) {
      return u2nt(c3__dtzz, 'f', u2_no);
    }
    else {
      u2_noun ip_gen = u2h(p_gen);
      u2_noun tp_gen = u2t(p_gen);

      return u2nq
        (c3__wtcl,
                u2k(ip_gen),
                u2nt(c3__dtzz, 'f', u2_yes),
	        _open_in_wtbr(ter, tp_gen));
    }
  }
  _open_do_pqr(wtkt)   //  ?^
  {
    return u2nq
      (c3__wtcl,
              u2nt(c3__wtts,
		           u2nt(c3__axil, c3__atom, u2_blip),
	                   u2k(p_gen)),
              u2k(r_gen),
              u2k(q_gen));
  }
  _open_do_pq(wtgl)   //  ?<
  {
    return u2nq
      (c3__wtcl,
            u2k(p_gen),
            u2nc(c3__zpzp, u2_nul),
            u2k(q_gen));
  }
  _open_do_pqr(wtdt)  //  ?.
  {
    return u2nq(c3__wtcl,
                        u2k(p_gen),
                        u2k(r_gen),
                        u2k(q_gen));
  }
  _open_do_pq(wtgr)   //  ?>
  {
    return u2nq
      (c3__wtcl,
              u2k(p_gen),
              u2k(q_gen),
              u2nc(c3__zpzp, u2_nul));
  }
  _open_do_pq(wthp)  //  ?-
  {
    if ( (u2_nul == q_gen) ) {
      return u2nc(c3__zpfs,
                          u2nc(c3__cnzz,
                                       u2k(p_gen)));
    }
    else {
      u2_noun iq_gen = u2h(q_gen);
      u2_noun tq_gen = u2t(q_gen);
      u2_noun piq_gen = u2h(iq_gen);
      u2_noun qiq_gen = u2t(iq_gen);

      return u2nq
        (
         c3__wtcl,
         u2nt(c3__wtts,
                      u2k(piq_gen),
                      u2k(p_gen)),
         u2k(qiq_gen),
         _open_in_wthp(ter, p_gen, tq_gen));
    }
  }
  _open_do_p(wtpm)    //  ?&
  {
    if ( (u2_nul == p_gen) ) {
      return u2nt(c3__dtzz, 'f', u2_yes);
    }
    else {
      u2_noun ip_gen = u2h(p_gen);
      u2_noun tp_gen = u2t(p_gen);

      return u2nq
        (c3__wtcl,
                u2k(ip_gen),
	        _open_in_wtpm(ter, tp_gen),
                u2nt(c3__dtzz, 'f', u2_no));
    }
  }
  _open_do_pqr(wtls)  //  ?+
  {    u2_noun tul = u2nc(
                        u2nc(u2nc(c3__axil, c3__noun),
                                     u2k(q_gen)),
                        u2_nul);
    u2_noun zal = j2_mbc(Pt2, weld)(r_gen, tul);
    u2_noun ret = u2nt(c3__wthp, u2k(p_gen), zal);

    u2z(tul);
    return ret;

  }
  _open_do_pqr(wtpt)  //  ?@
  {
    return u2nq(c3__wtcl,
                        u2nt(c3__wtts,
                                     u2nt(c3__axil,
                                                  c3__atom,
                                                  u2_blip),
                                     u2k(p_gen)),
		        u2k(q_gen),
                        u2k(r_gen));
  }
  _open_do_pqr(wtsg)    //  ?~
  {
    return u2nq(c3__wtcl,
                        u2nt(c3__wtts,
                                     u2nc(c3__axil, c3__null),
                                     u2k(p_gen)),
                        u2k(q_gen),
                        u2k(r_gen));
  }
  _open_do_p(wtzp)    //  ?!
  {
    return u2nq
      (c3__wtcl,
              u2k(p_gen),
              u2nt(c3__dtzz, 'f', u2_no),
              u2nt(c3__dtzz, 'f', u2_yes));
  }
/***
****
***/
  _open_do_pq(zpcb)    //  !_
  {
    return u2k(q_gen);
  }
  _open_do_p(zpgr)    //  !>
  {
    return u2nq
      (c3__cnhp,
              u2nc(c3__cnzy, c3__onan),
              u2nt(c3__zpsm,
	                   u2nc(c3__bctr,
		                        u2nc(c3__herb,
			                             u2nc(c3__cnzy,
							          c3__abel))),
		           u2k(p_gen)),
              u2_nul);
  }
/***
****
***/
  _open_do_pq(clhp) //  :-
  {
    return u2nc
      (u2k(p_gen),
              u2k(q_gen));
  }
  _open_do_pq(clcb) //  :_
  {
    return u2nc
      (u2k(q_gen),
              u2k(p_gen));
  }
  _open_do_p(clcn) //  :%
  {
    return u2nc
      (u2nc(c3__clsg,
                           u2k(p_gen)),
              u2nc(c3__bczp, c3__null));
  }
  _open_do_pqrs(clkt) //  :^
  {
    return u2nq
      (u2k(p_gen),
              u2k(q_gen),
              u2k(r_gen),
              u2k(s_gen));
  }
  _open_do_pqr(clls)  //  :+
  {
    return u2nt
      (u2k(p_gen), u2k(q_gen), u2k(r_gen));
  }
  _open_do_p(clsg)    //  :~
  {
    if ( (u2_nul == p_gen) ) {
      return u2nt(c3__dtzz, 'n', u2_nul);
    }
    else {
      u2_noun ip_gen = u2h(p_gen);
      u2_noun tp_gen = u2t(p_gen);

      return u2nc(u2k(ip_gen),
		          _open_in_clsg(ter, tp_gen));
    }
  }
  _open_do_p(cltr)    //  :*
  {
    if ( (u2_nul == p_gen) ) {
      return u2nc(c3__zpzp, u2_nul);
    }
    else {
      u2_noun ip_gen = u2h(p_gen);
      u2_noun tp_gen = u2t(p_gen);

      if ( (u2_nul == tp_gen) ) {
        return u2k(ip_gen);
      } else {
        return u2nc
          (u2k(ip_gen),
                  _open_in_cltr(ter, tp_gen));
      }
    }
  }
/***
****
***/
  _open_do_pq(cncb)   //  %_
  {
    return u2nc
      (c3__ktls,
       u2nq(u2nc(c3__cnzz, u2k(p_gen)),
	            c3__cnts,
                    u2k(p_gen),
                    u2k(q_gen)));
  }
  _open_do_pq(cncl)   //  %:
  {
    return u2nq
      (c3__cnsg,
            u2nc(u2_blip, u2_nul),
            u2k(p_gen),
            u2k(q_gen));
  }
  _open_do_pq(cndt)   //  %.
  {
    return u2nt
      (c3__cnhp,
              u2k(q_gen),
              u2nc(u2k(p_gen), u2_nul));
  }
  _open_do_pqrs(cnkt) //  %^
  {
    return u2nq
      (c3__cnhp,
              u2k(p_gen),
              u2k(q_gen),
              u2nt(u2k(r_gen),
                           u2k(s_gen),
                           u2_nul));
  }
  _open_do_pq(cnhp)   //  %-
  {
    if ( (u2_nul == q_gen) ) {
      return u2nt
        (c3__tsgr,
                u2k(p_gen),
                u2nc(c3__cnzy, u2_blip));
    } else {
      return u2nq(c3__cncl,
                          u2k(p_gen),
                          c3__cltr,
                          u2k(q_gen));
    }
  }
  _open_do_pqr(cnls)  //  %+
  {
    return u2nc
      (c3__cnhp,
              u2nq(u2k(p_gen),
                           u2k(q_gen),
                           u2k(r_gen),
                           u2_nul));
  }
  _open_do_pqr(cnsg)  //  %~
  {
    return u2nq
      (c3__cntr,
              u2k(p_gen),
              u2k(q_gen),
              u2nc(u2nc(u2nc(u2nc(u2_nul, 6), 0), u2k(r_gen)), 0));
  }
  _open_do_p(cnzy)  //  %cnzy
  {
    return u2nt
      (c3__cnts,
              u2nc(u2k(p_gen), u2_nul),
              u2_nul);
  }
  _open_do_p(cnzz)  //  %cnzz
  {
    return u2nt
      (c3__cnts, u2k(p_gen), u2_nul);
  }
/***
****
***/
  _open_do_p(hxgl)  //  #<
  {
    return u2nq
      (c3__cnhp,
              u2nc(c3__cnzy, c3__noah),
              u2nc(c3__zpgr,
		           u2nc(c3__cltr, u2k(p_gen))),
              u2_nul);
  }
  _open_do_p(hxgr)  //  #>
  {
    return u2nq
      (c3__cnhp,
              u2nc(c3__cnzy, c3__cain),
              u2nc(c3__zpgr,
		           u2nc(c3__cltr, u2k(p_gen))),
              u2_nul);
  }
/***
****
***/
   _open_do_pq(ktdt)   //  ^.
  {
    return u2nt
      (c3__ktls,
       u2nq(c3__cnhp, u2k(p_gen), u2k(q_gen), u2_nul),
       u2k(q_gen));
  }
   _open_do_pq(kthp)   //  ^-
  {
    return u2nt
      (c3__ktls,
              j2_mcy(Pt6, al, bunt)(_al_core(ter, p_gen), p_gen),
              u2k(q_gen));
  }
/***
****
***/
  _open_do_pq(brcb)   //  |_
  {
    return u2nt(c3__tsls,
		        u2nc(c3__bctr, u2k(p_gen)),
		        u2nc(c3__brcn, u2k(q_gen)));
  }
  _open_do_pq(brkt)   //  |^
  {
    u2_noun diz = u2nc(c3__ash, u2k(p_gen));
    u2_noun ret = u2nt
      (
       c3__tsgr,
       u2nc(
             c3__brcn,
             j2_mcc(Pt4, by, put)(q_gen, u2_blip, diz)),
       u2nc(c3__cnzy, u2_blip));

    u2z(diz);
    return ret;
  }
  _open_do_pq(brls)   //  |+
  {
    return u2nc(c3__ktbr,
                        u2nt(c3__brts,
                                     u2k(p_gen),
                                     u2k(q_gen)));
  }
  _open_do_p(brwt)   //  |?
  {
    return u2nt(c3__ktwt,
                        c3__brdt,
                        u2k(p_gen));
  }
/***
****
***/
  _open_do_pq(sgts)   //  ~=
  {
    return u2nt
      (c3__sggr,
              u2nc(c3__germ, u2k(p_gen)),
              u2k(q_gen));
  }
  _open_do_pq(sgbr)   //  ~|
  {
    return u2nt
      (c3__sggr,
              u2nc(c3__yelp, u2k(p_gen)),
              u2k(q_gen));
  }
  _open_do_pq(sggl)   //  ~>
  {
    return u2nt
      (c3__tsgl,
              u2nq(c3__sggr, u2k(p_gen), u2_nul, 1),
              u2k(q_gen));
  }
  _open_do_pq(sgbc)    //  ~$
  {
    return u2nt(c3__sggr,
                        u2nq(c3__live,
                                     c3__dtzz, u2_blip, u2k(p_gen)),
                        u2k(q_gen));
  }
  _open_do_pq(sgcb)    //  ~_
  {
    return u2nt
      (c3__sggr,
              u2nc(c3__mean,
	                   u2nc(c3__brdt,
		                        u2k(p_gen))),
              u2k(q_gen));
  }
    static u2_noun
    _sgcn_a(
            u2_noun r_gen,
            u2_noun nob)
    {
      if ( u2_no == u2du(r_gen) ) {
        return u2k(nob);
      } else {
        u2_noun ir_gen = u2h(r_gen);
        u2_noun tr_gen = u2t(r_gen);
        u2_noun pir_gen, qir_gen;

        u2_cx_cell(ir_gen, &pir_gen, &qir_gen);

        return u2nc
          (u2nc
                    (
                     u2nt(c3__dtzz, u2_blip, u2k(pir_gen)),
                     u2nc(c3__zpts, u2k(qir_gen))),
                  _sgcn_a(tr_gen, nob));
      }
    }
  _open_do_pqrs(sgcn) //  ~%
  {
    return u2nt
      (c3__sggl,
              u2nq
                (c3__fast,
                        c3__clls,
                        u2nt(c3__dtzz, u2_blip, u2k(p_gen)),
                        u2nt
                          (u2nc(c3__zpts, u2k(q_gen)),
                                  c3__clsg,
                                  _sgcn_a(r_gen, u2_nul))),
              u2k(s_gen));
  }
  _open_do_pq(sgfs) //  ~/
  {
     return u2nc
      (c3__sgcn,
              u2nq(u2k(p_gen),
                           u2nc(u2_nul, 7),
                           u2_nul,
                           u2k(q_gen)));
  }
  _open_do_pq(sgls)   //  ~+
  {
    return u2nt
      (c3__sggr,
              u2nq(c3__memo, c3__dtzz, u2_blip, u2k(p_gen)),
              u2k(q_gen));
  }
  _open_do_pqr(sgpm)   //  ~&
  {
    return u2nt
      (
       c3__sggr,
       u2nt(
	     c3__slog,
	     u2nt(c3__dtzy, u2_blip, u2k(p_gen)),
	     u2nq(c3__cnhp, u2nc(c3__cnzy, c3__cain),
		   u2nc(c3__zpgr, u2k(q_gen)), u2_nul)),
       u2k(r_gen));
  }
  _open_do_pqrs(sgwt)   //  ~?
  {
    return u2nq(c3__tsgl,
                        u2k(s_gen),
                        c3__wtdt,
                        u2nq(u2k(q_gen),
                                     u2nc(u2_nul, 1),
                                     c3__sgpm,
		                     u2nt(u2k(p_gen),
                                                  u2k(r_gen),
                                                  u2nc(u2_nul, 1))));
  }
/***
****
***/
    static u2_noun                                                //  produce
    _smcl_in(
             u2_noun q_gen)                                       //  retain
    {
      u2_noun hq_gen = u2h(q_gen);
      u2_noun tq_gen = u2t(q_gen);

      if ( u2_no == u2du(tq_gen) ) {
        return u2nt(c3__tsgr,
                            u2nc(u2_nul, 3),
                            u2k(hq_gen));
      } else {
        return u2nc
          (
           c3__cnhp,
           u2nq(
                 u2nc(u2_nul, 2),
                 u2nt(c3__tsgr,
                              u2nc(u2_nul, 3),
                              u2k(hq_gen)),
                 _smcl_in(tq_gen),
                 u2_nul));
      }
    }
  _open_do_pq(smcl)
  {
    if ( u2_no == u2du(q_gen) ) {
      return u2nc(c3__zpzp, u2_nul);
    }
    else if ( u2_nul == u2t(q_gen) ) {
      return u2k(u2h(q_gen));
    }
    else {
      return u2nt
        (
         c3__tsls,
         u2k(p_gen),
         _smcl_in(q_gen));
    }
  }
#if 0
  _open_do_pq(smsm)
  {
    return
      u2nt(c3__tsgr, u2nq(c3__ktts, c3__v, u2_nul, 1),
	    u2nt(c3__tsls,
		  u2nt(c3__ktts, c3__a,
			u2nt(c3__tsgr, u2nc(c3__cnzy, c3__v),
			      u2k(p_gen))),
		  u2nt(c3__tsls,
			u2nt(c3__ktts, c3__b,
			      u2nt(c3__tsgr,
				    u2nc(c3__cnzy, c3__v),
				    u2k(q_gen))),
			u2nt(c3__tsls,
			      u2nt(c3__ktts, c3__c,
				    u2nq(c3__cnhp,
					  u2nc(c3__cnzy, c3__a),
					  u2nc(c3__cnzy, c3__b),
					  u2_nul)),
			      u2nt(c3__wtgr,
				    u2nt(c3__dtts,
					  u2nc(c3__cnzy, c3__c),
					  u2nc(c3__cnzy, c3__b)),
				    u2nc(c3__cnzy, c3__c))))));
  }
#endif
/* functions
*/
  /** open
  **/
    static u2_weak
    _open_in(
	     u2_noun ter,
             u2_noun gen)
    {
      u2_noun p_gen, q_gen, r_gen, s_gen;

      if ( u2_yes == u2ud(gen) ) {
        // printf("studly\n");
        // u2_err("stud m", gen);
        return u2_cm_bail(c3__exit);

        return u2nt
          (c3__cnts,
                  u2nc(u2k(gen), u2_nul),
                  u2_nul);
      }
      else switch ( u2h(gen) ) {
        default: return u2_none;

        case u2_nul: {
          return u2nt
            (c3__cnts,
                    u2nc(u2k(gen), u2_nul),
                    u2_nul);
        }

#     define _open_p(stem) \
          case c3__##stem: \
            return _open_in_##stem(ter, u2t(gen));  \

#     define _open_pq(stem) \
          case c3__##stem: \
            if ( u2_no == u2_cr_cell(u2t(gen), &p_gen, &q_gen) ) { \
              return u2_cm_bail(c3__fail); \
            } else return _open_in_##stem(ter, p_gen, q_gen);

#     define _open_pqr(stem) \
          case c3__##stem: \
            if ( u2_no == u2_cr_trel(u2t(gen), &p_gen, &q_gen, &r_gen) ) { \
              return u2_cm_bail(c3__fail); \
            } else return _open_in_##stem(ter, p_gen, q_gen, r_gen);

#     define _open_pqrs(stem) \
          case c3__##stem: \
            if ( u2_no == u2_cr_qual\
                          (u2t(gen), &p_gen, &q_gen, &r_gen, &s_gen) )\
            { \
              return u2_cm_bail(c3__fail); \
            } else return _open_in_##stem(ter, p_gen, q_gen, r_gen, s_gen);

	_open_p   (bccb);
	_open_p   (bctr);
	_open_p   (bczp);

        _open_p   (brdt);
        _open_pq  (brcb);
        _open_p   (brhp);
        _open_pq  (brkt);
        _open_pq  (brls);
	_open_p   (brwt);

	_open_pq  (clcb);
	_open_p   (clcn);
	_open_pq  (clhp);
        _open_pqrs(clkt);
        _open_pqr (clls);
        _open_p   (cltr);
        _open_p   (clsg);
        _open_pq  (cncb);
        _open_pq  (cncl);
        _open_pq  (cndt);
        _open_pqrs(cnkt);
        _open_pq  (cnhp);
        _open_pqr (cnls);
        _open_pqr (cnsg);
        _open_p   (cnzy);
        _open_p   (cnzz);

	_open_p   (hxgl);
	_open_p   (hxgr);

	_open_pq  (ktdt);
	_open_pq  (kthp);

	_open_pq  (sgts);
	_open_pq  (sgbr);
	_open_pq  (sggl);
	_open_pq  (sgbc);
	_open_pq  (sgcb);
	_open_pqrs(sgcn);
	_open_pq  (sgfs);
	_open_pq  (sgls);
	_open_pqr (sgpm);
	_open_pqrs(sgwt);

        _open_pq  (smcl);
	//	_open_pq  (smsm);

	_open_pq  (tsbr);
	_open_pq  (tscl);
	_open_pqr (tsdt);
        _open_pq  (tsgl);
        _open_pq  (tshp);
        _open_pq  (tsls);
	_open_p   (tssg);

        _open_pqr (wtdt);
        _open_pq  (wtgl);
	_open_pqr (wtpt);
	_open_pqr (wtsg);
        _open_p   (wtzp);
        _open_p   (wtbr);
	_open_pq  (wthp);
        _open_pq  (wtgr);
        _open_pqr (wtls);
        _open_pqr (wtkt);
        _open_p   (wtpm);

	_open_pq  (zpcb);
	_open_p   (zpgr);
      }
    }

  /** rake
  **/
    u2_noun
    j2_mcy(Pt6, ap, rake)(
                          u2_noun gen)
    {
      u2_noun p_gen, q_gen;

      if ( u2_yes == u2ud(gen) ) {
        return u2nc(u2k(gen), u2_nul);
      }
      else switch ( u2h(gen) ) {
        default: return u2_cm_error("rake-twig");

        case u2_nul:  return u2nc(u2k(gen), u2_nul);

        case c3__cnzy: {
          return u2nc(u2k(u2t(gen)), u2_nul);
        }
        case c3__cnzz: {
          return u2k(u2t(gen));
        }
        case c3__cnts: {
          if ( u2_no == u2_cr_cell(u2t(gen), &p_gen, &q_gen) ) {
            return u2_cm_bail(c3__fail);
          }
          else {
            if ( u2_nul != q_gen ) {
              return u2_cm_bail(c3__fail);
            }
            else {
              return u2k(p_gen);
            }
          }
        }
        case c3__zpcb: {
          if ( u2_no == u2_cr_cell(u2t(gen), &p_gen, &q_gen) ) {
            return u2_cm_bail(c3__fail);
          }
          else return j2_mcy(Pt6, ap, rake)(q_gen);
        }
      }
    }
    u2_noun                                                       //  transfer
    j2_mc(Pt6, ap, rake)(
                         u2_noun cor)                             //  retain
    {
      u2_noun gen;

      if ( u2_none == (gen = u2_cr_at(u2_cv_sam, cor)) ) {
        return u2_cm_bail(c3__fail);
      } else {
        return j2_mcy(Pt6, ap, rake)(gen);
      }
    }

  /** hack
  **/
    u2_noun                                                       //  transfer
    j2_mcy(Pt6, ap, hack)(
                          u2_noun ter,                            //  retain
                          u2_noun gen)                            //  retain
    {
      u2_noun p_gen, q_gen;
      u2_noun ret;

      if ( u2_yes == u2du(u2h(gen)) ) {
        return u2nt(u2_yes,
                            u2k(u2h(gen)),
                            u2k(u2t(gen)));
      }
      else switch ( u2h(gen) ) {
        case c3__tsgr: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
        {
          if ( (u2_no == u2du(p_gen)) || (u2_nul != u2h(p_gen)) ) {
            return u2nc(u2_no, u2k(gen));
          }
          else {
            u2_noun pyr = j2_mcy(Pt6, ap, hack)(ter, q_gen);

            if ( u2_yes == u2h(pyr) ) {
              ret = u2nt
                (u2_yes,
                        u2nt(c3__tsgr,
                                     u2k(p_gen),
                                     u2k(u2h(u2t(pyr)))),
                        u2nt(c3__tsgr,
                                     u2k(p_gen),
                                     u2k(u2t(u2t(pyr)))));
            }
            else {
              ret = u2nc
                (u2_no,
                        u2nt(c3__tsgr,
                                     u2k(p_gen),
                                     u2k(u2t(pyr))));
            }
            u2z(pyr);
            return ret;
          }
        }
        case c3__zpcb: u2_cx_cell(u2t(gen), &p_gen, &q_gen);
        {
          u2_noun pyr = j2_mcy(Pt6, ap, hack)(ter, q_gen);

          if ( u2_yes == u2h(pyr) ) {
            ret = u2nt
              (u2_yes,
                      u2nt(c3__zpcb,
                                   u2k(p_gen),
                                   u2k(u2h(u2t(pyr)))),
                      u2nt(c3__zpcb,
                                   u2k(p_gen),
                                   u2k(u2t(u2t(pyr)))));
          }
          else {
            ret = u2nc
              (u2_no,
                      u2nt(c3__zpcb,
                                   u2k(p_gen),
                                   u2k(u2t(pyr))));
          }
          u2z(pyr);
          return ret;
        }
        default: break;
      }

      {
        u2_noun voq = _ap_open_l(ter, gen);

        if ( u2_none == voq ) {
          return u2nc(u2_no, u2k(gen));
        }
        else if ( u2_yes == u2_cr_sing(voq, gen) ) {
          return u2nc(u2_no, voq);
        }
        else {
          ret = j2_mcy(Pt6, ap, hack)(ter, voq);

          u2z(voq);
          return ret;
        }
      }
    }

    u2_noun                                                       //  transfer
    j2_mc(Pt6, ap, hack)(
                         u2_noun cor)                             //  retain
    {
      u2_noun gen;

      if ( u2_none == (gen = u2_cr_at(u2_cv_sam, cor)) ) {
        return u2_cm_bail(c3__fail);
      } else {
        u2_noun ter = u2_cr_at(u2_cv_con, cor);

        return j2_mcy(Pt6, ap, hack)(ter, gen);
      }
    }

/* boilerplate
*/
  u2_noun                                                         //  transfer
  j2_mbi(Pt6, ap)(
                  u2_noun ter,                                    //  retain
                  u2_noun gen)                                    //  retain
  {
    u2_weak hoc = u2_cj_look(u2k(ter), "ap");

    if ( u2_none == hoc ) {
      return u2_cm_bail(c3__fail);
    } else {
      u2_noun gat = u2_cn_nock_on(u2k(ter), u2k(hoc));
      u2_noun cor = u2_ci_molt(u2k(gat),
                                      u2_cv_sam, u2k(gen),
                                      0);

      u2z(hoc);
      u2z(gat);
      return cor;
    }
  }

/* open
*/
  static u2_noun
  _ap_open_n(
             u2_noun ter,
             u2_noun gen)
  {
    u2_noun cor = j2_mbi(Pt6, ap)(ter, gen);
    u2_weak hoc = u2_cj_look(u2k(cor), "open");

    if ( u2_none == hoc ) {
      return u2_cm_bail(c3__fail);
    } else {
      u2_noun pro = u2_cn_nock_on(cor, u2k(hoc));

      u2z(hoc);
      return pro;
    }
  }
  static u2_noun                                                  //  transfer
  _ap_open_l(
             u2_noun ter,                                         //  retain
             u2_noun gen)                                         //  retain
  {
    u2_weak pro = _open_in(ter, gen);

    if ( u2_none != pro ) {
      return pro;
    } else {
      u2_ho_jet *jet_j = &j2_mbj(Pt6, ap)[_ap_jet_open];

      // c3_assert(jet_j->sat_s & u2_jet_live);
      jet_j->sat_s &= ~u2_jet_live;
      {
        pro = _ap_open_n(ter, gen);
        // u2_err("gen", gen);
        // u2_err("pro", pro);
      }
      jet_j->sat_s |= u2_jet_live;

      return pro;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ap, open)(
                        u2_noun ter,                              //  retain
                        u2_noun gen)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mbj(Pt6, ap)[_ap_jet_open];

    if ( !(jet_j->sat_s & u2_jet_live) ) {
      return _ap_open_n(ter, gen);
    }
    else {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return _ap_open_l(ter, gen);
      }
      else {
        c3_m fun_m   = c3__open;
        u2_noun pro = u2_cz_find(fun_m, gen);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = _ap_open_l(ter, gen);
        }
        return u2_cz_save(fun_m, gen, pro);
      }
    }
  }

  u2_noun                                                       //  transfer
  j2_mc(Pt6, ap, open)(
                       u2_noun cor)                             //  retain
  {
    u2_noun gen;

    if ( u2_none == (gen = u2_cr_at(u2_cv_sam, cor)) ) {
      return u2_cm_bail(c3__fail);
    } else {
      u2_noun ter = u2_cr_at(u2_cv_con, cor);

      return j2_mcy(Pt6, ap, open)(ter, gen);
    }
  }

/* structures
*/
  u2_ho_driver
  j2_mbd(Pt6, ap)[] = {
    { }
  };
