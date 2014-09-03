/* j/6/seek.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_noun
  _seek_flat(
             u2_noun wob)
  {
    if ( u2_nul == wob ) {
      return u2_nul;
    } else {
      u2_noun i_wob = u2h(wob);
      u2_noun t_wob = u2t(wob);

      return u2nc
        (u2nc(u2k(u2h(i_wob)),
                             u2nt(c3__ash, u2_nul, 1)),
                _seek_flat(t_wob));
    }
  }

#if 0
  static u2_noun
  _seek_silk_yew(
                 u2_noun van,
                 u2_noun syx,
                 u2_noun qq_tor)
  {
    if ( u2_nul == qq_tor ) {
      return u2_nul;
    }
    else {
      u2_noun iqq_tor  = u2h(qq_tor);
      u2_noun qiqq_tor = u2t(iqq_tor);
      u2_noun yon      = _seek_silk_yew(van, syx, u2t(qq_tor));

      if ( c3__yew != u2h(qiqq_tor) ) {
        return yon;
      } else {
        u2_noun nuy = u2_cqf_look(syx, u2t(qiqq_tor));

        if ( u2_nul == nuy ) {
          return u2_cm_error("silk");
        }
        else {
          yon = u2nc(u2k(u2t(nuy)), yon);
          u2z(nuy);
          return yon;
        }
      }
    }
  }
  static u2_noun
  _seek_silk_yaw(u2_noun
                 u2_noun hey)
  {
    u2_atom axe = 0;

    while ( u2_nul != hey ) {
      if ( axe == 0 ) {
        axe = u2h(u2h(hey));
      } else if ( axe != u2h(u2h(hey)) ) {
        return u2_cm_error("silk");
      }
      hey = u2t(hey);
    }
  }

  static u2_noun
  _seek_silk_fum(u2_noun
                 u2_noun hey,
                 u2_noun qq_tor)
  {
    if ( u2_nul == qq_tor ) {
      return u2_nul;
    }
    c3_assert(u2_nul != hey);
    return u2nc
      (u2nc(u2k(u2h(u2h(qq_tor))),
                           u2k(u2t(u2h(hey)))),
              _seek_silk_fum(u2t(hey), u2t(qq_tor)));
  }

  static u2_noun
  _seek_silk(
             u2_noun van,
             u2_noun syx,
             u2_noun tor)
  {
    u2_noun p_tor, q_tor, pq_tor, qq_tor;
    u2_noun hey, ret;

    u2_cr_cell(tor, &p_tor, &q_tor);
    if ( u2_yes == u2h(q_tor) ) {
      return u2_nul;
    }
    u2_cr_cell(u2t(q_tor), &pq_tor, &qq_tor);

    hey = _seek_silk_yew(van, syx, qq_tor);
    if ( u2_nul == hey ) {
      return u2_nul;
    }
    if ( u2_ckb_lent(u2k(hey)) !=
         u2_ckb_lent(u2k(qq_tor)) )
    {
      return u2_cm_error("silk");
    }

    ret = u2nq
      (u2_nul,
              u2_no,
              u2_cqc_peg(pq_tor, _seek_silk_yaw(hey)),
              _seek_silk_fum(hey, qq_tor));

    u2z(hey);
    return ret;
  }
#endif

  u2_noun
  _cqfu_seek(
                        u2_noun van,
                        u2_noun sut,
                        u2_noun way,
                        u2_noun hyp)
  {
    if ( u2_nul == hyp ) {
      return u2nt
        (1, u2_yes, u2k(sut));
    }
    else if ( u2_no == u2du(hyp) ) {
      return u2_cm_bail(c3__fail);
    }
    else {
      u2_noun i_hyp = u2h(hyp);
      u2_noun t_hyp = u2t(hyp);
      u2_noun zar;
      u2_noun p_zar, q_zar;
      u2_noun yip, syp, ret;

      if ( u2_yes == u2du(i_hyp) ) {
        yip = u2k(i_hyp);
      } else {
        yip = u2nt(u2_no, 0, u2k(i_hyp));
      }

      zar = _cqfu_seek(van, sut, way, t_hyp);
      u2_cr_cell(zar, &p_zar, &q_zar);

#if 0
      if ( u2_yes == u2h(yip) ) {
        sic = u2_nul;
      } else {
        // sic = _seek_silk(van, u2h(u2t(yip)), zar);
        sic = u2_nul;
      }
      if ( u2_nul != sic ) {
        u2z(yip);
        u2z(zar);

        return u2t(sic);
      }
#endif

      if ( u2_yes == u2h(q_zar) ) {
        syp = u2k(u2t(q_zar));
      } else {
        u2_noun pq_zar, qq_zar;
        u2_noun wip;

        u2_cr_cell(u2t(q_zar), &pq_zar, &qq_zar);
        wip = _seek_flat(qq_zar);
        syp = u2_cqfu_fire(van, sut, wip);

        u2z(wip);
      }

      if ( u2_no == u2h(yip) ) {
        u2_noun p_yip, q_yip, hud;

        if ( u2_no == u2_cr_cell(u2t(yip), &p_yip, &q_yip) ) {
          return u2_cm_bail(c3__fail);
        }
        hud = u2_cqfu_fink(van, syp, p_yip, way, q_yip);
        {
          u2_noun p_hud, q_hud;

          u2_cr_cell(hud, &p_hud, &q_hud);

          ret = u2nc(u2_cqc_peg(p_zar, p_hud),
                             u2k(q_hud));
          u2z(hud);
        }
      }
      else {
        u2_noun p_yip = u2t(yip);

        if ( u2_no == u2ud(p_yip) ) {
          return u2_cm_bail(c3__fail);
        }
        else {
          ret = u2nt
            (u2_cqc_peg(p_zar, p_yip),
                    u2_yes,
                    u2_cqfu_peek(van, syp, way, p_yip));
        }
      }
      u2z(yip);
      u2z(syp);
      u2z(zar);
      return ret;
    }
  }

/* boilerplate
*/
  u2_noun
  u2_cwfu_seek(
                       u2_noun cor)
  {
    u2_noun sut, way, hyp, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &way,
                                u2_cv_sam_3, &hyp,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqfu_seek(van, sut, way, hyp);
    }
  }

  u2_noun
  u2_cqfu_seek(u2_noun van,
                        u2_noun sut,
                        u2_noun way,
                        u2_noun hyp)
  {
    c3_m    fun_m = c3__seek;
    u2_noun pro   = u2_cz_find_3(fun_m, sut, way, hyp);

    if ( u2_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_seek(van, sut, way, hyp);

      return u2_cz_save_3(fun_m, sut, way, hyp, pro);
    }
  }

  u2_noun
  u2_cqfu_seep(u2_noun van,
                        u2_noun sut,
                        u2_noun way,
                        u2_noun hyp)
  {
    u2_noun zar = u2_cqfu_seek(van, sut, way, hyp);
    u2_noun p_zar = u2h(zar);
    u2_noun q_zar = u2t(zar);

    if ( u2_yes != u2h(q_zar) ) {
      return u2_cm_bail(c3__exit);
    }
    else {
      u2_noun ret = u2nc(u2k(p_zar),
                                 u2k(u2t(q_zar)));

      u2z(zar);
      return ret;
    }
  }
