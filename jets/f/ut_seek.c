/* j/6/seek.c
**
*/
#include "all.h"


/* logic
*/
  static u3_noun
  _seek_flat(u3_noun wob)
  {
    if ( u3_nul == wob ) {
      return u3_nul;
    } else {
      u3_noun i_wob = u3h(wob);
      u3_noun t_wob = u3t(wob);

      return u3nc(u3nc(u3k(u3h(i_wob)),
                           u3nt(c3__ash, u3_nul, 1)),
                  _seek_flat(t_wob));
    }
  }

#if 0
  static u3_noun
  _seek_silk_yew(
                 u3_noun van,
                 u3_noun syx,
                 u3_noun qq_tor)
  {
    if ( u3_nul == qq_tor ) {
      return u3_nul;
    }
    else {
      u3_noun iqq_tor  = u3h(qq_tor);
      u3_noun qiqq_tor = u3t(iqq_tor);
      u3_noun yon      = _seek_silk_yew(van, syx, u3t(qq_tor));

      if ( c3__yew != u3h(qiqq_tor) ) {
        return yon;
      } else {
        u3_noun nuy = u3qf_look(syx, u3t(qiqq_tor));

        if ( u3_nul == nuy ) {
          return u3m_error("silk");
        }
        else {
          yon = u3nc(u3k(u3t(nuy)), yon);
          u3z(nuy);
          return yon;
        }
      }
    }
  }
  static u3_noun
  _seek_silk_yaw(u3_noun
                 u3_noun hey)
  {
    u3_atom axe = 0;

    while ( u3_nul != hey ) {
      if ( axe == 0 ) {
        axe = u3h(u3h(hey));
      } else if ( axe != u3h(u3h(hey)) ) {
        return u3m_error("silk");
      }
      hey = u3t(hey);
    }
  }

  static u3_noun
  _seek_silk_fum(u3_noun
                 u3_noun hey,
                 u3_noun qq_tor)
  {
    if ( u3_nul == qq_tor ) {
      return u3_nul;
    }
    c3_assert(u3_nul != hey);
    return u3nc
      (u3nc(u3k(u3h(u3h(qq_tor))),
                           u3k(u3t(u3h(hey)))),
              _seek_silk_fum(u3t(hey), u3t(qq_tor)));
  }

  static u3_noun
  _seek_silk(
             u3_noun van,
             u3_noun syx,
             u3_noun tor)
  {
    u3_noun p_tor, q_tor, pq_tor, qq_tor;
    u3_noun hey, ret;

    u3r_cell(tor, &p_tor, &q_tor);
    if ( c3y == u3h(q_tor) ) {
      return u3_nul;
    }
    u3r_cell(u3t(q_tor), &pq_tor, &qq_tor);

    hey = _seek_silk_yew(van, syx, qq_tor);
    if ( u3_nul == hey ) {
      return u3_nul;
    }
    if ( u3kb_lent(u3k(hey)) !=
         u3kb_lent(u3k(qq_tor)) )
    {
      return u3m_error("silk");
    }

    ret = u3nq
      (u3_nul,
              c3n,
              u3qc_peg(pq_tor, _seek_silk_yaw(hey)),
              _seek_silk_fum(hey, qq_tor));

    u3z(hey);
    return ret;
  }
#endif

  u3_noun
  _cqfu_seek(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    if ( u3_nul == hyp ) {
      return u3nt
        (1, c3y, u3k(sut));
    }
    else if ( c3n == u3du(hyp) ) {
      return u3m_bail(c3__fail);
    }
    else {
      u3_noun i_hyp = u3h(hyp);
      u3_noun t_hyp = u3t(hyp);
      u3_noun zar;
      u3_noun p_zar, q_zar;
      u3_noun yip, syp, ret;

      if ( c3y == u3du(i_hyp) ) {
        yip = u3k(i_hyp);
      } else {
        yip = u3nt(c3n, 0, u3nc(u3_nul, u3k(i_hyp)));
      }

      zar = _cqfu_seek(van, sut, way, t_hyp);
      u3r_cell(zar, &p_zar, &q_zar);

#if 0
      if ( c3y == u3h(yip) ) {
        sic = u3_nul;
      } else {
        // sic = _seek_silk(van, u3h(u3t(yip)), zar);
        sic = u3_nul;
      }
      if ( u3_nul != sic ) {
        u3z(yip);
        u3z(zar);

        return u3t(sic);
      }
#endif

      if ( c3y == u3h(q_zar) ) {
        syp = u3k(u3t(q_zar));
      } else {
        u3_noun pq_zar, qq_zar;
        u3_noun wip;

        u3r_cell(u3t(q_zar), &pq_zar, &qq_zar);
        wip = _seek_flat(qq_zar);
        syp = u3qfu_fire(van, sut, wip);

        u3z(wip);
      }

      if ( c3n == u3h(yip) ) {
        u3_noun p_yip, q_yip, hud;

        if ( c3n == u3r_cell(u3t(yip), &p_yip, &q_yip) ) {
          return u3m_bail(c3__fail);
        }
        hud = u3qfu_fink(van, syp, p_yip, way, q_yip);
        {
          u3_noun p_hud, q_hud;

          u3r_cell(hud, &p_hud, &q_hud);

          ret = u3nc(u3qc_peg(p_zar, p_hud),
                     u3k(q_hud));
          u3z(hud);
        }
      }
      else {
        u3_noun p_yip = u3t(yip);

        if ( c3n == u3ud(p_yip) ) {
          return u3m_bail(c3__fail);
        }
        else {
          ret = u3nt(u3qc_peg(p_zar, p_yip),
                     c3y,
                     u3qfu_peek(van, syp, way, p_yip));
        }
      }
      u3z(yip);
      u3z(syp);
      u3z(zar);
      return ret;
    }
  }

/* boilerplate
*/
  u3_noun
  u3wfu_seek(u3_noun cor)
  {
    u3_noun sut, way, hyp, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &way,
                               u3x_sam_3, &hyp,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_seek(van, sut, way, hyp);
    }
  }

  u3_noun
  u3qfu_seek(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    c3_m    fun_m = c3__seek;
    u3_noun pro   = u3z_find_3(fun_m, sut, way, hyp);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_seek(van, sut, way, hyp);

      return u3z_save_3(fun_m, sut, way, hyp, pro);
    }
  }

  u3_noun
  u3qfu_seep(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    u3_noun zar = u3qfu_seek(van, sut, way, hyp);
    u3_noun p_zar = u3h(zar);
    u3_noun q_zar = u3t(zar);

    if ( c3y != u3h(q_zar) ) {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun ret = u3nc(u3k(p_zar), u3k(u3t(q_zar)));

      u3z(zar);
      return ret;
    }
  }

  u3_noun
  u3wfu_seep(u3_noun cor)
  {
    u3_noun sut, way, hyp, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &way,
                               u3x_sam_3, &hyp,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qfu_seep(van, sut, way, hyp);
    }
  }

