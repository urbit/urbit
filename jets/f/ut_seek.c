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


  u3_noun
  _cqfu_seek(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    c3_assert(0);

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
#if 1
  u3_noun
  u3qfu_seek(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "seek");

    return u3n_kick_on(u3i_molt(gat, 
                                u3x_sam_2, 
                                u3k(way), 
                                u3x_sam_3, 
                                u3k(hyp),
                                0));
  }
#else
  u3_noun
  u3qfu_seek(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    c3_m    fun_m = c3__seek + !!u3r_at(u3qfu_van_vet, van);
    u3_noun pro   = u3z_find_3(fun_m, sut, way, hyp);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_seek(van, sut, way, hyp);

      return u3z_save_3(fun_m, sut, way, hyp, pro);
    }
  }
#endif

