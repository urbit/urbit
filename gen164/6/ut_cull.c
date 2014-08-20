/* j/6/cull.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun
  j2_mcx(Pt6, ut, cull)(u2_noun, u2_noun, u2_bean, u2_atom, u2_noun);

  static u2_noun
  _cull_in(
           u2_noun van,                                           //  retain
           u2_noun sut,                                           //  retain
           u2_bean pol,                                           //  retain
           u2_atom axe,                                           //  retain
           u2_noun ref,                                           //  retain
           u2_noun now,                                           //  retain
           u2_noun lat,                                           //  retain
           u2_noun vil)                                           //  retain
  {
    u2_noun ret, p_sut, q_sut;

    if ( u2_no == u2du(sut) ) switch ( sut ) {
      default: return u2_cm_bail(c3__fail);

      case c3__noun: {
        u2_noun pyr = j2_mcy(Pt6, ut, repo)(van, sut);
        u2_noun yot = j2_mcx(Pt6, ut, cull)(van, pyr, pol, axe, ref);

        if ( u2_yes == u2_cr_sing(pyr, yot) ) {
          ret = u2k(sut);
        } else {
          ret = u2k(yot);
        }
        u2z(pyr);
        u2z(yot);
        break;
      }
      case c3__void: {
        ret = c3__void;
      }
    }
    else switch ( u2h(sut) ) {
      default: return u2_cm_bail(c3__fail);

      case c3__atom: {
        ret = c3__void;
        break;
      }
      case c3__bull: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        u2_noun hic = j2_mcx(Pt6, ut, cull)
                                    (van, q_sut, pol, axe, ref);

        if ( u2_yes == u2_cr_sing(hic, q_sut) ) {
          ret = u2k(sut);
        } else {
          ret = j2_mby(Pt6, bull)(p_sut, hic);
        }
        u2z(hic);

        break;
      }
      case c3__cell: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        u2_noun hub;

        if ( 2 == now ) {
          hub = j2_mcx(Pt6, ut, cull)(van, p_sut, pol, lat, ref);
          ret = j2_mby(Pt6, cell)(hub, q_sut);
        } else {
          hub = j2_mcx(Pt6, ut, cull)(van, q_sut, pol, lat, ref);
          ret = j2_mby(Pt6, cell)(p_sut, hub);
        }
        u2z(hub);
        break;
      }
      case c3__core: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        if ( 3 != now ) {
          ret = u2k(sut);
        } else {
          u2_noun hub = j2_mcx(Pt6, ut, cull)
                                   (van, p_sut, pol, lat, ref);

          ret = j2_mby(Pt6, core)(hub, q_sut);

          u2z(hub);
        }
        break;
      }
      case c3__face: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        u2_noun hic = j2_mcx(Pt6, ut, cull)
                                    (van, q_sut, pol, axe, ref);

        if ( u2_yes == u2_cr_sing(hic, q_sut) ) {
          ret = u2k(sut);
        } else {
          ret = j2_mby(Pt6, face)(p_sut, hic);
        }
        u2z(hic);

        break;
      }
      case c3__fork: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
      {
        if ( u2_yes == j2_mcc(Pt4, in, has)(vil, sut) ) {
          return c3__void;
        } else {
          u2_noun zoc = j2_mcc(Pt4, in, put)(vil, sut);
          u2_noun sin = _cull_in
            (van, p_sut, pol, axe, ref, now, lat, zoc);
          u2_noun dex = _cull_in
            (van, q_sut, pol, axe, ref, now, lat, zoc);

          ret = j2_mby(Pt6, fork)(sin, dex);
          u2z(sin);
          u2z(dex);
          u2z(zoc);

          return ret;
        }
      }
      case c3__cube:
      case c3__hold:
      {
        u2_noun pyr = j2_mcy(Pt6, ut, repo)(van, sut);
        u2_noun yot = _cull_in(van, pyr, pol, axe, ref, now, lat, vil);

        if ( u2_yes == u2_cr_sing(pyr, yot) ) {
          if ( c3__void == pyr ) {
            ret = c3__void;
          }
          else ret = u2k(sut);
        } else {
          ret = u2k(yot);
        }
        u2z(pyr);
        u2z(yot);
        break;
      }
    }
    u2z(lat);
    return ret;
  }

  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, cull)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_bean pol,                              //  retain
                        u2_atom axe,                              //  retain
                        u2_noun ref)                              //  retain
  {
    if ( 1 == axe ) {
      if ( u2_yes == pol ) {
        return j2_mcy(Pt6, ut, fuse)(van, sut, ref);
      } else {
        return j2_mcy(Pt6, ut, crop)(van, sut, ref);
      }
    } else {
      u2_atom now = j2_mbc(Pt3, cap)(axe);
      u2_atom lat = j2_mbc(Pt3, mas)(axe);

      return _cull_in(van, sut, pol, axe, ref, now, lat, u2_nul);
    }
  }

/* boilerplate
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, cull)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, cull)(
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, axe, pol, ref, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &pol,
                                u2_cv_sam_6, &axe,
                                u2_cv_sam_7, &ref,
                                u2_cv_con, &van, 0)) ||
         (u2_no == u2ud(axe)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return j2_mcx(Pt6, ut, cull)(van, sut, pol, axe, ref);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, cull)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_bean pol,                              //  retain
                        u2_atom axe,                              //  retain
                        u2_noun ref)                              //  retain
  {
    u2_weak hoc = u2_cj_look(u2k(van), "cull");

    if ( u2_none == hoc ) {
      c3_assert(!"register cull");
      return u2_none;
    } else {
      u2_weak von = u2_ci_molt(u2k(van), u2_cv_sam, u2k(sut), 0);
      u2_weak gat = u2_cn_nock_on(von, hoc);
      u2_weak cor = u2_ci_molt(u2k(gat),
                                      u2_cv_sam_2, pol,
                                      u2_cv_sam_6, u2k(axe),
                                      u2_cv_sam_7, u2k(ref),
                                      0);

      if ( (u2_none == j2_mcj(Pt6, ut, cull)[0].xip) ) {
        u2_noun xip = u2_cj_find(u2k(cor));

        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, cull)[0].xip = xip;
      }
      u2z(gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, cull)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_bean pol,                              //  retain
                        u2_noun axe,                              //  retain
                        u2_noun ref)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, cull)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, cull)(van, sut, pol, axe, ref);
      }
      else {
        c3_m    fun_m = c3__cull;
        u2_noun pro   = u2_ch_find_4(fun_m, sut, pol, axe, ref);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, cull)(van, sut, pol, axe, ref);

          return u2_ch_save_4(fun_m, sut, pol, axe, ref, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, cull)(van, sut, pol, axe, ref);
      fol = u2h(cor);

      pro = u2_ho_use(jet_j, cor, fol);
      if ( u2_none == pro ) return u2_cm_bail(c3__fail);

      u2z(cor);
      u2z(fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, cull)(
                        u2_noun cor)
  {
    u2_noun sut, pol, axe, ref, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &pol,
                                u2_cv_sam_6, &axe,
                                u2_cv_sam_7, &ref,
                                u2_cv_con, &van,
                                0)) ||
         (pol > 1) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2nq
        (u2k(sut), pol, u2k(axe), u2k(ref));
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, cull)[] = {
    { ".2", c3__hevy,
        j2_mc(Pt6, ut, cull),
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pt6, ut, cull), c3__cull,
    },
    { }
  };
