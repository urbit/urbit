/* j/6/find.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  static u2_noun
  _find_in(u2_wire wir_r,
           u2_noun van,
           u2_noun sut,
           u2_noun dep,
           u2_noun way,
           u2_noun cog,
           u2_noun gil)
  {
    u2_noun p_sut, q_sut;

    if ( u2_yes == u2_stud(sut) ) {
      return u2_bc(wir_r, u2_rx(wir_r, dep), u2_nul);
    }
    else switch ( u2_h(sut) ) {
      default: return u2_bc(wir_r, u2_rx(wir_r, dep), u2_nul);

      case c3__cell: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          u2_noun taf = _find_in(wir_r, van, p_sut, dep, way, cog, gil);
          u2_noun p_taf = u2_h(taf);
          u2_noun q_taf = u2_t(taf);
          u2_noun ret;

          if ( u2_nul == q_taf ) {
            u2_noun bov = _find_in(wir_r, van, q_sut, p_taf, way, cog, gil);
            u2_noun p_bov = u2_h(bov);
            u2_noun q_bov = u2_t(bov);

            if ( u2_nul == q_bov ) {
              ret = u2_rx(wir_r, bov);
            } 
            else {
              u2_noun puq_bov, quq_bov;

              u2_mean(q_bov, 6, &puq_bov, 7, &quq_bov, 0);
              ret = u2_bq
                (wir_r, u2_rx(wir_r, p_bov),
                        u2_nul,
                        j2_mbc(Pt3, peg)(wir_r, 3, puq_bov),
                        u2_rx(wir_r, quq_bov));
            }
            u2_rl_lose(wir_r, bov);
          } 
          else {
            u2_noun puq_taf, quq_taf;

            u2_mean(q_taf, 6, &puq_taf, 7, &quq_taf, 0);
            ret = u2_bq
              (wir_r, u2_rx(wir_r, p_taf),
                      u2_nul,
                      j2_mbc(Pt3, peg)(wir_r, 2, puq_taf),
                      u2_rx(wir_r, quq_taf));
          }
          u2_rl_lose(wir_r, taf);
          return ret;
        }
      }
      case c3__core: {
        u2_noun pq_sut, qq_sut, rq_sut;
        u2_noun prq_sut, qrq_sut;

        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ||
             (u2_no == u2_as_trel(q_sut, &pq_sut, &qq_sut, &rq_sut)) ||
             (u2_no == u2_as_cell(rq_sut, &prq_sut, &qrq_sut)) )
        {
          return u2_bl_bail(wir_r, c3__fail);
        } else {
          u2_noun zem = j2_mby(Pt6, look)(wir_r, cog, qrq_sut);

          if ( (u2_nul != zem) && (0 != dep) ) {
            u2_noun ped;

            ped = j2_mbc(Pt1, dec)(wir_r, dep);
            u2_rz(wir_r, dep); dep = ped;

            u2_rl_lose(wir_r, zem);
            zem = u2_nul;
          }

          if ( u2_nul == zem ) {
            u2_noun taf = _find_in(wir_r, van, p_sut, dep, way, cog, gil);
            u2_noun p_taf = u2_h(taf);
            u2_noun q_taf = u2_t(taf);

            if ( u2_nul == q_taf ) {
              u2_rl_lose(wir_r, taf);
              return u2_bc(wir_r, u2_rx(wir_r, dep), u2_nul);
            } 
            else {
              u2_noun puq_taf, quq_taf;
              u2_noun pro;

              u2_mean(q_taf, 6, &puq_taf, 7, &quq_taf, 0);

              if ( u2_no == j2_mcy(Pt6, ut, park)
                    (wir_r, van, sut, way, puq_taf) )
              {
                u2_noun weh = j2_mcy(Pt6, ut, shep)
                  (wir_r, van, "way", 'a', u2_rx(wir_r, way));
                u2_noun waz = j2_mcy(Pt6, ut, shep)
                  (wir_r, van, "axis", 'd', u2_rx(wir_r, puq_taf));

                u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, weh));
                u2_bl_push(wir_r, u2_bc(wir_r, c3__mean, waz));
                return u2_bl_error(wir_r, "find-park");
              }
              else {
                pro = u2_bq
                  (wir_r, u2_rx(wir_r, p_taf),
                          u2_nul,
                          j2_mbc(Pt3, peg)(wir_r, 3, puq_taf),
                          u2_rx(wir_r, quq_taf));

                u2_rl_lose(wir_r, taf);
                return pro;
              }
            }
          }
          else {
            u2_noun u_zem = u2_t(zem);
            u2_noun pu_zem = u2_h(u_zem);
            u2_noun qu_zem = u2_t(u_zem);
            u2_noun mut;
            u2_noun pro;

            mut = u2_bt(wir_r, c3__core,
                               u2_rx(wir_r, p_sut),
                               u2_bt(wir_r, c3__gold,
                                            u2_rx(wir_r, qq_sut),
                                            u2_rx(wir_r, rq_sut)));

            pro = u2_bc
              (wir_r, 
               0,
               u2_bq(wir_r, 
                     u2_nul,
                     _1,
                     u2_no,
                     u2_bc
                       (wir_r, j2_mbc(Pt3, peg)(wir_r, 2, pu_zem),
                               u2_bc(wir_r, 
                                     u2_bc(wir_r, mut, u2_rx(wir_r, qu_zem)),
                                     u2_nul))));
            u2_rz(wir_r, zem);
            return pro;
          }
        }
      }
      case c3__face: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        } else { 
          if ( u2_no == u2_sing(cog, p_sut) ) {
            return u2_bc(wir_r, u2_rx(wir_r, dep), u2_nul);
          } else {
            if ( 0 == dep ) {
              return u2_bc
                (wir_r, 0, 
                        u2_bq(wir_r, u2_nul, _1, u2_yes, u2_rx(wir_r, q_sut)));
            } else {
              return u2_bc
                (wir_r, j2_mbc(Pt1, dec)(wir_r, dep), u2_nul);
            }
          }
        }
      }
      case c3__cube:
      case c3__fine: {
        u2_type fop = j2_mcy(Pt6, ut, repo)(wir_r, van, sut);
        u2_noun ret = _find_in(wir_r, van, fop, dep, way, cog, gil);
        
        u2_rz(wir_r, fop);
        return ret;
      }
      case c3__fork: {
        if ( (u2_no == u2_as_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u2_bl_bail(wir_r, c3__fail);
        }

        if ( (u2_yes == j2_mcc(Pt4, in, has)(wir_r, gil, q_sut)) ) {
          return _find_in(wir_r, van, p_sut, dep, way, cog, gil);
        } 
        else if ( (u2_yes == j2_mcc(Pt4, in, has)(wir_r, gil, p_sut)) ) {
          return _find_in(wir_r, van, q_sut, dep, way, cog, gil);
        } 
        else {
          u2_noun hax = _find_in(wir_r, van, p_sut, dep, way, cog, gil);
          u2_noun yor = _find_in(wir_r, van, q_sut, dep, way, cog, gil);
          u2_noun q_hax = u2_t(hax);
          u2_noun q_yor = u2_t(yor);
          u2_noun puq_hax, quq_hax;
          u2_noun puq_yor, quq_yor;
          u2_noun ret;

          if ( u2_nul != q_hax ) {
            u2_mean(q_hax, 6, &puq_hax, 7, &quq_hax, 0);
          }
          if ( u2_nul != q_yor ) {
            u2_mean(q_yor, 6, &puq_yor, 7, &quq_yor, 0);
          }

          while ( 1 ) {
            if ( u2_yes == u2_sing(hax, yor) ) {
              ret = u2_rx(wir_r, hax); break;
            }

            if ( u2_nul == q_hax ) {
              if ( u2_nul == q_yor ) {
                return u2_bl_error(wir_r, "find-fork");
              } 
              else {
                u2_noun pek = j2_mcy(Pt6, ut, peek)
                  (wir_r, van, p_sut, way, puq_yor);
                u2_noun nuz = j2_mcy(Pt6, ut, nest)
                  (wir_r, van, c3__void, u2_no, pek);

                u2_rz(wir_r, pek);

                if ( u2_yes == nuz ) {
                  ret = u2_rx(wir_r, yor); break;
                } 
                else return u2_bl_error(wir_r, "find-fork");
              }
            }
            else {
              if ( u2_nul == q_yor ) {
                u2_noun pek = j2_mcy(Pt6, ut, peek)
                  (wir_r, van, q_sut, way, puq_hax);
                u2_noun nuz = j2_mcy(Pt6, ut, nest)
                  (wir_r, van, c3__void, u2_no, pek);

                u2_rz(wir_r, pek);

                if ( u2_yes == nuz ) {
                  ret = u2_rx(wir_r, hax); break;
                } 
                else return u2_bl_error(wir_r, "find-fork");
              }
              else {
                if ( u2_no == u2_sing(puq_hax, puq_yor) ) {
                  return u2_bl_error(wir_r, "find-fork");
                }

                if ( u2_yes == u2_h(quq_hax) ) {
                  if ( u2_yes != u2_h(quq_yor) ) {
                    return u2_bl_error(wir_r, "find-fork");
                  } else {
                    u2_noun pquq_hax = u2_t(quq_hax);
                    u2_noun pquq_yor = u2_t(quq_yor);

                    ret = u2_bc
                      (wir_r, 
                       0,
                       u2_bq
                        (wir_r, 
                         u2_nul,
                         u2_rx(wir_r, puq_hax),
                         u2_yes,
                         j2_mby(Pt6, fork)(wir_r, pquq_hax, pquq_yor)));
                    break;
                  }
                }
                else {
                  if ( u2_yes == u2_h(quq_yor) ) {
                    return u2_bl_error(wir_r, "find-fork");
                  }
                  else {
                    u2_noun pquq_hax, qquq_hax, pquq_yor, qquq_yor;

                    u2_bi_cell(wir_r, u2_t(quq_hax), &pquq_hax, &qquq_hax);
                    u2_bi_cell(wir_r, u2_t(quq_yor), &pquq_yor, &qquq_yor);

                    if ( u2_no == u2_sing(pquq_hax, pquq_yor) ) {
                      return u2_bl_error(wir_r, "find-fork");
                    } else {
                      ret = u2_bc
                        (wir_r, 
                         0,
                         u2_bq
                          (wir_r, u2_nul,
                                  u2_rx(wir_r, puq_hax),
                                  u2_no,
                                  u2_bc
                                    (wir_r, u2_rx(wir_r, pquq_hax),
                                            j2_mbc(Pt2, weld)
                                              (wir_r, qquq_hax, qquq_yor))));
                      break;
                    }
                  }
                }
              }
            }
          }
          u2_rz(wir_r, yor);
          u2_rz(wir_r, hax);

          return ret;
        }
      } 
      case c3__hold: p_sut = u2_t(sut);
      {
        if ( (u2_yes == j2_mcc(Pt4, in, has)(wir_r, gil, sut)) ) {
          return u2_bc(wir_r, u2_rx(wir_r, dep), u2_nul);
        } 
        else {
          u2_noun zoc = j2_mcc(Pt4, in, put)(wir_r, gil, sut);
          u2_type fop = j2_mcy(Pt6, ut, repo)(wir_r, van, sut);
          u2_noun pro = _find_in(wir_r, van, fop, dep, way, cog, zoc);

          u2_rl_lose(wir_r, fop);
          u2_rl_lose(wir_r, zoc);

          return pro;
        }
      }
    }
  }

  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, find)(u2_wire wir_r, 
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dep,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun cog)                              //  retain
  {
    return _find_in(wir_r, van, sut, dep, way, cog, u2_nul);
  }

/* boilerplate
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, find)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, find)(u2_wire wir_r, 
                       u2_noun cor)                               //  retain
  {
    u2_noun sut, dep, way, cog, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &dep, 
                                u2_cv_sam_6, &way, 
                                u2_cv_sam_7, &cog, 
                                u2_cv_con, &van, 
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mcx(Pt6, ut, find)(wir_r, van, sut, dep, way, cog);
    }
  }

  u2_weak                                                         //  transfer
  j2_mci(Pt6, ut, find)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain 
                        u2_noun dep,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun cog)                              //  retain
  {
    u2_weak hoc = u2_ds_look(wir_r, van, "find");

    if ( u2_none == hoc ) {
      c3_assert(!"register find");
      return u2_none;
    } else {
      u2_weak von = u2_rl_molt(wir_r, van, u2_cv_sam, u2_rx(wir_r, sut), 0);
      u2_weak gat = u2_nk_soft(wir_r, von, hoc);
      u2_weak cor = u2_rl_molt(wir_r, gat, 
                                      u2_cv_sam_2, u2_rx(wir_r, dep), 
                                      u2_cv_sam_6, u2_rx(wir_r, way), 
                                      u2_cv_sam_7, u2_rx(wir_r, cog), 
                                      0);

      if ( (u2_none == j2_mcj(Pt6, ut, find)[0].xip) ) {
        u2_noun xip = u2_ds_find(wir_r, cor);
     
        c3_assert(u2_none != xip);
        j2_mcj(Pt6, ut, find)[0].xip = xip;
      }
      u2_rl_lose(wir_r, gat);
      return cor;
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, find)(u2_wire wir_r,
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dep,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun cog)                              //  retain
  {
    u2_ho_jet *jet_j = &j2_mcj(Pt6, ut, find)[0];

    if ( (jet_j->sat_s & u2_jet_live) && !(jet_j->sat_s & u2_jet_test) ) {
      if ( !(jet_j->sat_s & u2_jet_memo) ) {
        return j2_mcx(Pt6, ut, find)(wir_r, van, sut, dep, way, cog);
      }
      else {
        c3_m    fun_m = c3__find;
        u2_noun pro   = u2_rl_find_qual(wir_r, fun_m, sut, dep, way, cog);

        if ( u2_none != pro ) {
          return pro;
        }
        else {
          pro = j2_mcx(Pt6, ut, find)(wir_r, van, sut, dep, way, cog);

          return u2_rl_save_qual(wir_r, fun_m, sut, dep, way, cog, pro);
        }
      }
    }
    else {
      u2_noun cor, fol, pro;

      cor = j2_mci(Pt6, ut, find)(wir_r, van, sut, dep, way, cog);
      fol = u2_h(cor);

      pro = u2_ho_use(wir_r, jet_j, cor, fol);
      if ( u2_none == pro ) return u2_bl_bail(wir_r, c3__fail);

      u2_rz(wir_r, cor);
      u2_rz(wir_r, fol);

      return pro;
    }
  }

  u2_weak
  j2_mck(Pt6, ut, find)(u2_wire wir_r,
                        u2_noun cor)
  {
    u2_noun sut, dep, way, cog, van;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &dep, 
                                u2_cv_sam_6, &way, 
                                u2_cv_sam_7, &cog, 
                                u2_cv_con, &van, 
                                0)) ||
         (u2_none == (sut = u2_frag(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2_rq
        (wir_r, u2_rx(wir_r, sut), 
                u2_rx(wir_r, dep), 
                u2_rx(wir_r, way), 
                u2_rx(wir_r, cog));
    }
  }

/* structures
*/
  u2_ho_jet 
  j2_mcj(Pt6, ut, find)[] = {
    { ".2", c3__hevy, 
        j2_mc(Pt6, ut, find), 
        Tier6_b_memo,
        u2_none, u2_none,
        j2_mck(Pt6, ut, find), c3__find,
    },
    { }
  };
