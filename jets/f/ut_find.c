/* j/6/find.c
**
*/
#include "all.h"


/* logic
*/
  static u3_noun
  _fino_in(u3_noun van,
           u3_noun sut,
           u3_noun dep,
           u3_noun way,
           u3_noun cog,
           u3_noun gil)
  {
    u3_noun p_sut, q_sut;

    if ( c3y == u3ud(sut) ) {
      return u3nc(u3k(dep), u3_nul);
    }
    else switch ( u3h(sut) ) {
      default: return u3nc(u3k(dep), u3_nul);

      case c3__bull: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        }
        else {
          if ( c3n == u3r_sing(cog, u3h(p_sut)) ) {
            return _fino_in(van, q_sut, dep, way, cog, gil);
          }
          else {
            if ( 0 == dep ) {
              return u3nc(0,
                          u3nt(u3_nul,
                               1,
                               u3nt(2, u3k(p_sut),
                                    u3k(q_sut))));
            } else {
              return _fino_in(van, q_sut, u3qa_dec(dep), way, cog, gil);
            }
          }
        }
      }
      case c3__cell: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        } else {
          u3_noun taf = _fino_in(van, p_sut, dep, way, cog, gil);
          u3_noun p_taf = u3h(taf);
          u3_noun q_taf = u3t(taf);
          u3_noun ret;

          if ( u3_nul == q_taf ) {
            u3_noun bov = _fino_in(van, q_sut, p_taf, way, cog, gil);
            u3_noun p_bov = u3h(bov);
            u3_noun q_bov = u3t(bov);

            if ( u3_nul == q_bov ) {
              ret = u3k(bov);
            }
            else {
              u3_noun puq_bov, quq_bov;

              u3r_mean(q_bov, 6, &puq_bov, 7, &quq_bov, 0);
              ret = u3nq(u3k(p_bov),
                         u3_nul,
                         u3qc_peg(3, puq_bov),
                         u3k(quq_bov));
            }
            u3z(bov);
          }
          else {
            u3_noun puq_taf, quq_taf;

            u3r_mean(q_taf, 6, &puq_taf, 7, &quq_taf, 0);
            ret = u3nq(u3k(p_taf),
                       u3_nul,
                       u3qc_peg(2, puq_taf),
                       u3k(quq_taf));
          }
          u3z(taf);
          return ret;
        }
      }
      case c3__core: {
        u3_noun pq_sut, qq_sut, rq_sut;
        u3_noun prq_sut, qrq_sut;

        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ||
             (c3n == u3r_trel(q_sut, &pq_sut, &qq_sut, &rq_sut)) ||
             (c3n == u3r_cell(rq_sut, &prq_sut, &qrq_sut)) )
        {
          return u3m_bail(c3__fail);
        } else {
          u3_noun zem = u3qf_look(cog, qrq_sut);

          if ( (u3_nul != zem) && (0 != dep) ) {
            u3_noun ped;

            ped = u3qa_dec(dep);
            u3z(dep); dep = ped;

            u3z(zem);
            zem = u3_nul;
          }

          if ( u3_nul == zem ) {
            u3_noun taf = _fino_in(van, p_sut, dep, way, cog, gil);
            u3_noun p_taf = u3h(taf);
            u3_noun q_taf = u3t(taf);

            if ( u3_nul == q_taf ) {
              u3z(taf);
              return u3nc(u3k(dep), u3_nul);
            }
            else {
              u3_noun puq_taf, quq_taf;
              u3_noun pro;

              u3r_mean(q_taf, 6, &puq_taf, 7, &quq_taf, 0);

              if ( c3n == u3qfu_park(van, sut, way, puq_taf) )
              {
                u3_noun weh = u3qfu_shep(van, "way", 'a', u3k(way));
                u3_noun waz = u3qfu_shep(van, "axis", 'd', u3k(puq_taf));

                u3t_push(u3nc(c3__mean, weh));
                u3t_push(u3nc(c3__mean, waz));
                return u3m_error("find-park");
              }
              else {
                pro = u3nq(u3k(p_taf),
                           u3_nul,
                           u3qc_peg(3, puq_taf),
                           u3k(quq_taf));

                u3z(taf);
                return pro;
              }
            }
          }
          else {
            u3_noun u_zem = u3t(zem);
            u3_noun pu_zem = u3h(u_zem);
            u3_noun qu_zem = u3t(u_zem);
            u3_noun mut;
            u3_noun pro;

            mut = u3nt(c3__core,
                       u3k(p_sut),
                       u3nt(c3__gold,
                            u3k(qq_sut),
                            u3k(rq_sut)));

            pro = u3nc(0,
                       u3nq(u3_nul,
                            1,
                            1,
                            u3nc(u3qc_peg(2, pu_zem),
                                 u3nc(u3nc(mut, u3k(qu_zem)),
                                      u3_nul))));
            u3z(zem);
            return pro;
          }
        }
      }
      case c3__face: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        } else {
          if ( c3n == u3r_sing(cog, p_sut) ) {
            return u3nc(u3k(dep), u3_nul);
          } else {
            if ( 0 == dep ) {
              return u3nc(0, u3nq(u3_nul, 1, 0, u3k(q_sut)));
            } else {
              return u3nc
                (u3qa_dec(dep), u3_nul);
            }
          }
        }
      }
      case c3__cube: {
        u3_noun fop = u3qfu_repo(van, sut);
        u3_noun pro = _fino_in(van, fop, dep, way, cog, gil);

        u3z(fop);
        return pro;
      }
      case c3__fork: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        }

        if ( (c3y == u3qdi_has(gil, q_sut)) ) {
          return _fino_in(van, p_sut, dep, way, cog, gil);
        }
        else if ( (c3y == u3qdi_has(gil, p_sut)) ) {
          return _fino_in(van, q_sut, dep, way, cog, gil);
        }
        else {
          u3_noun hax = _fino_in(van, p_sut, dep, way, cog, gil);
          u3_noun yor = _fino_in(van, q_sut, dep, way, cog, gil);
          u3_noun p_hax = u3h(hax);
          u3_noun p_yor = u3h(yor);
          u3_noun q_hax = u3t(hax);
          u3_noun q_yor = u3t(yor);
          u3_noun puq_hax, quq_hax;
          u3_noun puq_yor, quq_yor;
          u3_noun ret = 0;

          if ( u3_nul != q_hax ) {
            u3r_mean(q_hax, 6, &puq_hax, 7, &quq_hax, 0);
          }
          if ( u3_nul != q_yor ) {
            u3r_mean(q_yor, 6, &puq_yor, 7, &quq_yor, 0);
          }

          if ( c3y == u3r_sing(hax, yor) ) {
            ret = u3k(hax);
          }
          else {
            if ( (c3n == u3r_sing(p_hax, p_yor)) ||
                 ((u3_nul == q_hax) || (u3_nul == q_yor)) ||
                 (c3n == u3r_sing(puq_hax, puq_yor)) ||
                 (c3n == u3r_sing(u3h(quq_hax), u3h(quq_yor))) )
            {
              return u3m_error("find-fork");
            }
            switch ( u3h(quq_hax) ) {
              case 0: {
                u3_noun pquq_hax = u3t(quq_hax);
                u3_noun pquq_yor = u3t(quq_yor);

                ret = u3nc(u3k(p_hax),
                           u3nq(u3_nul,
                                u3k(puq_hax),
                                0,
                                u3qf_fork(pquq_hax, pquq_yor)));
                break;
              }
              case 1: {
                u3_noun pquq_hax, qquq_hax, pquq_yor, qquq_yor;

                u3x_cell(u3t(quq_hax), &pquq_hax, &qquq_hax);
                u3x_cell(u3t(quq_yor), &pquq_yor, &qquq_yor);

                if ( c3n == u3r_sing(pquq_hax, pquq_yor) ) {
                  return u3m_error("find-fork");
                } else {
                  ret = u3nc(u3k(p_hax),
                             u3nq(u3_nul,
                                  u3k(puq_hax),
                                  1,
                                  u3nc(u3k(pquq_hax),
                                       u3qb_weld(qquq_hax, qquq_yor))));
                  break;
                }
              }
              case 2: {
                u3_noun pquq_hax, qquq_hax, pquq_yor, qquq_yor;
                u3_noun ppquq_hax, qpquq_hax, rpquq_hax, spquq_hax;
                u3_noun ppquq_yor, qpquq_yor, rpquq_yor, spquq_yor;

                u3x_cell(u3t(quq_hax), &pquq_hax, &qquq_hax);
                u3x_cell(u3t(quq_yor), &pquq_yor, &qquq_yor);
                u3x_qual(pquq_hax,
                         &ppquq_hax, &qpquq_hax, &rpquq_hax, &spquq_hax);
                u3x_qual(pquq_yor,
                         &ppquq_yor, &qpquq_yor, &rpquq_yor, &spquq_yor);

                if ( (c3n == u3r_sing(ppquq_hax, ppquq_yor)) ||
                     (c3n == u3r_sing(qpquq_hax, qpquq_yor)) ||
                     (c3n == u3r_sing(rpquq_hax, rpquq_yor)) )
                {
                  return u3m_error("find-fork");
                }
                else {
                  ret = u3nc(u3k(p_hax),
                             u3nq(u3_nul,
                                  u3k(puq_hax),
                                  2,
                                  u3nc(u3nq(u3k(ppquq_hax),
                                            u3k(qpquq_hax),
                                            u3k(rpquq_hax),
                                            u3qf_fork(spquq_hax,
                                                      spquq_yor)),
                                       u3qf_fork(qquq_hax, qquq_yor))));
                  break;
                }
              }
            }
          }
          u3z(yor);
          u3z(hax);

          return ret;
        }
      }
      case c3__hold: p_sut = u3t(sut);
      {
        if ( (c3y == u3qdi_has(gil, sut)) ) {
          return u3nc(u3k(dep), u3_nul);
        }
        else {
          u3_noun zoc = u3qdi_put(gil, sut);
          u3_noun fop = u3qfu_repo(van, sut);
          u3_noun pro = _fino_in(van, fop, dep, way, cog, zoc);

          u3z(fop);
          u3z(zoc);

          return pro;
        }
      }
    }
  }

  static u3_noun
  _find_in(u3_noun van,
           u3_noun sut,
           u3_noun dep,
           u3_noun way,
           u3_noun cog,
           u3_noun gil)
  {
    u3_noun p_sut, q_sut, pp_sut, qp_sut, rp_sut, sp_sut;

    if ( c3y == u3ud(sut) ) {
      return u3nc(u3k(dep), u3_nul);
    }
    else switch ( u3h(sut) ) {
      default: return u3nc(u3k(dep), u3_nul);

      case c3__bull: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ||
             (c3n == u3r_qual(p_sut, &pp_sut, &qp_sut, &rp_sut, &sp_sut)) )
        {
          return u3m_bail(c3__fail);
        }
        else {
          if ( c3n == u3r_sing(cog, pp_sut) ) {
            return _find_in
              (van, q_sut, dep, way, cog, gil);
          }
          else {
            if ( 0 == dep ) {
              return u3nc(0,
                          u3nq(u3_nul,
                               u3k(rp_sut),
                               c3y,
                               u3k(sp_sut)));
            } else {
              return _find_in(van, q_sut, u3qa_dec(dep), way, cog, gil);
              return u3nc(u3qa_dec(dep), u3_nul);
            }
          }
        }
      }
      case c3__cell: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        } else {
          u3_noun taf = _find_in(van, p_sut, dep, way, cog, gil);
          u3_noun p_taf = u3h(taf);
          u3_noun q_taf = u3t(taf);
          u3_noun ret;

          if ( u3_nul == q_taf ) {
            u3_noun bov = _find_in(van, q_sut, p_taf, way, cog, gil);
            u3_noun p_bov = u3h(bov);
            u3_noun q_bov = u3t(bov);

            if ( u3_nul == q_bov ) {
              ret = u3k(bov);
            }
            else {
              u3_noun puq_bov, quq_bov;

              u3r_mean(q_bov, 6, &puq_bov, 7, &quq_bov, 0);
              ret = u3nq(u3k(p_bov),
                         u3_nul,
                         u3qc_peg(3, puq_bov),
                         u3k(quq_bov));
            }
            u3z(bov);
          }
          else {
            u3_noun puq_taf, quq_taf;

            u3r_mean(q_taf, 6, &puq_taf, 7, &quq_taf, 0);
            ret = u3nq(u3k(p_taf),
                       u3_nul,
                       u3qc_peg(2, puq_taf),
                       u3k(quq_taf));
          }
          u3z(taf);
          return ret;
        }
      }
      case c3__core: {
        u3_noun pq_sut, qq_sut, rq_sut;
        u3_noun prq_sut, qrq_sut;

        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ||
             (c3n == u3r_trel(q_sut, &pq_sut, &qq_sut, &rq_sut)) ||
             (c3n == u3r_cell(rq_sut, &prq_sut, &qrq_sut)) )
        {
          return u3m_bail(c3__fail);
        } else {
          u3_noun zem = u3qf_look(cog, qrq_sut);

          if ( (u3_nul != zem) && (0 != dep) ) {
            u3_noun ped;

            ped = u3qa_dec(dep);
            u3z(dep); dep = ped;

            u3z(zem);
            zem = u3_nul;
          }

          if ( u3_nul == zem ) {
            u3_noun taf = _find_in(van, p_sut, dep, way, cog, gil);
            u3_noun p_taf = u3h(taf);
            u3_noun q_taf = u3t(taf);

            if ( u3_nul == q_taf ) {
              u3z(taf);
              return u3nc(u3k(dep), u3_nul);
            }
            else {
              u3_noun puq_taf, quq_taf;
              u3_noun pro;

              u3r_mean(q_taf, 6, &puq_taf, 7, &quq_taf, 0);

              if ( c3n == u3qfu_park(van, sut, way, puq_taf) )
              {
                u3_noun weh = u3qfu_shep
                  (van, "way", 'a', u3k(way));
                u3_noun waz = u3qfu_shep
                  (van, "axis", 'd', u3k(puq_taf));

                u3t_push(u3nc(c3__mean, weh));
                u3t_push(u3nc(c3__mean, waz));
                return u3m_error("find-park");
              }
              else {
                pro = u3nq(u3k(p_taf),
                           u3_nul,
                           u3qc_peg(3, puq_taf),
                           u3k(quq_taf));

                u3z(taf);
                return pro;
              }
            }
          }
          else {
            u3_noun u_zem = u3t(zem);
            u3_noun pu_zem = u3h(u_zem);
            u3_noun qu_zem = u3t(u_zem);
            u3_noun mut;
            u3_noun pro;

            mut = u3nt(c3__core,
                       u3k(p_sut),
                       u3nt(c3__gold,
                            u3k(qq_sut),
                            u3k(rq_sut)));

            pro = u3nc(0,
                       u3nq(u3_nul,
                            1,
                            c3n,
                            u3nc(u3qc_peg(2, pu_zem),
                                 u3nc(u3nc(mut, u3k(qu_zem)),
                                      u3_nul))));
            u3z(zem);
            return pro;
          }
        }
      }
      case c3__face: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        } else {
          if ( c3n == u3r_sing(cog, p_sut) ) {
            return u3nc(u3k(dep), u3_nul);
          } else {
            if ( 0 == dep ) {
              return u3nc(0,
                          u3nq(u3_nul, 
                               1, 
                               c3y, 
                               u3k(q_sut)));
            } else {
              return u3nc(u3qa_dec(dep), u3_nul);
            }
          }
        }
      }
      case c3__cube: {
        u3_noun fop = u3qfu_repo(van, sut);
        u3_noun pro = _find_in(van, fop, dep, way, cog, gil);

        u3z(fop);
        return pro;
      }
      case c3__fork: {
        if ( (c3n == u3r_trel(sut, 0, &p_sut, &q_sut)) ) {
          return u3m_bail(c3__fail);
        }

        if ( (c3y == u3qdi_has(gil, q_sut)) ) {
          return _find_in(van, p_sut, dep, way, cog, gil);
        }
        else if ( (c3y == u3qdi_has(gil, p_sut)) ) {
          return _find_in(van, q_sut, dep, way, cog, gil);
        }
        else {
          u3_noun hax = _find_in(van, p_sut, dep, way, cog, gil);
          u3_noun yor = _find_in(van, q_sut, dep, way, cog, gil);
          u3_noun p_hax = u3h(hax);
          u3_noun p_yor = u3h(yor);
          u3_noun q_hax = u3t(hax);
          u3_noun q_yor = u3t(yor);
          u3_noun puq_hax, quq_hax;
          u3_noun puq_yor, quq_yor;
          u3_noun ret = 0;

          if ( u3_nul != q_hax ) {
            u3r_mean(q_hax, 6, &puq_hax, 7, &quq_hax, 0);
          }
          if ( u3_nul != q_yor ) {
            u3r_mean(q_yor, 6, &puq_yor, 7, &quq_yor, 0);
          }

          if ( c3y == u3r_sing(hax, yor) ) {
            ret = u3k(hax);
          }
          else {
            if ( (c3n == u3r_sing(p_hax, p_yor)) ||
                 ((u3_nul == q_hax) || (u3_nul == q_yor)) ||
                 (c3n == u3r_sing(puq_hax, puq_yor)) ||
                 (c3n == u3r_sing(u3h(quq_hax), u3h(quq_yor))) )
            {
              return u3m_error("find-fork");
            }
            switch ( u3h(quq_hax) ) {
              case c3y: {
                u3_noun pquq_hax = u3t(quq_hax);
                u3_noun pquq_yor = u3t(quq_yor);

                ret = u3nc(u3k(p_hax),
                           u3nq(u3_nul,
                                u3k(puq_hax),
                                c3y,
                                u3qf_fork(pquq_hax, pquq_yor)));
                break;
              }
              case c3n: {
                u3_noun pquq_hax, qquq_hax, pquq_yor, qquq_yor;

                u3x_cell(u3t(quq_hax), &pquq_hax, &qquq_hax);
                u3x_cell(u3t(quq_yor), &pquq_yor, &qquq_yor);

                if ( c3n == u3r_sing(pquq_hax, pquq_yor) ) {
                  return u3m_error("find-fork");
                } else {
                  ret = u3nc(u3k(p_hax),
                             u3nq(u3_nul,
                                  u3k(puq_hax),
                                  c3n,
                                  u3nc(u3k(pquq_hax),
                                       u3qb_weld
                                       (qquq_hax, qquq_yor))));
                  break;
                }
              }
            }
          }
          u3z(yor);
          u3z(hax);

          return ret;
        }
      }
      case c3__hold: p_sut = u3t(sut);
      {
        if ( (c3y == u3qdi_has(gil, sut)) ) {
          return u3nc(u3k(dep), u3_nul);
        }
        else {
          u3_noun zoc = u3qdi_put(gil, sut);
          u3_noun fop = u3qfu_repo(van, sut);
          u3_noun pro = _find_in(van, fop, dep, way, cog, zoc);

          u3z(fop);
          u3z(zoc);

          return pro;
        }
      }
    }
  }

  u3_noun
  _cqfu_find(u3_noun van,
             u3_noun sut,
             u3_noun dep,
             u3_noun way,
             u3_noun cog)
  {
    return _find_in(van, sut, dep, way, cog, u3_nul);
  }

  u3_noun
  _cqfu_fino(u3_noun van,
             u3_noun sut,
             u3_noun dep,
             u3_noun way,
             u3_noun cog)
  {
    return _fino_in(van, sut, dep, way, cog, u3_nul);
  }

/* boilerplate
*/
  u3_noun
  u3wfu_fino(u3_noun cor)
  {
    u3_noun sut, dep, way, cog, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &dep,
                               u3x_sam_6, &way,
                               u3x_sam_7, &cog,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_fino(van, sut, dep, way, cog);
    }
  }

  u3_noun
  u3qfu_fino(u3_noun van,
             u3_noun sut,
             u3_noun dep,
             u3_noun way,
             u3_noun cog)
  {
    c3_m    fun_m = c3__fino;
    u3_noun pro   = u3z_find_4(fun_m, sut, dep, way, cog);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_fino(van, sut, dep, way, cog);

      return u3z_save_4(fun_m, sut, dep, way, cog, pro);
    }
  }


/* boilerplate
*/
  u3_noun
  u3wfu_find(u3_noun cor)
  {
    u3_noun sut, dep, way, cog, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &dep,
                               u3x_sam_6, &way,
                               u3x_sam_7, &cog,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_find(van, sut, dep, way, cog);
    }
  }

  u3_noun
  u3qfu_find(u3_noun van,
             u3_noun sut,
             u3_noun dep,
             u3_noun way,
             u3_noun cog)
  {
    c3_m    fun_m = c3__find;
    u3_noun pro   = u3z_find_4(fun_m, sut, dep, way, cog);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_find(van, sut, dep, way, cog);

      return u3z_save_4(fun_m, sut, dep, way, cog, pro);
    }
  }
