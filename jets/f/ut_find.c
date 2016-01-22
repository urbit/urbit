/* j/6/find.c
**
*/
#include "all.h"

  static u3_noun
  _find_buck(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun p_heg,
             u3_noun q_heg,
             u3_noun axe,
             u3_noun lon,
             u3_noun gil);

  static u3_noun
  u3qfu_felt(u3_noun van,
             u3_noun sut,
             u3_noun lap)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "felt");

    return u3n_kick_on(u3i_molt(gat, 
                                u3x_sam, 
                                u3k(lap), 
                                0));
  }

  static u3_noun
  u3qfu_fine(u3_noun van,
             u3_noun sut,
             u3_noun tor)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "fine");

    return u3n_kick_on(u3i_molt(gat, 
                                u3x_sam, 
                                u3k(tor), 
                                0));
  }

  static u3_noun
  u3qfu_funk(u3_noun van,
             u3_noun fid)
  {
    u3_noun gat = u3j_hook(u3k(van), "funk");

    return u3n_kick_on(u3i_molt(gat, 
                                u3x_sam, 
                                u3k(fid), 
                                0));
  }

  extern void
  u3_lo_tank(c3_l tab_l, u3_noun tac);

  void
  _funk_dump(u3_noun van,
             c3_c*   cap_c,
             u3_noun fid)
  {
    u3_noun foj;

    fprintf(stderr, "%s\r\n", cap_c);
    
    foj = u3qfu_funk(van, fid);
    u3_lo_tank(1, foj);
  }

  static u3_noun
  u3qfu_perk(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun met)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "perk");

    return u3n_kick_on(u3i_molt(gat, 
                                u3x_sam_2, 
                                u3k(way), 
                                u3x_sam_3, 
                                u3k(met),
                                0));
  }


  static u3_noun
  _find_buck_here(u3_noun van,
                  u3_noun sut,
                  u3_noun way,
                  u3_noun p_heg,
                  u3_noun q_heg,
                  u3_noun axe,
                  u3_noun lon,
                  u3_noun gil)
  {
    if ( 0 == p_heg ) {
      return u3nq
        (c3y,
         u3nt(u3_nul,
              u3nc(u3_nul, u3k(axe)),
              u3k(lon)),
         c3y,
         u3k(sut));
    } 
    else {
      return u3nt
        (c3n, c3y, u3qa_dec(p_heg));
    }
  }
  static u3_noun
  _find_buck_lose(u3_noun van,
                  u3_noun sut,
                  u3_noun way,
                  u3_noun p_heg,
                  u3_noun q_heg,
                  u3_noun axe,
                  u3_noun lon,
                  u3_noun gil)
  {
    return u3nt(c3n, c3y, u3k(p_heg));
  }
  static u3_noun
  _find_buck_stop(u3_noun van,
                  u3_noun sut,
                  u3_noun way,
                  u3_noun p_heg,
                  u3_noun q_heg,
                  u3_noun axe,
                  u3_noun lon,
                  u3_noun gil)
  {
    if ( u3_nul == q_heg ) {
      return _find_buck_here(van, sut, way, p_heg, q_heg, axe, lon, gil);
    }
    else {
      return _find_buck_lose(van, sut, way, p_heg, q_heg, axe, lon, gil);
    }
  }

  static u3_noun
  _find_buck_cell(u3_noun van,
                  u3_noun sut,
                  u3_noun way,
                  u3_noun p_heg,
                  u3_noun q_heg,
                  u3_noun axe,
                  u3_noun lon,
                  u3_noun gil)
  {
    u3_noun p_sut, q_sut;

    u3x_cell(u3t(sut), &p_sut, &q_sut);
    {
      if ( u3_nul == q_heg ) {
        return _find_buck_here(van, sut, way, p_heg, q_heg, axe, lon, gil);
      }
      else {
        u3_noun exa = u3qc_peg(axe, 2);
        u3_noun hed = _find_buck(van, p_sut, way, p_heg, q_heg, exa, lon, gil);

        u3z(exa);
        if ( (u3_nul == hed) || (c3y == u3h(hed)) || (c3n == u3h(u3t(hed))) ) {
          return hed;
        }
        else {
          u3_noun exa = u3qc_peg(axe, 3);
          u3_noun tal = _find_buck
            (van, q_sut, way, u3t(u3t(hed)), q_heg, exa, lon, gil);

          u3z(exa);
          u3z(hed);

          return tal;
        }
      }
    }
  }

  static u3_noun
  _find_buck_core(u3_noun van,
                  u3_noun sut,
                  u3_noun way,
                  u3_noun p_heg,
                  u3_noun q_heg,
                  u3_noun axe,
                  u3_noun lon,
                  u3_noun gil)
  {
    u3_noun p_sut, q_sut, pq_sut, qq_sut, rq_sut, prq_sut, qrq_sut;

    u3x_cell(u3t(sut), &p_sut, &q_sut);
    u3x_trel(q_sut, &pq_sut, &qq_sut, &rq_sut);
    u3x_cell(rq_sut, &prq_sut, &qrq_sut);
    {
      if ( u3_nul == q_heg ) {
        return _find_buck_here(van, sut, way, p_heg, q_heg, axe, lon, gil);
      }
      else {
        u3_noun zem = u3qf_look(u3t(q_heg), qrq_sut);

        if ( (u3_nul != zem) && (0 != p_heg) ) {
          u3_noun ped;

          ped = u3qa_dec(p_heg);
          u3z(p_heg); p_heg = ped;

          u3z(zem);
          zem = u3_nul;
        }

        if ( u3_nul != zem ) {
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

          pro = u3nt
            (c3y,
             u3nc(u3nc(u3_nul, u3k(axe)), u3k(lon)),
             u3nt(c3n,
                  u3qc_peg(2, pu_zem),
                  u3nt(u3nc(mut, u3k(qu_zem)), u3_nul, u3_nul)));
          
          u3z(zem);
          return pro;
        }
        else {
          u3_noun pec = u3qfu_perk(van, sut, way, pq_sut);
          u3_noun pro;

          if ( c3n == u3h(pec) ) {
            pro = _find_buck_lose(van, sut, way, p_heg, q_heg, axe, lon, gil);
          }
          else if ( c3y == u3t(pec) ) {
            u3_noun exa = u3qc_peg(axe, 3);

            pro = _find_buck(van, p_sut, way, p_heg, q_heg, exa, lon, gil);
            u3z(exa);
          }
          else {
            u3_noun sam = u3qfu_peek(van, p_sut, way, 2);
            u3_noun exa = u3qc_peg(axe, 6);

            pro = _find_buck(van, sam, way, p_heg, q_heg, exa, lon, gil);
            u3z(exa);
            u3z(sam);
          }
          u3z(pec);
          return pro;
        }
      }
    }
  }

  static u3_noun 
  _find_twin(u3_noun van,
             u3_noun hax, 
             u3_noun yor)
  {
    if ( c3y == u3r_sing(hax, yor) ) {
      return u3k(hax);
    }
    else if ( u3_nul == hax ) {
      return u3k(yor);
    }
    else if ( u3_nul == yor ) {
      return u3k(hax);
    }
    else if ( c3n == u3h(hax) ) {
      if ( (c3n != u3h(yor)) ) {
        return u3m_error("find-fork-a");
      }
      else {
        u3_noun p_hax = u3t(hax);                   //  (each @ud {span nock})
        u3_noun p_yor = u3t(yor);                   //  (each @ud {span nock})

        if ( (c3n != u3h(p_hax)) || (c3n != u3h(p_yor)) ) {
          return u3m_error("find-fork-b");
        }
        else {
          u3_noun pp_hax = u3t(p_hax);              //  {span nock}
          u3_noun pp_yor = u3t(p_yor);              //  {span nock}
          u3_noun ppp_hax = u3h(pp_hax);            //  span
          u3_noun ppp_yor = u3h(pp_yor);            //  span
          u3_noun qpp_hax = u3h(pp_hax);            //  nock
          u3_noun qpp_yor = u3h(pp_yor);            //  nock

          if ( c3n == u3r_sing(qpp_hax, qpp_yor) ) {
            return u3m_error("find-fork-c");
          }
          return u3nq(c3n,
                      c3n,
                      u3kf_fork(u3nt(u3k(ppp_hax), u3k(ppp_yor), u3_nul)),
                      u3k(qpp_hax));
        }
      }
    }
    else {
      if ( c3n == u3h(yor) ) {
        return u3m_error("find-fork-d");
      }
      else {
        u3_noun p_hax = u3t(hax);                   //  {vein opal}
        u3_noun p_yor = u3t(yor);                   //  {vein opal}
        u3_noun pp_hax = u3h(p_hax);                //  vein
        u3_noun pp_yor = u3h(p_yor);                //  vein
        u3_noun qp_hax = u3t(p_hax);                //  opal
        u3_noun qp_yor = u3t(p_yor);                //  opal
     
        if ( c3n == u3r_sing(pp_hax, pp_yor) ) {
          return u3m_error("find-fork-e");
        }
        else {
          if ( c3y == u3h(qp_hax) ) {
            if ( c3y != u3h(qp_yor) ) {
              return u3m_error("find-fork-f");
            } else {
              u3_noun pqp_hax = u3t(qp_hax);        //  span
              u3_noun pqp_yor = u3t(qp_yor);        //  span

              return 
                u3nq(c3y,
                     u3k(pp_hax),
                     c3y,
                     u3kf_fork(u3nt(u3k(pqp_hax), u3k(pqp_yor), u3_nul)));
            }
          }
          else if ( c3n != u3h(qp_yor) ) {
            return u3m_error("find-fork-g");
          } else {
            u3_noun pqp_hax = u3h(u3t(qp_hax));     //  axis
            u3_noun pqp_yor = u3h(u3t(qp_yor));     //  axis
            u3_noun qqp_hax = u3t(u3t(qp_hax));     //  (set {span foot})
            u3_noun qqp_yor = u3t(u3t(qp_yor));     //  (set {span foot})

            if ( c3n == u3r_sing(pqp_hax, pqp_yor) ) {
              return u3m_error("find-fork-h");
            } else {
              return 
                u3nt(c3y, 
                     u3k(pp_hax),
                     u3nt(c3n, u3k(pqp_hax), u3qdi_uni(qqp_hax, qqp_yor)));
            }
          }
        }
      }
    }
    // error: { return u3m_error("find-fork"); }
  }

  static u3_noun
  _find_buck_fork_twin(u3_noun van, u3_noun wiz)
  {
    if ( u3_nul == wiz ) {
      return u3_nul;
    } 
    else if ( u3_nul == u3t(wiz) ) {
      return u3k(u3h(wiz));
    }
    else {
      u3_noun hax = u3k(u3h(wiz));
      u3_noun yor = _find_buck_fork_twin(van, u3t(wiz));
      u3_noun fid = _find_twin(van, hax, yor);

      u3z(hax);
      u3z(yor);

      return fid;
    }
  }

  static u3_noun 
  _find_buck_fork_turn(u3_noun van,
                       u3_noun yed,
                       u3_noun way,
                       u3_noun p_heg,
                       u3_noun q_heg,
                       u3_noun axe,
                       u3_noun lon,
                       u3_noun gil)
  {
    if ( u3_nul == yed ) {
      return u3_nul;
    } 
    else {
      u3_noun fid = _find_buck(van, u3h(yed), way, p_heg, q_heg, axe, lon, gil);

      return u3nc
        (fid,
         _find_buck_fork_turn
            (van, u3t(yed), way, p_heg, q_heg, axe, lon, gil));
    }
  }
  
  static u3_noun
  _find_buck_fork(u3_noun van,
                  u3_noun sut,
                  u3_noun way,
                  u3_noun p_heg,
                  u3_noun q_heg,
                  u3_noun axe,
                  u3_noun lon,
                  u3_noun gil)
  {
    u3_noun p_sut = u3t(sut);
    u3_noun yed = u3qdi_tap(p_sut, u3_nul);
    u3_noun wiz;
    u3_noun ret;

    wiz = _find_buck_fork_turn(van, yed, way, p_heg, q_heg, axe, lon, gil);
    u3z(yed);
 
    ret = _find_buck_fork_twin(van, wiz);
    u3z(wiz);
   
    return ret;
  }

  static u3_noun
  _find_buck_face_next(u3_noun van,
                       u3_noun q_sut,
                       u3_noun qp_sut,
                       u3_noun way,
                       u3_noun p_heg,
                       u3_noun q_heg,
                       u3_noun axe,
                       u3_noun lon,
                       u3_noun gil)
  {
    if ( u3_nul == qp_sut ) {
      u3_noun nol = u3nc(u3_nul, u3k(lon));
      u3_noun ret = _find_buck(van, q_sut, way, p_heg, q_heg, axe, nol, gil);
      u3z(nol);
      return ret;
    }
    else {
      u3_noun iqp_sut = u3h(qp_sut);      //  (pair span nock)
      u3_noun tqp_sut = u3t(qp_sut);      //  (list (pair span nock))
      u3_noun piqp_sut = u3h(iqp_sut);    //  span
      u3_noun qiqp_sut = u3t(iqp_sut);    //  nock
      u3_noun fid = _find_buck            //  pony
        (van, piqp_sut, way, p_heg, q_heg, 1, u3_nul, u3_nul);

      if ( u3_nul == fid ) {
        return u3_nul;
      }
      else if ( (c3n == u3h(fid)) && (c3y == u3h(u3t(fid))) ) {
        u3_noun ret;

        ret = _find_buck_face_next
          (van, q_sut, tqp_sut, way, p_heg, q_heg, u3t(u3t(fid)), lon, gil);
        u3z(fid);
        return ret;
      }
      else {
        u3_noun tor;                      //  port
        u3_noun vat;                      //  (pair span nock)
        u3_noun ret;
        u3_noun dog = u3nc(0, u3k(axe));  //  nock

        if ( c3y == u3h(fid) ) {
          tor = u3k(fid);
        } else {
          tor = u3nc(c3n, u3k(u3t(u3t(fid))));
        }
        u3z(fid);

        vat = u3qfu_fine(van, q_sut, tor);
        u3z(tor);

        ret = u3nq
          (c3n, 
           c3n, 
           u3k(u3h(vat)), 
           u3qf_comb(u3t(vat), u3qf_comb(dog, qiqp_sut)));

        u3z(vat);
        u3z(dog);

        return ret;
      }
    }
  }

  static u3_noun
  _find_buck_face(u3_noun van,
                  u3_noun sut,
                  u3_noun way,
                  u3_noun p_heg,
                  u3_noun q_heg,
                  u3_noun axe,
                  u3_noun lon,
                  u3_noun gil)
  {
    u3_noun p_sut, q_sut;

    u3x_cell(u3t(sut), &p_sut, &q_sut);

    if ( u3_nul == q_heg ) {
      return _find_buck_here(van, q_sut, way, p_heg, q_heg, axe, lon, gil);
    }
    else {
      u3_noun uq_heg = u3t(q_heg);          //  term

      if ( c3y == u3ud(p_sut) ) {
        if ( c3y == u3r_sing(p_sut, uq_heg) ) {
          return _find_buck_here(van, q_sut, way, p_heg, q_heg, axe, lon, gil);
        } 
        else {
          return _find_buck_lose(van, sut, way, p_heg, q_heg, axe, lon, gil);
        }
      }
      else {
        u3_noun pp_sut = u3h(p_sut);              //  (map term (unit port))
        u3_noun qp_sut = u3t(p_sut);              //  (list (pair span nock))
        u3_noun tyr = u3qdb_get(pp_sut, uq_heg);  //  (unit (unit port))

        if ( u3_nul == tyr ) {
          return _find_buck_face_next
            (van, q_sut, qp_sut, way, p_heg, q_heg, axe, lon, gil);
        }
        else {
          u3_noun u_tyr = u3t(tyr);       //  (unit port)

          if ( u3_nul == u_tyr ) {
            u3_noun nol = u3nc(u3_nul, u3k(lon));
            u3_noun dep = u3qa_inc(p_heg);
            u3_noun ret = _find_buck
                              (van, q_sut, way, dep, q_heg, axe, nol, gil);

            u3z(dep);
            u3z(nol);
            return ret;
          }
          else {
            u3_noun uu_tyr = u3t(u_tyr);

            if ( c3y == u3h(uu_tyr) ) {
              u3_noun puu_tyr = u3t(uu_tyr);      //  (pair vein opal)
              u3_noun ppuu_tyr = u3h(puu_tyr);    //  vein
              u3_noun qpuu_tyr = u3t(puu_tyr);    //  opal
              u3_noun nol =                       //  vein
                u3nt(u3_nul, u3nc(u3_nul, u3k(axe)), u3k(lon));
              u3_noun ret;
 
              ret = u3nt(c3y, u3qb_weld(ppuu_tyr, nol), u3k(qpuu_tyr));
              u3z(nol);
              return ret;
            }
            else {
              u3_noun puu_tyr = u3t(uu_tyr);      // (pair span nock)
              u3_noun ppuu_tyr = u3h(puu_tyr);    //  span
              u3_noun qpuu_tyr = u3t(puu_tyr);    //  nock
              u3_noun dog = u3nc(0, u3k(axe));    //  nock
              u3_noun ret;

              ret = u3nq(c3n, c3n, u3k(ppuu_tyr), u3qf_comb(dog, qpuu_tyr));
              u3z(dog);

              return ret;
            }
          }
        }
      }
    }
  }

  static u3_noun
  _find_buck(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun p_heg,
             u3_noun q_heg,
             u3_noun axe,
             u3_noun lon,
             u3_noun gil)
  {
    if ( c3n == u3du(sut) ) switch ( sut ) {
      default: return u3m_bail(c3__fail);

      case c3__noun: {
        // fprintf(stderr, "noun\r\n");
        return _find_buck_stop(van, sut, way, p_heg, q_heg, axe, lon, gil);
      }
      case c3__void: {
        // fprintf(stderr, "void\r\n");
        return _find_buck_stop(van, sut, way, p_heg, q_heg, axe, lon, gil);
      }
    }
    else switch ( u3h(sut) ) {
      default: return u3m_bail(c3__fail);

      case c3__atom: 
      {
        // fprintf(stderr, "atom\r\n");
        return _find_buck_stop(van, sut, way, p_heg, q_heg, axe, lon, gil);
      }
      case c3__cell: 
      {
        // fprintf(stderr, "cell\r\n");
        return _find_buck_cell(van, sut, way, p_heg, q_heg, axe, lon, gil);
      }
      case c3__core:
      {
        // fprintf(stderr, "core\r\n");
        return _find_buck_core(van, sut, way, p_heg, q_heg, axe, lon, gil);
      }
      case c3__face:
      {
        // fprintf(stderr, "face\r\n");
        return _find_buck_face(van, sut, way, p_heg, q_heg, axe, lon, gil);
      }
      case c3__fork:
      {
        // fprintf(stderr, "fork\r\n");
        return _find_buck_fork(van, sut, way, p_heg, q_heg, axe, lon, gil);
      }
      case c3__hold:
      {
        // fprintf(stderr, "hold\r\n");
        if ( (c3y == u3qdi_has(gil, sut)) ) {
          return u3nt(c3n, c3y, u3k(p_heg));
        }
        else {
          u3_noun zoc = u3qdi_put(gil, sut);
          u3_noun fop = u3qfu_repo(van, sut);
          u3_noun pro = _find_buck(van, fop, way, p_heg, q_heg, axe, lon, zoc);

          u3z(fop);
          u3z(zoc);

          return pro;
        }
      }
    }
  }


  static u3_noun
  _find_limb(u3_noun van,
             u3_noun sut,                  //  span
             u3_noun way,                  //  ?(%read %rite %free %both)
             u3_noun i_hyp,                //  limb
             u3_noun p_mor)                //  palo
  {
    u3_noun pp_mor = u3h(p_mor);          //  vein
    u3_noun qp_mor = u3t(p_mor);          //  opal
    {
      u3_noun ref    = u3qfu_felt(van, sut, qp_mor);
      u3_noun lon    = u3k(pp_mor);
      u3_noun heg    = (c3y == u3du(i_hyp))
                         ? u3k(i_hyp)
                         : u3nq(c3n, 0, u3_nul, u3k(i_hyp));
      u3_noun ret;
      
      if ( c3y == u3h(heg) ) {
        u3_noun p_heg = u3t(heg);         //  axis

        ret = u3nq
          (c3y,
           u3nc(u3nc(u3_nul, u3k(p_heg)), u3k(lon)),
           c3y,
           u3qfu_peek(van, ref, way, p_heg));
      }
      else {
        u3_noun p_heg = u3h(u3t(heg));    //  @ud
        u3_noun q_heg = u3t(u3t(heg));    //  (unit term)

        ret = _find_buck
                (van, ref, way, p_heg, q_heg, 1, lon, u3_nul);
      }
      u3z(heg);
      u3z(lon);
      u3z(ref);

      return ret;
    }
  }

  static u3_noun
  _find_pony(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    if ( u3_nul == hyp ) {
      return u3nq(c3y, u3_nul, c3y, u3k(sut));
    }
    else {
      u3_noun i_hyp = u3h(hyp);
      u3_noun t_hyp = u3t(hyp);
      u3_noun mor = _find_pony(van, sut, way, t_hyp);

      if ( c3n == u3h(mor) ) {
        u3_noun p_mor = u3t(mor);

        if ( c3y == u3h(p_mor) ) {
          return mor;
        } 
        else {
          u3_noun pp_mor = u3t(p_mor);      //  {span nock}
          u3_noun ppp_mor = u3h(pp_mor);    //  span
          u3_noun qpp_mor = u3h(pp_mor);    //  nock
          u3_noun gen = u3nt(c3__wing, u3k(i_hyp), u3_nul);
          u3_noun fex = u3qfu_mint(van, ppp_mor, c3__noun, gen);
          u3_noun ret = u3nq(c3n, 
                             c3n,
                             u3k(u3h(fex)),
                             u3qf_comb(qpp_mor, u3t(fex)));
          u3z(fex);
          u3z(gen);
          u3z(mor);

          return ret;
        }
      }
      else {
        u3_noun p_mor = u3t(mor);
        u3_noun ret = _find_limb(van, sut, way, i_hyp, p_mor);

        u3z(mor);
        return ret;
      }
    }
  }

#if 0
  static u3_noun
  u3qfu_fund(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    u3_noun von = u3i_molt(u3k(van), u3x_sam, u3k(sut), 0);
    u3_noun gat = u3j_hook(von, "fund");

    return u3n_kick_on(u3i_molt(gat, 
                                u3x_sam_2,
                                u3k(way), 
                                u3x_sam_3,
                                u3k(hyp), 
                                0));
  }
#endif

  void
  find_error(u3_noun cor,
             u3_noun old,
             u3_noun new)
  {
    u3_noun sut, way, hyp, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &way,
                               u3x_sam_3, &hyp,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      c3_assert(0);
    }
    u3m_p("hyp", hyp);
    u3a_wash(old);
    u3a_wash(new);
    fprintf(stderr, "old mug %x, new mug %x\r\n", 
                    u3r_mug(old), u3r_mug(new));
    u3qfu_dump(van, "sut", sut);
    _funk_dump(van, "old", old);
    _funk_dump(van, "new", new);
  }

  static u3_noun
  _cqfu_fond(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    return _find_pony(van, sut, way, hyp);
#if 0
    u3_noun old = u3qfu_fund(van, sut, way, hyp);
    u3_noun new = _find_pony(van, sut, way, hyp);

    u3z(new);
    return old;

    u3_noun old = u3qfu_fund(van, sut, way, hyp);
    u3_noun new = _find_pony(van, sut, way, hyp);

    if ( c3n == u3r_sing(old, new) ) {
      u3m_p("hyp", hyp);
      u3qfu_dump(van, "sut", sut);
      _funk_dump(van, "old", old);
      _funk_dump(van, "new", new);

      exit(1);
    }
    else fprintf(stderr, "correct\r\n");
    u3z(new);
    return old;
#endif
  }

  u3_noun
  _cqfu_find(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    u3_noun taf = _find_pony(van, sut, way, hyp);

    if ( u3_nul == taf ) {
      goto error;
    }
    else {
      if ( c3y == u3h(taf) ) {
        return taf;
      }
      else {
        if ( c3n == u3h(u3t(taf)) ) {
          u3_noun fat = u3nc(c3n, u3k(u3t(u3t(taf))));

          u3z(taf); 
          return fat;
        }
        else {
          goto error;
        }
      }
    }
    error: {
      u3m_p("wing", hyp);

      return u3m_error("find");
    }
  }

/* boilerplate
*/
  u3_noun
  u3wfu_find(u3_noun cor)
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
      return _cqfu_find(van, sut, way, hyp);
    }
  }

  u3_noun
  u3qfu_find(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    c3_m    fun_m = c3__find + !!u3r_at(u3qfu_van_vet, van);
    u3_noun pro   = u3z_find_3(fun_m, sut, way, hyp);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_find(van, sut, way, hyp);

      return u3z_save_3(fun_m, sut, way, hyp, pro);
    }
  }

/* boilerplate
*/
  u3_noun
  u3wfu_fond(u3_noun cor)
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
      return _cqfu_fond(van, sut, way, hyp);
    }
  }

  u3_noun
  u3qfu_fond(u3_noun van,
             u3_noun sut,
             u3_noun way,
             u3_noun hyp)
  {
    c3_m    fun_m = c3__fond + !!u3r_at(u3qfu_van_vet, van);
    u3_noun pro   = u3z_find_3(fun_m, sut, way, hyp);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_fond(van, sut, way, hyp);

      return u3z_save_3(fun_m, sut, way, hyp, pro);
    }
  }
