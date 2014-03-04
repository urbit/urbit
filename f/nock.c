/* f/nock.c
**
** This file is in the public domain.
*/
#include "all.h"

  /** Types.
  **/
    typedef u2_atom u2_kode;      /* 0=good, 1=need, 2=exit */

  /** Forward declarations.
  **/
    static u2_noun _nock_cool(u2_noun, u2_noun);
    static u2_noun _nock_mool(u2_noun, u2_noun, u2_kode*);
    static u2_noun _nock_molg(u2_noun, u2_noun, u2_kode*);

/* _nock_pray_mool(): load from namespace, in virtual mode.
*/
static u2_noun
_nock_pray_mool(u2_noun gof, u2_kode *pon)                        //  transfer
{
  u2_noun lad = u2_hevn_at(lad);

  c3_assert(u2_yes == u2du(lad));
  c3_assert(0 == *pon);
  {
    u2_noun i_lad = u2fh(lad);
    u2_noun t_lad = u2ft(lad);
    u2_noun pro;
    u2_noun hoe;

    u2_hevn_at(lad) = t_lad;
    if ( 0 != (hoe = u2_cm_trap()) ) {
      u2_cm_done();

      return u2_cm_bail(u2k(u2h(hoe)));
    }
    else {
      if ( u2_nul == t_lad ) {
        pro = u2_cn_mung(u2k(i_lad), u2k(gof));
      } else {
        pro = _nock_molg(u2k(i_lad), u2k(gof), pon);
      }
      u2_cm_done();

      c3_assert(t_lad == u2_hevn_at(lad));
      u2_hevn_at(lad) = lad;

      if ( 0 != *pon ) {
        u2z(gof);

        return pro;
      } else {
        if ( u2_no == u2du(pro) ) {
          *pon = 1;
          u2z(pro);
          return u2nc(gof, u2_nul);
        }
        else {
          u2_noun res = u2k(u2t(pro));

          u2z(gof);
          u2z(pro);
          return res;
        }
      }
    }
  }
}

/* _nock_pray_cool(): load from namespace, in kernel mode.
*/
static u2_noun
_nock_pray_cool(u2_noun gof)                                      //  transfer
{
  //  This should just exit - but for various reasons, all historical,
  //  we could be actually minking here.  Therefore we have to respect
  //  the mink if it exists.
  //
  u2_noun lad = u2_hevn_at(lad);

  if ( u2_nul == lad ) {
    return u2_cm_bowl(u2nc(c3__need, u2nc(gof, u2_nul)));
  }
  else {
    u2_kode pon = 0;
    u2_noun mog = _nock_pray_mool(gof, &pon);

    if ( 0 == pon ) {
      return mog;
    }
    else if ( 1 == pon ) {
      return u2_cm_bowl(u2nc(c3__need, mog));
    }
    else if ( 2 == pon ) {
      return u2_cm_bowl(u2nc(c3__exit, mog));
    }
    else { c3_assert(0); return 0; }
  }
}

/* _nock_hint(): hint with code, data, subject, formula.  nock/mink.
*/
static u2_noun                                                    //  produce
_nock_hint(u2_noun  zep,                                          //  transfer
           u2_noun  hod,                                          //  transfer
           u2_noun  bus,                                          //  transfer
           u2_noun  nex,                                          //  transfer
           u2_bean* pon)
{
  u2_noun pro;

  switch ( zep ) {
    default: u2z(zep); u2z(hod);
             return pon ? _nock_mool(bus, nex, pon) : _nock_cool(bus, nex);

    case c3__lose:
    case c3__yelp:
    case c3__bean:
    case c3__mean:
    case c3__spot: {
      u2_noun tax = u2_wire_tax(u2_Wire);
      u2_noun tac = u2nc(zep, hod);

#if 0
      if ( c3__spot == zep ) {
        printf("spot %d/%d : %d/%d\n",
               u2h(u2h(u2t(hod))),
               u2t(u2h(u2t(hod))),
               u2h(u2t(u2t(hod))),
               u2t(u2t(u2t(hod))));
      }
#endif
      u2_wire_tax(u2_Wire) = u2nc(tac, tax);
      {
        pro = pon ? _nock_mool(bus, nex, pon) : _nock_cool(bus, nex);
      }
      tax = u2k(tax);
      u2z(u2_wire_tax(u2_Wire));
      u2_wire_tax(u2_Wire) = tax;

      return pro;
    }

    case c3__slog: {
      u2_tx_sys_bit(u2_Wire, u2_yes);
      u2_tx_slog(u2_Wire, hod);
      u2_tx_sys_bit(u2_Wire, u2_no);

      u2z(hod);
      return pon ? _nock_mool(bus, nex, pon) : _nock_cool(bus, nex);
    }

    case c3__mine: {
      pro = pon ? _nock_mool(bus, nex, pon) : _nock_cool(bus, nex);

      if ( !pon || (u2_no != *pon) ) {
        u2_tx_sys_bit(u2_Wire, u2_yes);
        pro = u2_ds_mine(u2_Wire, hod, pro);
        u2_tx_sys_bit(u2_Wire, u2_no);
      }
      u2z(hod);
      return pro;
    }

    case c3__germ: {
      pro = pon ? _nock_mool(bus, nex, pon) : _nock_cool(bus, nex);

      if ( u2_yes == u2_sing(pro, hod) ) {
        u2z(pro); return hod;
      } else {
        u2z(hod); return pro;
      }
    }

    case c3__fast: {
      pro = pon ? _nock_mool(bus, nex, pon) : _nock_cool(bus, nex);

      if ( !pon || (u2_no != *pon) ) {
        u2_noun p_hod, q_hod, r_hod;

        /* XX: translate hod to old clue form.
        */
        if ( u2_no == u2_as_trel(hod, &p_hod, &q_hod, &r_hod) ) {
          u2z(hod);
          return pro;
        }
        else {
          u2_noun xod;

          if ( u2_yes == u2_dust(q_hod) &&
               (_1 == u2_h(q_hod)) &&
               (_0 == u2_t(q_hod)) ) {
            q_hod = 0;
          }
          xod = u2_rt(u2_Wire, u2k(q_hod),
                             u2k(p_hod),
                             u2k(r_hod));
          u2z(hod);
          hod = xod;
        }
        u2_tx_sys_bit(u2_Wire, u2_yes);
        pro = u2_ds_mine(u2_Wire, hod, pro);
        u2_tx_sys_bit(u2_Wire, u2_no);
      }
      u2z(hod);
      return pro;
    }

    case c3__memo: {
      u2z(hod);
      {
        pro = u2_rl_find_cell(u2_Wire, 0, bus, nex);

        if ( pro != u2_none ) {
          u2_tx_did_fin(u2_Wire, 1);
          u2z(bus);
          u2z(nex);

          return pro;
        } else {
          u2_noun sav;

          pro = pon ? _nock_mool(u2k(bus), u2k(nex), pon)
                    : _nock_cool(u2k(bus), u2k(nex));

          if ( !pon || (u2_no != *pon) ) {
            u2_tx_sys_bit(u2_Wire, u2_yes);
            sav = u2_rl_save_cell(u2_Wire, 0, bus, nex, pro);
            u2_tx_sys_bit(u2_Wire, u2_no);

            u2_tx_did_pod(u2_Wire, 1);
            u2_tx_did_fin(u2_Wire, 1);
          }
          else sav = pro;

          u2z(bus); u2z(nex);
          return sav;
        }
      }
    }

    case c3__ping: {
      u2_tx_sys_bit(u2_Wire, u2_yes);
      u2_tx_did_act(u2_Wire, hod);
      u2_tx_sys_bit(u2_Wire, u2_no);
      u2z(hod);

      return pon ? _nock_mool(bus, nex, pon) : _nock_cool(bus, nex);
    }

    case c3__live: {
      u2_bean qox;

      u2_tx_sys_bit(u2_Wire, u2_yes);
      qox = u2_tx_task_in(u2_Wire, hod);
      u2_tx_sys_bit(u2_Wire, u2_no);

      u2z(hod);
      if ( u2_no == qox ) {
        return pon ? _nock_mool(bus, nex, pon) : _nock_cool(bus, nex);
      } else {
        pro = pon ? _nock_mool(bus, nex, pon) : _nock_cool(bus, nex);

        u2_tx_task_out(u2_Wire);
        return pro;
      }
    }

    case c3__sole: {
      u2z(hod);
      {
        pro = pon ? _nock_mool(bus, nex, pon) : _nock_cool(bus, nex);

        if ( u2_none == pro ) {
          return u2_none;
        }
        else if ( !pon || (u2_no != *pon) ) {
          u2_noun nuu;

          u2_tx_sys_bit(u2_Wire, u2_yes);
          nuu = u2_rl_uniq(u2_Wire, pro);
          u2_tx_sys_bit(u2_Wire, u2_no);

          u2_tx_did_fin(u2_Wire, 1);
          if ( nuu == pro ) {
            u2_tx_did_pod(u2_Wire, 1);
          }
        }
        return pro;
      }
    }
  }
}

/* _nock_cool(): nock, transferring arguments.
*/
static u2_noun
_nock_cool(u2_noun bus,
           u2_noun fol)
{
  u2_noun hib, gal;

  while ( 1 ) {
    u2_tx_did_hop(u2_Wire, 1);

    if ( u2_no == u2du(fol) ) {
      return u2_cm_bail(c3__exit);
    }
    else {
      hib = u2fh(fol);
      gal = u2ft(fol);
    }

    if ( u2_yes == u2du(hib) ) {
      u2_noun poz, riv;

      poz = _nock_cool(u2k(bus), u2k(hib));
      riv = _nock_cool(bus, u2k(gal));

      u2z(fol);
      return u2_cn_cell(poz, riv);
    }
    else switch ( hib ) {
      default: return u2_cm_bail(c3__exit);

      case 0: {
        if ( u2_no == u2_cr_ud(gal) ) {
          return u2_cm_bail(c3__exit);
        }
        else {
          u2_noun pro = u2k(u2at(gal, bus));

          u2z(bus); u2z(fol);
          return pro;
        }
      }
      case 1: {
        u2_noun pro = u2k(gal);

        u2z(bus); u2z(fol);
        return pro;
      }
      c3_assert(!"not reached");

      case 2: {
        if ( u2_no == u2du(gal) ) {
          return u2_cm_bail(c3__exit);
        }
        else {
          u2_noun nex = _nock_cool(u2k(bus), u2k(u2ft(gal)));
          u2_noun seb = _nock_cool(bus, u2k(u2fh(gal)));

          u2z(fol);
          bus = seb;
          fol = nex;
          continue;
        }
      }
      c3_assert(!"not reached");

      case 3: {
        u2_noun gof, pro;

        gof = _nock_cool(bus, u2k(gal));
        pro = u2du(gof);

        u2z(gof); u2z(fol);
        return pro;
      }
      c3_assert(!"not reached");

      case 4: {
        u2_noun gof, pro;

        gof = _nock_cool(bus, u2k(gal));
        if ( (u2_none == (pro = u2_rl_vint(u2_Wire, gof))) ) {
          return u2_cm_bail(c3__exit);
        }

        u2z(gof); u2z(fol);
        return pro;
      }
      c3_assert(!"not reached");

      case 5: {
        if ( u2_no == u2du(gal) ) {
          return u2_cm_bail(c3__exit);
        }
        else {
          u2_noun wim = _nock_cool(bus, u2k(gal));
          u2_noun pro;

          if ( u2_no == u2du(wim) ) {
            return u2_cm_bail(c3__exit);
          }
          else pro = u2_cr_sing(u2h(wim), u2t(wim));

          u2z(wim); u2z(fol);
          return pro;
        }
      }
      c3_assert(!"not reached");

      case 6: {
        u2_noun b_gal, c_gal, d_gal;

        u2_cx_trel(gal, &b_gal, &c_gal, &d_gal);
        {
          u2_noun tys = _nock_cool(u2k(bus), u2k(b_gal));
          u2_noun nex;

          if ( 0 == tys ) {
            nex = u2k(c_gal);
          } else if ( 1 == tys ) {
            nex = u2k(d_gal);
          } else return u2_cm_bail(c3__exit);

          u2z(fol);
          fol = nex;
          continue;
        }
      }
      c3_assert(!"not reached");

      case 7: {
        u2_noun b_gal, c_gal;

        u2_cx_cell(gal, &b_gal, &c_gal);
        {
          u2_noun bod = _nock_cool(bus, u2k(b_gal));
          u2_noun nex = u2k(c_gal);

          u2z(fol);
          bus = bod;
          fol = nex;
          continue;
        }
      }
      c3_assert(!"not reached");

      case 8: {
        u2_noun b_gal, c_gal;

        // c3_assert(!"got 8 (nock)!");
        u2_cx_cell(gal, &b_gal, &c_gal);
        {
          u2_noun bod = u2nc(_nock_cool(u2k(bus), u2k(b_gal)), bus);
          u2_noun nex = u2k(c_gal);

          u2z(fol);
          bus = bod;
          fol = nex;
          continue;
        }
      }
      c3_assert(!"not reached");

      case 9: {
        u2_noun b_gal, c_gal;

        u2_cx_cell(gal, &b_gal, &c_gal);
        if ( u2_no == u2ud(b_gal) ) {
          return u2_cm_bail(c3__exit);
        }
        else {
          u2_noun seb = _nock_cool(bus, u2k(c_gal));
          u2_weak xip;

          u2_tx_sys_bit(u2_Wire, u2_yes);
          xip = u2_ds_find(u2_Wire, seb);

          if ( u2_none != xip ) {
            u2_noun pro = u2_ho_kick(u2_Wire, xip, seb, b_gal);

            u2_tx_sys_bit(u2_Wire, u2_no);
            if ( u2_none == pro ) {
              return u2_cm_bail(c3__exit);
            }
            else {
              u2z(seb); u2z(fol);
              return pro;
            }
          }
          else {
            u2_tx_sys_bit(u2_Wire, u2_no);
            {
              u2_noun nex = u2_ct(u2at(b_gal, seb));

              u2z(fol);
              bus = seb;
              fol = nex;
              continue;
            }
          }
        }
      }
      c3_assert(!"not reached");

      case 10: {
        u2_noun p_gal, q_gal;

        u2_cx_cell(gal, &p_gal, &q_gal);
        {
          u2_noun zep, hod, nex, pro;

          if ( u2_yes == u2du(p_gal) ) {
            u2_noun b_gal = u2fh(p_gal);
            u2_noun c_gal = u2ft(p_gal);
            u2_noun d_gal = q_gal;

            zep = u2k(b_gal);
            hod = _nock_cool(u2k(bus), u2_ct(c_gal));
            nex = u2_ct(d_gal);
          }
          else {
            u2_noun b_gal = p_gal;
            u2_noun c_gal = q_gal;

            zep = u2k(b_gal);
            hod = u2_nul;
            nex = u2_ct(c_gal);
          }

          u2_cz(fol);
          pro = _nock_hint(zep, hod, bus, nex, 0);
          return pro;
        }
      }

      case 11: {
        u2_noun gof, pro;

        gof = _nock_cool(bus, u2k(gal));
        pro = _nock_pray_cool(gof);

        u2z(fol);
        return pro;
      }
      c3_assert(!"not reached");
    }
  }
}

/* nock_mool(): fast internal mink interface.  Arguments transferred.
*/
u2_noun
_nock_mool(u2_noun  bus,
           u2_noun  fol,
           u2_kode* pon)
{
  u2_noun hib, gal;

  c3_assert(u2_yes == *pon);

  while ( 1 ) {
    u2_tx_did_hop(u2_Wire, 1);

    if ( u2_no == u2du(fol) ) {
      *pon = 2;
      u2z(bus); u2z(fol);
      return u2_cm_wail();
    }
    else {
      hib = u2fh(fol);
      gal = u2ft(fol);
    }

    if ( u2_yes == u2du(hib) ) {
      u2_noun poz, riv;
      u2_kode h_pon = 0, t_pon = 0;

      poz = _nock_mool(u2k(bus), u2k(hib), &h_pon);
      if ( 2 == h_pon ) { *pon = 2; u2z(bus); u2z(fol); return poz; }

      riv = _nock_mool(bus, u2k(gal), &t_pon);
      u2z(fol);
      if ( 2 == t_pon ) { *pon = 2; u2z(poz); return riv; }

      if ( (1 == h_pon) || (1 == t_pon) ) {
        u2_noun lal;

        *pon = 1;

        if ( 0 == h_pon ) {
          u2z(poz); lal = riv;
        } else if ( 0 == t_pon ) {
          u2z(riv); lal = poz;
        } else {
          lal = u2_ckb_weld(poz, riv);
        }
        return lal;
      }
      return u2_cn_cell(poz, riv);
    }
    else switch ( hib ) {
      default: *pon = 2; u2z(bus); u2z(fol); return u2_cm_wail();

      case 0: {
        if ( u2_no == u2_cr_ud(gal) ) {
          *pon = 2; u2z(bus); u2z(fol); return u2_cm_wail();
        }
        else {
          u2_weak pro = u2_cr_at(gal, bus);

          if ( u2_none == pro ) {
            *pon = 2; u2z(bus); u2z(fol); return u2_cm_wail();
          }
          else {
            pro = u2k(pro);
            u2z(bus); u2z(fol);

            return pro;
          }
        }
      }
      case 1: {
        u2_noun pro = u2k(gal);

        u2z(bus); u2z(fol);
        return pro;
      }
      c3_assert(!"not reached");

      case 2: {
        if ( (u2_no == u2du(gal)) || (u2_no == u2du(u2fh(gal))) ) {
          *pon = 2; u2z(bus); u2z(fol); return u2_cm_wail();
        }
        else {
          u2_noun neb;

          neb = _nock_mool(bus, u2k(gal), pon);
          u2z(fol);
          if ( 0 != *pon ) { return neb; }

          bus = u2k(u2fh(neb));
          fol = u2k(u2ft(neb));
          u2z(neb);
          continue;
        }
      }
      c3_assert(!"not reached");

      case 3: {
        u2_noun gof, pro;

        gof = _nock_mool(bus, u2k(gal), pon);
        u2z(fol);
        if ( 0 != *pon ) { return gof; }

        pro = u2du(gof);
        u2z(gof);

        return pro;
      }
      c3_assert(!"not reached");

      case 4: {
        u2_noun gof, pro;

        gof = _nock_mool(bus, u2k(gal), pon);
        u2z(fol);
        if ( 0 != *pon ) { return gof; }

        if ( u2_none == (pro = u2_rl_vint(u2_Wire, gof)) ) {
          *pon = 2; u2z(gof); return u2_cm_wail();
        }
        u2z(gof);

        return pro;
      }
      c3_assert(!"not reached");

      case 5: {
        u2_noun gof, pro;

        gof = _nock_mool(bus, u2k(gal), pon);
        u2z(fol);
        if ( 0 != *pon ) { return gof; }

        if ( u2_no == u2du(gof) ) {
          *pon = 2; u2z(gof); return u2_cm_wail();
        }
        pro = u2_cr_sing(u2h(gof), u2t(gof));
        u2z(gof);

        return pro;
      }
      c3_assert(!"not reached");

      case 6: {
        u2_noun b_gal, cd_gal, c_gal, d_gal;

        if ( u2_no == u2_cr_cell(gal, &b_gal, &cd_gal) ) {
          *pon = 2; u2z(bus); u2z(fol); return u2_cm_wail();
        }
        else {
          u2_noun tys, nex;

          tys = _nock_mool(u2k(bus), u2k(b_gal), pon);
          if ( 0 != *pon ) { u2z(bus); u2z(fol); return tys; }

          if ( u2_no == u2_cr_cell(cd_gal, &c_gal, &d_gal) ) {
            *pon = 2; u2z(bus); u2z(fol); return u2_cm_wail();
          }

          if ( 0 == tys ) {
            nex = u2k(c_gal);
          } else if ( 1 == tys ) {
            nex = u2k(d_gal);
          } else {
            *pon = 2; u2z(bus); u2z(fol); return u2_cm_wail();
          }

          u2z(fol);
          fol = nex;
          continue;
        }
      }
      c3_assert(!"not reached");

      case 7: {
        u2_noun b_gal, c_gal;

        if ( u2_no == u2_cr_cell(gal, &b_gal, &c_gal) ) {
          *pon = 2; u2z(bus); u2z(fol); return u2_cm_wail();
        }
        else {
          u2_noun bod, nex;

          bod = _nock_mool(bus, u2k(b_gal), pon);
          if ( 0 != *pon ) { u2z(fol); return bod; }

          nex = u2k(c_gal);
          u2z(fol);

          bus = bod;
          fol = nex;
          continue;
        }
      }
      c3_assert(!"not reached");

      case 8: {
        u2_noun b_gal, c_gal;

        // c3_assert(!"got 8 (mink)!");
        if ( u2_no == u2_cr_cell(gal, &b_gal, &c_gal) ) {
          *pon = 2; u2z(bus); u2z(fol); return u2_cm_wail();
        }
        else {
          u2_noun wib, bod, nex;

          wib = _nock_mool(u2k(bus), u2k(b_gal), pon);
          if ( 0 != *pon ) { u2z(bus); u2z(fol); return wib; }

          bod = u2nc(wib, bus);
          nex = u2k(c_gal);
          u2z(fol);

          bus = bod;
          fol = nex;
          continue;
        }
      }
      c3_assert(!"not reached");

      case 9: {
        u2_noun b_gal, c_gal;

        if ( (u2_no == u2_cr_cell(gal, &b_gal, &c_gal)) ||
             (u2_no == u2ud(b_gal)) )
        {
          *pon = 2; u2z(bus); u2z(fol); return u2_cm_wail();
        }
        else {
          u2_noun seb;
          u2_weak xip;

          seb = _nock_mool(bus, u2k(c_gal), pon);
          u2z(fol);
          if ( 0 != *pon ) { return seb; }

          u2_tx_sys_bit(u2_Wire, u2_yes);
          xip = u2_ds_find(u2_Wire, seb);
          u2_tx_sys_bit(u2_Wire, u2_no);

          if ( u2_none != xip ) {
            u2_noun pro;

            u2_tx_sys_bit(u2_Wire, u2_yes);
            pro = u2_ho_kicq(u2_Wire, xip, seb, b_gal, pon);
            u2_tx_sys_bit(u2_Wire, u2_no);
            u2z(seb);

            if ( u2_none == pro ) {
              *pon = 2; return u2_cm_wail();
            }
            else return pro;
          }
          else {
            u2_noun nex = u2_cr_at(b_gal, seb);

            if ( u2_none == nex ) {
              *pon = 2; u2z(seb); return u2_cm_wail();
            }
            bus = seb;
            fol = u2k(nex);
            continue;
          }
        }
      }
      c3_assert(!"not reached");

      case 10: {
        u2_noun p_gal, q_gal;

        if ( u2_no == u2_cr_cell(gal, &p_gal, &q_gal) ) {
          *pon = 2; u2z(bus); u2z(fol); return u2_cm_wail();
        }
        else {
          u2_noun zep, hod, nex;

          if ( u2_yes == u2du(p_gal) ) {
            u2_noun b_gal = u2fh(p_gal);
            u2_noun c_gal = u2ft(p_gal);
            u2_noun d_gal = q_gal;

            hod = _nock_mool(u2k(bus), u2_ct(c_gal), pon);
            if ( 0 != *pon ) { u2z(fol); return hod; }

            zep = u2k(b_gal);
            nex = u2k(d_gal);
            u2z(fol);
          }
          else {
            u2_noun b_gal = p_gal;
            u2_noun c_gal = q_gal;

            zep = u2k(b_gal);
            hod = u2_nul;
            nex = u2k(c_gal);

            u2z(fol);
          }

          return _nock_hint(zep, hod, bus, nex, pon);
        }
      }

      case 11: {
        u2_noun gof;

        gof = _nock_mool(bus, u2k(gal), pon);
        u2z(fol);
        if ( 0 != *pon ) { return gof; }

        return _nock_pray_mool(gof, pon);
      }
      c3_assert(!"not reached");
    }
  }
}

/* nock_molg(): function call (mung) with kode.  Arguments transferred.
*/
static u2_noun
_nock_molg(u2_noun  gat,
           u2_noun  sam,
           u2_kode* pon)
{
  if ( (u2_no == u2du(gat)) || (u2_no == u2du(u2t(gat))) ) {
    *pon = 2; return u2_cm_wail();
  }
  else {
    u2_noun cor, fol;

    cor = u2nc(u2k(u2h(gat)), u2nc(sam, u2k(u2t(u2t(gat)))));
    fol = u2k(u2h(gat));
    u2z(gat);

    //  XX  try to chip with u2_ds_find?  but a rare case...
    return _nock_mool(cor, fol, pon);
  }
}

/* _nock_moog(): u2_cn_mink() with fly set.
*/
static u2_noun
_nock_moog(u2_noun bus,
           u2_noun fol)
{
  u2_noun res;
  u2_kode pon;

  pon = 0;
  {
    u2_noun hoe;

    if ( 0 != (hoe = u2_cm_trap()) ) {
      if ( u2h(hoe) == c3__exit ) {
        res = u2nc(2, u2k(u2t(hoe)));

        c3_assert(0);
        u2z(hoe);
      }
      else if ( u2h(hoe) == c3__need ) {
        res = u2nc(1, u2k(u2t(hoe)));
        u2z(hoe);
      }
      else {
        u2_noun wac = u2k(u2h(hoe));

        u2z(hoe);
        return u2_cm_bail(wac);
      }
    }
    else {
      u2_noun pro = _nock_mool(bus, fol, &pon);

      u2_cm_done();
      res = u2nc(pon, pro);
    }
  }
  return res;
}

/* u2_cn_nock(): external nock interface.
*/
u2_noun
u2_cn_nock(u2_noun bus,
           u2_noun fol)
{
  u2_noun pro;
  u2_bean bit;

  bit = u2_tx_sys_bit(u2_Wire, u2_no);
  //  c3_assert(bit == u2_yes);
  bit = u2_tx_glu_bit(u2_Wire, u2_yes);

  pro = _nock_cool(bus, fol);

  u2_tx_sys_bit(u2_Wire, u2_yes);
  u2_tx_glu_bit(u2_Wire, bit);

  return pro;
}

/* u2_cn_mink(): logical virtual nock.
*/
u2_noun
u2_cn_mink(u2_noun bus,
           u2_noun fol,
           u2_noun fly)
{
  u2_noun res;
  u2_bean bit;

  bit = u2_tx_sys_bit(u2_Wire, u2_no);
  c3_assert(bit == u2_yes);
  bit = u2_tx_glu_bit(u2_Wire, u2_yes);

  {
    u2_noun lad;

    lad = u2_hevn_at(lad);
    u2_hevn_at(lad) = u2nc(fly, u2k(lad));
    {
      res = _nock_moog(bus, fol);
    }
    // c3_assert(lad == u2ft(u2_hevn_at(lad)));

    u2z(u2_hevn_at(lad));
    u2_hevn_at(lad) = lad;
  }

  u2_tx_sys_bit(u2_Wire, u2_yes);
  u2_tx_glu_bit(u2_Wire, bit);

  return res;
}

/* u2_cn_moch(): blind mink with empty fly.
*/
u2_noun
u2_cn_moch(u2_noun bus,
           u2_noun fol)
{
  u2_noun fly = u2nc(u2nc(0, 0), u2nc(1, 0));

  return u2_cn_mink(bus, fol, fly);
}

/*** Deprecated:
***/

/* u2_nk_soft():
**
**   Compute `(nock bus fol)`, interpreter first.
*/
u2_noun                                                           //  transfer
u2_nk_soft(u2_wire wir_r,
           u2_noun bus,                                           //  transfer
           u2_noun fol)                                           //  retain
{
  return u2_cn_nock(bus, u2_ct(fol));
}

/* u2_nk_nock():
**
**   Compute `(nock bus fol)`.
*/
u2_weak                                                           //  transfer
u2_nk_nock(u2_wire wir_r,
           u2_weak bus,                                           //  transfer
           u2_weak fol)                                           //  retain
{
  if ( u2_none == fol ) {
    u2_rl_lose(wir_r, bus);
    return u2_none;
  }
  else if ( u2_none == bus ) {
    return u2_none;
  }
  else {
    return u2_nk_soft(wir_r, bus, fol);
  }
}

/* u2_nk_kick():
**
**   Fire `gat` without changing the sample.
*/
u2_weak                                                           //  transfer
u2_nk_kick(u2_wire wir_r,
           u2_weak gat)                                           //  retain
{
  u2_noun xip;

  if ( u2_none != (xip = u2_ds_find(wir_r, gat)) ) {
    u2_noun pro = u2_ho_kick(wir_r, xip, gat, u2_cv_noc);

    return pro;
  }
  else {
    return u2_nk_nock
      (wir_r,
       u2_rx(wir_r, gat),
       u2_sh(gat));
  }
}

/* u2_nk_mong():
**
**   Call with new convention.
*/
u2_noun                                                           //  transfer
u2_nk_mong(u2_wire wir_r,
           u2_noun gat,                                           //  retain
           u2_noun sam)                                           //  transfer
{
  u2_noun cor, xip;

  cor = u2_rc
      (wir_r,
       u2_rx(wir_r, u2_sh(gat)),
       u2_rc(wir_r, sam, u2_rx(wir_r, u2_st(u2_st(gat)))));

  if ( u2_none != (xip = u2_ds_find(wir_r, cor)) ) {
    u2_noun pro = u2_ho_kick(wir_r, xip, cor, u2_cv_noc);

    u2_rz(wir_r, cor);
    return pro;
  }
  else return u2_nk_nock(wir_r, cor, u2_sh(gat));
}
