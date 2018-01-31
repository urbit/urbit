/* g/n.c
**
*/
#include "all.h"

static u3_noun _n_nock_on(u3_noun bus, u3_noun fol);

      /* u3_term_io_hija(): hijack console for cooked print.
      */
        FILE*
        u3_term_io_hija(void);

      /* u3_term_io_loja(): release console from cooked print.
      */
        void
        u3_term_io_loja(int x);

      /* uL, uH: wrap hijack/lojack around fprintf.
      **
      **  uL(fprintf(uH, ...));
      */
#       define uH    u3_term_io_hija()
#       define uL(x) u3_term_io_loja(x)


/* _n_hint(): process hint.
*/
static u3_noun
_n_hint(u3_noun zep, 
        u3_noun hod,
        u3_noun bus,
        u3_noun nex)
{
  switch ( zep ) {
    default: {
      // u3m_p("weird zep", zep);
      u3a_lose(zep);
      u3a_lose(hod);

      return _n_nock_on(bus, nex);
    }

    case c3__hunk:
    case c3__lose:
    case c3__mean:
    case c3__spot: {
      u3_noun tac = u3nc(zep, hod);
      u3_noun pro;

      u3t_push(tac);
#if 0
      {
        static int low_i;

        if ( !low_i ) {
          low_i = 1;
          if ( 0 == (u3R->pro.nox_d % 65536ULL) ) {
            if ( c3__spot == zep ) {
              uL(fprintf(uH, "spot %d/%d : %d/%d\r\n",
                             u3h(u3h(u3t(hod))),
                             u3t(u3h(u3t(hod))),
                             u3h(u3t(u3t(hod))),
                             u3t(u3t(u3t(hod)))));
            }
          }
          low_i = 0;
        }
      }
#endif
      pro = _n_nock_on(bus, nex);
      u3t_drop();

      return pro;
    }

    case c3__live: {
      if ( c3y == u3ud(hod) ) {
        u3t_off(noc_o);
        u3t_heck(hod);
        u3t_on(noc_o);
      } else {
        u3z(hod);
      }
      return _n_nock_on(bus, nex);
    }

    case c3__slog: {
      if ( !(u3C.wag_w & u3o_quiet) ) {
        u3t_off(noc_o);
        u3t_slog(hod);
        u3t_on(noc_o);
      }
      return _n_nock_on(bus, nex);
    }

    case c3__germ: {
      u3_noun pro = _n_nock_on(bus, nex);

      if ( c3y == u3r_sing(pro, hod) ) {
        u3z(pro); return hod;
      } else {
        u3z(hod); return pro;
      }
    }

    case c3__fast: {
      u3_noun pro = _n_nock_on(bus, nex);

      u3t_off(noc_o);
      u3j_mine(hod, u3k(pro));
      u3t_on(noc_o);

      return pro;
    }

    case c3__memo: {
      u3z(hod);
#if 0
      return _n_nock_on(bus, nex);
#else
      {
        u3_noun pro = u3z_find_2(144 + c3__nock, bus, nex);

        if ( pro != u3_none ) {
          u3z(bus); u3z(nex);
          return pro;
        }
        pro = _n_nock_on(u3k(bus), u3k(nex));

        if ( &(u3H->rod_u) != u3R ) {
          u3z_save_2(144 + c3__nock, bus, nex, pro);
        }

        u3z(bus); u3z(nex);

        return pro;
      }
#endif
    }

    case c3__sole: {
      u3z(hod);
      {
        u3_noun pro = _n_nock_on(bus, nex);

        // return u3z_uniq(pro);
        return pro;
      }
    }
  }
}

/* _n_mush_in(): see _n_mush().
*/
static u3_noun
_n_mush_in(u3_noun val)
{
  if ( c3n == u3du(val) ) {
    return u3_nul;
  }
  else {
    u3_noun h_val = u3h(val);
    u3_noun ite;

    if ( c3n == u3ud(h_val) ) {
      ite = u3nc(c3__leaf, u3_nul);
    } else {
      ite = u3nc(c3__leaf, u3qe_trip(h_val));
    }
    return u3nc(ite, _n_mush_in(u3t(val)));
  }
}

/* _n_mush(): tank from failed path request.
*/
static u3_noun 
_n_mush(u3_noun val)
{
  u3_noun pro;

  pro = u3nt(c3__rose,
             u3nt(u3nc('/', u3_nul), u3nc('/', u3_nul), u3_nul),
             _n_mush_in(val));
  u3z(val);
  return pro;
}

/* _n_nock_on(): produce .*(bus fol).  Do not virtualize.
*/
static u3_noun
_n_nock_on(u3_noun bus, u3_noun fol)
{
  u3_noun hib, gal;

  while ( 1 ) {
    hib = u3h(fol);
    gal = u3t(fol);

#ifdef U3_CPU_DEBUG
    u3R->pro.nox_d += 1;
#endif

    if ( c3y == u3r_du(hib) ) {
      u3_noun poz, riv;

      poz = _n_nock_on(u3k(bus), u3k(hib));
      riv = _n_nock_on(bus, u3k(gal));

      u3a_lose(fol);
      return u3i_cell(poz, riv);
    }
    else switch ( hib ) {
      default: return u3m_bail(c3__exit);

      case 0: {
        if ( c3n == u3r_ud(gal) ) {
          return u3m_bail(c3__exit);
        }
        else {
          u3_noun pro = u3k(u3at(gal, bus));

          u3a_lose(bus); u3a_lose(fol);
          return pro;
        }
      }
      c3_assert(!"not reached");

      case 1: {
        u3_noun pro = u3k(gal);

        u3a_lose(bus); u3a_lose(fol);
        return pro;
      }
      c3_assert(!"not reached");

      case 2: {
        u3_noun nex = _n_nock_on(u3k(bus), u3k(u3t(gal)));
        u3_noun seb = _n_nock_on(bus, u3k(u3h(gal)));

        u3a_lose(fol);
        bus = seb;
        fol = nex;
        continue;
      }
      c3_assert(!"not reached");

      case 3: {
        u3_noun gof, pro;

        gof = _n_nock_on(bus, u3k(gal));
        pro = u3r_du(gof);

        u3a_lose(gof); u3a_lose(fol);
        return pro;
      }
      c3_assert(!"not reached");

      case 4: {
        u3_noun gof, pro;

        gof = _n_nock_on(bus, u3k(gal));
        pro = u3i_vint(gof);

        u3a_lose(fol);
        return pro;
      }
      c3_assert(!"not reached");

      case 5: {
        u3_noun wim = _n_nock_on(bus, u3k(gal));
        u3_noun pro = u3r_sing(u3h(wim), u3t(wim));

        u3a_lose(wim); u3a_lose(fol);
        return pro;
      }
      c3_assert(!"not reached");

      case 6: {
        u3_noun b_gal, c_gal, d_gal;

        u3x_trel(gal, &b_gal, &c_gal, &d_gal);
        {
          u3_noun tys = _n_nock_on(u3k(bus), u3k(b_gal));
          u3_noun nex;

          if ( 0 == tys ) {
            nex = u3k(c_gal);
          } else if ( 1 == tys ) {
            nex = u3k(d_gal);
          } else return u3m_bail(c3__exit);

          u3a_lose(fol);
          fol = nex;
          continue;
        }
      }
      c3_assert(!"not reached");

      case 7: {
        u3_noun b_gal, c_gal;

        u3x_cell(gal, &b_gal, &c_gal);
        {
          u3_noun bod = _n_nock_on(bus, u3k(b_gal));
          u3_noun nex = u3k(c_gal);

          u3a_lose(fol);
          bus = bod;
          fol = nex;
          continue;
        }
      }
      c3_assert(!"not reached");

      case 8: {
        u3_noun b_gal, c_gal;

        u3x_cell(gal, &b_gal, &c_gal);
        {
          u3_noun heb = _n_nock_on(u3k(bus), u3k(b_gal));
          u3_noun bod = u3nc(heb, bus);
          u3_noun nex = u3k(c_gal);

          u3a_lose(fol);
          bus = bod;
          fol = nex;
          continue;
        }
      }
      c3_assert(!"not reached");

      case 9: {
        u3_noun b_gal, c_gal;

        u3x_cell(gal, &b_gal, &c_gal);
        {
          u3_noun seb = _n_nock_on(bus, u3k(c_gal));
          u3_noun pro;
         
          u3t_off(noc_o);
          pro = u3j_kick(seb, b_gal);
          u3t_on(noc_o);

          if ( u3_none != pro ) {
            u3a_lose(fol);
            return pro;
          }
          else {
            if ( c3n == u3r_ud(b_gal) ) {
              return u3m_bail(c3__exit);
            }
            else {
              u3_noun nex = u3k(u3at(b_gal, seb));

              u3a_lose(fol);
              bus = seb;
              fol = nex;
              continue;
            }
          }
        }
      }
      c3_assert(!"not reached");

      case 10: {
        u3_noun p_gal, q_gal;

        u3x_cell(gal, &p_gal, &q_gal);
        {
          u3_noun zep, hod, nex;

          if ( c3y == u3r_du(p_gal) ) {
            u3_noun b_gal = u3h(p_gal);
            u3_noun c_gal = u3t(p_gal);
            u3_noun d_gal = q_gal;

            zep = u3k(b_gal);
            hod = _n_nock_on(u3k(bus), u3k(c_gal));
            nex = u3k(d_gal);
          }
          else {
            u3_noun b_gal = p_gal;
            u3_noun c_gal = q_gal;

            zep = u3k(b_gal);
            hod = u3_nul;
            nex = u3k(c_gal);
          }

          u3a_lose(fol);
          return _n_hint(zep, hod, bus, nex);
        }
      }

      case 11: {
        u3_noun ref = _n_nock_on(u3k(bus), u3k(u3h(gal)));
        u3_noun gof = _n_nock_on(bus, u3k(u3t(gal)));
        u3_noun val;

        u3t_off(noc_o);
        val = u3m_soft_esc(ref, u3k(gof));
        u3t_on(noc_o);

        if ( !_(u3du(val)) ) {
          u3m_bail(u3nt(1, gof, 0));
        } 
        if ( !_(u3du(u3t(val))) ) {
          //
          //  replace with proper error stack push
          //
          u3t_push(u3nc(c3__hunk, _n_mush(gof)));
          return u3m_bail(c3__exit);
        }
        else {
          u3_noun pro;

          u3z(gof);
          u3z(fol);
          pro = u3k(u3t(u3t(val)));
          u3z(val);

          return pro;
        }
      }  
      c3_assert(!"not reached");
    }
  }
}

/* u3n_nock_on(): produce .*(bus fol).  Do not virtualize.
*/
u3_noun
u3n_nock_on(u3_noun bus, u3_noun fol)
{
  u3_noun pro;

  u3t_on(noc_o);
  pro = _n_nock_on(bus, fol);
  u3t_off(noc_o);

  return pro;
}

/* u3n_kick_on(): fire `gat` without changing the sample.
*/
u3_noun
u3n_kick_on(u3_noun gat)
{
  return u3j_kink(gat, 2);
}

c3_w exc_w;

/* u3n_slam_on(): produce (gat sam).
*/
u3_noun
u3n_slam_on(u3_noun gat, u3_noun sam)
{
  u3_noun cor = u3nc(u3k(u3h(gat)), u3nc(sam, u3k(u3t(u3t(gat)))));

#if 0
  if ( &u3H->rod_u == u3R ) {
    if ( exc_w == 1 ) {
      c3_assert(0);
    } 
    exc_w++;
  }
#endif
  u3z(gat);
  return u3n_kick_on(cor);
}

/* u3n_nock_et(): produce .*(bus fol), as ++toon, in namespace.
*/
u3_noun
u3n_nock_et(u3_noun gul, u3_noun bus, u3_noun fol)
{
  return u3m_soft_run(gul, u3n_nock_on, bus, fol);
}

/* u3n_slam_et(): produce (gat sam), as ++toon, in namespace.
*/
u3_noun
u3n_slam_et(u3_noun gul, u3_noun gat, u3_noun sam)
{
  return u3m_soft_run(gul, u3n_slam_on, gat, sam);
}

/* u3n_nock_an(): as slam_in(), but with empty fly.
*/
u3_noun
u3n_nock_an(u3_noun bus, u3_noun fol)
{
  u3_noun gul = u3nt(u3nt(1, 0, 0), 0, 0);  //  |=(a/{* *} ~)

  return u3n_nock_et(gul, bus, fol);
}

/* _n_mush_in(): see _n_mush().
*/
static u3_noun
_n_mush_in(u3_noun val)
{
  if ( c3n == u3du(val) ) {
    return u3_nul;
  }
  else {
    u3_noun h_val = u3h(val);
    u3_noun ite;

    if ( c3n == u3ud(h_val) ) {
      ite = u3nc(c3__leaf, u3_nul);
    } else {
      ite = u3nc(c3__leaf, u3qe_trip(h_val));
    }
    return u3nc(ite, _n_mush_in(u3t(val)));
      case c3_
    }
  }
}

#define FRAG 0
#define QUOT 1
#define NOCK 2
#define DEEP 3
#define BUMP 4
#define SAME 5
#define BAIL 6
#define HEAD 7
#define TAIL 8
#define COPY 9
#define SWAP 10
#define CONS 11
#define SCON 12
#define SKIN 13
#define SKIP 14
#define WISH 15
#define KICK 16

static inline c3_y
_n_emit(u3_noun *ops, u3_noun op)
{
  *ops = u3nc(op, *ops);
  if ( c3n == u3du(op) ) {
    return sizeof(c3_y);
  }
  else switch ( u3h(op) ) {
    case SKIP:
    case SKIN: 
      return sizeof(c3_y) + sizeof(c3_s);
    case QUOT:
    case QUIP:
    case FRAG:
    case TICK:
    case KICK:
      return sizeof(c3_y) + sizeof(u3_noun);
    default:
      c3_assert(0);
  }
}

static c3_s _n_comp(u3_noun*, u3_noun, c3_o);

static c3_s _n_bint(u3_noun* ops, u3_noun hif, u3_noun nef, c3_o tel_o)
{
  if ( c3n == u3du(hif) ) {
    // no currently recognized static hints
    return _n_comp(ops, nef, tel_o);
  }
  else {
    c3_s tot_s = 0;
    u3_noun zep, hod;
    u3x_cell(hif, &zep, &hod);

    switch ( zep ) {
      default:
        tos += _n_emit(ops, COPY);
        tos += _n_comp(ops, hod, c3n);
        tos += _n_emit(ops, TOSS);
        tos += _n_comp(ops, nef, tel_o);
        break;

      case c3__hunk:
      case c3__lose:
      case c3__mean:
      case c3__spot: 
        tot_s += _n_emit(ops, u3nc(QUIP, zep));
        tot_s += _n_emit(ops, COPY);
        tot_s += _n_comp(ops, hod, c3n);
        tot_s += _n_emit(ops, CONS);
        tot_s += _n_emit(ops, STAP);
        tot_s += _n_comp(ops, nef, c3n);
        tot_s += _n_emit(ops, STOP);
        break;

      case c3__live: 
        tot_s += _n_emit(ops, COPY);
        tot_s += _n_comp(ops, hod, c3n);
        tot_s += _n_emit(ops, PEEP);
        tot_s += _n_emit(ops, u3nc(SKIN, sizeof(c3_y) +
                                         sizeof(c3_y) + sizeof(c3_s)));
        tot_s += _n_emit(ops, HECK);
        tot_s += _n_emit(ops, u3nc(SKIP, sizeof(c3_y)));
        tot_s += _n_emit(ops, TOSS);
        tot_s += _n_comp(ops, nef, tel_o);
        break;

      case c3__slog: 
        tot_s += _n_emit(ops, COPY);
        tot_s += _n_comp(ops, hod, c3n);
        tot_s += _n_emit(ops, SLOG);
        tot_s += _n_comp(ops, nef, tel_o);
        break;

      case c3__fast: 
        tot_s += _n_emit(ops, COPY);
        tot_s += _n_comp(ops, hod, c3n);
        tot_s += _n_emit(ops, SWAP);
        tot_s += _n_comp(ops, nef, c3n);
        tot_s += _n_emit(ops, FAST);
        break;

      case c3__memo: {
        u3_noun nop = u3_nul;
        c3_s n_s    = _n_comp(&nop, nef, c3n);

        n_s   += _n_emit(ops, PUMO);
        tot_s += _n_comp(ops, hod, c3n);
        tot_s += _n_emit(ops, GEMO);
        tot_s += _n_emit(ops, PEEP);
        tot_s += _n_emit(ops, u3nc(SKIN, sizeof(c3_y) + 
                                         sizeof(c3_y) + sizeof(c3_s)));
        tot_s += _n_emit(ops, TAIL);
        tot_s += _n_emit(ops, u3nc(SKIP, n_s));

        _n_apen(ops, nop);
        tot_s += n_s;
        break;
      }
    }
    return tot_s;
  }
}

/* fol is RETAINED */
static c3_s
_n_comp(u3_noun* ops, u3_noun fol, c3_o tel_o) {
  c3_s tot_s = 0;
  u3_noun cod, arg, hed, tel;
  u3x_cell(fol, &cod, &arg);

  if ( c3y == u3du(cod) ) {
    tot_s += _n_emit(ops, COPY);
    tot_s += _n_comp(ops, cod, c3n);
    tot_s += _n_emit(ops, SWAP);
    tot_s += _n_comp(ops, arg, c3n);
    tot_s += _n_emit(ops, CONS);
  }
  else switch ( cod ) {
    case 0: 
      if ( c3n == u3ud(arg) ) {
        return u3m_bail(c3__exit);
      }
      switch ( arg ) {
        case 0:
          tot_s += _n_emit(ops, BAIL);
          break;
        case 1:
          break;
        case 2:
          tot_s += _n_emit(ops, HEAD);
          break;
        case 3:
          tot_s += _n_emit(ops, TAIL);
          break;
        default:
          tot_s += _n_emit(ops, u3nc(FRAG, u3k(arg)));
      }
      break;
    case 1: {
      tot_s += _n_emit(ops, u3nc(QUOT, u3k(arg)));
      break;
    }
    case 2:
      u3x_cell(arg, &hed, &tel);
      tot_s += _n_emit(ops, COPY);
      tot_s += _n_comp(ops, hed, c3n);
      tot_s += _n_emit(ops, SWAP);
      tot_s += _n_comp(ops, tel, c3n);
      tot_s += _n_emit(ops, (tel_o ? NOCT : NOCK));
      break;
    case 3:
      tot_s += _n_comp(ops, arg, c3n);
      tot_s += _n_emit(ops, DEEP);
      break;
    case 4:
      tot_s += _n_comp(ops, arg, c3n);
      tot_s += _n_emit(ops, BUMP);
      break;
    case 5:
      u3x_cell(arg, &hed, &tel);
      tot_s += _n_emit(ops, COPY);
      tot_s += _n_comp(ops, hed, c3n);
      tot_s += _n_emit(ops, SWAP);
      tot_s += _n_comp(ops, tel, c3n);
      tot_s += _n_emit(ops, SAME);
      break;
    case 6: {
      u3_noun mid;
      u3x_trel(arg, &hed, &mid, &tel);

      tot_s += _n_comp(ops, hed, c3n);

      u3_noun yep   = u3_nul,
              nop   = u3_nul;
      c3_s    y_s   = _n_comp(&yep, mid, tel_o),
              n_s   = _n_comp(&nop, tel, tel_o),
              sin_s = y_s + sizeof(c3_y) + sizeof(c3_s);

      tot_s += _n_emit(ops, u3nc(SKIN, sin_s));
      _n_apen(ops, yep);
      tot_s += y_s;

      tot_s += _n_emit(ops, u3nc(SKIP, n_s));
      _n_apen(ops, nop);
      tot_s += n_s;
      break;
    }
    case 7:
      u3x_cell(arg, &hed, &tel);
      tot_s += _n_comp(ops, hed, c3n);
      tot_s += _n_comp(ops, tel, tel_o);
      break;
    case 8:
      u3x_cell(arg, &hed, &tel);
      tot_s += _n_emit(ops, COPY);
      tot_s += _n_comp(ops, hed, c3n);
      tot_s += _n_emit(ops, SCON);
      tot_s += _n_comp(ops, tel, tel_o);
      break;
    case 9:
      u3x_cell(arg, &hed, &tel);
      if ( 3 == u3qc_cap(hed) ) {
        u3_noun mac = u3nq(7, u3k(tel), 2, u3nt(u3nc(0, 1), 0, u3k(hed)));
        tot_s += _n_comp(ops, mac, tel_o);
        u3z(mac);
      }
      else {
        tot_s += _n_comp(ops, tel, c3n);
        tot_s += _n_emit(ops, u3nc((tel_o ? TICK : KICK), u3k(hed)));
      }
      break;
    case 10:
      u3x_cell(arg, &hed, &tel);
      tot_s += _n_bint(ops, hed, tel, tel_o);
      break;
    case 11:
      u3x_cell(arg, &hed, &tel);
      tot_s += _n_emit(ops, COPY);
      tot_s += _n_comp(ops, hed, c3n);
      tot_s += _n_emit(ops, SWAP);
      tot_s += _n_comp(ops, tel, c3n);
      tot_s += _n_emit(ops, WISH);
      break;
  }
  return tot_s;
}

static c3_y*
_n_asm(u3_noun ops)
{
  u3_noun   top   = ops;
  c3_s      i_s   = _n_comp(&ops, fol);
  c3_y*     buf_y = u3a_malloc(sizeof(c3_y) * (i_s+1));

  buf_y[i_s] = HALT;
  while ( --i_s >= 0 ) {
    u3_noun op = u3h(ops);
    if ( c3y == u3ud(op) ) {
      buf_y[i_s] = (c3_y) u3h(ops);
    }
    else {
      u3_noun cod = u3h(op);
      switch ( cod ) {
        case SKIP:
        case SKIN: {
          c3_s off_s = u3t(op);
          buf_y[i_s--] = (c3_y) (off_s >> 8);
          buf_y[i_s--] = (c3_y) off_s;
          buf_y[i_s]   = (c3_y) cod;
          break;
        }
        case QUOT:
        case QUIP:
        case FRAG:
        case TICK:
        case KICK: {
          c3_w non_w = u3t(op);
          buf_y[i_s--] = (c3_y) (non_w >> 24);
          buf_y[i_s--] = (c3_y) (non_w >> 16);
          buf_y[i_s--] = (c3_y) (non_w >> 8);
          buf_y[i_s--] = (c3_y) non_w;
          buf_y[i_s]   = (c3_y) cod;
          break;
        }
        default:
          c3_assert(0);
      }
    }
    ops = u3t(ops);
  }

  u3z(ops);
  return buf_y;
}

static inline void
_n_push(u3_noun a)
{
  u3_noun* p = (u3_noun*) u3a_push(sizeof(u3_noun));
  *p = a;
}

static inline u3_noun*
_n_peek()
{
  return (u3_noun*) u3a_peek(sizeof(u3_noun));
}

static inline u3_noun
_n_pop()
{
  u3_noun r = *(_n_peek());
  u3a_pop(sizeof(u3_noun));
  return r;
}

static inline void
_n_toss()
{
  u3z(_n_pop());
}

static inline c3_s
_n_resh(c3_y* buf, c3_s* ip_s)
{
  c3_y les = buf[(*ip_s)++];
  c3_y mos = buf[(*ip_s)++];
  return les | (mos << 8);
}

static inline u3_noun
_n_rean(c3_y* buf, c3_s* ip_s)
{
  c3_y one = buf[(*ip_s)++],
       two = buf[(*ip_s)++],
       tre = buf[(*ip_s)++],
       qua = buf[(*ip_s)++];
  return one | (two << 8) | (tre << 16) | (qua << 24);
}

static inline c3_y*
_n_bite(u3_noun fol)
{
  return _n_asm(_n_comp(&bok, fol, c3y));
}

static c3_y*
_n_find(u3_noun fol)
{
  u3_noun got = u3h_get(u3R->byc.har_p, fol);
  if ( u3_none != got ) {
    return u3a_to_ptr(got);
  }
  else {
    c3_y* gop = _n_bite(fol);
    got = u3a_to_off(gop);
    u3h_put(u3R->byc.har_p, fol, got);
    return gop;
  }
}

static u3_noun 
_n_burn(c3_y* pog)
{
  static void* lab[] = {
    &&do_halt, &&do_copy, &&do_swap,
    &&do_toss, &&do_skip, &&do_skin,
    &&do_cons, &&do_scon,
    &&do_head, &&do_tail, &&do_frag,
    &&do_quot, &&do_quip,
    &&do_nock, &&do_noct,
    &&do_deep, &&do_peep,
    &&do_bump, &&do_same,
  };
  #define BURN() goto *lab[pog[ip_s++]]

  c3_s sip_s, ip_s = 0;
  c3_y  op;
  c3_y* gop;
  u3_noun* top;
  u3_noun* up;
  u3_noun x, o;
  u3p(void) empty = u3R->cap_p;

  BURN();
  while ( 1 ) {
    do_halt: 
      x = _n_pop();
      c3_assert( empty == u3R->cap_p );
      return x;

    do_copy:
      top = _n_peek();
      _n_push(u3k(*top));
      BURN();

    do_swap:
      up   = (u3_noun*) u3a_peek(sizeof(u3_noun) + sizeof(u3_noun));
      top  = _n_peek();
      x    = *top;
      *top = *up;
      *up  = x;
      BURN();

    do_toss:
      _n_toss();
      BURN();

    do_skip:
      ip_s += _n_resh(pog, &ip_s);
      BURN();

    do_skin:
      sip_s  = _n_resh(pog, &ip_s);
      x      = _n_pop();
      if ( c3n == x ) {
        ip_s += sip_s;
      }
      else if ( c3y != x ) {
        return u3m_bail(c3__exit);
      }
      BURN();

    do_cons:
      x    = _n_pop();
      top  = _n_peek();
      *top = u3nc(*top, x);
      BURN();

    do_scon:
      x    = _n_pop();
      top  = _n_peek();
      *top = u3nc(x, *top);
      BURN();

    do_head:
      top  = _n_peek();
      o    = *top;
      if ( c3n == u3du(o) ) {
        return u3m_bail(c3__exit);
      }
      *top = u3k(u3h(o));
      u3z(o);
      BURN();

    do_tail:
      top  = _n_peek();
      o    = *top;
      if ( c3n == u3du(o) ) {
        return u3m_bail(c3__exit);
      }
      *top = u3k(u3t(o));
      u3z(o);
      BURN();

    do_frag:
      top  = _n_peek();
      o    = *top;
      x    = u3x_at(_n_rean(pog, &ip_s), o);
      *top = u3k(x);
      u3z(o);
      BURN();

    do_quot:
      _n_toss();
    do_quip:
      _n_push(_n_rean(pog, &ip_s));
      BURN();

    do_nock:
      gop = _n_find(_n_pop());
      _n_push(_n_burn(gop));
      BURN();

    do_noct:
      pog  = _n_find(_n_pop());
      ip_s = 0;
      BURN();

    do_deep:
      top  = _n_peek();
      o    = *top;
      *top = u3du(o);
      u3z(o);
      BURN();

    do_peep:
      top = _n_peek();
      _n_push(u3du(*top));
      BURN();

    do_bump:
      top = _n_peek();
      o   = *top;
      *top = u3i_vint(o);
      u3z(o);
      BURN();

    do_same:
      x    = _n_pop();
      top  = _n_peek();
      o    = *top;
      *top = u3r_sing(x, o);
      u3z(x);
      u3z(o);
      BURN();
  }
}
