/* g/n.c
**
*/
#include "all.h"

static u3_noun _n_nock_on(u3_noun bus, u3_noun fol);
static u3_noun _n_burn_on(u3_noun bus, u3_noun fol);

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
  //pro = _n_nock_on(bus, fol);
  pro = _n_burn_on(bus, fol);
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

/* These must match the order in the section marked OPCODE TABLE */
#define HALT 0
#define BAIL 1
#define COPY 2
#define SWAP 3
#define TOSS 4
#define AUTO 5
#define AULT 6
#define HEAD 7
#define HELD 8
#define TAIL 9
#define TALL 10
#define FRAS 11
#define FRAG 12
#define FRAB 13
#define FLAS 14
#define FLAG 15
#define FLAB 16
#define LIT0 17
#define LIT1 18
#define LITB 19
#define LITS 20
#define LITN 21
#define LIL0 22
#define LIL1 23
#define LILB 24
#define LILS 25
#define LILN 26
#define NOLK 27
#define NOCT 28
#define NOCK 29
#define DEEP 30
#define BUMP 31
#define SAM0 32
#define SAM1 33
#define SAMB 34
#define SAMS 35
#define SAMN 36
#define SAME 37
#define SALM 38
#define SAMC 39
#define SKIP 40
#define SBIP 41
#define SKIN 42
#define SBIN 43
#define SNOC 44
#define SNOL 45
#define SLAM 46
#define KICB 47
#define KICS 48
#define KICK 49
#define SLAT 50
#define TICB 51
#define TICS 52
#define TICK 53
#define WILS 54
#define WISH 55
#define CUSH 56
#define DROP 57
#define HECK 58
#define SLOG 59
#define FALT 60
#define FAST 61
#define SKIB 62
#define SKIM 63
#define SLIB 64
#define SLIM 65
#define SAVE 66

/* defining this will cause the bytecode interpreter to print out every opcode
 * as it executes, along with some other information. very spammy.
 */

#if 0
// match to OPCODE TABLE
static char* names[] = {
  "halt", "bail",
  "copy", "swap", "toss",
  "auto", "ault",
  "head", "held",
  "tail", "tall",
  "fras", "frag", "frab",
  "flas", "flag", "flab",
  "lit0", "lit1",
  "litb", "lits", "litn",
  "lil0", "lil1",
  "lilb", "lils", "liln",
  "nolk", "noct", "nock",
  "deep", "bump",
  "sam0", "sam1",
  "samb", "sams", "samn",
  "same", "salm", "samc",
  "skip", "sbip",
  "skin", "sbin",
  "snoc", "snol",
  "slam", "kicb", "kics", "kick",
  "slat", "ticb", "tics", "tick",
  "wils", "wish",
  "cush", "drop",
  "heck", "slog",
  "falt", "fast",
  "skib", "skim",
  "slib", "slim",
  "save"
};
#endif

/* _n_apen(): emit the instructions contained in src to dst
 */
static inline void
_n_apen(u3_noun* dst, u3_noun src)
{
  *dst = u3qb_weld(src, *dst);
}

/* _n_emit(): return the size (in bytes) of an opcode's argument
 */
static inline c3_y
_n_arg(c3_y cod_y)
{
  switch ( cod_y ) {
    case FRAB: case FLAB: case LILB: case LITB: case SAMB:
    case SBIP: case SBIN: case KICB: case TICB:
      return sizeof(c3_y);

    case FRAS: case FLAS: case LILS: case LITS: case SAMS:
    case SKIP: case SKIN: case KICS: case TICS:
      return sizeof(c3_s);

    case CUSH: case FRAG: case FLAG: case LILN: case LITN:
    case SAMN: case TICK: case KICK:
      return sizeof(u3_noun);

    case SKIM: case SLIM:
      return sizeof(c3_s) + sizeof(u3_noun);

    case SKIB: case SLIB:
      return sizeof(c3_y) + sizeof(u3_noun);

    default:
      return 0;
  }
}

/* _n_emit(): emit a single instruction to ops, returning
 *            the size (in bytes) required to store that
 *            opcode.
 */
static inline c3_y
_n_emit(u3_noun *ops, u3_noun op)
{
  *ops = u3nc(op, *ops);
  return sizeof(c3_y) + (c3n == u3du(op) ? 0 : _n_arg(u3h(op)));
}

static c3_s _n_comp(u3_noun*, u3_noun, c3_o, c3_o);

/* _n_bint(): hint-processing helper for _n_comp.
 *            hif: hint-formula (first part of 10). RETAIN.
 *            nef: next-formula (second part of 10). RETAIN.
 */
static c3_s
_n_bint(u3_noun* ops, u3_noun hif, u3_noun nef, c3_o los_o, c3_o tel_o)
{
  if ( c3n == u3du(hif) ) {
    // no currently recognized static hints
    return _n_comp(ops, nef, los_o, tel_o);
  }
  else {
    c3_s tot_s = 0;
    u3_noun zep, hod;
    u3x_cell(hif, &zep, &hod);

    switch ( zep ) {
      default:
        tot_s += _n_comp(ops, hod, c3n, c3n);
        tot_s += _n_emit(ops, TOSS);
        tot_s += _n_comp(ops, nef, los_o, tel_o);
        break;

      case c3__hunk:
      case c3__lose:
      case c3__mean:
      case c3__spot: 
        tot_s += _n_comp(ops, hod, c3n, c3n);
        tot_s += _n_emit(ops, u3nc(CUSH, zep));
        tot_s += _n_comp(ops, nef, los_o, c3n);
        tot_s += _n_emit(ops, DROP);
        break;

      case c3__live:
        tot_s += _n_comp(ops, hod, c3n, c3n);
        tot_s += _n_emit(ops, HECK);
        tot_s += _n_comp(ops, nef, los_o, tel_o);
        break;

      case c3__slog: 
        tot_s += _n_comp(ops, hod, c3n, c3n);
        tot_s += _n_emit(ops, SLOG);
        tot_s += _n_comp(ops, nef, los_o, tel_o);
        break;

      // germ and sole are unused...

      case c3__fast: 
        tot_s += _n_comp(ops, hod, c3n, c3n);
        tot_s += _n_emit(ops, SWAP);
        tot_s += _n_comp(ops, nef, c3n, c3n);
        tot_s += _n_emit(ops, (c3y == los_o) ? FALT : FAST);
        break;

      case c3__memo: {
        u3_noun mem = u3_nul;
        c3_s mem_s = 0;
        c3_y op_y;

        // we just throw away the hint (why is this not a static hint?)
        tot_s += _n_comp(ops, hod, c3n, c3n);
        tot_s += _n_emit(ops, TOSS);

        // memoizing code always loses TOS because SAVE needs [pro key]
        mem_s += _n_comp(&mem, nef, c3y, c3n);
        mem_s += _n_emit(&mem, SAVE);

        op_y = (c3y == los_o)
             ? (( mem_s <= 0xFF ) ? SLIB : SLIM)
             : (( mem_s <= 0xFF ) ? SKIB : SKIM);
        tot_s += _n_emit(ops, u3nt(op_y, mem_s, u3k(nef)));
        tot_s += mem_s; _n_apen(ops, mem);
        break;
      }
    }
    return tot_s;
  }
}

/* _n_comp(): compile nock formula to reversed opcode list
 *            ops is a pointer to a list (to be emitted to)
 *            fol is the nock formula to compile. RETAIN.
 *            los_o indicates whether we should remove our subject from the stack
 *            tel_o is yes if this formula is in tail position
 *            return: the number of bytes needed for this opcode list
 */
static c3_s
_n_comp(u3_noun* ops, u3_noun fol, c3_o los_o, c3_o tel_o)
{
  c3_y op_y;
  c3_s tot_s = 0;
  u3_noun cod, arg, hed, tel;
  u3x_cell(fol, &cod, &arg);
  if ( c3y == u3du(cod) ) {
    tot_s += _n_comp(ops, cod, c3n, c3n);
    tot_s += _n_emit(ops, SWAP);
    tot_s += _n_comp(ops, arg, c3n, c3n);
    tot_s += _n_emit(ops, (c3y == los_o ) ? AULT : AUTO);
  }
  else switch ( cod ) {
    case 0:
      if ( c3n == u3ud(arg) ) {
        u3m_bail(c3__exit);
        return 0;
      }
      switch ( arg ) {
        case 0:
          tot_s += _n_emit(ops, BAIL);
          break;
        case 1:
          if ( c3n == los_o ) {
            tot_s += _n_emit(ops, COPY);
          }
          break;
        case 2:
          tot_s += _n_emit(ops, (c3y == los_o) ? HELD : HEAD);
          break;
        case 3:
          tot_s += _n_emit(ops, (c3y == los_o) ? TALL : TAIL);
          break;
        default:
          op_y = (c3y == los_o)
               ? (arg <= 0xFF ? FLAB : arg <= 0xFFFF ? FLAS : FLAG)
               : (arg <= 0xFF ? FRAB : arg <= 0xFFFF ? FRAS : FRAG);
          tot_s += _n_emit(ops, u3nc(op_y, arg));
          break;
      }
      break;

    case 1:
      switch ( arg ) {
        case 0:
          tot_s += _n_emit(ops, (c3y == los_o) ? LIL0 : LIT0);
          break;
        case 1:
          tot_s += _n_emit(ops, (c3y == los_o) ? LIL1 : LIT1);
          break;
        default:
          op_y = (c3y == los_o)
               ? (arg <= 0xFF ? LILB : arg <= 0xFFFF ? LILS : LILN)
               : (arg <= 0xFF ? LITB : arg <= 0xFFFF ? LITS : LITN);
          tot_s += _n_emit(ops, u3nc(op_y, u3k(arg)));
          break;
      }
      break;

    case 2:
      u3x_cell(arg, &hed, &tel);
      tot_s += _n_comp(ops, hed, c3n, c3n);
      tot_s += _n_emit(ops, SWAP);
      tot_s += _n_comp(ops, tel, c3n, c3n);
      /* things in tail position replace (so, lose) top of stack,
       * so NOCT "loses" and there is no non-losing version */
      op_y   = (c3y == tel_o)  ? NOCT 
             : ((c3y == los_o) ? NOLK : NOCK);
      tot_s += _n_emit(ops, op_y);
      break;

    case 3:
      tot_s += _n_comp(ops, arg, los_o, c3n);
      tot_s += _n_emit(ops, DEEP);
      break;

    case 4:
      tot_s += _n_comp(ops, arg, los_o, c3n);
      tot_s += _n_emit(ops, BUMP);
      break;

    case 5: {
      u3x_cell(arg, &hed, &tel);

      if ( c3n == u3du(hed) ) {
        tot_s += _n_comp(ops, arg, los_o, c3n);
        tot_s += _n_emit(ops, SAMC);
      }
      else {
        c3_t hec_t, tec_t;
        hec_t = (1 == u3h(hed));
        if ( c3n == u3du(tel) ) {
          u3m_bail(c3__exit);
          break;
        }
        else {
          tec_t = (1 == u3h(tel));
        }
        if ( hec_t && tec_t ) {
          if ( c3y == u3r_sing(u3t(hed), u3t(tel)) ) {
            tot_s += _n_emit(ops, (c3y == los_o) ? LIL0 : LIT0);
          }
          else {
            tot_s += _n_emit(ops, (c3y == los_o) ? LIL1 : LIT1);
          }
        }
        else if ( !hec_t && !tec_t ) {
          tot_s += _n_comp(ops, hed, c3n, c3n);
          tot_s += _n_emit(ops, SWAP);
          tot_s += _n_comp(ops, tel, c3n, c3n);
          tot_s += _n_emit(ops, (c3y == los_o) ? SALM : SAME);
        }
        else {
          tot_s += _n_comp(ops, (hec_t ? tel : hed), los_o, c3n);
          u3_noun lit = u3t(hec_t ? hed : tel);
          switch ( lit ) {
            case 0:
              tot_s += _n_emit(ops, SAM0);
              break;
            case 1:
              tot_s += _n_emit(ops, SAM1);
              break;
            default:
              op_y = lit <= 0xFF ? SAMB : lit <= 0xFFFF ? SAMS : SAMN;
              tot_s += _n_emit(ops, u3nc(op_y, u3k(lit)));
          }
        }
      }
      break;
    }

    case 6: {
      u3_noun mid, 
              yep = u3_nul,
              nop = u3_nul;
      c3_s    yep_s, nop_s;
      u3x_trel(arg, &hed, &mid, &tel);
      tot_s += _n_comp(ops, hed, c3n, c3n);
      yep_s  = _n_comp(&yep, mid, los_o, tel_o);
      nop_s  = _n_comp(&nop, tel, los_o, tel_o);
      op_y   = (nop_s <= 0xFF ? SBIP : SKIP);
      yep_s += _n_emit(&yep, u3nc(op_y, nop_s));
      op_y   = (yep_s <= 0xFF ? SBIN : SKIN);
      tot_s += _n_emit(ops, u3nc(op_y, yep_s));
      tot_s += yep_s; _n_apen(ops, yep);
      tot_s += nop_s; _n_apen(ops, nop);
      break;
    }

    case 7: 
      u3x_cell(arg, &hed, &tel);
      tot_s += _n_comp(ops, hed, los_o, c3n);
      tot_s += _n_comp(ops, tel, c3y, tel_o);
      break;

    case 8: 
      u3x_cell(arg, &hed, &tel);
      tot_s += _n_comp(ops, hed, c3n, c3n);
      tot_s += _n_emit(ops, (c3y == los_o) ? SNOL : SNOC);
      tot_s += _n_comp(ops, tel, c3y, tel_o);
      break;

    case 9:
      u3x_cell(arg, &hed, &tel);
      if ( 3 == u3qc_cap(hed) ) {
        u3_noun mac = u3nq(7, u3k(tel), 2, u3nt(u3nc(0, 1), 0, u3k(hed)));
        tot_s += _n_comp(ops, mac, los_o, tel_o);
        u3z(mac);
      }
      else {
        tot_s += _n_comp(ops, tel, (c3y == tel_o ? c3y : los_o), c3n);
        if ( 2 == hed ) {
          tot_s += _n_emit(ops, (c3y == tel_o) ? SLAT : SLAM);
        }
        else {
          op_y = (c3y == tel_o) 
               ? (hed <= 0xFF ? TICB : hed <= 0xFFFF ? TICS : TICK)
               : (hed <= 0xFF ? KICB : hed <= 0xFFFF ? KICS : KICK);
          tot_s += _n_emit(ops, u3nc(op_y, u3k(hed)));
        }
      }
      break;

    case 10:
      u3x_cell(arg, &hed, &tel);
      tot_s += _n_bint(ops, hed, tel, los_o, tel_o);
      break;

    case 11:
      u3x_cell(arg, &hed, &tel);
      tot_s += _n_comp(ops, hed, c3n, c3n);
      tot_s += _n_emit(ops, SWAP);
      tot_s += _n_comp(ops, tel, c3n, c3n);
      tot_s += _n_emit(ops, (c3y == los_o) ? WILS : WISH);
      break;

    default:
      u3m_bail(c3__exit);
      return 0;
  }
  return tot_s;
}

#if 0
static void _n_print_byc(c3_y* pog, c3_s her_s);
#endif

/* _n_asm(): assemble an accumulated list of instructions (i.e. from _n_comp)
 */
static c3_y*
_n_asm(u3_noun ops, c3_s len_s)
{
  u3_noun   top   = ops;
  c3_y*     buf_y = u3a_malloc(sizeof(c3_y) * (len_s+1));
  c3_s      i_s   = len_s;

  buf_y[i_s] = HALT;
  while ( i_s-- > 0 ) {
    u3_noun op = u3h(ops);
    if ( c3y == u3ud(op) ) {
      buf_y[i_s] = (c3_y) u3h(ops);
    }
    else {
      u3_noun cod = u3h(op);
      switch ( cod ) {
        case FRAB: case FLAB: case LILB: case LITB:
        case SAMB: case SBIP: case SBIN:
        case KICB: case TICB:
          buf_y[i_s--] = (c3_y) u3t(op);
          buf_y[i_s]   = (c3_y) cod;
          break;

        case FRAS: case FLAS: case LILS: case LITS:
        case SAMS: case SKIP: case SKIN:
        case KICS: case TICS: {
          c3_s off_s   = u3t(op);
          buf_y[i_s--] = (c3_y) (off_s >> 8);
          buf_y[i_s--] = (c3_y) off_s;
          buf_y[i_s]   = (c3_y) cod;
          break;
        }

        case CUSH: case FRAG: case FLAG:
        case LILN: case LITN:
        case SAMN: case TICK: case KICK: {
          c3_w non_w   = u3k(u3t(op));
          buf_y[i_s--] = (c3_y) (non_w >> 24);
          buf_y[i_s--] = (c3_y) (non_w >> 16);
          buf_y[i_s--] = (c3_y) (non_w >> 8);
          buf_y[i_s--] = (c3_y) non_w;
          buf_y[i_s]   = (c3_y) cod;
          break;
        }

        case SKIB: case SLIB: {
          c3_w non_w   = u3k(u3t(u3t(op)));
          buf_y[i_s--] = (c3_y) (non_w >> 24);
          buf_y[i_s--] = (c3_y) (non_w >> 16);
          buf_y[i_s--] = (c3_y) (non_w >> 8);
          buf_y[i_s--] = (c3_y) non_w;
          buf_y[i_s--] = (c3_y) u3h(u3t(op));
          buf_y[i_s]   = (c3_y) cod;
          break;
        }

        case SKIM: case SLIM: {
          c3_w non_w   = u3k(u3t(u3t(op)));
          c3_s sip_s   = u3h(u3t(op));
          buf_y[i_s--] = (c3_y) (non_w >> 24);
          buf_y[i_s--] = (c3_y) (non_w >> 16);
          buf_y[i_s--] = (c3_y) (non_w >> 8);
          buf_y[i_s--] = (c3_y) non_w;
          buf_y[i_s--] = (c3_y) (sip_s >> 8);
          buf_y[i_s--] = (c3_y) sip_s;
          buf_y[i_s]   = (c3_y) cod;
          break;
        }

        default:
          u3m_bail(c3__exit);
          return 0;
      }
    }
    ops = u3t(ops);
  }
  // this will trigger if we ever have a nock formula that compiles to more
  // than 2^16 opcodes. if needed, ip can be a c3_w.
  c3_assert(u3_nul == ops);

  u3z(top);
  return buf_y;
}

/* _n_push(): push a noun onto the stack. RETAIN
 *            mov: -1 north, 1 south
 *            off: 0 north, -1 south
 */
static inline void
_n_push(c3_ys mov, c3_ys off, u3_noun a)
{
  u3R->cap_p += mov;
  u3_noun* p = u3to(u3_noun, u3R->cap_p + off);
  //u3_noun* p = (u3_noun*) u3a_push(sizeof(u3_noun));
  *p = a;
}

/* _n_peek(): pointer to noun at top of stack
 *            off: 0 north, -1 south
 */
static inline u3_noun*
_n_peek(c3_ys off)
{
  return u3to(u3_noun, u3R->cap_p + off);
  //return (u3_noun*) u3a_peek(sizeof(u3_noun));
}

/* _n_peet(): address of the next-to-top of stack
 *            mov: -1 north, 1 south
 *            off: 0 north, -1 south
 */
static inline u3_noun*
_n_peet(c3_ys mov, c3_ys off)
{
  return u3to(u3_noun, (u3R->cap_p - mov) + off);
}

/* _n_pop(): pop a noun from the cap stack
 *           mov: -1 north, 1 south
 */
static inline void
_n_pop(c3_ys mov)
{
  u3R->cap_p -= mov;
}

/* _n_pep(): pop and return noun from the cap stack
 *           mov: -1 north, 1 south
 *           off: 0 north, -1 south
 */
static inline u3_noun
_n_pep(c3_ys mov, c3_ys off)
{
  u3_noun r = *(_n_peek(off));
  _n_pop(mov);
  return r;
}

/* _n_toss(): pep and lose
 */
static inline void
_n_toss(c3_ys mov, c3_ys off)
{
  u3z(_n_pep(mov, off));
}

/* _n_resh(): read a c3_s from the bytecode stream
 */
static inline c3_s
_n_resh(c3_y* buf, c3_s* ip_s)
{
  c3_y les = buf[(*ip_s)++];
  c3_y mos = buf[(*ip_s)++];
  return les | (mos << 8);
}

/* _n_rean(): read a noun from the bytecode stream.
 *            refcount is NOT incremented.
 */
static inline u3_noun
_n_rean(c3_y* buf, c3_s* ip_s)
{
  c3_y one = buf[(*ip_s)++],
       two = buf[(*ip_s)++],
       tre = buf[(*ip_s)++],
       qua = buf[(*ip_s)++];
  return one | (two << 8) | (tre << 16) | (qua << 24);
}

/* _n_bite(): compile a nock formula to bytecode
 */
static inline c3_y*
_n_bite(u3_noun fol)
{
  u3_noun bok = u3_nul;
  c3_s len_s  = _n_comp(&bok, fol, c3y, c3y);
  c3_y* buf_y = _n_asm(bok, len_s);
  return buf_y;
}

/* _n_find(): return bytecode for given formula. fol is RETAINED.
 */
static inline c3_y*
_n_find(u3_noun fol)
{
  u3a_road* rod_u = u3R;

  while ( 1 ) {
    u3_weak jaw = u3h_gut(rod_u->byc.har_p, fol);

    if ( u3_none != jaw ) {
      return u3a_into(jaw);
    }

    if ( rod_u->par_p ) {
      rod_u = u3to(u3_road, rod_u->par_p);
    }
    else break;
  }

  {
    c3_y* gop = _n_bite(fol);
    u3h_put(u3R->byc.har_p, fol, u3a_outa(gop));
    return gop;
  }
}

/* _n_swap(): swap two items on the top of the stack, return pointer to top
 */
static inline u3_noun*
_n_swap(c3_ys mov, c3_ys off)
{
  u3_noun* top = _n_peek(off);
  u3_noun* up   = _n_peet(mov, off);
  u3_noun  tmp  = *up;
  *up  = *top;
  *top = tmp;
  return top;
}

/* _n_kale(): bail(exit) if not cell
 */ 
static inline u3_noun
_n_kale(u3_noun a)
{
  if ( c3n == u3du(a) ) {
    u3m_bail(c3__exit);
  }
  return a;
}

typedef struct {
  c3_y* pog;
  c3_s  ip_s;
} burnframe;

/* _n_burn(): pog: program
 *            bus: subject
 *            mov: -1 north, 1 south
 *            off: 0 north, -1 south
 */
static u3_noun
_n_burn(c3_y* pog, u3_noun bus, c3_ys mov, c3_ys off)
{
  /* OPCODE TABLE */
  static void* lab[] = {
    &&do_halt, &&do_bail,
    &&do_copy, &&do_swap, &&do_toss,
    &&do_auto, &&do_ault,
    &&do_head, &&do_held,
    &&do_tail, &&do_tall,
    &&do_fras, &&do_frag, &&do_frab,
    &&do_flas, &&do_flag, &&do_flab,
    &&do_lit0, &&do_lit1,
    &&do_litb, &&do_lits, &&do_litn,
    &&do_lil0, &&do_lil1,
    &&do_lilb, &&do_lils, &&do_liln,
    &&do_nolk, &&do_noct, &&do_nock,
    &&do_deep, &&do_bump,
    &&do_sam0, &&do_sam1,
    &&do_samb, &&do_sams, &&do_samn,
    &&do_same, &&do_salm, &&do_samc,
    &&do_skip, &&do_sbip,
    &&do_skin, &&do_sbin,
    &&do_snoc, &&do_snol,
    &&do_slam, &&do_kicb, &&do_kics, &&do_kick,
    &&do_slat, &&do_ticb, &&do_tics, &&do_tick,
    &&do_wils, &&do_wish,
    &&do_cush, &&do_drop,
    &&do_heck, &&do_slog,
    &&do_falt, &&do_fast,
    &&do_skib, &&do_skim,
    &&do_slib, &&do_slim,
    &&do_save
  };

  c3_s sip_s, ip_s = 0;
  u3_noun* top;
  u3_noun x, o;
  u3p(void) empty;
  burnframe* fam;

  empty = u3R->cap_p;
  _n_push(mov, off, bus);

#ifdef U3_CPU_DEBUG
  u3R->pro.nox_d += 1;
#endif
#ifdef VERBYC
  #define BURN() fprintf(stderr, "%s ", names[pog[ip_s]]); goto *lab[pog[ip_s++]]
#else
  #define BURN() goto *lab[pog[ip_s++]]
#endif
  BURN();
  {
    do_halt: // [product ...burnframes...]
      x = _n_pep(mov, off);
#ifdef VERBYC
      fprintf(stderr, "return\r\n");
#endif
      if ( empty == u3R->cap_p ) {
        return x;
      }
      else {
        fam  = u3to(burnframe, u3R->cap_p) + off;
        pog  = fam->pog;
        ip_s = fam->ip_s;

        u3R->cap_p = u3of(burnframe, fam - (mov+off));
        _n_push(mov, off, x);
#ifdef VERBYC
        _n_print_byc(pog, ip_s);
#endif
        BURN();
      }

    do_bail:
      u3m_bail(c3__exit);
      return u3_none;

    do_copy:
      top = _n_peek(off);
      _n_push(mov, off, u3k(*top));
      BURN();

    do_swap:
      _n_swap(mov, off);
      BURN();

    do_toss:
      _n_toss(mov, off);
      BURN();

    do_auto:                         // [tel bus hed]
      x    = _n_pep(mov, off);       // [bus hed]
      top  = _n_swap(mov, off);      // [hed bus]
      *top = u3nc(*top, x);          // [pro bus]
      BURN();

    do_ault:                         // [tel bus hed]
      x    = _n_pep(mov, off);       // [bus hed]
      _n_toss(mov, off);             // [hed]
      top  = _n_peek(off);      
      *top = u3nc(*top, x);          // [pro]
      BURN();

    do_head:
      top  = _n_peek(off);
      _n_push(mov, off, u3k(u3h(_n_kale(*top))));
      BURN();

    do_held:
      top  = _n_peek(off);
      o    = _n_kale(*top);
      *top = u3k(u3h(o));
      u3z(o);
      BURN();

    do_tail:
      top = _n_peek(off);
      _n_push(mov, off, u3k(u3t(_n_kale(*top))));
      BURN();

    do_tall:
      top  = _n_peek(off);
      o    = _n_kale(*top);
      *top = u3k(u3t(o));
      u3z(o);
      BURN();

    do_fras:
      x = _n_resh(pog, &ip_s);
      goto frag_in;

    do_frag:
      x = _n_rean(pog, &ip_s);
      goto frag_in;

    do_frab:
      x = pog[ip_s++];
    frag_in:
      top = _n_peek(off);
      _n_push(mov, off, u3k(u3x_at(x, *top)));
      BURN();

    do_flas:
      x = _n_resh(pog, &ip_s);
      goto flag_in;

    do_flag:
      x = _n_rean(pog, &ip_s);
      goto flag_in;

    do_flab:
      x = pog[ip_s++];
    flag_in:
      top  = _n_peek(off);
      o    = *top;
      *top = u3k(u3x_at(x, o));
      u3z(o);
      BURN();

    do_lit0:
      _n_push(mov, off, 0);
      BURN();

    do_lit1:
      _n_push(mov, off, 1);
      BURN();

    do_litb:
      _n_push(mov, off,  pog[ip_s++]);
      BURN();

    do_lits:
      _n_push(mov, off, _n_resh(pog, &ip_s));
      BURN();

    do_litn:
      _n_push(mov, off, u3k(_n_rean(pog, &ip_s)));
      BURN();

    do_lil1:
      x = 1;
      goto lil_in;

    do_lilb:
      x = pog[ip_s++];
      goto lil_in;

    do_lils:
      x = _n_resh(pog, &ip_s);
      goto lil_in;

    do_liln:
      x = u3k(_n_rean(pog, &ip_s));
      goto lil_in;

    do_lil0:
      x = 0;
    lil_in:
      top = _n_peek(off);
      u3z(*top);
      *top = x;
      BURN();

    do_noct:                // [fol old bus]
      o = _n_pep(mov, off); // [old bus]
      _n_toss(mov, off);    // [bus]
      goto nock_out;

    do_nolk:                // [fol old bus]
      o = _n_pep(mov, off); // [old bus]
      _n_toss(mov, off);    // [bus]
      goto nock_in;

    do_nock:                // [fol old bus]
      o = _n_pep(mov, off); // [old bus]
      _n_swap(mov, off);    // [bus old]
    nock_in:
      x          = _n_pep(mov, off);
      fam        = u3to(burnframe, u3R->cap_p) + off + mov;
      u3R->cap_p = u3of(burnframe, fam - off);
      fam->ip_s  = ip_s;
      fam->pog   = pog;
      _n_push(mov, off, x);
    nock_out:
      pog  = _n_find(o);             
      ip_s = 0;
#ifdef U3_CPU_DEBUG
    u3R->pro.nox_d += 1;
#endif
#ifdef VERBYC
      fprintf(stderr, "\r\nnock jump: %u\r\n", o);
      _n_print_byc(pog, ip_s);
#endif
      u3z(o);
      BURN();
      
    do_deep:
      top  = _n_peek(off);
      o    = *top;
      *top = u3du(o);
      u3z(o);
      BURN();

    do_bump:
      top  = _n_peek(off);
      *top = u3i_vint(*top);
      BURN();

    do_sam0:
      top = _n_peek(off);
      if ( *top == 0 ) {
        *top = c3y;
      }
      else {
        u3z(*top);
        *top = c3n;
      }
      BURN();

    do_sam1:
      top = _n_peek(off);
      if ( *top == 1 ) {
        *top = c3y;
      }
      else {
        u3z(*top);
        *top = c3n;
      }
      BURN();

    do_samb:
      top = _n_peek(off);
      if ( *top == pog[ip_s++] ) {
        *top = c3y;
      }
      else {
        u3z(*top);
        *top = c3n;
      }
      BURN();

    do_sams:
      top = _n_peek(off);
      if ( *top == _n_resh(pog, &ip_s) ) {
        *top = c3y;
      }
      else {
        u3z(*top);
        *top = c3n;
      }
      BURN();

    do_samn:
      top  = _n_peek(off);
      o    = *top;
      *top = u3r_sing(o, _n_rean(pog, &ip_s));
      u3z(o);
      BURN();

    do_same:
      x = _n_pep(mov, off);
      _n_swap(mov, off);
      goto same_in;

    do_salm:
      x = _n_pep(mov, off);
      _n_toss(mov, off);
      goto same_in;

    same_in:
      top  = _n_peek(off);
      o    = *top;
      *top = u3r_sing(x, o);
      u3z(o);
      u3z(x);
      BURN();

    do_samc:
      top  = _n_peek(off);
      o    = *top;
      *top = u3r_sing(u3h(o), u3t(o));
      u3z(o);
      BURN();

    do_skip:
      ip_s += _n_resh(pog, &ip_s);
      BURN();

    do_sbip:
      ip_s += pog[ip_s] + 1;
      BURN();

    do_skin:
      sip_s  = _n_resh(pog, &ip_s);
      goto skin_in;

    do_sbin:
      sip_s  = pog[ip_s++];
    skin_in:
      x      = _n_pep(mov, off);
      if ( c3n == x ) {
        ip_s += sip_s;
      }
      else if ( c3y != x ) {
        u3m_bail(c3__exit);
        return u3_none;
      }
      BURN();

    do_snoc: // [hed tel]
      x    = _n_pep(mov, off);
      top  = _n_peek(off);
      _n_push(mov, off, u3nc(x, u3k(*top)));
      BURN();

    do_snol:
      x    = _n_pep(mov, off);
      top  = _n_peek(off);
      *top = u3nc(x, *top);
      BURN();

    do_kics:
      x = _n_resh(pog, &ip_s);
      goto kick_in;
      
    do_kick:
      x = _n_rean(pog, &ip_s);
      goto kick_in;

    do_kicb:
      x = pog[ip_s++];
      goto kick_in;
    do_slam:
      x = 2;
    kick_in:
      top  = _n_peek(off);
      o    = *top;
      u3t_off(noc_o);
      *top = u3j_kick(o, x);
      u3t_on(noc_o);
      if ( u3_none == *top ) {
        u3_noun fol = u3x_at(x, o);
        _n_toss(mov, off);

        fam        = u3to(burnframe, u3R->cap_p) + off + mov;
        u3R->cap_p = u3of(burnframe, fam - off);
        fam->ip_s  = ip_s;
        fam->pog   = pog;

        pog  = _n_find(fol);
        ip_s = 0;
#ifdef U3_CPU_DEBUG
    u3R->pro.nox_d += 1;
#endif
#ifdef VERBYC
        fprintf(stderr, "\r\nhead kick jump: %u, sp: %p\r\n", fol, top);
        _n_print_byc(pog, ip_s);
#endif
        _n_push(mov, off, o);
      }
#ifdef VERBYC
      else {
        fprintf(stderr, "head jet\r\n");
      }
#endif
      BURN();

    do_tics:
      x = _n_resh(pog, &ip_s);
      goto tick_in;
      
    do_tick:
      x = _n_rean(pog, &ip_s);
      goto tick_in;

    do_ticb:
      x = pog[ip_s++];
      goto tick_in;

    do_slat:
      x = 2;
    tick_in:
      top  = _n_peek(off);
      o    = *top;
      u3t_off(noc_o);
      *top = u3j_kick(o, x);
      u3t_on(noc_o);
      if ( u3_none == *top ) {
        u3_noun fol = u3x_at(x, o);
        *top = o;
        pog  = _n_find(fol);
        ip_s = 0;
#ifdef U3_CPU_DEBUG
    u3R->pro.nox_d += 1;
#endif
#ifdef VERBYC
        fprintf(stderr, "\r\ntail kick jump: %u, sp: %p\r\n", fol, top);
        _n_print_byc(pog, ip_s);
#endif
      }
#ifdef VERBYC
      else {
        fprintf(stderr, "tail jet\r\n");
      }
#endif
      BURN();

    do_wils:                   // [gof bus ref]
      o = _n_pep(mov,off);     // [bus ref]
      _n_toss(mov, off);       // [ref]
      top = _n_peek(off);
      goto wish_in;

    do_wish:                   // [gof bus ref]
      o = _n_pep(mov,off);     // [bus ref]
      top = _n_swap(mov, off); // [ref bus]
    wish_in:
      u3t_off(noc_o);
      x   = u3m_soft_esc(*top, u3k(o));
      u3t_on(noc_o);

      if ( c3n == u3du(x) ) {
        u3m_bail(u3nt(1, o, 0));
        return u3_none;
      }
      else if ( c3n == u3du(u3t(x)) ) {
        //  replace with proper error stack push
        u3t_push(u3nc(c3__hunk, _n_mush(o)));
        u3m_bail(c3__exit);
        return u3_none;
      }
      else {
        u3z(o);
        *top = u3k(u3t(u3t(x)));
        u3z(x);
        BURN();
      }

    do_cush:
      x = _n_rean(pog, &ip_s);
      o = _n_pep(mov, off);
      u3t_push(u3nc(u3k(x), o));
      BURN();

    do_drop:
      u3t_drop();
      BURN();

    do_heck:
      x = _n_pep(mov, off);
      if ( c3y == u3ud(x) ) {
        u3t_off(noc_o);
        u3t_heck(x);
        u3t_on(noc_o);
      }
      else {
        u3z(x);
      }
      BURN();

    do_slog:
      x = _n_pep(mov, off);
      if ( !(u3C.wag_w & u3o_quiet) ) {
        u3t_off(noc_o);
        u3t_slog(x);
        u3t_on(noc_o);
      }
      else {
        u3z(x);
      }
      BURN();

    do_falt:                   // [pro bus clu]
      o   = _n_pep(mov, off);  // [bus clu]
      _n_toss(mov, off);       // [clu]
      top = _n_peek(off);
      goto fast_in;

    do_fast:                   // [pro bus clu]
      o   = _n_pep(mov, off);  // [bus clu]
      top = _n_swap(mov, off); // [clu bus]
    fast_in:
      u3t_off(noc_o);
      u3j_mine(*top, u3k(o));
      u3t_on(noc_o);
      *top = o;
      BURN();

    do_skim:
      sip_s = _n_resh(pog, &ip_s);
      goto skim_in;

    do_skib:
      sip_s = pog[ip_s++];
    skim_in:
      top = _n_peek(off);
      x   = u3k(*top);
      goto skim_out;

    do_slim:
      sip_s = _n_resh(pog, &ip_s);
      goto slim_in;

    do_slib:
      sip_s = pog[ip_s++];
    slim_in:
      x = _n_pep(mov, off);
    skim_out:
      o = _n_rean(pog, &ip_s);
      x = u3nc(x, u3k(o));
      o = u3z_find(144 + c3__nock, x);
      if ( u3_none == o ) {
        _n_push(mov, off, x);
        _n_push(mov, off, u3k(u3h(x)));
      }
      else {
        ip_s += sip_s;
        _n_push(mov, off, o);
        u3z(x);
      }
      BURN();

    do_save:
      x   = _n_pep(mov, off);
      top = _n_peek(off);
      o   = *top;
      if ( &(u3H->rod_u) != u3R ) {
        u3z_save(144 + c3__nock, o, x);
      }
      *top = x;
      u3z(o);
      BURN();
  }
}

#if 0
/* _n_print_byc(): print bytecode. used for debugging.
 */
static void
_n_print_byc(c3_y* pog, c3_s her_s)
{
  c3_s ip_s = 0;
  if ( her_s == 0 ) {
    fprintf(stderr, "begin: {");
  }
  else {
    fprintf(stderr, "resume: {");
  }
  int first = 1;
  while ( pog[ip_s] ) {
    if ( first ) {
      first = 0;
    }
    else if (ip_s == her_s) {
      fprintf(stderr, " [*]");
    }
    else {
      fprintf(stderr, " ");
    }
    switch ( pog[ip_s] ) {
      default:
        fprintf(stderr, "%s", names[pog[ip_s++]]);
        break;

      case FRAB:
      case FLAB:
      case LILB:
      case LITB:
      case SAMB:
      case SBIP:
      case SBIN:
      case KICB:
      case TICB:
        fprintf(stderr, "[%s ", names[pog[ip_s++]]);
        fprintf(stderr, "%u]", pog[ip_s++]);
        break;

      case FRAS:
      case FLAS:
      case LILS:
      case LITS:
      case SAMS:
      case SKIP:
      case SKIN:
      case KICS:
      case TICS:
        fprintf(stderr, "[%s ", names[pog[ip_s++]]);
        fprintf(stderr, "%u]", _n_resh(pog, &ip_s));
        break;

      case CUSH:
      case FRAG:
      case FLAG:
      case LILN:
      case LITN:
      case SAMN:
      case TICK:
      case KICK:
        fprintf(stderr, "[%s ", names[pog[ip_s++]]);
        fprintf(stderr, "%u]", _n_rean(pog, &ip_s));
        break;

      case SKIB:
      case SLIB:
        fprintf(stderr, "[%s", names[pog[ip_s++]]);
        fprintf(stderr, " %u ", pog[ip_s++]);
        fprintf(stderr, "%u]", _n_rean(pog, &ip_s));
        break;

      case SKIM:
      case SLIM:
        fprintf(stderr, "[%s", names[pog[ip_s++]]);
        fprintf(stderr, " %u ", _n_resh(pog, &ip_s));
        fprintf(stderr, "%u]", _n_rean(pog, &ip_s));
        break;
    }
  }
  fprintf(stderr, " halt}\r\n");
}
#endif

/* _n_burn_on(): produce .*(bus fol) with bytecode interpreter
 */
static u3_noun
_n_burn_on(u3_noun bus, u3_noun fol)
{
  c3_y* pog = _n_find(fol);
  c3_ys mov, off;

  u3z(fol);
  if ( c3y == u3a_is_north(u3R) ) {
    mov = -1;
    off = 0;
  }
  else {
    mov = 1;
    off = -1;
  }
  u3_noun pro = _n_burn(pog, bus, mov, off);
  return pro;
}

#if 0
/* u3n_burn_on(): useful for distinguishing the bytecode interpreter from the
 *                tree-walking interpreter.
 */
u3_noun
u3n_burn_on(u3_noun bus, u3_noun fol)
{
  u3_noun pro;
  u3t_on(noc_o);
  pro = _n_burn_on(bus, fol);
  u3t_off(noc_o);
  return pro;
}
#endif

/* _n_take_narg(): helper for copying noun-parts of bytecode */
static void
_n_take_narg(c3_y* pog, c3_y* gop, c3_s sip_s, c3_s* ip_s)
{
  c3_s i_s;
  while ( sip_s-- > 0 ) {
    gop[*ip_s] = pog[*ip_s];
    *ip_s += 1;
  }
  u3_noun x = u3a_take(_n_rean(pog, ip_s));
  i_s = *ip_s;
  gop[--i_s] = (c3_y) (x >> 24);
  gop[--i_s] = (c3_y) (x >> 16);
  gop[--i_s] = (c3_y) (x >> 8);
  gop[--i_s] = (c3_y) x;
}

/* _n_take_byc(): copy bytecode from a junior road
 */
static c3_y*
_n_take_byc(c3_y* pog)
{
  c3_y  i_y;
  c3_s  ip_s, len_s;
  c3_y* gop, cod_y;

  // measure
  for ( ip_s = 0; (cod_y = pog[ip_s]) != HALT; ++ip_s ) {
    ip_s += _n_arg(cod_y);
  }
  len_s = ip_s + 1;

  gop = u3a_malloc(len_s);
  for ( ip_s = 0; ip_s < len_s; ) {
    cod_y = gop[ip_s] = pog[ip_s];
    ip_s += 1;
    switch ( cod_y ) {
      default:
        for ( i_y = _n_arg(cod_y); i_y > 0; --i_y ) {
          gop[ip_s] = pog[ip_s];
          ip_s += 1;
        }
        break;

      case CUSH: case FRAG: case FLAG: case LILN: case LITN:
      case SAMN: case TICK: case KICK:
        _n_take_narg(pog, gop, 0, &ip_s);
        break;

      case SKIB: case SLIB:
        _n_take_narg(pog, gop, 1, &ip_s);
        break;

      case SKIM: case SLIM:
        _n_take_narg(pog, gop, 2, &ip_s);
        break;
    }
  }
  return gop;
}

/* _n_reap(): reap key and value from byc table.
*/
static void
_n_reap(u3_noun kev)
{
  u3_noun fol = u3h(kev);
  u3_noun got = u3t(kev);
  u3_noun lof = u3a_take(fol);
  c3_y*   pog = u3a_into(got);
  c3_y*   gop = _n_take_byc(pog);
  u3_noun tog = u3a_outa(gop);
  u3h_put(u3R->byc.har_p, lof, tog);
  u3z(lof);
}

/* u3n_beep(): promote bytecode state.
*/
void
u3n_beep(u3p(u3h_root) har_p)
{
  u3h_walk(har_p, _n_reap);
}

#if 0
/* _n_print_stack(): print out the cap stack up to a designated "empty"
 *                   used only for debugging
 */
static void _n_print_stack(u3p(u3_noun) empty) {
  c3_w cur_p = u3R->cap_p;
  fprintf(stderr, "[");
  int first = 1;
  while ( cur_p != empty ) {
    if ( first ) {
      first = 0;
    }
    else {
      fprintf(stderr, " ");
    }
    if ( c3y == u3a_is_north(u3R) ) {
      fprintf(stderr, "%u", *(u3to(u3_noun, cur_p)));
      cur_p++;
    }
    else {
      fprintf(stderr, "%u", *(u3to(u3_noun, cur_p-1)));
      cur_p--;
    }
  }
  fprintf(stderr, "]\r\n");
}
#endif
