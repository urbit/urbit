/* vere/reck.c
**
*/
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <dirent.h>
#include <uv.h>
#include <ncurses/curses.h>
#include <termios.h>
#include <ncurses/term.h>

#include "all.h"
#include "vere/vere.h"

/* _reck_mole(): parse simple atomic mole.
*/
static u3_noun
_reck_mole(u3_noun  fot,
           u3_noun  san,
           c3_d*    ato_d)
{
  u3_noun uco = u3dc("slaw", fot, san);
  u3_noun p_uco, q_uco;

  if ( (c3n == u3r_cell(uco, &p_uco, &q_uco)) ||
       (u3_nul != p_uco) )
  {
    uL(fprintf(uH, "strange mole %s\n", u3r_string(san)));

    u3z(fot); u3z(uco); return c3n;
  }
  else {
    *ato_d = u3r_chub(0, q_uco);

    u3z(fot); u3z(uco); return c3y;
  }
}

/* _reck_lily(): parse little atom.
*/
static u3_noun
_reck_lily(u3_noun fot, u3_noun txt, c3_l* tid_l)
{
  c3_d ato_d;

  if ( c3n == _reck_mole(fot, txt, &ato_d) ) {
    return c3n;
  } else {
    if ( ato_d >= 0x80000000ULL ) {
      return c3n;
    } else {
      *tid_l = (c3_l) ato_d;

      return c3y;
    }
  }
}

/*  _reck_orchid(): parses only a number as text
 *
 *    Parses a text string which contains a decimal number. In practice, this
 *    number is always '1'.
 */
static u3_noun
_reck_orchid(u3_noun fot, u3_noun txt, c3_l* tid_l)
{
  c3_c* str = u3r_string(txt);
  c3_d ato_d = strtol(str, NULL, 10);
  free(str);

  if ( ato_d >= 0x80000000ULL ) {
    return c3n;
  } else {
    *tid_l = (c3_l) ato_d;

    return c3y;
  }
}

/* _reck_kick_term(): apply terminal outputs.
*/
static u3_noun
_reck_kick_term(u3_pier* pir_u, u3_noun pox, c3_l tid_l, u3_noun fav)
{
  u3_noun p_fav;

  if ( c3n == u3du(fav) ) {
    u3z(pox); u3z(fav); return c3n;
  }
  else switch ( u3h(fav) ) {
    default: u3z(pox); u3z(fav); return c3n;
    case c3__bbye:
    {
      u3z(pox); u3z(fav); return c3y;
    } break;

    case c3__blit: p_fav = u3t(fav);
    {
      u3_term_ef_blit(tid_l, u3k(p_fav));

      u3z(pox); u3z(fav); return c3y;
    } break;

    // this can return through dill due to our fscked up boot sequence
    //
    case c3__send: {
      u3_noun lan = u3k(u3h(u3t(fav)));
      u3_noun pac = u3k(u3t(u3t(fav)));

      fprintf(stderr, "kick: strange send\r\n");
      u3_ames_ef_send(pir_u, lan, pac);
      u3z(pox); u3z(fav); return c3y;
    } break;

    case c3__logo:
    {
      u3_pier_exit();
      u3_Host.xit_i = u3t(fav);

      u3z(pox); u3z(fav); return c3y;
    } break;

    case c3__init: p_fav = u3t(fav);
    {
      // king ignores %init
      // u3A->own = u3nc(u3k(p_fav), u3A->own);
      // uL(fprintf(uH, "kick: init: %d\n", p_fav));
      u3z(pox); u3z(fav); return c3y;
    } break;

    case c3__mass: p_fav = u3t(fav);
    {
      u3z(pox); u3z(fav);

      //  gc the kingdom
      //
      uv_timer_start(&u3K.tim_u, (uv_timer_cb)u3_king_grab, 0, 0);
      return c3y;
    } break;
  }
  c3_assert(!"not reached"); return 0;
}

/* _reck_kick_http(): apply http effects.
*/
static u3_noun
_reck_kick_http(u3_pier* pir_u,
                u3_noun  pox,
                c3_l     sev_l,
                c3_l     coq_l,
                c3_l     seq_l,
                u3_noun  fav)
{
  u3_noun p_fav, q_fav;

  if ( c3n == u3du(fav) ) {
    u3z(pox); u3z(fav); return c3n;
  }
  else switch ( u3h(fav) ) {
    default: u3z(pox); u3z(fav); return c3n;

    case c3__form: p_fav = u3t(fav);
    {
      u3_http_ef_form(u3k(p_fav));

      // The control server has now started.
      //
      // If we're in daemon mode, we need to inform the parent process
      // that we've finished booting.
      if (u3_Host.bot_f) {
        u3_Host.bot_f();
      }

      u3z(pox); u3z(fav);
      return c3y;
    }

    case c3__that: p_fav = u3t(fav);
    {
      u3_http_ef_that(u3k(p_fav));

      u3z(pox); u3z(fav);
      return c3y;
    }

    case c3__thus: p_fav = u3h(u3t(fav)); q_fav = u3t(u3t(fav));
    {
      u3_cttp_ef_thus(u3r_word(0, p_fav), u3k(q_fav));

      u3z(pox); u3z(fav);
      return c3y;
    }
    case c3__thou: p_fav = u3t(fav);
    {
      u3_http_ef_thou(sev_l, coq_l, seq_l, u3k(p_fav));

      u3z(pox); u3z(fav);
      return c3y;
    } break;
  }
  c3_assert(!"not reached"); return c3n;
}

/* _reck_kick_behn(): apply packet network outputs.
*/
static u3_noun
_reck_kick_behn(u3_pier* pir_u, u3_noun pox, u3_noun fav)
{
  switch ( u3h(fav) ) {
    default: break;

    case c3__doze: {
      u3_behn_ef_doze(pir_u, u3k(u3t(fav)));
      u3z(pox); u3z(fav); return c3y;
    } break;
  }
  u3z(pox); u3z(fav); return c3n;
}

/* _reck_kick_sync(): apply sync outputs.
*/
static u3_noun
_reck_kick_sync(u3_pier* pir_u, u3_noun pox, u3_noun fav)
{
  switch ( u3h(fav) ) {
    default: break;
    case c3__dirk: {
      u3_unix_ef_dirk(pir_u, u3k(u3t(fav)));
      u3z(pox); u3z(fav); return c3y;
    }
    case c3__ergo: {
      u3_noun mon = u3k(u3h(u3t(fav)));
      u3_noun can = u3k(u3t(u3t(fav)));

      u3_unix_ef_ergo(pir_u, mon, can);
      u3z(pox); u3z(fav); return c3y;
    } break;
    case c3__ogre: {
      u3_unix_ef_ogre(pir_u, u3k(u3t(fav)));
      u3z(pox); u3z(fav); return c3y;
    }
    case c3__hill: {
      u3_unix_ef_hill(pir_u, u3k(u3t(fav)));
      u3z(pox); u3z(fav); return c3y;
    }
  }

  //  XX obviously not right!
  //  ? looks fine to me
  u3z(pox); u3z(fav); return c3n;
}

/* _reck_kick_newt(): apply packet network outputs.
*/
static u3_noun
_reck_kick_newt(u3_pier* pir_u, u3_noun pox, u3_noun fav)
{
  switch ( u3h(fav) ) {
    default: break;
    case c3__send: {
      u3_noun lan = u3k(u3h(u3t(fav)));
      u3_noun pac = u3k(u3t(u3t(fav)));

      u3_ames_ef_send(pir_u, lan, pac);
      u3z(pox); u3z(fav); return c3y;
    } break;

    case c3__turf: {
      u3_ames_ef_turf(pir_u, u3k(u3t(fav)));
      u3z(pox); u3z(fav); return c3y;
    } break;

  }
  u3z(pox); u3z(fav); return c3n;
}

/* _reck_kick_ames(): apply packet network outputs.
*/
static u3_noun
_reck_kick_ames(u3_pier* pir_u, u3_noun pox, u3_noun fav)
{
  u3_noun p_fav;

  switch ( u3h(fav) ) {
    default: break;
    case c3__init: p_fav = u3t(fav);
    {
      // king ignores %init
      // u3A->own = u3nc(u3k(p_fav), u3A->own);
      // uL(fprintf(uH, "kick: init: %d\n", p_fav));
      u3z(pox); u3z(fav); return c3y;
    } break;
  }
  u3z(pox); u3z(fav); return c3n;
}

/* _reck_kick_spec(): apply an effect, by path.
*/
static u3_noun
_reck_kick_spec(u3_pier* pir_u, u3_noun pox, u3_noun fav)
{
  u3_noun i_pox, t_pox;

  if ( (c3n == u3r_cell(pox, &i_pox, &t_pox)) ||
       ((i_pox != u3_blip) &&
        (i_pox != c3__gold) &&
        (i_pox != c3__iron) &&
        (i_pox != c3__lead)) )
  {
    u3z(pox); u3z(fav); return c3n;
  } else {
    u3_noun it_pox, tt_pox;

    if ( (c3n == u3r_cell(t_pox, &it_pox, &tt_pox)) ) {
      u3z(pox); u3z(fav); return c3n;
    }
    else switch ( it_pox ) {
      default: u3z(pox); u3z(fav); return c3n;

      case c3__http: {
        u3_noun pud = tt_pox;
        u3_noun p_pud, t_pud, tt_pud, q_pud, r_pud, s_pud;
        c3_l    sev_l, coq_l, seq_l;

        if ( (c3n == u3r_cell(pud, &p_pud, &t_pud)) ||
             (c3n == _reck_lily(c3__uv, u3k(p_pud), &sev_l)) )
        {
          u3z(pox); u3z(fav); return c3n;
        }

        if ( u3_nul == t_pud ) {
          coq_l = seq_l = 0;
        }
        else {
          if ( (c3n == u3r_cell(t_pud, &q_pud, &tt_pud)) ||
               (c3n == _reck_lily(c3__ud, u3k(q_pud), &coq_l)) )
          {
            u3z(pox); u3z(fav); return c3n;
          }

          if ( u3_nul == tt_pud ) {
            seq_l = 0;
          } else {
            if ( (c3n == u3r_cell(tt_pud, &r_pud, &s_pud)) ||
                 (u3_nul != s_pud) ||
                 (c3n == _reck_lily(c3__ud, u3k(r_pud), &seq_l)) )
            {
              u3z(pox); u3z(fav); return c3n;
            }
          }
        }
        return _reck_kick_http(pir_u, pox, sev_l, coq_l, seq_l, fav);
      } break;

      case c3__behn: {
        return _reck_kick_behn(pir_u, pox, fav);
      } break;

      case c3__clay:
      case c3__boat:
      case c3__sync: {
        return _reck_kick_sync(pir_u, pox, fav);
      } break;

      case c3__newt: {
        return _reck_kick_newt(pir_u, pox, fav);
      } break;

      case c3__ames: {
        if ( (u3_nul != tt_pox) ) {
          u3z(pox); u3z(fav); return c3n;
        }
        else {
          return _reck_kick_ames(pir_u, pox, fav);
        }
      } break;

      case c3__init: {
        // king ignores %init
        // p_fav = u3t(fav);
        // u3A->own = u3nc(u3k(p_fav), u3A->own);
        // uL(fprintf(uH, "kick: init: %d\n", p_fav));
        u3z(pox); u3z(fav); return c3y;
      } break;

      case c3__term: {
        u3_noun pud = tt_pox;
        u3_noun p_pud, q_pud;
        c3_l    tid_l;

        if ( (c3n == u3r_cell(pud, &p_pud, &q_pud)) ||
             (u3_nul != q_pud) ||
             (c3n == _reck_orchid(c3__ud, u3k(p_pud), &tid_l)) )
        {
          uL(fprintf(uH, "term: bad tire\n"));
          u3z(pox); u3z(fav); return c3n;
        } else {
          return _reck_kick_term(pir_u, pox, tid_l, fav);
        }
      } break;
    }
  }
  c3_assert(!"not reached");
  return c3n;
}

/* _reck_kick_norm(): non path-specific effect handling.
*/
static u3_noun
_reck_kick_norm(u3_pier* pir_u, u3_noun pox, u3_noun fav)
{
  if ( c3n == u3du(fav) ) {
    u3z(pox); u3z(fav); return c3n;
  }
  else switch ( u3h(fav) ) {
    default: u3z(pox); u3z(fav); return c3n;

    case c3__vega:
    {
      uL(fprintf(uH, "<<<reset>>>\n"));
      u3z(pox); u3z(fav);

      //  reclaim memory from persistent caches
      //
      u3m_reclaim();

      return c3y;
    }
    case c3__exit:
    {
      uL(fprintf(uH, "<<<goodbye>>>\n"));
      u3_pier_exit();

      u3z(pox); u3z(fav); return c3y;
    } break;
  }
  c3_assert(!"not reached"); return c3n;
  u3z(pox); u3z(fav); return c3n;
}

/* u3_reck_kick(): handle effect.
*/
void
u3_reck_kick(u3_pier* pir_u, u3_noun ovo)
{
  if ( (c3n == _reck_kick_spec(pir_u, u3k(u3h(ovo)), u3k(u3t(ovo)))) &&
       (c3n == _reck_kick_norm(pir_u, u3k(u3h(ovo)), u3k(u3t(ovo)))) )
  {
#if 0
    if ( (c3__warn != u3h(u3t(ovo))) &&
         (c3__text != u3h(u3t(ovo))) &&
         (c3__note != u3h(u3t(ovo))) )
#endif
#if 1
    if ( (c3__crud == u3h(u3t(ovo))) )
#if 0
         (c3__talk == u3h(u3t(ovo))) ||
         (c3__helo == u3h(u3t(ovo))) ||
         (c3__init == u3h(u3t(ovo))) )
#endif
    {
      u3_pier_work(pir_u,
                   u3nt(u3_blip, c3__term, u3_nul),
                   u3nc(c3__flog, u3k(u3t(ovo))));
    }
    else {
      u3_noun tox = u3do("spat", u3k(u3h(ovo)));
      uL(fprintf(uH, "kick: lost %%%s on %s\n",
                     u3r_string(u3h(u3t(ovo))),
                     u3r_string(tox)));
      u3z(tox);
#if 0
      if ( c3__hear == u3h(u3t(ovo)) ) {
        c3_assert(0);
      }
#endif
    }
#endif
  }
  u3z(ovo);
}
