/* v/reck.c
**
**  This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <dirent.h>
#include <stdint.h>
#include <uv.h>
#include <curses.h>
#include <termios.h>
#include <term.h>

#include "all.h"
#include "v/vere.h"

/* _reck_mole(): parse simple atomic mole.
*/
static u3_noun
_reck_mole(u3_noun  fot,
           u3_noun  san,
           c3_d*    ato_d)
{
  u3_noun uco = u3do("slay", san);
  u3_noun p_uco, q_uco, r_uco, s_uco;

  if ( (c3n == u3r_qual(uco, &p_uco, &q_uco, &r_uco, &s_uco)) ||
       (0 != p_uco) ||
       (0 != q_uco) ||
       (c3n == u3r_sing(fot, r_uco)) )
  {
    uL(fprintf(uH, "strange mole %s\n", u3r_string(san)));

    u3z(fot); u3z(uco); return c3n;
  }
  else {
    *ato_d = u3r_chub(0, s_uco);

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

/* _reck_spac(): print n spaces.
*/
void _reck_spac(c3_w n)
{
  for (; n > 0; n--)
    uL(fprintf(uH," "));
}

/* _reck_print_memory: print memory amount. cf u3a_print_memory().
*/
void
_reck_print_memory(c3_w wor_w)
{
  c3_w byt_w = (wor_w * 4);
  c3_w gib_w = (byt_w / 1000000000);
  c3_w mib_w = (byt_w % 1000000000) / 1000000;
  c3_w kib_w = (byt_w % 1000000) / 1000;
  c3_w bib_w = (byt_w % 1000);

  if ( gib_w ) {
    uL(fprintf(uH, "GB/%d.%03d.%03d.%03d\r\n", 
        gib_w, mib_w, kib_w, bib_w));
  }
  else if ( mib_w ) {
    uL(fprintf(uH, "MB/%d.%03d.%03d\r\n", mib_w, kib_w, bib_w));
  }
  else if ( kib_w ) {
    uL(fprintf(uH, "KB/%d.%03d\r\n", kib_w, bib_w));
  }
  else {
    uL(fprintf(uH, "B/%d\r\n", bib_w));
  }
}

/*  _reck_meme_noun(): get memory usage, in words, of noun. RETAIN.
*/
c3_w
_reck_meme_noun(u3p(u3h_root) hax, u3_noun non, c3_t dud)
{
  u3_weak got = u3h_git(hax, dud ? non & 0x7fffffff : non);

  if (u3_none != got) {
    return 0;                                           //  I think? maybe 1
  }
  else {
    c3_w res;

    if (!(non & 0x80000000)) {
      res = 1;
    }
    if (_(u3ud(non))) {
      res = 2 + u3r_met(5, non);
    }
    else {
      res = 1
            + _reck_meme_noun(hax, u3h(non), dud)
            + _reck_meme_noun(hax, u3t(non), dud);
    }

    u3h_put(hax, dud ? non & 0x7fffffff : non, res);

    return res;
  }
}

/* _reck_meme_prof(): print memory profile. RETAIN.
*/
void
_reck_meme_prof(u3p(u3h_root) hax, c3_w den, u3_noun mas)
{
  u3_noun h_mas, t_mas;

  if (c3n == u3r_cell(mas, &h_mas, &t_mas)) {
    _reck_spac(den);
    uL(fprintf(uH, "mistyped mass\r\n"));
    return;
  }
  if (c3y == h_mas) {
    _reck_spac(den);
    _reck_print_memory(_reck_meme_noun(hax, t_mas, false));
    _reck_spac(den);
    _reck_print_memory(_reck_meme_noun(hax, t_mas, true));
  }
  else if (c3n == h_mas) {
    u3_noun it_mas, tt_mas, pit_mas, qit_mas;
    while (u3_nul != t_mas)
    {
      _reck_spac(den);
      if (c3n == u3r_cell(t_mas, &it_mas, &tt_mas)) {
        uL(fprintf(uH, "mistyped mass list\r\n"));
        return;
      }
      else if (c3n == u3r_cell(it_mas, &pit_mas, &qit_mas)) {
        uL(fprintf(uH, "mistyped mass list element\r\n"));
        return;
      }
      else {
        c3_c* pit_c = u3m_pretty(pit_mas);
        uL(fprintf(uH, "%s\r\n", pit_c));
        free(pit_c);

        _reck_meme_prof(hax, den+2, qit_mas);

        t_mas = tt_mas;
      }
    }
  }
  else {
    _reck_spac(den);
    uL(fprintf(uH, "mistyped mass head\r\n"));
    return;
  }
}

/* _reck_kick_term(): apply terminal outputs.
*/
static u3_noun
_reck_kick_term(u3_noun pox, c3_l tid_l, u3_noun fav)
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

    case c3__logo:
    {
      u3_Host.liv = c3n;
      u3_Host.xit_i = u3t(fav);

      u3z(pox); u3z(fav); return c3y;
    } break;

    case c3__init: p_fav = u3t(fav);
    {
      u3A->own = u3nc(u3k(p_fav), u3A->own);

      u3_unix_ef_init(u3k(p_fav));
    
      u3_noun hox =  u3dc("scot", 'p', u3k(p_fav));
      c3_c* nam_c = u3r_string(hox);
      
      uL(fprintf(uH, "kick: init: %s\n", nam_c));
      free(nam_c); u3z(pox); u3z(hox); u3z(fav); return c3y;
    } break;

    case c3__mass: p_fav = u3t(fav);
    {
      uL(fprintf(uH, "memory profile:\r\n"));

      u3p(u3h_root) hax = u3h_new();

      _reck_meme_prof(hax, 0, p_fav);

      u3h_free(hax);

      u3z(pox); u3z(fav); return c3y;
    } break;
  }
  c3_assert(!"not reached"); return 0;
}

/* _reck_kick_http(): apply http effects.
*/
static u3_noun
_reck_kick_http(u3_noun  pox,
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

/* _reck_kick_sync(): apply sync outputs.
*/
static u3_noun
_reck_kick_sync(u3_noun pox, u3_noun fav)
{
  switch ( u3h(fav) ) {
    default: break;
    case c3__ergo: {
      u3_noun who = u3k(u3h(u3t(fav)));
      u3_noun syd = u3k(u3h(u3t(u3t(fav))));
      u3_noun rel = u3k(u3h(u3t(u3t(u3t(fav)))));
      u3_noun can = u3k(u3t(u3t(u3t(u3t(fav)))));

      u3_unix_ef_ergo(who, syd, rel, can);
      u3z(pox); u3z(fav); return c3y;
    } break;
  }

  //  XX obviously not right!
  //
  u3z(pox); u3z(fav); return c3n;
}

static u3_noun
_reck_kick_newt(u3_noun pox, u3_noun fav)
{
  switch ( u3h(fav) ) {
    default: break;
    case c3__send: {
      u3_noun lan = u3k(u3h(u3t(fav)));
      u3_noun pac = u3k(u3t(u3t(fav)));

      u3_ames_ef_send(lan, pac);
      u3z(pox); u3z(fav); return c3y;
    } break;
  }
  u3z(pox); u3z(fav); return c3n;
}

/* _reck_kick_ames(): apply packet network outputs.
*/
static u3_noun
_reck_kick_ames(u3_noun pox, u3_noun fav)
{
  u3_noun p_fav;

  switch ( u3h(fav) ) {
    default: break;
    case c3__init: p_fav = u3t(fav);
    {
      u3A->own = u3nc(u3k(p_fav), u3A->own);

      u3_unix_ef_init(u3k(p_fav));

      // uL(fprintf(uH, "kick: init: %d\n", p_fav));
      u3z(pox); u3z(fav); return c3y;
    } break;
  }
  u3z(pox); u3z(fav); return c3n;
}

/* _reck_kick_spec(): apply an effect, by path.
*/
static u3_noun
_reck_kick_spec(u3_noun pox, u3_noun fav)
{
  u3_noun i_pox, t_pox;
  u3_noun p_fav;

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
      default: return c3n;

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
        return _reck_kick_http(pox, sev_l, coq_l, seq_l, fav);
      } break;

      case c3__clay:
      case c3__sync: {
        return _reck_kick_sync(pox, fav);
      } break;

      case c3__newt: {
        return _reck_kick_newt(pox, fav);
      } break;

      case c3__ames: {
        if ( (u3_nul != tt_pox) ) {
          u3z(pox); u3z(fav); return c3n;
        }
        else {
          return _reck_kick_ames(pox, fav);
        }
      } break;

      case c3__init: p_fav = u3t(fav);
      {
        u3A->own = u3nc(u3k(p_fav), u3A->own);

        u3_unix_ef_init(u3k(p_fav));

        // uL(fprintf(uH, "kick: init: %d\n", p_fav));
        u3z(pox); u3z(fav); return c3y;
      } break;

      case c3__term: {
        u3_noun pud = tt_pox;
        u3_noun p_pud, q_pud;
        c3_l    tid_l;

        if ( (c3n == u3r_cell(pud, &p_pud, &q_pud)) ||
             (u3_nul != q_pud) ||
             (c3n == _reck_lily(c3__ud, u3k(p_pud), &tid_l)) )
        {
          uL(fprintf(uH, "term: bad tire\n"));
          u3z(pox); u3z(fav); return c3n;
        } else {
          return _reck_kick_term(pox, tid_l, fav);
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
_reck_kick_norm(u3_noun pox, u3_noun fav)
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

      //  u3_ds_wipe(u3_Wire);  //  doesn't work

      return c3y;
    }
    case c3__exit:
    {
      uL(fprintf(uH, "<<<goodbye>>>\n"));
      u3_lo_bail();

      u3z(pox); u3z(fav); return c3y;
    } break;
  }
  c3_assert(!"not reached"); return c3n;
  u3z(pox); u3z(fav); return c3n;
}

/* u3_reck_kick(): handle effect.
*/
void
u3_reck_kick(u3_noun ovo)
{
  if ( (c3n == _reck_kick_spec(u3k(u3h(ovo)), u3k(u3t(ovo)))) &&
       (c3n == _reck_kick_norm(u3k(u3h(ovo)), u3k(u3t(ovo)))) )
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
      u3v_plan(u3nt(u3_blip, c3__term, u3_nul),
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
