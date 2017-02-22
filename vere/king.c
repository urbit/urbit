#include <uv.h>
#include "all.h"
#include "vere/vere.h"

/*
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::  wyrd: requires auth to a single relevant ship       ::
::  doom: requires auth to the daemon itself            ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  fate                                                ::  client to lord
  $%  $:  $auth                                         ::  authenticate client
          p/(unit ship)                                 ::  what to auth
          q/@                                           ::  auth secret
      ==                                                ::
  $%  $:  $wyrd                                         ::  ship action
          p/ship                                        ::  which ship
          q/wyrd                                        ::  the action
      ==                                                ::
      $:  $doom                                         ::  daemon command
          p/doom                                        ::  the command
      ==                                                ::
  ==                                                    ::
::                                                      ::
++  wyrd                                                ::  ship action
  $%  $:  $susp                                         ::  release this pier
          $~                                            ::
      ==                                                ::
      $:  $vent                                         ::  generate event
          p/ovum                                        ::  wire and card
      ==                                                ::
  ==                                                    ::
::                                                      ::
++  doom                                                ::  daemon command
  $%  $:  $boot                                         ::  boot new pier
          p/ship                                        ::  ship
          q/@                                           ::  generator or ticket
          r/(map @t *)                                  ::  debug options
      ==                                                ::
      $:  $exit                                         ::  end the daemon
          $~                                            ::
      ==                                                ::
      $:  $pier                                         ::  acquire a pier
          p/(unit @t)                                   ::  unix path
      ==                                                ::
      $:  $root                                         ::  admin ship actions
          p/ship                                        ::  which ship
          q/wyrd                                        ::  the action
  ==                                                    ::
++  cede                                                ::  lord to client
  $%  $:  $cede                                         ::  send cards
          p/ship                                        ::  sending ship
          q/(list ovum)                                 ::  actions
      ==                                                ::
      $:  $firm                                         ::  accept command
          $~                                            ::
      ==                                                ::
      $:  $deny                                         ::  reject command
          p/@t                                          ::  error message
      ==                                                ::
  ==                                                    ::
::                                                      ::
*/

void _king_auth(u3_noun auth);

void _king_wyrd(u3_noun ship_wyrd);
  void _king_susp(u3_atom ship, u3_noun susp);
  void _king_vent(u3_atom ship, u3_noun vent);

void _king_doom(u3_noun doom);
  void _king_boot(u3_noun boot);
  void _king_exit(u3_noun exit);
  void _king_pier(u3_noun pier);
  void _king_root(u3_noun root);

void _king_defy_fate()
{
  exit(1);
}

void _king_fate(void *vod_p, u3_noun mat)
{
  u3_noun fate = u3ke_cue(u3k(mat));
  u3_noun load;
  void (*next)(u3_noun);

  c3_assert(_(u3a_is_cell(fate)));
  c3_assert(_(u3a_is_cat(u3h(fate))));

  switch ( u3h(fate) ) {
    case c3__auth:
      next = _king_auth;
      break;
    case c3__wyrd:
      next = _king_wyrd;
      break;
    case c3__doom:
      next = _king_doom;
      break;
    default:
      _king_defy_fate();
  }

  load = u3k(u3t(fate));
  u3z(fate);
  next(load);
}

void _king_auth(u3_noun auth)
{
}

void _king_wyrd(u3_noun ship_wyrd)
{
  u3_atom ship;
  u3_noun wyrd;
  u3_noun load;
  void (*next)(u3_atom, u3_noun);

  c3_assert(_(u3a_is_cell(ship_wyrd)));
  c3_assert(_(u3a_is_atom(u3h(ship_wyrd))));
  ship = u3k(u3h(ship_wyrd));
  wyrd = u3k(u3t(ship_wyrd));
  u3z(ship_wyrd);

  c3_assert(_(u3a_is_cell(wyrd)));
  c3_assert(_(u3a_is_cat(u3h(wyrd))));

  switch ( u3h(wyrd) ) {
    case c3__susp:
      next = _king_susp;
      break;
    case c3__vent:
      next = _king_vent;
      break;
    default:
      _king_defy_fate();
  }

  load = u3k(u3t(wyrd));
  u3z(wyrd);
  next(ship, load);
}

void _king_susp(u3_atom ship, u3_noun susp)
{
}

void _king_vent(u3_atom ship, u3_noun vent)
{
  /* stub; have to find pier from ship */
  u3z(ship);
  u3_pier_work(u3_pier_stub(), u3h(vent), u3t(vent));
  u3z(vent);
}

void _king_doom(u3_noun doom)
{
  u3_noun load;
  void (*next)(u3_noun);

  c3_assert(_(u3a_is_cell(doom)));
  c3_assert(_(u3a_is_cat(u3h(doom))));

  switch ( u3h(doom) ) {
    case c3__boot:
      next = _king_boot;
      break;
    case c3__exit:
      next = _king_exit;
      break;
    case c3__pier:
      next = _king_pier;
      break;
    case c3__root:
      next = _king_root;
      break;
    default:
      _king_defy_fate();
  }

  load = u3k(u3t(doom));
  u3z(doom);
  next(load);
}

void _king_boot(u3_noun boot)
{
  u3_noun pax_n, sys_n;
  c3_c *pax_c, *sys_c;
  uv_prepare_t *pep_u = u3a_malloc(sizeof(uv_prepare_t)); /* put in u3_pier? */

  pax_n = u3k(u3h(u3t(boot)));
  sys_n = u3k(u3h(u3t(u3t(boot))));
  u3z(boot);

  pax_c = u3r_string(pax_n);
  u3z(pax_n);
  sys_c = u3r_string(sys_n);
  u3z(sys_n);

  fprintf(stderr, "boot %s %s\r\n", pax_c, sys_c);
  u3_pier_boot(pax_c, sys_c, pep_u);
}

void _king_exit(u3_noun exit)
{
}

void _king_pier(u3_noun pier)
{
}

void _king_root(u3_noun root)
{
}

void _king_bail(u3_moor *vod_p, const c3_c *err_c)
{
  u3_moor *free_p;
  fprintf(stderr, "_king_bail: %s\r\n", err_c);
  if ( vod_p == 0 ) {
    free_p = u3K.cli_u;
    u3K.cli_u = u3K.cli_u->nex_u;
    u3a_free(free_p);
  } else {
    free_p = vod_p->nex_u;
    vod_p->nex_u = vod_p->nex_u->nex_u;
    u3a_free(free_p);
  }
}

void _king_socket_connect(uv_stream_t *sock, int status)
{
  u3_moor *mor_u;
  if ( u3K.cli_u == 0 ) {
    u3K.cli_u = u3a_malloc(sizeof(u3_moor));
    mor_u = u3K.cli_u;
    mor_u->vod_p = 0;
    mor_u->nex_u = 0;
  } else {
    for (mor_u = u3K.cli_u; mor_u->nex_u; mor_u = mor_u->nex_u);
    mor_u->nex_u = u3a_malloc(sizeof(u3_moor));
    mor_u->nex_u->vod_p = mor_u;
    mor_u = mor_u->nex_u;
    mor_u->nex_u = 0;
  }

  uv_pipe_init(u3L, &mor_u->pyp_u, 0);
  mor_u->pok_f = _king_fate;
  mor_u->bal_f = (u3_bail)_king_bail;

  uv_accept(sock, (uv_stream_t *)&mor_u->pyp_u);
  u3_newt_read((u3_moat *)mor_u);
}

void _king_loop_init()
{
  /* move signals out of unix.c */
  {
    u3_usig* sig_u;

    sig_u = c3_malloc(sizeof(u3_usig));
    uv_signal_init(u3L, &sig_u->sil_u);

    sig_u->num_i = SIGTERM;
    sig_u->nex_u = u3_Host.sig_u;
    u3_Host.sig_u = sig_u;
  }
  {
    u3_usig* sig_u;

    sig_u = c3_malloc(sizeof(u3_usig));
    uv_signal_init(u3L, &sig_u->sil_u);

    sig_u->num_i = SIGINT;
    sig_u->nex_u = u3_Host.sig_u;
    u3_Host.sig_u = sig_u;
  }
  {
    u3_usig* sig_u;

    sig_u = c3_malloc(sizeof(u3_usig));
    uv_signal_init(u3L, &sig_u->sil_u);

    sig_u->num_i = SIGWINCH;
    sig_u->nex_u = u3_Host.sig_u;
    u3_Host.sig_u = sig_u;
  }
}

void _king_loop_exit()
{
  /*  all needs to move extept unlink */
  c3_l cod_l;

  cod_l = u3a_lush(c3__unix);
  u3_unix_io_exit(u3_pier_stub());
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__ames);
  u3_ames_io_exit(u3_pier_stub());
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__term);
  u3_term_io_exit();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__http);
  u3_http_io_exit();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__cttp);
  u3_cttp_io_exit();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__save);
  u3_save_io_exit(u3_pier_stub());
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__behn);
  u3_behn_io_exit(u3_pier_stub());
  u3a_lop(cod_l);

  unlink("/tmp/urbit.sock");
}

void u3_king_commence()
{
  u3_Host.lup_u = uv_default_loop();

  /* start up a "fast-compile" arvo for internal use only
  */
  u3m_boot_pier();
  {
    extern c3_w u3_Ivory_length_w;
    extern c3_y u3_Ivory_pill_y[];
    u3_noun     lit;

    lit = u3i_bytes(u3_Ivory_length_w, u3_Ivory_pill_y);
    u3v_boot_lite(lit);
  }

  /* listen on command socket */
  uv_pipe_init(u3L, &u3K.cmd_u, 0);
  uv_pipe_bind(&u3K.cmd_u, "/tmp/urbit.sock");
  uv_listen((uv_stream_t *)&u3K.cmd_u, 128, _king_socket_connect);
  fprintf(stderr, "cmd socket up\r\n");

  _king_loop_init();

  uv_run(u3L, UV_RUN_DEFAULT);

  _king_loop_exit();
  exit(0);
}
