#include <uv.h>
#include "all.h"
#include "vere/vere.h"

/*
++  fate                                                ::  client to lord
  $%  $:  $wyrd                                         ::  send a card
          p/ship                                        ::  target ship
          q/ovum                                        ::  action
      ==                                                ::
      $:  $doom                                         ::  control the daemon
          p/(unit ship)                                 ::  who to command
          q/doom                                        ::  the command
      ==                                                ::
  ==                                                    ::
::                                                      ::
++  cede                                                ::  lord to client
  $@  $?  $firm                                         ::  accept command
          $deny                                         ::  reject command
      ==                                                ::
  $%  $:  $cede                                         ::  send cards
          p/ship                                        ::  sending ship
          q/(list ovum)                                 ::  actions
      ==                                                ::
  ==                                                    ::
::                                                      ::
++  doom                                                ::  daemon command
  $@  $?  $susp                                         ::  release a pier
          $exit                                         ::  end the daemon
      ==                                                ::
  $%  $:  $auth                                         ::  auth this client
          p/@                                           ::  auth secret
      ==                                                ::
      $:  $boot                                         ::  boot new pier
          p/@                                           ::  generator or ticket
          q/(unit @t)                                   ::  unix path to arvo
          r/(unit @t)                                   ::  unix path to pill
          s/(map @t *)                                  ::  debug options
      ==                                                ::
      $:  $pier                                         ::  acquire a pier
          p/(unit @t)                                   ::  unix path
      ==                                                ::
  ==                                                    ::
*/

void _king_defy_fate()
{
  exit(1);
}

void _king_wyrd(u3_noun wyrd)
{
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

void _king_doom(u3_noun doom)
{
  u3_noun load;
  if ( c3y == u3ud(doom) ) {
    c3_assert(_(u3a_is_cat(doom)));
    switch ( doom ) {
      case c3__susp:
        break;
      case c3__exit:
        break;
      default:
        _king_defy_fate();
    }
  } else {
    switch ( u3h(doom) ) {
      case c3__auth:
        break;
      case c3__boot:
        load = u3k(u3t(doom));
        u3z(doom);
        _king_boot(load);
        break;
      case c3__pier:
        break;
      default:
        _king_defy_fate();
    }
  }
}

void _king_fate(void *vod_p, u3_noun mat)
{
  u3_noun fate = u3ke_cue(u3k(mat));
  u3_noun load;
  c3_assert(_(u3a_is_cat(u3h(fate))));
  c3_assert(_(u3a_is_cell(u3t(fate))));
  switch ( u3h(fate) ) {
    case c3__wyrd:
      load = u3k(u3t(u3t(fate)));
      u3z(fate);
      _king_wyrd(load);
      break;
    case c3__doom:
      load = u3k(u3t(u3t(fate)));
      u3z(fate);
      _king_doom(load);
      break;
    default:
      _king_defy_fate();
  }
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
}

void _king_loop_exit()
{
  c3_l cod_l;

  cod_l = u3a_lush(c3__unix);
  u3_unix_io_exit();
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
  u3_save_io_exit();
  u3a_lop(cod_l);

  cod_l = u3a_lush(c3__behn);
  u3_behn_io_exit();
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
