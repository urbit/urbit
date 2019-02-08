/* vere/king.c
**
** the main loop of the daemon process
*/
#include <unistd.h>
#include <uv.h>
#include "all.h"
#include "vere/vere.h"

//  stash config flags for serf
//
static c3_c sag_w;

/*
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::  wyrd: requires auth to a single relevant ship       ::
::  doom: requires auth to the daemon itself            ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
|%                                                      ::
+$  fate                                                ::  client to lord
  $%  [%auth p=(unit ship) q=@]                         ::  authenticate client
      [%wyrd p=ship q=wyrd]                             ::  ship action
      [%doom p=doom]                                    ::  daemon command
  ==                                                    ::
+$  wyrd                                                ::  ship action
  $%  [%susp ~]                                         ::  release this pier
      [%vent p=ovum]                                    ::  generate event
  ==                                                    ::
+$  doom                                                ::  daemon command
  $%  [%boot p=boot q=@pill r=@t]                       ::  boot (r=pier)
      [%exit ~]                                         ::  end the daemon
      [%pier p=(unit @t)]                               ::  acquire a pier
      [%root p=ship q=wyrd]                             ::  admin ship actions
  ==                                                    ::
+$  boot                                                ::  boot procedures
  $%  [%come p=(unit ship)]                             ::  mine a comet
      [%dawn p=seed]                                    ::  real keys
      [%fake p=who]                                     ::  fake keys
  ==                                                    ::
+$  pill                                                ::  boot sequence
  (each path=@t pill=@)                                 ::
+$  cede                                                ::  lord to client
  $%  [%cede p=ship q[(list ovum)]                      ::  send cards
      [%firm ~]                                         ::  accept command
      [%deny p=@t]                                      ::  reject command
  ==                                                    ::
--                                                      ::
*/

void _king_auth(u3_noun auth);

void _king_wyrd(u3_noun ship_wyrd);
  void _king_susp(u3_atom ship, u3_noun susp);
  void _king_vent(u3_atom ship, u3_noun vent);

void _king_doom(u3_noun doom);
  void _king_boot(u3_noun boot);
    void _king_come(u3_noun star, u3_noun pill, u3_noun path);
    void _king_dawn(u3_noun seed, u3_noun pill, u3_noun path);
    void _king_fake(u3_noun ship, u3_noun pill, u3_noun path);
  void _king_exit(u3_noun exit);
  void _king_pier(u3_noun pier);
  void _king_root(u3_noun root);


/* _king_defy_fate(): invalid fate
*/
void
_king_defy_fate()
{
  exit(1);
}

/* _king_fate(): top-level fate parser
*/
void
_king_fate(void *vod_p, u3_noun mat)
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

/* _king_auth(): auth parser
*/
void
_king_auth(u3_noun auth)
{
}

/* _king_wyrd(): wyrd parser
*/
void
_king_wyrd(u3_noun ship_wyrd)
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

/* _king_susp(): susp parser
*/
void
_king_susp(u3_atom ship, u3_noun susp)
{
}

/* _king_vent(): vent parser
*/
void
_king_vent(u3_atom ship, u3_noun vent)
{
  /* stub; have to find pier from ship */
  u3z(ship);
  u3_pier_work(u3_pier_stub(), u3h(vent), u3t(vent));
  u3z(vent);
}

/* _king_doom(): doom parser
*/
void
_king_doom(u3_noun doom)
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

/* _king_boot(): boot parser
*/
void
_king_boot(u3_noun bul)
{
  u3_noun boot, pill, path;
  void (*next)(u3_noun, u3_noun, u3_noun);

  c3_assert(_(u3a_is_cell(bul)));
  u3x_trel(bul, &boot, &pill, &path);
  c3_assert(_(u3a_is_cat(u3h(boot))));

  switch ( u3h(boot) ) {
    case c3__fake: {
      next = _king_fake;
      break;
    }
    case c3__come: {
      next = _king_come;
      break;
    }
    case c3__dawn: {
      next = _king_dawn;
      break;
    }
    default:
      return _king_defy_fate();
  }

  next(u3k(u3t(boot)), u3k(pill), u3k(path));
  u3z(bul);
}

/* _king_fake(): boot with fake keys
*/
void
_king_fake(u3_noun ship, u3_noun pill, u3_noun path)
{
  u3_pier_boot(sag_w, ship, u3nc(c3__fake, u3k(ship)), pill, path);
}

/* _king_come(): mine a comet under star (unit)
**
**   XX revise to exclude star argument
*/
void
_king_come(u3_noun star, u3_noun pill, u3_noun path)
{
  _king_dawn(u3_dawn_come(), pill, path);
}

/* _king_dawn(): boot from keys, validating
*/
void
_king_dawn(u3_noun seed, u3_noun pill, u3_noun path)
{
  u3_pier_boot(sag_w, u3k(u3h(seed)), u3_dawn_vent(seed), pill, path);
}

/* _king_exit(): exit parser
*/
void
_king_exit(u3_noun exit)
{
}

/* _king_pier(): pier parser
*/
void
_king_pier(u3_noun pier)
{
  if ( (c3n == u3du(pier)) ||
       (c3n == u3ud(u3t(pier))) ) {
    u3m_p("king: invalid pier", pier);
    exit(1);
  }

  u3_pier_stay(sag_w, u3k(u3t(pier)));
  u3z(pier);
}

/* _king_root(): root parser
*/
void
_king_root(u3_noun root)
{
}

/* _king_bail(): bail for command socket newt
*/
void
_king_bail(u3_moor *vod_p, const c3_c *err_c)
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

/* _king_socket_connect(): callback for new connections
*/
void
_king_socket_connect(uv_stream_t *sock, int status)
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

/* _boothack_pill(): parse CLI pill arguments into (each path pill)
*/
static u3_noun
_boothack_pill(void)
{
  if ( 0 == u3_Host.ops_u.pil_c ) {
    //  XX download default pill
    //  XX support -u
    //
    fprintf(stderr, "boot: new ship must specify pill (-B)\r\n");
    exit(1);
  }

  return u3nc(c3y, u3i_string(u3_Host.ops_u.pil_c));
}

/* _boothack_key(): parse a private key file or value
*/
static u3_noun
_boothack_key(u3_noun kef)
{
  u3_noun seed, ship;

  {
    u3_noun des = u3dc("slaw", c3__uw, u3k(kef));

    if ( u3_nul == des ) {
      c3_c* kef_c = u3r_string(kef);
      fprintf(stderr, "dawn: invalid private keys: %s\r\n", kef_c);
      free(kef_c);
      exit(1);
    }

    //  +seed:able:jael: private key file
    //
    seed = u3ke_cue(u3k(u3t(des)));
    //  local reference, not counted
    //
    ship = u3h(seed);
    u3z(des);
    u3z(kef);
  }

  if ( 0 != u3_Host.ops_u.who_c ) {
    u3_noun woh = u3i_string(u3_Host.ops_u.who_c);
    u3_noun whu = u3dc("slaw", 'p', u3k(woh));

    if ( u3_nul == whu ) {
      fprintf(stderr, "dawn: invalid ship specificed with -w %s\r\n",
                                                 u3_Host.ops_u.who_c);
      exit(1);
    }

    if ( c3n == u3r_sing(ship, u3t(whu)) ) {
      u3_noun how = u3dc("scot", 'p', u3k(ship));
      c3_c* how_c = u3r_string(u3k(how));
      fprintf(stderr, "dawn: mismatch between -w %s and -K %s\r\n",
                                                 u3_Host.ops_u.who_c, how_c);

      u3z(how);
      free(how_c);
      exit(1);
    }

    u3z(woh);
    u3z(whu);
  }

  return seed;
}

/* _boothack_doom(): parse CLI arguments into c3__doom
*/
static u3_noun
_boothack_doom(void)
{
  u3_noun pax = u3i_string(u3_Host.dir_c);
  u3_noun bot;

  if ( c3n == u3_Host.ops_u.nuu ) {
    return u3nt(c3__pier, u3_nul, pax);
  }
  else if ( 0 != u3_Host.ops_u.fak_c ) {
    u3_noun fak = u3i_string(u3_Host.ops_u.fak_c);
    u3_noun whu = u3dc("slaw", 'p', u3k(fak));

    if ( u3_nul == whu ) {
      fprintf(stderr, "boot: malformed -F ship %s\r\n", u3_Host.ops_u.fak_c);
      exit(1);
    }

    bot = u3nc(c3__fake, u3k(u3t(whu)));

    u3z(whu);
    u3z(fak);
  }
  else if ( 0 != u3_Host.ops_u.who_c ) {
    u3_noun kef;

    if ( 0 != u3_Host.ops_u.key_c ) {
      kef = u3m_file(u3_Host.ops_u.key_c);
    }
    else if ( 0 != u3_Host.ops_u.gen_c ) {
      kef = u3i_string(u3_Host.ops_u.gen_c);
    }
    else {
      fprintf(stderr, "boot: must specify a key with -k or -G\r\n");
      exit(1);
    }

    bot = u3nc(c3__dawn, _boothack_key(kef));
  }
  else {
    //  XX allow parent star to be specified?
    //
    bot = u3nc(c3__come, u3_nul);
  }

  return u3nq(c3__boot, bot, _boothack_pill(), pax);
}

/* _boothack_cb(): callback for the boothack self-connection
**  (as if we were a client process)
*/
void
_boothack_cb(uv_connect_t *conn, int status)
{
  u3_mojo *moj_u = conn->data;

  u3_noun dom = u3nc(c3__doom, _boothack_doom());
  u3_atom mat = u3ke_jam(dom);

  u3_newt_write(moj_u, mat, 0);
}

/* _king_loop_init(): stuff that comes before the event loop
*/
void
_king_loop_init()
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

  /* boot hack */
  {
    u3_moor *mor_u = c3_malloc(sizeof(u3_moor));
    uv_connect_t *conn = c3_malloc(sizeof(uv_connect_t));
    conn->data = mor_u;
    uv_pipe_init(u3L, &mor_u->pyp_u, 0);
    uv_pipe_connect(conn, &mor_u->pyp_u, u3K.soc_c, _boothack_cb);
  }
}

/* _king_loop_exit(): cleanup after event loop
*/
void
_king_loop_exit()
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

  unlink(u3K.soc_c);
}

/* u3_king_commence(): start the daemon
*/
void
u3_king_commence()
{
  u3_Host.lup_u = uv_default_loop();

  //  start up a "fast-compile" arvo for internal use only
  //  (with hashboard always disabled)
  //
  sag_w = u3C.wag_w;
  u3C.wag_w |= u3o_hashless;

  u3m_boot_pier();
  {
    u3_noun lit;

    if ( 0 != u3_Host.ops_u.lit_c ) {
      lit = u3m_file(u3_Host.ops_u.lit_c);
    }
    else {
      extern c3_w u3_Ivory_length_w;
      extern c3_y u3_Ivory_pill_y[];

      lit = u3i_bytes(u3_Ivory_length_w, u3_Ivory_pill_y);
    }

    u3v_boot_lite(lit);
  }

  /* listen on command socket
  */
  {
    c3_c buf_c[256];

    sprintf(buf_c, "/tmp/urbit-sock-%d", getpid());
    u3K.soc_c = strdup(buf_c);
  }

  uv_pipe_init(u3L, &u3K.cmd_u, 0);
  uv_pipe_bind(&u3K.cmd_u, u3K.soc_c);
  uv_listen((uv_stream_t *)&u3K.cmd_u, 128, _king_socket_connect);
  fprintf(stderr, "cmd socket up\r\n");

  _king_loop_init();

  uv_run(u3L, UV_RUN_DEFAULT);

  _king_loop_exit();
  exit(0);
}
