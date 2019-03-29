/* vere/king.c
**
** the main loop of the daemon process
*/
#include <curl/curl.h>
#include <unistd.h>
#include <uv.h>
#include "all.h"
#include "vere/vere.h"

//  stash config flags for serf
//
static c3_c sag_w;

/*
::  king/client protocol:
::
|%
::  +fate: client to lord
::
+$  fate
  $%  ::  authenticate client
      ::
      [%auth p=(unit ship) q=@]
      ::  ship action
      ::
      [%wyrd p=ship q=wyrd]
      ::  daemon command
      ::
      [%doom p=doom]
  ==
::  +wyrd: ship action
::
::    Should require auth to a single relevant ship
::
+$  wyrd
  $%  :: release this pier
      ::
      ::    XX not implemented
      ::
      [%susp ~]
      ::  generate event
      ::
      ::    XX partially implemented
      ::
      [%vent p=ovum]
  ==
::  +doom: daemon command
::
::    Should require auth to the daemon itself
::
+$  doom
  $%  ::  boot
      ::
      ::  p: boot procedure
      ::  q: pill specifier
      ::  r: path to pier
      ::
      [%boot p=boot q=pill r=@t]
      ::  end the daemon
      ::
      ::    XX not implemented
      ::
      [%exit ~]
      ::  acquire a pier
      ::
      ::    XX used for restart, may not be right
      ::
      [%pier p=(unit @t)]
      ::  admin ship actions
      ::
      ::    XX not implemented
      ::
      [%root p=ship q=wyrd]
  ==
::  +boot: boot procedures
::
+$  boot
  $%  ::  mine a comet
      ::
      ::  p: optionally under a specific star
      ::
      [%come p=(unit ship)]
      ::  boot with real keys
      ::
      ::    And perform pre-boot validation, retrieve snapshot, etc.
      ::
      [%dawn p=seed]
      ::  boot with fake keys
      ::
      ::  p: identity
      ::
      [%fake p=ship]
  ==
::  +pill: boot-sequence ingredients
::
+$  pill
  %+  each
    ::  %&: complete pill (either +brass or +solid)
    ::
    ::  p: jammed pill
    ::  q: optional %into ovum overriding that of .p
    ::
    [p=@ q=(unit ovum)]
  ::  %|: incomplete pill (+ivory)
  ::
  ::    XX not implemented, needs generation of
  ::    %veer ova for install %zuse and the vanes
  ::
  ::  p: jammed pill
  ::  q: module ova
  ::  r: userspace ova
  ::
  [p=@ q=(list ovum) r=(list ovum)]
::  +cede: lord to client
::
::  XX not implemented
::
+$  cede
  $%  ::  send cards
      ::
      ::    XX presumably the effects of %vent in +wyrd
      ::
      [%cede p=ship q=(list ovum)]
      ::  accept command
      ::
      [%firm ~]
      ::  reject command
      ::
      [%deny p=@t]
  ==
--
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
    c3_free(free_p);
  }
  else {
    free_p = vod_p->nex_u;
    vod_p->nex_u = vod_p->nex_u->nex_u;
    c3_free(free_p);
  }
}

/* _king_socket_connect(): callback for new connections
*/
void
_king_socket_connect(uv_stream_t *sock, int status)
{
  u3_moor *mor_u;

  if ( u3K.cli_u == 0 ) {
    u3K.cli_u = c3_malloc(sizeof(u3_moor));
    mor_u = u3K.cli_u;
    mor_u->vod_p = 0;
    mor_u->nex_u = 0;
  }
  else {
    for (mor_u = u3K.cli_u; mor_u->nex_u; mor_u = mor_u->nex_u);

    mor_u->nex_u = c3_malloc(sizeof(u3_moor));
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

/* _king_curl_alloc(): allocate a response buffer for curl
**  XX deduplicate with dawn.c
*/
static size_t
_king_curl_alloc(void* dat_v, size_t uni_t, size_t mem_t, uv_buf_t* buf_u)
{
  size_t siz_t = uni_t * mem_t;
  buf_u->base = c3_realloc(buf_u->base, 1 + siz_t + buf_u->len);

  memcpy(buf_u->base + buf_u->len, dat_v, siz_t);
  buf_u->len += siz_t;
  buf_u->base[buf_u->len] = 0;

  return siz_t;
}

/* _king_get_atom(): HTTP GET url_c, produce the response body as an atom.
**  XX deduplicate with dawn.c
*/
static u3_noun
_king_get_atom(c3_c* url_c)
{
  CURL *curl;
  CURLcode result;
  long cod_l;

  uv_buf_t buf_u = uv_buf_init(c3_malloc(1), 0);

  if ( !(curl = curl_easy_init()) ) {
    fprintf(stderr, "failed to initialize libcurl\n");
    exit(1);
  }

  curl_easy_setopt(curl, CURLOPT_URL, url_c);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, _king_curl_alloc);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void*)&buf_u);

  result = curl_easy_perform(curl);
  curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &cod_l);

  //  XX retry?
  //
  if ( CURLE_OK != result ) {
    fprintf(stderr, "failed to fetch %s: %s\n",
                    url_c, curl_easy_strerror(result));
    exit(1);
  }
  if ( 300 <= cod_l ) {
    fprintf(stderr, "error fetching %s: HTTP %ld\n", url_c, cod_l);
    exit(1);
  }

  curl_easy_cleanup(curl);

  return u3i_bytes(buf_u.len, (const c3_y*)buf_u.base);
}

/* _get_cmd_output(): Run a shell command and capture its output.
   Exits with an error if the command fails or produces no output.
   The 'out_c' parameter should be an array of sufficient length to hold
   the command's output, up to a max of len_c characters.
*/
static void
_get_cmd_output(c3_c *cmd_c, c3_c *out_c, c3_w len_c)
{
  FILE *fp = popen(cmd_c, "r");
  if ( NULL == fp ) {
    fprintf(stderr, "'%s' failed\n", cmd_c);
    exit(1);
  }

  if ( NULL == fgets(out_c, len_c, fp) ) {
    fprintf(stderr, "'%s' produced no output\n", cmd_c);
    exit(1);
  }

  pclose(fp);
}

/* _arvo_hash(): get a shortened hash of the last git commit
   that modified the sys/ directory in arvo.
   hax_c must be an array with length >= 11.
*/
static void
_arvo_hash(c3_c *out_c, c3_c *arv_c)
{
  c3_c cmd_c[2048];

  sprintf(cmd_c, "git -C %s log -1 HEAD --format=%%H -- sys/", arv_c);
  _get_cmd_output(cmd_c, out_c, 11);

  out_c[10] = 0;  //  end with null-byte
}

/* _git_pill_url(): produce a URL from which to download a pill
   based on the location of an arvo git repository.
*/
static void
_git_pill_url(c3_c *out_c, c3_c *arv_c)
{
  c3_c hax_c[11];

  assert(NULL != arv_c);

  if ( 0 != system("which git >> /dev/null") ) {
    fprintf(stderr, "boot: could not find git executable\r\n");
    exit(1);
  }

  _arvo_hash(hax_c, arv_c);
  sprintf(out_c, "https://bootstrap.urbit.org/git-%s.pill", hax_c);
}

/* _boothack_pill(): parse CLI pill arguments into +pill specifier
*/
static u3_noun
_boothack_pill(void)
{
  u3_noun arv = u3_nul;
  u3_noun pil;

  if ( 0 != u3_Host.ops_u.pil_c ) {
    fprintf(stderr, "boot: loading pill %s\r\n", u3_Host.ops_u.pil_c);
    pil = u3m_file(u3_Host.ops_u.pil_c);
  }
  else {
    c3_c url_c[2048];

    if ( (c3y == u3_Host.ops_u.git) &&
       (0 != u3_Host.ops_u.arv_c) )
    {
      _git_pill_url(url_c, u3_Host.ops_u.arv_c);
    }
    else {
      c3_assert( 0 != u3_Host.ops_u.url_c );
      strcpy(url_c, u3_Host.ops_u.url_c);
    }

    fprintf(stderr, "boot: downloading pill %s\r\n", url_c);
    pil = _king_get_atom(url_c);
  }

  if ( 0 != u3_Host.ops_u.arv_c ) {
    fprintf(stderr, "boot: preparing filesystem from %s\r\n",
                    u3_Host.ops_u.arv_c);
    arv = u3nc(u3_nul, u3_unix_initial_into_card(u3_Host.ops_u.arv_c));
  }

  return u3nt(c3y, pil, arv);
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

  uv_timer_init(u3L, &u3K.tim_u);

  uv_pipe_init(u3L, &u3K.cmd_u, 0);
  uv_pipe_bind(&u3K.cmd_u, u3K.soc_c);
  uv_listen((uv_stream_t *)&u3K.cmd_u, 128, _king_socket_connect);
  fprintf(stderr, "cmd socket up\r\n");

  _king_loop_init();

  uv_run(u3L, UV_RUN_DEFAULT);

  _king_loop_exit();
  exit(0);
}

/* u3_king_grab(): gc the kingdom
*/
void
u3_king_grab(void* vod_p)
{
  //  XX fix leaks and enable
  //
#if 0
  c3_w man_w = 0, pir_w = 0;
  FILE* fil_u = stderr;

  c3_assert( u3R == &(u3H->rod_u) );

  fprintf(fil_u, "measuring king:\r\n");

  man_w = u3m_mark(fil_u);
  pir_w = u3_pier_mark(fil_u);

  u3a_print_memory(fil_u, "total marked", man_w + pir_w);
  u3a_print_memory(fil_u, "sweep", u3a_sweep());
#endif
}
