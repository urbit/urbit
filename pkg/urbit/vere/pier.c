/* vere/pier.c
*/
#include <ent.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <sigsegv.h>
#include <stdint.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <uv.h>
#include <errno.h>

#include "all.h"
#include "vere/vere.h"

#define PIER_WORK_BATCH 10ULL

#undef VERBOSE_PIER

/* _pier_peek_plan(): add a u3_pico to the peek queue
*/
static void
_pier_peek_plan(u3_pier* pir_u, u3_pico* pic_u)
{
  if (!pir_u->pec_u.ent_u) {
    c3_assert( !pir_u->pec_u.ext_u );
    pir_u->pec_u.ent_u = pir_u->pec_u.ext_u = pic_u;
  }
  else {
    pir_u->pec_u.ent_u->nex_u = pic_u;
    pir_u->pec_u.ent_u = pic_u;
  }

  u3_pier_spin(pir_u);
}

/* _pier_peek_next(): pop u3_pico off of peek queue
*/
static u3_pico*
_pier_peek_next(u3_pier* pir_u)
{
  u3_pico* pic_u = pir_u->pec_u.ext_u;

  if (pic_u) {
    pir_u->pec_u.ext_u = pic_u->nex_u;
    if (!pir_u->pec_u.ext_u) {
      pir_u->pec_u.ent_u = 0;
    }

    pic_u->nex_u = 0;
  }

  return pic_u;
}

/* _pier_work_send(): send new events for processing
*/
static void
_pier_work_send(u3_work* wok_u)
{
  u3_auto* car_u = wok_u->car_u;
  u3_pier* pir_u = wok_u->pir_u;
  u3_lord* god_u = pir_u->god_u;
  c3_w     len_w = 0;

  //  calculate work batch size
  {
    //  XX work depth, or full lord send-stack depth?
    //
    if ( PIER_WORK_BATCH > god_u->dep_w ) {
      len_w = PIER_WORK_BATCH - god_u->dep_w;
    }
  }

  //  send batch
  //
  {
    u3_ovum* egg_u;
    u3_pico* pic_u;
    u3_noun ovo;
    // u3_noun    ovo, now, bit = u3qc_bex(48);

    // {
    //   struct timeval tim_tv;
    //   gettimeofday(&tim_tv, 0);
    //   now = u3_time_in_tv(&tim_tv);
    // }

    while ( len_w && car_u && (egg_u = u3_auto_next(car_u, &ovo)) ) {
      len_w--;
      u3_lord_work(god_u, egg_u, ovo);
      // u3_lord_work(god_u, egg_u, u3nc(u3k(now), ovo));
      // now = u3ka_add(now, u3k(bit));

      //  queue events depth first
      //
      car_u = egg_u->car_u;

      //  interleave scry requests
      //
      if ( len_w && (pic_u = _pier_peek_next(pir_u)) )
      {
        len_w--;
        u3_lord_peek(god_u, pic_u);
        u3_pico_free(pic_u);
      }
    }

    //  if there's room left in the batch, fill it up with remaining scries
    //
    while ( len_w-- && (pic_u = _pier_peek_next(pir_u)) )
    {
      u3_lord_peek(god_u, pic_u);
      u3_pico_free(pic_u);
    }

    // u3z(now); u3z(bit);
  }
}

/* _pier_work(): advance event processing.
*/
static void
_pier_work(u3_work* wok_u)
{
  u3_pier* pir_u = wok_u->pir_u;

  if ( c3n == pir_u->liv_o ) {
    pir_u->liv_o = u3_auto_live(wok_u->car_u);

    //  all i/o drivers are fully initialized
    //
    if ( c3y == pir_u->liv_o ) {
      //  XX this is when "boot" is actually complete
      //  XX even better would be after neighboring with our sponsor
      //
      u3l_log("pier (%" PRIu64 "): live\r\n", pir_u->god_u->eve_d);

      //  XX move callbacking to king
      //
      if ( u3_Host.bot_f ) {
        u3_Host.bot_f();
      }
    }
  }

  if ( u3_psat_work == pir_u->sat_e ) {
    _pier_work_send(wok_u);
  }
  else {
    c3_assert( u3_psat_done == pir_u->sat_e );
  }
}

/* _pier_on_lord_work_spin(): start spinner
*/
static void
_pier_on_lord_work_spin(void* ptr_v, u3_atom pin, c3_o del_o)
{
  u3_pier* pir_u = ptr_v;

  c3_assert(  (u3_psat_wyrd == pir_u->sat_e)
           || (u3_psat_work == pir_u->sat_e)
           || (u3_psat_done == pir_u->sat_e) );

  u3_term_start_spinner(pin, del_o);
}

/* _pier_on_lord_work_spin(): stop spinner
*/
static void
_pier_on_lord_work_spun(void* ptr_v)
{
  u3_pier* pir_u = ptr_v;

  c3_assert(  (u3_psat_wyrd == pir_u->sat_e)
           || (u3_psat_work == pir_u->sat_e)
           || (u3_psat_done == pir_u->sat_e) );

  u3_term_stop_spinner();
}

/* _pier_on_lord_work_done(): event completion from worker.
*/
static void
_pier_on_lord_work_done(void*    ptr_v,
                        u3_ovum* egg_u,
                        u3_noun    act)
{
  u3_pier* pir_u = ptr_v;

  c3_assert(  (u3_psat_work == pir_u->sat_e)
           || (u3_psat_done == pir_u->sat_e) );

#ifdef VERBOSE_PIER
  fprintf(stderr, "pier (%" PRIu64 "): work: done\r\n", tac_u->eve_d);
#endif

  u3_auto_done(egg_u);

  u3_auto_kick(pir_u->wok_u->car_u, act);

  _pier_work(pir_u->wok_u);
}

/* _pier_on_lord_work_bail(): event failure from worker.
*/
static void
_pier_on_lord_work_bail(void* ptr_v, u3_ovum* egg_u, u3_noun lud)
{
  u3_pier* pir_u = ptr_v;

#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: work: bail\r\n");
#endif

  c3_assert(  (u3_psat_work == pir_u->sat_e)
           || (u3_psat_done == pir_u->sat_e) );

  u3_auto_bail(egg_u, lud);

  //  XX groace
  //
  if ( pir_u->wok_u ) {
    _pier_work(pir_u->wok_u);
  }
}

/* _pier_work_time(): set time.
*/
static void
_pier_work_time(u3_pier* pir_u)
{
  struct timeval tim_tv;
  gettimeofday(&tim_tv, 0);

  // XX save to pier
  //
  u3v_time(u3_time_in_tv(&tim_tv));
}

/* _pier_work_fore_cb(): run on every loop iteration before i/o polling.
*/
static void
_pier_work_fore_cb(uv_prepare_t* pep_u)
{
  u3_work* wok_u = pep_u->data;
  _pier_work_time(wok_u->pir_u);
}

/* _pier_work_afte_cb(): run on every loop iteration after i/o polling.
*/
static void
_pier_work_afte_cb(uv_check_t* cek_u)
{
  u3_work* wok_u = cek_u->data;
  _pier_work(wok_u);
}

/* _pier_work_idle_cb(): run on next loop iteration.
*/
static void
_pier_work_idle_cb(uv_idle_t* idl_u)
{
  u3_work* wok_u = idl_u->data;
  _pier_work(wok_u);
  uv_idle_stop(idl_u);
}

/* u3_pier_spin(): (re-)activate idle handler
*/
void
u3_pier_spin(u3_pier* pir_u)
{
  //  XX return c3n instead?
  //
  if (  u3_psat_work == pir_u->sat_e
     || u3_psat_done == pir_u->sat_e )
  {
    u3_work* wok_u = pir_u->wok_u;

    if ( !uv_is_active((uv_handle_t*)&wok_u->idl_u) ) {
      uv_idle_start(&wok_u->idl_u, _pier_work_idle_cb);
    }
  }
}

/* u3_pier_peek(): read namespace.
*/
void
u3_pier_peek(u3_pier*   pir_u,
             u3_noun      gan,
             u3_noun      ful,
             void*      ptr_v,
             u3_peek_cb fun_f)
{
  u3_pico* pic_u = u3_pico_init();

  pic_u->ptr_v = ptr_v;
  pic_u->fun_f = fun_f;
  pic_u->gan   = gan;
  //
  pic_u->typ_e = u3_pico_full;
  pic_u->ful   = ful;

  _pier_peek_plan(pir_u, pic_u);
}

/* u3_pier_peek_last(): read namespace, injecting ship and case.
*/
void
u3_pier_peek_last(u3_pier*   pir_u,
                  u3_noun      gan,
                  c3_m       car_m,
                  u3_atom      des,
                  u3_noun      pax,
                  void*      ptr_v,
                  u3_peek_cb fun_f)
{
  u3_pico* pic_u = u3_pico_init();

  pic_u->ptr_v = ptr_v;
  pic_u->fun_f = fun_f;
  pic_u->gan   = gan;
  //
  pic_u->typ_e       = u3_pico_once;
  pic_u->las_u.car_m = car_m;
  pic_u->las_u.des   = des;
  pic_u->las_u.pax   = pax;

  _pier_peek_plan(pir_u, pic_u);
}

/* _pier_stab(): parse path
*/
static u3_noun
_pier_stab(u3_noun pac)
{
  return u3do("stab", pac);
}

/* _pier_on_scry_done(): scry callback.
*/
static void
_pier_on_scry_done(void* ptr_v, u3_noun nun)
{
  u3_pier* pir_u = ptr_v;
  u3_weak res = u3r_at(7, nun);

  if (u3_none == res) {
    u3l_log("pier: scry failed\n");
  }
  else {
    u3_weak out,    pad;
    c3_c   *ext_c, *pac_c;

    u3l_log("pier: scry succeeded\n");

    if ( u3_Host.ops_u.puk_c ) {
      pac_c = u3_Host.ops_u.puk_c;
    }
    else {
      pac_c = u3_Host.ops_u.pek_c;
    }

    //  try to serialize as requested
    //
    {
      u3_atom puf = u3i_string(u3_Host.ops_u.puf_c);
      if ( c3y == u3r_sing(c3__jam, puf) ) {
        out   = u3qe_jam(res);
        ext_c = "jam";
      }
      else if ( c3y == u3a_is_atom(res) ) {
        out   = u3dc("scot", u3k(puf), u3k(res));
        ext_c = "txt";
      }
      else {
        u3l_log("pier: cannot export cell as %s\n", u3_Host.ops_u.puf_c);
        out   = u3_none;
      }
      u3z(puf);
    }

    //  try to build export target path
    //
    {
      u3_noun pro = u3m_soft(0, _pier_stab, u3i_string(pac_c));
      if ( 0 == u3h(pro) ) {
        c3_w len_w = u3kb_lent(u3k(u3t(pro)));
        pad = u3nt(c3_s4('.', 'u', 'r', 'b'),
                   c3_s3('p', 'u', 't'),
                   u3qb_scag(len_w - 1, u3t(pro)));
      }
      else {
        u3l_log("pier: invalid export path %s\n", pac_c);
        pad = u3_none;
      }
      u3z(pro);
    }

    //  if serialization and export path succeeded, write to disk
    //
    if ( (u3_none != out) && (u3_none != pad) ) {
      c3_c fil_c[2048];
      snprintf(fil_c, 2048, "%s/.urb/put/%s.%s",
               pir_u->pax_c, pac_c+1, ext_c);

      u3_walk_save(fil_c, 0, out, pir_u->pax_c, pad);
      u3l_log("pier: scry result in %s\n", fil_c);
    }
  }

  u3l_log("pier: exit\n");
  u3_pier_exit(pir_u);

  u3z(nun);
}

/* _pier_work_init(): begin processing new events
*/
static void
_pier_work_init(u3_pier* pir_u)
{
  u3_work* wok_u;

  c3_assert( u3_psat_wyrd == pir_u->sat_e );

  pir_u->sat_e = u3_psat_work;
  pir_u->wok_u = wok_u = c3_calloc(sizeof(*wok_u));
  wok_u->pir_u = pir_u;

  _pier_work_time(pir_u);

  //  initialize pre i/o polling handle
  //
  uv_prepare_init(u3L, &wok_u->pep_u);
  wok_u->pep_u.data = wok_u;
  uv_prepare_start(&wok_u->pep_u, _pier_work_fore_cb);

  //  initialize post i/o polling handle
  //
  uv_check_init(u3L, &wok_u->cek_u);
  wok_u->cek_u.data = wok_u;
  uv_check_start(&wok_u->cek_u, _pier_work_afte_cb);

  //  initialize idle i/o polling handle
  //
  //    NB, not started
  //
  uv_idle_init(u3L, &wok_u->idl_u);
  wok_u->idl_u.data = wok_u;

  // //  setup u3_lord work callbacks
  // //
  // u3_lord_work_cb cb_u = {
  //   .ptr_v  = wok_u,
  //   .spin_f = _pier_on_lord_work_spin,
  //   .spun_f = _pier_on_lord_work_spun,
  //   .done_f = _pier_on_lord_work_done,
  //   .bail_f = _pier_on_lord_work_bail
  // };
  // u3_lord_work_init(pir_u->god_u, cb_u);

  //  XX this is messy, revise
  //
  if ( u3_Host.ops_u.pek_c ) {
    u3_noun pex = u3do("stab", u3i_string(u3_Host.ops_u.pek_c));
    u3_noun car;
    u3_noun dek;
    u3_noun pax;
    if ( c3n == u3r_trel(pex, &car, &dek, &pax)
      || c3n == u3a_is_cat(car) )
    {
      u3m_p("pier: invalid scry", pex);
      _pier_on_scry_done(pir_u, u3_nul);
    } else {
      //  run the requested scry, jam to disk, then exit
      //
      u3l_log("pier: scry\n");
      u3_pier_peek_last(pir_u, u3_nul, u3k(car), u3k(dek), u3k(pax),
                        pir_u, _pier_on_scry_done);
    }
    u3z(pex);
  }
  else {
    //  initialize i/o drivers
    //
    wok_u->car_u = u3_auto_init(pir_u);
    u3_auto_talk(wok_u->car_u);
  }

  _pier_work(wok_u);
}

/* _pier_wyrd_good(): %wyrd version negotation succeeded.
*/
static void
_pier_wyrd_good(u3_pier* pir_u, u3_ovum* egg_u)
{
  //  restore event callbacks
  //
  {
    u3_lord* god_u = pir_u->god_u;
    god_u->cb_u.work_done_f = _pier_on_lord_work_done;
    god_u->cb_u.work_bail_f = _pier_on_lord_work_bail;
  }

  //  initialize i/o drivers
  //
  _pier_work_init(pir_u);

  //  free %wyrd driver and ovum
  //
  {
    u3_auto* car_u = egg_u->car_u;
    u3_auto_done(egg_u);
    c3_free(car_u);
  }
}

/* _pier_wyrd_fail(): %wyrd version negotation failed.
*/
static void
_pier_wyrd_fail(u3_pier* pir_u, u3_ovum* egg_u, u3_noun lud)
{
  //  XX version negotiation failed, print upgrade message
  //
  u3l_log("pier: version negotation failed\n\n");

  //  XX only print trace with -v ?
  //
  if ( u3_nul != lud ) {
    u3_auto_bail_slog(egg_u, u3k(u3t(lud)));
  }

  u3z(lud);

  //  free %wyrd driver and ovum
  //
  {
    u3_auto* car_u = egg_u->car_u;
    u3_auto_done(egg_u);
    c3_free(car_u);
  }

  u3_pier_bail(pir_u);
}

//  XX organizing version constants
//
#define VERE_NAME  "vere"
#define VERE_ZUSE  420

/* _pier_wyrd_aver(): check for %wend effect and version downgrade. RETAIN
*/
static c3_o
_pier_wyrd_aver(u3_noun act)
{
  u3_noun fec, kel, ver;

  //    XX review, %wend re: %wyrd optional?
  //
  while ( u3_nul != act ) {
    u3x_cell(act, &fec, &act);

    if ( c3__wend == u3h(fec) ) {
      kel = u3t(fec);

      //  traverse $wynn, check for downgrades
      //
      while ( u3_nul != kel ) {
        u3x_cell(kel, &ver, &kel);

        //  check for %zuse downgrade
        //
        if (  (c3__zuse == u3h(ver))
           && (VERE_ZUSE != u3t(ver)) )
        {
          return c3n;
        }

        //  XX in the future, send %wend to serf
        //  to also negotiate downgrade of nock/hoon/&c?
        //  (we don't want to have to filter effects)
        //
      }
    }
  }

  return c3y;
}

/* _pier_on_lord_wyrd_done(): callback for successful %wyrd event.
*/
static void
_pier_on_lord_wyrd_done(void*    ptr_v,
                        u3_ovum* egg_u,
                        u3_noun    act)
{
  u3_pier* pir_u = ptr_v;

  c3_assert( u3_psat_wyrd == pir_u->sat_e );

  //  arvo's side of version negotiation succeeded
  //  traverse [gif_y] and validate
  //
  if ( c3n == _pier_wyrd_aver(act) ) {
    // u3_fact_free(tac_u);
    // u3_gift_free(gif_u);

    //  XX messaging, cli argument to bypass
    //
    u3l_log("pier: version negotiation failed; downgrade\n");
    _pier_wyrd_fail(pir_u, egg_u, u3_nul);
  }
  else {
    //  finalize %wyrd success
    //
    _pier_wyrd_good(pir_u, egg_u);

    //  XX fix
    //
    u3z(act);
  }
}

/* _pier_on_lord_wyrd_bail(): callback for failed %wyrd event.
*/
static void
_pier_on_lord_wyrd_bail(void* ptr_v, u3_ovum* egg_u, u3_noun lud)
{
  u3_pier* pir_u = ptr_v;

  c3_assert( u3_psat_wyrd == pir_u->sat_e );

  //  XX add cli argument to bypass negotiation failure
  //
#if 1
  //  print %wyrd failure and exit
  //
  //    XX check bail mote, retry on %intr, %meme, &c
  //
  _pier_wyrd_fail(pir_u, egg_u, lud);
#else
  //  XX temporary hack to fake %wyrd success
  //
  {
    _pier_wyrd_good(pir_u, egg_u);
    u3z(lud);
  }
#endif
}

/* _pier_wyrd_init(): construct %wyrd.
*/
static u3_noun
_pier_wyrd_card(u3_pier* pir_u)
{
  u3_lord* god_u = pir_u->god_u;
  u3_noun    sen;

  _pier_work_time(pir_u);

  {
    c3_l  sev_l;
    u3_noun now;
    struct timeval tim_u;
    gettimeofday(&tim_u, 0);

    now   = u3_time_in_tv(&tim_u);
    sev_l = u3r_mug(now);
    sen   = u3dc("scot", c3__uv, sev_l);

    u3z(now);
  }

  //  XX god_u not necessarily available yet, refactor call sites
  //
  u3_noun ver = u3nt(u3i_string(VERE_NAME),
                     u3dc("scot", c3__ta, u3i_string(URBIT_VERSION)),
                     u3_nul);
  u3_noun kel = u3nl(u3nc(c3__zuse, VERE_ZUSE),  //  XX from both king and serf?
                     u3nc(c3__lull, 330),        //  XX define
                     u3nc(c3__arvo, 240),        //  XX from both king and serf?
                     u3nc(c3__hoon, 140),        //  god_u->hon_y
                     u3nc(c3__nock, 4),          //  god_u->noc_y
                     u3_none);
  u3_noun wir = u3nc(c3__arvo, u3_nul);
  return u3nt(c3__wyrd, u3nc(sen, ver), kel);
}

/* _pier_wyrd_init(): send %wyrd.
*/
static void
_pier_wyrd_init(u3_pier* pir_u)
{
  u3_noun cad = _pier_wyrd_card(pir_u);
  u3_noun wir = u3nc(c3__arvo, u3_nul);

  pir_u->sat_e = u3_psat_wyrd;

  u3l_log("vere: checking version compatibility\n");

  {
    u3_lord* god_u = pir_u->god_u;
    u3_auto* car_u = c3_calloc(sizeof(*car_u));
    u3_ovum* egg_u = u3_ovum_init(0, u3_blip, wir, cad);
    u3_noun    ovo;

    car_u->pir_u = pir_u;
    car_u->nam_m = c3__wyrd;

    u3_auto_plan(car_u, egg_u);

    //  instead of subscribing with u3_auto_peer(),
    //  we swizzle the [god_u] callbacks for full control
    //
    god_u->cb_u.work_done_f = _pier_on_lord_wyrd_done;
    god_u->cb_u.work_bail_f = _pier_on_lord_wyrd_bail;

    c3_assert( u3_auto_next(car_u, &ovo) == egg_u );

    {
      // struct timeval tim_tv;
      // gettimeofday(&tim_tv, 0);
      // u3_lord_work(god_u, egg_u, u3nc(u3_time_in_tv(&tim_tv), ovo));
      u3_lord_work(god_u, egg_u, ovo);
    }
  }
}

/* _pier_on_lord_slog(): debug printf from worker.
*/
static void
_pier_on_lord_slog(void* ptr_v, c3_w pri_w, u3_noun tan)
{
  u3_pier* pir_u = ptr_v;

  if ( 0 != pir_u->sog_f ) {
    pir_u->sog_f(pir_u->sop_p, pri_w, u3k(tan));
  }

  u3_pier_tank(0, pri_w, tan);
}

/* _pier_on_lord_save(): worker (non-portable) snapshot complete.
*/
static void
_pier_on_lord_save(void* ptr_v)
{
  u3_pier* pir_u = ptr_v;

#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: (%" PRIu64 "): lord: save\r\n", pir_u->god_u->eve_d);
#endif

  // _pier_next(pir_u);
}

/* _pier_on_lord_cram(): worker state-export complete (portable snapshot).
*/
static void
_pier_on_lord_cram(void* ptr_v)
{
  u3_pier* pir_u = ptr_v;

#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: (%" PRIu64 "): lord: cram\r\n", pir_u->god_u->eve_d);
#endif

  // if ( u3_psat_done == pir_u->sat_e ) {
  //   fprintf(stderr, "snap cb exit\r\n");
  //   u3_lord_exit(pir_u->god_u);
  // }
  // else {
    // _pier_next(pir_u);
  // }
}

static void
_pier_done(u3_pier* pir_u);

/* _pier_on_lord_exit(): worker shutdown.
*/
static void
_pier_on_lord_exit(void* ptr_v)
{
  u3_pier* pir_u = ptr_v;

  //  the lord has already gone
  //
  pir_u->god_u = 0;

  if ( u3_psat_done != pir_u->sat_e ) {
    u3l_log("pier: serf shutdown unexpected\r\n");
    u3_pier_bail(pir_u);
  }
  //  if we made it all the way here, it's our jab to wrap up
  //
  else {
    _pier_done(pir_u);
  }
}

/* _pier_on_lord_bail(): worker error.
*/
static void
_pier_on_lord_bail(void* ptr_v)
{
  u3_pier* pir_u = ptr_v;

  //  the lord has already gone
  //
  pir_u->god_u = 0;

  u3_pier_bail(pir_u);
}

/* _pier_on_lord_live(): worker is ready.
*/
static void
_pier_on_lord_live(void* ptr_v)
{
  u3_pier* pir_u = ptr_v;
  _pier_wyrd_init(pir_u);
}

/* u3_pier_info(): print status info.
*/
void
u3_pier_info(u3_pier* pir_u)
{
  switch ( pir_u->sat_e ) {
    default: {
      u3l_log("pier: unknown state: %u\r\n", pir_u->sat_e);
    } break;

    case u3_psat_init: {
      u3l_log("pier: init\n");
    } break;

    case u3_psat_boot: {
      u3l_log("pier: boot\n");
    } break;

    case u3_psat_work: {
      u3l_log("pier: work\n");

      {
        u3_work* wok_u = pir_u->wok_u;

        if ( wok_u->car_u ) {
          u3_auto_info(wok_u->car_u);
        }
      }
    } break;

    case u3_psat_done: {
      u3l_log("pier: done\n");
    } break;
  }

  if ( pir_u->god_u ) {
    u3_lord_info(pir_u->god_u);
  }
}

/* _pier_init(): create a pier, loading existing.
*/
static u3_pier*
_pier_init(c3_w wag_w, c3_c* pax_c)
{
  //  create pier
  //
  u3_pier* pir_u = c3_calloc(sizeof(*pir_u));

  pir_u->pax_c = pax_c;
  pir_u->sat_e = u3_psat_init;
  pir_u->liv_o = c3n;

  // XX remove
  //
  pir_u->por_s = u3_Host.ops_u.por_s;

  //  initialize compute
  //
  {
    //  XX load/set secrets
    //
    c3_d tic_d[1];            //  ticket (unstretched)
    c3_d sec_d[1];            //  generator (unstretched)
    c3_d key_d[4];            //  secret (stretched)

    key_d[0] = key_d[1] = key_d[2] = key_d[3] = 0;

    u3_lord_cb cb_u = {
      .ptr_v = pir_u,
      .live_f = _pier_on_lord_live,
      .spin_f = _pier_on_lord_work_spin,
      .spun_f = _pier_on_lord_work_spun,
      .slog_f = _pier_on_lord_slog,
      .work_done_f = _pier_on_lord_work_done,
      .work_bail_f = _pier_on_lord_work_bail,
      .save_f = _pier_on_lord_save,
      .cram_f = _pier_on_lord_cram,
      .bail_f = _pier_on_lord_bail,
      .exit_f = _pier_on_lord_exit
    };

    if ( !(pir_u->god_u = u3_lord_init(pax_c, wag_w, key_d, cb_u)) )
    {
      c3_free(pir_u);
      return 0;
    }
  }

  return pir_u;
}

/* u3_pier_stay(): restart an existing pier.
*/
u3_pier*
u3_pier_stay(c3_w wag_w, u3_noun pax)
{
  u3_pier* pir_u;

  if ( !(pir_u = _pier_init(wag_w, u3r_string(pax))) ) {
    fprintf(stderr, "pier: stay: init fail\r\n");
    u3_king_bail();
    return 0;
  }

  u3z(pax);

  return pir_u;
}

/* u3_pier_save(): save a non-portable snapshot
*/
c3_o
u3_pier_save(u3_pier* pir_u)
{
#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: (%" PRIu64 "): save: plan\r\n", pir_u->god_u->eve_d);
#endif

  //  XX revise
  //
  if ( u3_psat_work == pir_u->sat_e ) {
    u3_lord_save(pir_u->god_u);
    return c3y;
  }

  return c3n;
}

/* u3_pier_cram(): save a portable snapshot.
*/
c3_o
u3_pier_cram(u3_pier* pir_u)
{
#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: (%" PRIu64 "): cram: plan\r\n", pir_u->god_u->eve_d);
#endif

  //  XX revise
  //
  if ( u3_psat_work == pir_u->sat_e ) {
    u3_lord_cram(pir_u->god_u);
    return c3y;
  }

  return c3n;
}

/* u3_pier_meld(): globally deduplicate persistent state.
*/
void
u3_pier_meld(u3_pier* pir_u)
{
#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: (%" PRIu64 "): meld: plan\r\n", pir_u->god_u->eve_d);
#endif

  u3_lord_meld(pir_u->god_u);
}

/* u3_pier_pack(): defragment persistent state.
*/
void
u3_pier_pack(u3_pier* pir_u)
{
#ifdef VERBOSE_PIER
  fprintf(stderr, "pier: (%" PRIu64 "): meld: plan\r\n", pir_u->god_u->eve_d);
#endif

  u3_lord_pack(pir_u->god_u);
}

/* _pier_work_close_cb(): dispose u3_work after closing handles.
*/
static void
_pier_work_close_cb(uv_handle_t* idl_u)
{
  u3_work* wok_u = idl_u->data;
  c3_free(wok_u);
}

/* _pier_work_close(): close drivers/handles in the u3_psat_work state.
*/
static void
_pier_work_close(u3_work* wok_u)
{
  u3_auto_exit(wok_u->car_u);

  uv_close((uv_handle_t*)&wok_u->pep_u, _pier_work_close_cb);
  uv_close((uv_handle_t*)&wok_u->cek_u, 0);
  uv_close((uv_handle_t*)&wok_u->idl_u, 0);
  wok_u->pep_u.data = wok_u;
}

/* _pier_done(): dispose pier.
*/
static void
_pier_free(u3_pier* pir_u)
{
  c3_free(pir_u->pax_c);
  c3_free(pir_u);
}

/* _pier_done(): graceful shutdown complete, notify king.
*/
static void
_pier_done(u3_pier* pir_u)
{
  //  XX unlink properly
  //
  u3K.pir_u = 0;
  _pier_free(pir_u);
  u3_king_done();
}

/* _pier_exit(): synchronous shutdown.
*/
static void
_pier_exit(u3_pier* pir_u)
{
  c3_assert( u3_psat_done == pir_u->sat_e );

  if ( pir_u->god_u ) {
    u3_lord_exit(pir_u->god_u);
    pir_u->god_u = 0;
  }
  else {
    //  otherwise called in _pier_on_lord_exit()
    //
    _pier_done(pir_u);
  }
}

/* _pier_work_exit(): commence graceful shutdown.
*/
static void
_pier_work_exit_cb(void* ptr_v, c3_d eve_d)
{
  u3_pier* pir_u = ptr_v;

  _pier_work_close(pir_u->wok_u);
  pir_u->wok_u = 0;

  _pier_exit(pir_u);
}

/* _pier_work_exit(): setup graceful shutdown callbacks.
*/
static void
_pier_work_exit(u3_pier* pir_u)
{
  pir_u->sat_e = u3_psat_done;

  //  XX revise, ensure worker save, run on worker exit?
  //
  _pier_work_exit_cb(pir_u, pir_u->god_u->eve_d);
}

/* u3_pier_exit(): graceful shutdown.
*/
void
u3_pier_exit(u3_pier* pir_u)
{
  switch ( pir_u->sat_e ) {
    default: {
      fprintf(stderr, "pier: unknown exit: %u\r\n", pir_u->sat_e);
      c3_assert(0);
    }

    case u3_psat_done: return;

    case u3_psat_work: return _pier_work_exit(pir_u);

    case u3_psat_init: break;

    case u3_psat_boot: {
      //  XX properly dispose boot
      //  XX also on actual boot
      //
      c3_free(pir_u->bot_u);
      pir_u->bot_u = 0;
    } break;
  }

  pir_u->sat_e = u3_psat_done;
  _pier_exit(pir_u);
}

/* u3_pier_bail(): immediately shutdown due to error.
*/
void
u3_pier_bail(u3_pier* pir_u)
{
  //  halt serf
  //
  if ( pir_u->god_u ) {
    u3_lord_halt(pir_u->god_u);
    pir_u->god_u = 0;
  }

  //  exig i/o drivers
  //
  if (  (u3_psat_work == pir_u->sat_e)
     && pir_u->wok_u )
  {
    _pier_work_close(pir_u->wok_u);
    pir_u->wok_u = 0;
  }

  pir_u->sat_e = u3_psat_done;

  _pier_done(pir_u);
}

/* c3_rand(): fill a 512-bit (16-word) buffer.
*/
void
c3_rand(c3_w* rad_w)
{
  if ( 0 != ent_getentropy(rad_w, 64) ) {
    fprintf(stderr, "c3_rand getentropy: %s\n", strerror(errno));
    //  XX review
    //
    u3_king_bail();
  }
}

/* _pier_dump_tape(): dump a tape, old style.  Don't do this.
*/
static void
_pier_dump_tape(FILE* fil_u, u3_noun tep)
{
  u3_noun tap = tep;

  while ( c3y == u3du(tap) ) {
    c3_c car_c;

    //  XX this utf-8 caution is unwarranted
    //
    //    we already write() utf8 directly to streams in term.c
    //
    // if ( u3h(tap) >= 127 ) {
    //   car_c = '?';
    // } else
    car_c = u3h(tap);

    putc(car_c, fil_u);
    tap = u3t(tap);
  }

  u3z(tep);
}

/* _pier_dump_wall(): dump a wall, old style.  Don't do this.
*/
static void
_pier_dump_wall(FILE* fil_u, u3_noun wol)
{
  u3_noun wal = wol;

  while ( u3_nul != wal ) {
    _pier_dump_tape(fil_u, u3k(u3h(wal)));

    putc(13, fil_u);
    putc(10, fil_u);

    wal = u3t(wal);
  }

  u3z(wol);
}

/* u3_pier_tank(): dump single tank.
*/
void
u3_pier_tank(c3_l tab_l, c3_w pri_w, u3_noun tac)
{
  u3_noun blu = u3_term_get_blew(0);
  c3_l  col_l = u3h(blu);
  FILE* fil_u = u3_term_io_hija();

  //  XX temporary, for urb.py test runner
  //
  if ( c3y == u3_Host.ops_u.dem ) {
    fil_u = stderr;
  }

  if ( c3n == u3_Host.ops_u.tem ) {
    switch ( pri_w ) {
        case 3: fprintf(fil_u, "\033[31m>>> "); break;
        case 2: fprintf(fil_u, "\033[33m>>  "); break;
        case 1: fprintf(fil_u, "\033[32m>   "); break;
    }
  }
  else {
    switch ( pri_w ) {
        case 3: fprintf(fil_u, ">>> "); break;
        case 2: fprintf(fil_u, ">>  "); break;
        case 1: fprintf(fil_u, ">   "); break;
    }
  }

  //  if we have no arvo kernel and can't evaluate nock
  //  only print %leaf tanks
  //
  if ( 0 == u3A->roc ) {
    if ( c3__leaf == u3h(tac) ) {
      _pier_dump_tape(fil_u, u3k(u3t(tac)));
      putc(13, fil_u);
      putc(10, fil_u);
    }
  }
  //  We are calling nock here, but hopefully need no protection.
  //
  else {
    u3_noun wol = u3dc("wash", u3nc(tab_l, col_l), u3k(tac));

    _pier_dump_wall(fil_u, wol);
  }

  if ( c3n == u3_Host.ops_u.tem ) {
    fprintf(fil_u, "\033[0m");
  }

  fflush(fil_u);

  u3_term_io_loja(0);
  u3z(blu);
  u3z(tac);
}

/* u3_pier_punt(): dump tank list.
*/
void
u3_pier_punt(c3_l tab_l, u3_noun tac)
{
  u3_noun cat = tac;

  while ( c3y == u3r_du(cat) ) {
    u3_pier_tank(tab_l, 0, u3k(u3h(cat)));
    cat = u3t(cat);
  }

  u3z(tac);
}

/* u3_pier_punt_goof(): dump a [mote tang] crash report.
*/
void
u3_pier_punt_goof(const c3_c* cap_c, u3_noun dud)
{
  u3_noun bud = dud;
  u3_noun mot, tan;

  u3x_cell(dud, &mot, &tan);

  u3l_log("\n");
  u3_pier_punt(0, u3qb_flop(tan));

  {
    c3_c* mot_c = u3r_string(mot);
    u3l_log("%s: bail: %%%s\r\n", cap_c, mot_c);
    c3_free(mot_c);
  }

  u3z(bud);
}

/* u3_pier_punt_ovum(): print ovum details.
*/
void
u3_pier_punt_ovum(const c3_c* cap_c, u3_noun wir, u3_noun tag)
{
  c3_c* tag_c = u3r_string(tag);
  u3_noun riw = u3do("spat", wir);
  c3_c* wir_c = u3r_string(riw);

  u3l_log("%s: %%%s event on %s failed\r\n\n", cap_c, tag_c, wir_c);

  c3_free(tag_c);
  c3_free(wir_c);
  u3z(riw);
}

/* u3_pier_sway(): print trace.
*/
void
u3_pier_sway(c3_l tab_l, u3_noun tax)
{
  u3_noun mok = u3dc("mook", 2, tax);

  u3_pier_punt(tab_l, u3k(u3t(mok)));
  u3z(mok);
}

/* u3_pier_mark(): mark all Loom allocations in all u3_pier structs.
*/
c3_w
u3_pier_mark(FILE* fil_u)
{
  return 0;
}
