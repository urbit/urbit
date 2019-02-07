/* vere/serf.c
**
**  the main loop of a worker process.
*/
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
#include <curses.h>
#include <termios.h>
#include <term.h>

#include "all.h"
#include <vere/vere.h>

    typedef struct _u3_serf {
      c3_d    evt_d;                        //  last event processed
      c3_l    mug_l;                        //  hash of state
      c3_d    key_d[4];                     //  disk key
      u3_moat inn_u;                        //  message input
      u3_mojo out_u;                        //  message output
    } u3_serf;
    static u3_serf u3V;

    /*  serf-lord protocol:
    **
    **  ++  plea                            ::  from serf to lord
    **    $%  $:  $play                     ::  send events
    **            $=  p                     ::
    **            %-  unit                  ::  ~ if no snapshot
    **            $:  p=@                   ::  first number expected
    **                q=@                   ::  mug of state
    **                r=[our=@p fak=?]      ::  [identity fake?]
    **        ==  ==                        ::
    **        $:  $done                     ::  event executed unchanged
    **            p/@                       ::  number of this event
    **            q/@                       ::  mug of state (or 0)
    **            r/(list ovum)             ::  actions
    **        ==                            ::
    **        $:  $work                     ::  replace and retry
    **            p/@                       ::  event number
    **            q/@                       ::  mug of state (or 0)
    **            r/(pair date ovum)        ::  event
    **    ==  ==                            ::
    **
    **  ++  writ                            ::  from lord to serf
    **    $%  $:  $exit                     ::  snapshot, then exit
    **            p/@                       ::  exit code
    **        ==                            ::
    **        $:  $save                     ::  save snapshot to disk
    **            p/@                       ::  number of old snaps to save
    **        ==                            ::
    **        $:  $work                     ::  execute event
    **            p/@                       ::  event number
    **            q/@                       ::  mug of state (or 0)
    **            r/(pair date ovum)        ::  event
    **    ==  ==                            ::
    */

/* _serf_fail(): failure stub.
*/
static void
_serf_fail(void* vod_p, const c3_c* wut_c)
{
  fprintf(stderr, "serf: fail: %s\r\n", wut_c);
  exit(1);
}

/* _serf_send(): send result back to lord.
*/
static void
_serf_send(u3_noun job)
{
  u3_newt_write(&u3V.out_u, u3ke_jam(job), 0);
}

/* _serf_send_replace(): send replacement job back to lord.
*/
static void
_serf_send_replace(c3_d evt_d, u3_noun ovo)
{
  fprintf(stderr, "serf_send_replace %lld %s\r\n", 
                  evt_d,
                  u3r_string(u3h(u3t(ovo)))); 

  _serf_send(u3nq(c3__work,
                  u3i_chubs(1, &evt_d),
                  u3V.mug_l,
                  u3nc(u3k(u3A->now), ovo)));
}

/* _serf_send_complete(): report completion.
*/
static void
_serf_send_complete(u3_noun vir)
{
  _serf_send(u3nq(c3__done,
                  u3i_chubs(1, &u3V.evt_d),
                  u3r_mug(u3A->roc),
                  vir));
}

/* _serf_lame(): event failed, replace with error event.
*/
static void
_serf_lame(c3_d evt_d, u3_noun ovo, u3_noun why, u3_noun tan)
{
  /* XX: the next crud will contain the original event.
  */
  u3z(ovo);
  _serf_send_replace(evt_d, u3nc(u3k(u3h(ovo)), u3nt(c3__crud, why, tan)));
}

/* _serf_sure(): event succeeded, report completion.
*/
static void
_serf_sure(u3_noun ovo, u3_noun vir, u3_noun cor)
{
  u3z(ovo);

  u3z(u3A->roc);
  u3A->roc = cor;

  _serf_send_complete(vir);
}

/* _serf_poke_live(): apply event.
*/
static void
_serf_poke_live(c3_d    evt_d,              //  event number
                c3_l    mug_l,              //  mug of state
                u3_noun job)                //  event date
{
  u3_noun now = u3k(u3h(job));
  u3_noun ovo = u3k(u3t(job));

  c3_assert(evt_d == u3V.evt_d + 1ULL);

  u3z(job);
  {
    u3_noun gon;

    if ( mug_l ) {
      c3_assert(u3r_mug(u3A->roc) == mug_l);
    }

    u3z(u3A->now);
    u3A->now = now;

    u3A->ent_d = evt_d;

#ifdef U3_EVENT_TIME_DEBUG
    struct timeval b4, f2, d0;
    gettimeofday(&b4, 0);

    if ( c3__belt != u3h(u3t(ovo)) ) {
      c3_c* txt_c = u3r_string(u3h(u3t(ovo)));

      fprintf(stderr, "serf: %s (%lld) live\r\n", txt_c, evt_d);
    }
#endif
  
    gon = u3m_soft(0, u3v_poke, u3k(ovo));

#ifdef U3_EVENT_TIME_DEBUG
    c3_c* txt_c = u3r_string(u3h(u3t(ovo)));
    c3_w ms_w;
    c3_w clr_w;

    gettimeofday(&f2, 0);
    timersub(&f2, &b4, &d0);
    ms_w = (d0.tv_sec * 1000) + (d0.tv_usec / 1000);
    clr_w = ms_w > 1000 ? 1 : ms_w < 100 ? 2 : 3; //  red, green, yellow
    if (c3__belt != u3h(u3t(ovo)) || clr_w != 2) {
      uL(fprintf(uH, "\x1b[3%dm%%%s (%lld) %4d.%02dms\x1b[0m\n",
                        clr_w, txt_c, evt_d, ms_w, 
                        (int) (d0.tv_usec % 1000) / 10));
    }
    free(txt_c);
#endif

    if ( u3_blip != u3h(gon) ) {
      //
      //  event rejected
      //  
      u3_noun why = u3k(u3h(gon));
      u3_noun tan = u3k(u3t(gon));

      u3z(gon);
      _serf_lame(evt_d, ovo, why, tan);
    }
    else {
      //  event accepted
      //
      u3V.evt_d = evt_d;
      {
        //  vir/(list ovum)  list of effects
        //  cor/arvo         arvo core
        //
        u3_noun vir = u3k(u3h(u3t(gon)));
        u3_noun cor = u3k(u3t(u3t(gon)));

        //  single-home
        //
        //    XX revise when real keys are supported
        //    XX dispatch on evt_d, wire, or card tag?
        //
        if ( c3__boot == u3h(u3t(ovo)) ) {
          //  ovo=[%boot *]
          //  vir=[[wire %init @p] ~]
          //  fec=[%init @p]
          //
          u3_noun fec = u3t(u3h(vir));

          c3_assert( c3__init == u3h(fec) );
          c3_assert( u3_none == u3A->our );

          u3A->our = u3k(u3t(fec));
          u3A->fak = ( c3__fake == u3h(u3t(u3t(ovo))) ) ? c3y : c3n;

          {
            u3_noun nam = u3dc("scot", 'p', u3k(u3A->our));
            c3_c* nam_c = u3r_string(nam);
            fprintf(stderr, "boot: ship: %s%s\r\n", nam_c,
                                            (c3y == u3A->fak) ? " (fake)" : "");
            free(nam_c);
            u3z(nam);
          }
        }

        _serf_sure(ovo, vir, cor);
      }
    }
  }
}

/* _serf_boot_fire(): execute boot sequence.
*/
static u3_noun
_serf_boot_fire(u3_noun eve)
{
  u3_noun cor = u3n_nock_on(eve, u3nt(2, u3nc(0, 3), u3nc(0, 2)));
  u3_noun pro;

  pro = u3k(u3r_at(7, cor));

  u3z(cor);
  return pro;
}

/* _serf_poke_boot(): apply initial-stage event.
*/
static void
_serf_poke_boot(c3_d    evt_d,
                c3_l    mug_l,
                u3_noun job)
{
  u3A->roe = u3nc(job, u3A->roe);

  c3_assert(evt_d == u3V.evt_d + 1ULL);
  u3V.evt_d = evt_d;
  fprintf(stderr, "serf: (%lld)| boot\r\n", evt_d);

  if ( evt_d == 5 ) {
    u3_noun eve = u3kb_flop(u3A->roe);
    u3_noun pru;
    
    u3A->roe = 0;
    fprintf(stderr, "serf: (5)| pill: %x\r\n", u3r_mug(eve));

    pru = u3m_soft(0, _serf_boot_fire, eve);
    if ( u3h(pru) != 0 ) {
      fprintf(stderr, "boot failed\r\n");
      exit(1);
    }

    fprintf(stderr, "serf: (5)| core: %x\r\n", u3r_mug(u3t(pru)));

    u3A->roc = u3k(u3t(pru));

    u3z(pru);
  }
  _serf_send(u3nq(c3__done,
                  u3i_chubs(1, &evt_d),
                  0,
                  u3_nul));
}

/* _serf_poke_work(): apply event.
*/
static void
_serf_poke_work(c3_d    evt_d,              //  event number
                c3_l    mug_l,              //  mug of state
                u3_noun job)                //  full event
{
  if ( evt_d < 6 ) {
    _serf_poke_boot(evt_d, mug_l, job);
  }
  else { 
    _serf_poke_live(evt_d, mug_l, job);
  }
}

/* _serf_poke_exit(): exit on command.
*/
static void
_serf_poke_exit(c3_w cod_w)                 //  exit code
{
  exit(cod_w);
}

/* _serf_poke(): 
*/
void
_serf_poke(void* vod_p, u3_noun mat)
{
  u3_noun jar = u3ke_cue(mat);

  if ( c3y != u3du(jar) ) {
    goto error;
  }
  else {
    u3_noun p_jar, q_jar, r_jar;

    switch ( u3h(jar) ) {
      case c3__work: {
        if ( (c3n == u3r_qual(jar, 0, &p_jar, &q_jar, &r_jar)) || 
             (c3n == u3ud(p_jar)) ||
             (u3r_met(6, p_jar) != 1) ||
             (c3n == u3ud(q_jar)) ||
             (u3r_met(5, q_jar) > 1) )
        {
          goto error;
        }
        _serf_poke_work(u3r_chub(0, p_jar),
                        u3r_word(0, q_jar),
                        u3k(r_jar));
        break;
      }
      case c3__exit: {
        if ( (c3n == u3r_cell(jar, 0, &p_jar)) || 
             (c3n == u3ud(p_jar)) ||
             (u3r_met(3, p_jar) > 1) )
        {
          goto error;
        }
        _serf_poke_exit(u3k(p_jar));
        break;
      }
      case c3__save: {
        if ( (c3n == u3r_cell(jar, 0, &p_jar)) || 
             (c3n == u3ud(p_jar)) ) {
          goto error;
        }
        fprintf(stderr, "serf: save\r\n");
        u3e_save();
        break;
      }
      default: {
        goto error;
      }
    }
    return;
  }

  error: {
    _serf_fail(0, "bad jar");
  }
  u3z(jar);
}

/* u3_serf_boot(): send startup message to manager.
*/
void
u3_serf_boot(void)
{
  c3_d nex_d  = 1ULL;
  u3_noun dat = u3_nul;

  if ( u3_none != u3A->our ) {
    nex_d = u3A->ent_d + 1ULL;
    dat   = u3nc(u3_nul, u3nt(u3i_chubs(1, &nex_d),
                              0, // XX u3r_mug(u3A->roc),
                              u3nc(u3k(u3A->our), u3k(u3A->fak))));

    /* disable hashboard for fake ships
    */
    if ( c3y == u3A->fak ) {
      u3C.wag_w |= u3o_hashless;
    }
  }

  fprintf(stderr, "serf: play %lld\r\n", nex_d);

  _serf_send(u3nc(c3__play, dat));
}

/* main(): main() when run as urbit-worker
*/
c3_i
main(c3_i argc, c3_c* argv[])
{
  uv_loop_t* lup_u = uv_default_loop();
  c3_c*      dir_c = argv[1];
  c3_c*      key_c = argv[2];
  c3_c*      wag_c = argv[3];

  c3_assert(4 == argc);

  /* load passkey
  */
  {
    sscanf(key_c, "%llx:%llx:%llx:%llx", &u3V.key_d[0],
                                         &u3V.key_d[1],
                                         &u3V.key_d[2], 
                                         &u3V.key_d[3]);
  }

  /* load runtime config
  */
  {
    sscanf(wag_c, "%" SCNu32, &u3C.wag_w);
  }

  /* boot image
  */
  {
    u3V.evt_d = u3m_boot_new(dir_c);
  }

  /* configure pipe to lord process
  */
  {
    c3_i err_i;

    err_i = uv_pipe_init(lup_u, &u3V.inn_u.pyp_u, 0);
    c3_assert(!err_i);
    uv_pipe_open(&u3V.inn_u.pyp_u, 0);

    err_i = uv_pipe_init(lup_u, &u3V.out_u.pyp_u, 0);
    c3_assert(!err_i);
    uv_pipe_open(&u3V.out_u.pyp_u, 1);
  }

  /* set up writing
  */
  u3V.out_u.bal_f = _serf_fail;

  /* start reading
  */
  u3V.inn_u.vod_p = &u3V;
  u3V.inn_u.pok_f = _serf_poke;
  u3V.inn_u.bal_f = _serf_fail;

  u3_newt_read(&u3V.inn_u);

  /* send start request
  */
  u3_serf_boot();

  /* enter loop
  */
  uv_run(lup_u, UV_RUN_DEFAULT);
  return 0;
}
