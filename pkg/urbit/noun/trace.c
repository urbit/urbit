/* g/t.c  -
**
** This file is in the public domain.
*/
#include "all.h"
#include "vere/vere.h"
#include <pthread.h>
#include <time.h>
#include <sys/stat.h>

static c3_o _ct_lop_o;

/* u3t_push(): push on trace stack.
*/
void
u3t_push(u3_noun mon)
{
  u3R->bug.tax = u3nc(mon, u3R->bug.tax);
}

/* u3t_mean(): push `[%mean roc]` on trace stack.
*/
void
u3t_mean(u3_noun roc)
{
  u3R->bug.tax = u3nc(u3nc(c3__mean, roc), u3R->bug.tax);
}

/* u3t_drop(): drop from meaning stack.
*/
void
u3t_drop(void)
{
  c3_assert(_(u3du(u3R->bug.tax)));
  {
    u3_noun tax = u3R->bug.tax;

    u3R->bug.tax = u3k(u3t(tax));
    u3z(tax);
  }
}

extern void
u3_pier_tank(c3_l tab_l, u3_noun tac);

#ifdef U3_EVENT_TIME_DEBUG
/* _t_slog_time(): slog timelapse.
*/
void
_t_slog_time(void)
{
  static int old;
  static struct timeval b4, f2, d0;
  static c3_d b4_d;
  c3_w ms_w;

  if ( old ) {
    gettimeofday(&f2, 0);
    timersub(&f2, &b4, &d0);
    ms_w = (d0.tv_sec * 1000) + (d0.tv_usec / 1000);
    if (ms_w > 1) {
#if 0
      fprintf(stderr, "%6d.%02dms: %9d ",
              ms_w, (int) (d0.tv_usec % 1000) / 10,
              ((int) (u3R->pro.nox_d - b4_d)));
#else
      fprintf(stderr, "%6d.%02dms ",
              ms_w, (int) (d0.tv_usec % 1000) / 10);
#endif
      gettimeofday(&b4, 0);
      b4_d = u3R->pro.nox_d;
    }
    else {
      printf("            ");
    }
  }
  else {
    gettimeofday(&b4, 0);
    b4_d = u3R->pro.nox_d;
  }
  old = 1;
}
#endif

/* u3t_slog(): print directly.
*/
void
u3t_slog(u3_noun hod)
{
#ifdef U3_EVENT_TIME_DEBUG
  _t_slog_time();
#endif

  if ( c3y == u3du(hod) ) {
    u3_noun pri = u3h(hod);

    switch ( pri ) {
      case 3: fprintf(stderr, ">>> "); break;
      case 2: fprintf(stderr, ">> "); break;
      case 1: fprintf(stderr, "> "); break;
    }
    u3_pier_tank(0, u3k(u3t(hod)));
  }
  u3z(hod);
}

/* u3t_shiv(): quick print.
*/
void
u3t_shiv(u3_noun hod)
{
#ifdef U3_EVENT_TIME_DEBUG
  _t_slog_time();
#endif

  if ( c3n == u3ud(hod) ) {
  }
  else {
    c3_c *str_c = u3r_string(hod);
    fprintf(stderr, "%s\r\n", str_c);
    free(str_c);
  }
}

/* u3t_heck(): profile point.
*/
void
u3t_heck(u3_atom cog)
{
#if 0
  u3R->pro.cel_d++;
#else
  c3_w len_w = u3r_met(3, cog);
  c3_c* str_c = alloca(1 + len_w);

  u3r_bytes(0, len_w, (c3_y *)str_c, cog);
  str_c[len_w] = 0;

  //  Profile sampling, because it allocates on the home road,
  //  only works on when we're not at home.
  //
  if ( &(u3H->rod_u) != u3R ) {
    u3a_road* rod_u;

    rod_u = u3R;
    u3R = &(u3H->rod_u);
    {
      if ( 0 == u3R->pro.day ) {
        u3R->pro.day = u3v_do("doss", 0);
      }
      u3R->pro.day = u3dc("pi-heck", u3i_string(str_c), u3R->pro.day);
    }
    u3R = rod_u;
  }
#endif
}

#if 0
static void
_ct_sane(u3_noun lab)
{
  if ( u3_nul != lab ) {
    c3_assert(c3y == u3du(lab));
    c3_assert(c3y == u3ud(u3h(lab)));
    _ct_sane(u3t(lab));
  }
}
#endif

#if 1
/* _t_samp_process(): process raw sample data from live road.
*/
static u3_noun
_t_samp_process(u3_road* rod_u)
{
  u3_noun pef   = u3_nul;           // (list (pair path (map path ,@ud)))
  u3_noun muf   = u3_nul;           // (map path ,@ud)
  c3_w    len_w = 0;

  //  Accumulate a label/map stack which collapses recursive segments.
  //
  while ( rod_u ) {
    u3_noun don = rod_u->pro.don;

    while ( u3_nul != don ) {
      //  Get surface allocated label
      //
      //  u3_noun lab = u3nc(u3i_string("foobar"), 0);
      u3_noun laj = u3h(don),
              lab = u3a_take(laj);
      u3a_wash(laj);

      //  Add the label to the traced label stack, trimming recursion.
      //
      {
        u3_noun old;

        if ( u3_none == (old = u3kdb_get(u3k(muf), u3k(lab))) ) {
          muf = u3kdb_put(muf, u3k(lab), len_w);
          pef = u3nc(u3nc(lab, u3k(muf)), pef);
          len_w += 1;
        }
        else {
          u3_assure(u3a_is_cat(old));

          u3z(muf);
          while ( len_w > (old + 1) ) {
            u3_noun t_pef = u3k(u3t(pef));

            len_w -= 1;
            u3z(pef);
            pef = t_pef;
          }
          muf = u3k(u3t(u3h(pef)));
          u3z(lab);
        }
      }
      don = u3t(don);
    }
    rod_u = u3tn(u3_road, rod_u->par_p);
  }
  u3z(muf);

  //  Lose the maps and save a pure label stack in original order.
  //
  {
    u3_noun pal = u3_nul;

    while ( u3_nul != pef ) {
      u3_noun h_pef = u3h(pef);
      u3_noun t_pef = u3k(u3t(pef));

      pal = u3nc(u3k(u3h(h_pef)), pal);

      u3z(pef);
      pef = t_pef;
    }

    // fprintf(stderr, "sample: stack length %d\r\n", u3kb_lent(u3k(pal)));
    return pal;
  }
}
#endif

/* u3t_samp(): sample.
*/
void
u3t_samp(void)
{
  if ( c3y == _ct_lop_o ) {
    // _ct_lop_o here is a mutex for modifying pro.don. we
    // do not want to sample in the middle of doing that, as
    // it can cause memory errors.
    return;
  }

  c3_w old_wag = u3C.wag_w;
  u3C.wag_w &= ~u3o_debug_cpu;
  u3C.wag_w &= ~u3o_trace;

  static int home = 0;
  static int away = 0;

  //  Profile sampling, because it allocates on the home road,
  //  only works on when we're not at home.
  //
  if ( &(u3H->rod_u) != u3R ) {
    home++;
    c3_l      mot_l;
    u3a_road* rod_u;

    if ( _(u3T.mal_o) ) {
      mot_l = c3_s3('m','a','l');
    }
    else if ( _(u3T.coy_o) ) {
      mot_l = c3_s3('c','o','y');
    }
    else if ( _(u3T.euq_o) ) {
      mot_l = c3_s3('e','u','q');
    }
    else if ( _(u3T.far_o) ) {
      mot_l = c3_s3('f','a','r');
    }
    else if ( _(u3T.noc_o) ) {
      c3_assert(!_(u3T.glu_o));
      mot_l = c3_s3('n','o','c');
    }
    else if ( _(u3T.glu_o) ) {
      mot_l = c3_s3('g','l','u');
    }
    else {
      mot_l = c3_s3('f','u','n');
    }

    rod_u = u3R;
    u3R = &(u3H->rod_u);
    {
      u3_noun lab = _t_samp_process(rod_u);

      c3_assert(u3R == &u3H->rod_u);
      if ( 0 == u3R->pro.day ) {
        /* bunt a +doss
        */
        u3R->pro.day = u3nt(u3nq(0, 0, 0, u3nq(0, 0, 0, 0)), 0, 0);
      }
      u3R->pro.day = u3dt("pi-noon", mot_l, lab, u3R->pro.day);
    }
    u3R = rod_u;
  }
  else {
    away++;
    // fprintf(stderr,"home: %06d away: %06d\r\n", home, away);
  }
  u3C.wag_w = old_wag;
}

/* u3t_come(): push on profile stack; return yes if active push.  RETAIN.
*/
c3_o
u3t_come(u3_noun lab)
{
  if ( (u3_nul == u3R->pro.don) || !_(u3r_sing(lab, u3h(u3R->pro.don))) ) {
    u3a_gain(lab);
    _ct_lop_o = c3y;
    u3R->pro.don = u3nc(lab, u3R->pro.don);
    _ct_lop_o = c3n;
    return c3y;
  }
  else return c3n;
}

/* u3t_flee(): pop off profile stack.
*/
void
u3t_flee(void)
{
  _ct_lop_o = c3y;
  u3_noun don  = u3R->pro.don;
  u3R->pro.don = u3k(u3t(don));
  _ct_lop_o = c3n;
  u3z(don);
}

/*  u3t_trace_open(): opens a trace file and writes the preamble.
*/
void
u3t_trace_open()
{
  c3_c fil_c[2048];
  snprintf(fil_c, 2048, "%s/.urb/put/trace", u3C.dir_c);

  struct stat st;
  if ( -1 == stat(fil_c, &st) ) {
    mkdir(fil_c, 0700);
  }

  c3_c lif_c[2048];
  snprintf(lif_c, 2048, "%s/%d.json", fil_c, u3_Host.tra_u.fun_w);

  u3_Host.tra_u.fil_u = fopen(lif_c, "w");
  u3_Host.tra_u.nid_w = (int)getpid();

  fprintf(u3_Host.tra_u.fil_u, "[ ");

  // We have two "threads", the event processing and the nock stuff.
  //   tid 1 = event processing
  //   tid 2 = nock processing
  fprintf(u3_Host.tra_u.fil_u,
      "{\"name\": \"process_name\", \"ph\": \"M\", \"pid\": %d, \"args\": "
      "{\"name\": \"urbit\"}},\n",
      u3_Host.tra_u.nid_w);
  fprintf(u3_Host.tra_u.fil_u,
          "{\"name\": \"thread_name\", \"ph\": \"M\", \"pid\": %d, \"tid\": 1, "
          "\"args\": {\"name\": \"Event Processing\"}},\n",
          u3_Host.tra_u.nid_w);
  fprintf(u3_Host.tra_u.fil_u,
          "{\"name\": \"thread_sort_index\", \"ph\": \"M\", \"pid\": %d, "
          "\"tid\": 1, \"args\": {\"sort_index\": 1}},\n",
          u3_Host.tra_u.nid_w);
  fprintf(u3_Host.tra_u.fil_u,
          "{\"name\": \"thread_name\", \"ph\": \"M\", \"pid\": %d, \"tid\": 2, "
          "\"args\": {\"name\": \"Nock\"}},\n",
          u3_Host.tra_u.nid_w);
  fprintf(u3_Host.tra_u.fil_u,
          "{\"name\": \"thread_sort_index\", \"ph\": \"M\", \"pid\": %d, "
          "\"tid\": 2, \"args\": {\"sort_index\": 2}},\n",
          u3_Host.tra_u.nid_w);
  u3_Host.tra_u.con_w = 5;
}

/*  u3t_trace_close(): closes a trace file. optional.
*/
void
u3t_trace_close()
{
  if (!u3_Host.tra_u.fil_u)
    return;

  // We don't terminate the JSON because of the file format.
  fclose(u3_Host.tra_u.fil_u);
  u3_Host.tra_u.con_w = 0;
  u3_Host.tra_u.fun_w++;
}

/*  u3t_trace_time(): microsecond clock
*/
c3_d u3t_trace_time()
{
  struct timeval tim_tv;
  gettimeofday(&tim_tv, 0);
  return 1000000ULL * tim_tv.tv_sec + tim_tv.tv_usec;
}

/* u3t_nock_trace_push(): push a trace onto the trace stack; returns yes if pushed.
 *
 * The trace stack is a stack of [path time-entered].
 */
c3_o
u3t_nock_trace_push(u3_noun lab)
{
  if (!u3_Host.tra_u.fil_u)
    return c3n;

  if ( (u3_nul == u3R->pro.trace) ||
       !_(u3r_sing(lab, u3h(u3h(u3R->pro.trace)))) ) {
    u3a_gain(lab);
    c3_d time = u3t_trace_time();
    u3R->pro.trace = u3nc(u3nc(lab, u3i_chubs(1, &time)), u3R->pro.trace);
    return c3y;
  }
  else {
    return c3n;
  }
}

/* u3t_nock_trace_pop(): pops a trace from the trace stack.
 *
 * When we remove the trace from the stack, we check to see if the sample is
 * large enough to process, as we'll otherwise keep track of individual +add
 * calls. If it is, we write it out to the tracefile.
 */
void
u3t_nock_trace_pop()
{
  if (!u3_Host.tra_u.fil_u)
    return;

  u3_noun trace  = u3R->pro.trace;
  u3R->pro.trace = u3k(u3t(trace));

  u3_noun item = u3h(trace);
  u3_noun lab = u3h(item);
  c3_d start_time = u3r_chub(0, u3t(item));

  // 33microseconds (a 30th of a millisecond).
  c3_d duration = u3t_trace_time() - start_time;
  if (duration > 33) {
    c3_c* name = u3m_pretty_path(lab);

    fprintf(u3_Host.tra_u.fil_u,
            "{\"cat\": \"nock\", \"name\": \"%s\", \"ph\":\"%c\", \"pid\": %d, "
            "\"tid\": 2, \"ts\": %" PRIu64 ", \"dur\": %" PRIu64 "}, \n",
            name,
            'X',
            u3_Host.tra_u.nid_w,
            start_time,
            duration);

    free(name);
    u3_Host.tra_u.con_w++;
  }

  u3z(trace);
}

/* u3t_event_trace(): dumps a simple event from outside nock.
*/
void
u3t_event_trace(const c3_c* name, c3_c type)
{
  if (!u3_Host.tra_u.fil_u)
    return;

  fprintf(u3_Host.tra_u.fil_u,
          "{\"cat\": \"event\", \"name\": \"%s\", \"ph\":\"%c\", \"pid\": %d, "
          "\"tid\": 1, \"ts\": %" PRIu64 ", \"id\": \"0x100\"}, \n",
          name,
          type,
          getpid(),
          u3t_trace_time());
  u3_Host.tra_u.con_w++;
}

extern FILE*
u3_term_io_hija(void);

extern void
u3_term_io_loja(int x);

extern void
u3_term_tape(u3_noun tep);

extern void
u3_term_wall(u3_noun wol);

/* u3t_print_steps: print step counter.
*/
void
u3t_print_steps(c3_c* cap_c, c3_d sep_d)
{
  FILE* fil_f = u3_term_io_hija();

  c3_w gib_w = (sep_d / 1000000000ULL);
  c3_w mib_w = (sep_d % 1000000000ULL) / 1000000ULL;
  c3_w kib_w = (sep_d % 1000000ULL) / 1000ULL;
  c3_w bib_w = (sep_d % 1000ULL);

  if ( sep_d ) {
    if ( gib_w ) {
      fprintf(fil_f, "%s: G/%d.%03d.%03d.%03d\r\n",
          cap_c, gib_w, mib_w, kib_w, bib_w);
    }
    else if ( mib_w ) {
      fprintf(fil_f, "%s: M/%d.%03d.%03d\r\n", cap_c, mib_w, kib_w, bib_w);
    }
    else if ( kib_w ) {
      fprintf(fil_f, "%s: K/%d.%03d\r\n", cap_c, kib_w, bib_w);
    }
    else if ( bib_w ) {
      fprintf(fil_f, "%s: %d\r\n", cap_c, bib_w);
    }
  }
  u3_term_io_loja(0);
}

/* u3t_damp(): print and clear profile data.
*/
void
u3t_damp(void)
{
  if ( 0 != u3R->pro.day ) {
    u3_noun wol = u3do("pi-tell", u3R->pro.day);

    fprintf(stderr, "\r\n");
    u3_term_wall(wol);

    /* bunt a +doss
    */
    u3R->pro.day = u3nt(u3nq(0, 0, 0, u3nq(0, 0, 0, 0)), 0, 0);
  }

  u3t_print_steps("nocks", u3R->pro.nox_d);
  u3t_print_steps("cells", u3R->pro.cel_d);

  u3R->pro.nox_d = 0;
  u3R->pro.cel_d = 0;
}

/* _ct_sigaction(): profile sigaction callback.
*/
void _ct_sigaction(c3_i x_i)
{
  // fprintf(stderr, "itimer!\r\n"); abort();
  u3t_samp();
}

/* u3t_init(): initialize tracing layer.
*/
void
u3t_init(void)
{
  u3T.noc_o = c3n;
  u3T.glu_o = c3n;
  u3T.mal_o = c3n;
  u3T.far_o = c3n;
  u3T.coy_o = c3n;
  u3T.euq_o = c3n;
}

/* u3t_boot(): turn sampling on.
*/
void
u3t_boot(void)
{
  if ( u3C.wag_w & u3o_debug_cpu ) {
    _ct_lop_o = c3n;
#if defined(U3_OS_osx) || defined(U3_OS_linux)
    //  skip profiling if we don't yet have an arvo kernel
    //
    if ( 0 == u3A->roc ) {
      return;
    }

    // Register _ct_sigaction to be called on `SIGPROF`.
    {
      struct sigaction sig_s = {{0}};
      sig_s.sa_handler = _ct_sigaction;
      sigemptyset(&(sig_s.sa_mask));
      sigaction(SIGPROF, &sig_s, 0);
    }

    // Unblock `SIGPROF` for this thread (we will block it again when `u3t_boff` is called).
    {
      sigset_t set;
      sigemptyset(&set);
      sigaddset(&set, SIGPROF);
      if ( 0 != pthread_sigmask(SIG_UNBLOCK, &set, NULL) ) {
        perror("pthread_sigmask");
      }
    }

    // Ask for SIGPROF to be sent every 10ms.
    {
      struct itimerval itm_v = {{0}};
      itm_v.it_interval.tv_usec = 10000;
      itm_v.it_value = itm_v.it_interval;
      setitimer(ITIMER_PROF, &itm_v, 0);
    }
#elif defined(U3_OS_bsd)
    // XX  "Profiling isn't yet supported on BSD"
#else
#   error "port: profiling"
#endif
  }
}

/* u3t_boff(): turn profile sampling off.
*/
void
u3t_boff(void)
{
  if ( u3C.wag_w & u3o_debug_cpu ) {
#if defined(U3_OS_osx) || defined(U3_OS_linux)
    // Mask SIGPROF signals in this thread (and this is the only
    // thread that unblocked them).
    {
      sigset_t set;
      sigemptyset(&set);
      sigaddset(&set, SIGPROF);
      if ( 0 != pthread_sigmask(SIG_BLOCK, &set, NULL) ) {
        perror("pthread_sigmask");
      }
    }

    // Disable the SIGPROF timer.
    {
      struct itimerval itm_v = {{0}};
      setitimer(ITIMER_PROF, &itm_v, 0);
    }

    // Ignore SIGPROF signals.
    {
      struct sigaction sig_s = {{0}};
      sigemptyset(&(sig_s.sa_mask));
      sig_s.sa_handler = SIG_IGN;
      sigaction(SIGPROF, &sig_s, 0);
    }

#elif defined(U3_OS_bsd)
    // XX  "Profiling isn't yet supported on BSD"
#else
#   error "port: profiling"
#endif
  }
}
